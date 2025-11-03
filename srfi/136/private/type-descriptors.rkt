#lang typed/racket/base

(require "types.rkt"
         (rename-in typed/racket/base/optional
                    [cast unsafe-cast]))

(provide Record-TypeTop std->rtd
         <record>
         record-type-descriptor?
         record-type-descriptor
         record-type-parent
         record-type-name
         record-type-constructor
         record-type-predicate
         record-type-fields
         make-record-type-descriptor
         make-record)


(define-new-subtype Record-TypeTop (std->rtd Struct-TypeTop))

(define record:record (unsafe-cast struct:record Record-TypeTop))
(define-syntax <record>
  (syntax-rules ()
    [(_) record:record]
    [(_ (f e ...)) (f e ... #f)]))

(define record-type-descriptor?
  (unsafe-cast
   (λ (v)
     (or (eq? v record:record)
         (and (struct-type? v)
              (record-type-descriptor? (record-type-parent v)))))
   (pred Record-TypeTop)))

(: record-type-descriptor (→ record Record-TypeTop))
(define (record-type-descriptor rt)
  (define-values (struct-type skipped?)
    (parameterize ([current-inspector record-inspector])
      (struct-info rt)))
  (std->rtd (assert struct-type)))

(: record-type-parent (→ Record-TypeTop (Option Record-TypeTop)))
(define (record-type-parent rtd)
  (define-values (name
                  init-field-cnt
                  auto-field-cnt
                  accessor-proc
                  mutator-proc
                  immutable-k-list
                  super-type
                  skipped?)
    (parameterize ([current-inspector record-inspector])
      (struct-type-info rtd)))
  (and (not (eq? super-type record:record))
       (std->rtd (assert super-type))))

(: record-type-name (→ Record-TypeTop Symbol))
(define (record-type-name rtd)
  (define-values (name
                  init-field-cnt
                  auto-field-cnt
                  accessor-proc
                  mutator-proc
                  immutable-k-list
                  super-type
                  skipped?)
    (parameterize ([current-inspector record-inspector])
      (struct-type-info rtd)))
  name)

(: record-type-constructor (→ Record-TypeTop Procedure))
(define (record-type-constructor rtd)
  (parameterize ([current-inspector record-inspector])
    (struct-type-make-constructor rtd)))

(: record-type-predicate (→ Record-TypeTop Procedure))
(define (record-type-predicate rtd)
  (parameterize ([current-inspector record-inspector])
    (struct-type-make-predicate rtd)))

(define unknown (gensym 'unknown))
(: record-type-fields
   (→ Record-TypeTop
      (Listof (List Symbol Procedure Procedure))))
(define (record-type-fields rtd)
  (define-values (name
                  init-field-cnt
                  auto-field-cnt
                  accessor-proc
                  mutator-proc
                  immutable-k-list
                  super-type
                  skipped?)
    (parameterize ([current-inspector record-inspector])
      (struct-type-info rtd)))
  (for/list ([i : Natural (in-range init-field-cnt)])
    (define name unknown)               ; FIXME no `unknown'
    (define (get [rt : record])
      (((unsafe-cast accessor-proc (→ record Integer (→ Any)))
        rt i)))
    (define (set [rt : record] v)
      (((unsafe-cast accessor-proc (→ record Integer (→ Any Void)))
        rt i)
       v))
    (list name get set)))

(: make-record-type-descriptor
   (→* (Symbol (Listof (∪ Symbol (List (∪ 'immutable 'mutable) Symbol))))
       (Record-TypeTop)
       Record-TypeTop))
(define (make-record-type-descriptor name fieldspecs [super-type record:record])
  (define-values (this-type
                  constructor-proc
                  predicate-proc
                  accessor-proc
                  mutator-proc)
    (make-struct-type name super-type (length fieldspecs) 0 #f '() record-type-inspector))
  (std->rtd this-type))

(: make-record (→ Record-TypeTop VectorTop record))
(define (make-record rkd field-vector)
  (define constructor-proc (unsafe-cast (record-type-constructor rkd) (→ Any * record)))
  (apply constructor-proc (vector->list field-vector)))
