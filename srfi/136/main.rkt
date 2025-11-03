#lang typed/racket/base

(require "private/types.rkt"
         "private/define-record-type.rkt"
         "private/type-descriptors.rkt")

(provide define-record-type
         record
         record?
         Record-TypeTop
         record-type-descriptor?
         record-type-descriptor
         record-type-parent
         record-type-name
         record-type-constructor
         record-type-predicate
         record-type-fields
         make-record-type-descriptor
         make-record)
