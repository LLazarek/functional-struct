#lang typed/racket

(provide struct)

(require (rename-in (only-in "struct-lib.rkt" struct)
                    [struct new-struct])
         syntax/parse/define
         (for-syntax "util.rkt"
                     racket/list))

(define-syntax-parse-rule (struct name:id
                            {~optional super:id}
                            ([field-name:id {~datum :} field-type] ...)
                            #:type-name type-name:id
                            {~optional {~and mutable-kw #:mutable}})
  #:do [(when (equal? (syntax->datum #'name) (syntax->datum #'type-name))
          (raise-syntax-error 'struct
                              "struct name and its type name must be different"
                              this-syntax))]

  #:with [field-accessor-name ...] (map (build-id #'name 'accessor)
                                        (syntax->list #'(field-name ...)))
  #:with [field-mutator-name ...] (if (attribute mutable-kw)
                                      (map (build-id #'name 'mutator)
                                           (syntax->list #'(field-name ...)))
                                      '())
  #:with predicate ((build-id #'name 'predicate) #f)

  #:with constructor-type #'(-> field-type ... type-name)
  #:with [field-accessor-type ...] (for/list ([t (in-list (attribute field-type))])
                                     #`(-> type-name #,t))
  #:with [field-mutator-type ...] (if (attribute mutable-kw)
                                      (for/list ([t (in-list (attribute field-type))])
                                        #`(-> type-name #,t void))
                                      '())
  #:with predicate-type #'(-> Any Boolean : type-name)

  (begin
    (new-struct name {~? super} (field-name ...) {~? mutable-kw})
    (define-type type-name (List predicate-type
                                 Any
                                 Any
                                 Any
                                 constructor-type
                                 Any))
    (: name constructor-type)
    (: field-accessor-name field-accessor-type)
    ...
    (: field-mutator-name field-mutator-type)
    ...
    (: predicate predicate-type)))
