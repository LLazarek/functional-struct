#lang typed/racket

(provide struct)

(require syntax/parse/define
         (for-syntax "util.rkt"
                     racket/list
                     racket/match))

(define-syntax-parse-rule (struct name:id
                            {~optional super:id}
                            ([field-name:id {~datum :} field-type] ...)
                            #:type-name type-name:id
                            {~optional {~and mutable-kw #:mutable}})

  #:do [(define field-ids (syntax->list #'(field-name ...)))]

  #:with this-make-struct (gensym)
  #:with this-struct->predicate (gensym)
  #:with [this-struct->generic-accessor ...] (build-list (length field-ids)
                                                   (λ _ (gensym)))
  #:with [this-struct->generic-mutator ...] (build-list (length field-ids)
                                                   (λ _ (gensym)))


  #:with (accessor ...) (map (build-id #'name 'accessor)
                             field-ids)
  #:with (mutator ...) (map (build-id #'name 'mutator)
                            field-ids)
  #:with predicate ((build-id #'name 'predicate))
  #:with key (gensym (syntax->datum #'name))
  #:with tmp-id (build-tmp-id #'name)
  #:with [mutator-def ...] (if (attribute mutable-kw)
                               #`(begin
                                   (define (mutator a-struct v)
                                     ((this-struct->generic-mutator a-struct) 'field-name v))
                                   ...)
                               #'[])

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

  #:with constructor-type #'(-> field-type ... type-name)
  #:with [field-accessor-type ...] (for/list ([t (in-list (attribute field-type))])
                                     #`(-> type-name #,t))
  #:with [field-mutator-type ...] (if (attribute mutable-kw)
                                      (for/list ([t (in-list (attribute field-type))])
                                        #`(-> type-name #,t void))
                                      '())
  #:with predicate-type #'(-> Any Boolean : type-name)
  #:with struct-info-id ((build-id #'name 'struct-info-id))
  #:with parent-struct-info-id (if (attribute super)
                                   ((build-id #'super 'struct-info-id))
                                   #f)
  #:with parent-struct-info-id-stx (if (attribute super)
                                       #'#'parent-struct-info-id
                                       #'#f)

  #:with [[super-accessor-name/reversed ...]
          [super-mutator-name/reversed ...]] (if (attribute super)
                                                 (match (syntax-local-value #'parent-struct-info-id
                                                                             void)
                                                   [(list _ _ _ accessor-names mutator-names _)
                                                    (list accessor-names mutator-names)])
                                                 (list empty empty))

  #:with mutable-bool (if (attribute mutable-kw)
                          #'#t
                          #'#f)

  (begin

    (define-type type-name (List predicate-type
                                 Any
                                 Any
                                 Any ; <super-type?>
                                 constructor-type
                                 Any))
    (: name constructor-type)
    (: field-accessor-name field-accessor-type)
    ...
    (: field-mutator-name field-mutator-type)
    ...
    (: predicate predicate-type)

    (require/typed "struct-lib.rkt"
      [(make-struct this-make-struct)
       (-> Any
           Any
           Any
           Any
           constructor-type
           #;(List constructor-type
                 predicate-type
                 field-accessor-type ...
                 field-mutator-type ...))]
      [(struct->predicate this-struct->predicate)
       (-> Any (-> Any Boolean))]
      [(struct->generic-accessor this-struct->generic-accessor)
       (-> type-name (-> Any field-type))]
      ...
      [(struct->generic-mutator this-struct->generic-mutator)
       (-> type-name (-> Any field-type Void))]
      ...)

    (define tmp-id
      (this-make-struct 'key
                        {~? super #f}
                        '(field-name ...)
                        mutable-bool))

    (define-syntax struct-info-id
      (list #f
            #'tmp-id
            #'predicate
            (append (reverse (list #'field-accessor-name ...))
                    (list #'super-accessor-name/reversed ...))
            (append (reverse (list #'field-mutator-name ...))
                    (list  #'super-mutator-name/reversed ...))
            parent-struct-info-id-stx))


    (define-match-expander name
      (λ (stx)
        (syntax-parse stx
          [(_ field-pat (... ...))
           #'(? predicate
                (app
                 struct->list
                 (list field-pat (... ...))))]))
      (λ (stx)
        (syntax-parse stx
          [stx:id #'tmp-id]
          [(_ args (... ...))
           #`(tmp-id args (... ...))])))

    (define (predicate a-struct) ((this-struct->predicate a-struct) 'key))

    (define (accessor a-struct) ((this-struct->generic-accessor a-struct) 'field-name))
    ...

    mutator-def
    ...))
