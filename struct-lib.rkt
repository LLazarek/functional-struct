#lang racket

(provide struct-copy
         struct->list
         struct->predicate
         struct->generic-accessor
         struct->generic-mutator
         struct->field-names
         struct->constructor
         make-struct)

(require (for-syntax syntax/parse
                     racket/base
                     racket/struct-info
                     racket/syntax)
         syntax/parse/define)

;; todo: switch to use struct-info to get (struct-out ...) working.

;; struct-type/c: (-> (listof any/c) (-> (listof procedure?)))

(define (struct->predicate an-instance)
  (first an-instance))

(define (struct->generic-accessor an-instance)
  (second an-instance))

(define (struct->generic-mutator an-instance)
  (third an-instance))

;; (-> struct-type/c (listof any/c))
(define (struct->list an-instance)
  ((fourth an-instance)))

;; (-> struct-type/c (-> (listof any/c) struct-type/c)
(define (struct->constructor an-instance)
  (fifth an-instance))

(define (struct->field-names an-instance)
  (sixth an-instance))

(define-simple-macro (struct-copy struct-name e [field:id new-field-v] ...)
  (let ([v e]
        [update-map (hash {~@ 'field new-field-v} ...)])
    (apply struct-name
           (for/list ([a-field (in-list (struct->field-names v))]
                      [a-value (in-list (struct->list v))])
             (hash-ref update-map a-field (thunk a-value))))))

(define base-struct-instance
  (list
   (λ (instance) #f)
   (λ (field-name)
     (error (format "field ~a not found" field-name)))
   (λ (field-name field-value)
     (error
      (format "either struct is not muttable or field ~a not found"
              field-name)))
   (thunk empty)
   (λ field-values
     (error
      (format "base struct constructor doesn't exist")))
   empty))

;; (-> symbol? (or/c struct-type/c #f) (listof symbol?) boolean?
;;     struct-type/c)
(define (make-struct key super field-names mutable?)
  (define which-container
    (if mutable? make-hash make-immutable-hash))
  (define pivot (length field-names))
  (λ field-values
    (define-values (super-field-values this-field-values)
      (split-at-right field-values pivot))
    (define super-instance
      (cond [super
             (apply super super-field-values)]
            [(empty? super-field-values)
             base-struct-instance]
            [else
             (error
              (format
               "don't know what to do with these extra field values:~a"
               super-field-values))]))
    (define super-predicate (struct->predicate super-instance))
    (define super-accessors (struct->generic-accessor super-instance))
    (define super-mutators (struct->generic-mutator super-instance))
    (define super-fields (struct->field-names super-instance))
    (define this-instance-contents
      (for/list ([field-name field-names]
                 [field-value this-field-values])
        (cons field-name field-value)))
    (define this-instance
      (which-container this-instance-contents))
    (define (this-predicate that-key)
      (or (symbol=? key that-key) (super-predicate that-key)))
    (define (this-accessors field-name)
      (hash-ref
       this-instance
       field-name
       (thunk (super-accessors field-name))))
    (define (this-mutators field-name field-value)
      (cond [(and (member field-name field-names) mutable?)
             (hash-set! this-instance field-name field-value)]
            [else (super-mutators field-name field-value)]))
    (define (this-values)
      (append
       (struct->list super-instance)
       (for/list ([field-name field-names])
         (hash-ref this-instance field-name))))
    (define this-constructor
      (make-struct key super field-names mutable?))
    (list this-predicate
          this-accessors
          this-mutators
          this-values
          this-constructor
          (append super-fields
                  field-names))))


