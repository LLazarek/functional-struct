#lang racket

(provide new-struct
         struct->list
         struct->constructor)

(require (for-syntax syntax/parse
                     racket/base
                     racket/struct-info
                     racket/syntax))
         

;; struct-type/c: (-> (listof any/c) (-> (listof procedure?)))

;; (-> struct-type/c (listof any/c)) 
(define (struct->list an-instance)
  ((fourth an-instance)))

;; (-> struct-type/c (-> (listof any/c) struct-type/c) 
(define (struct->constructor an-instance)
  (fifth an-instance))
  

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
      (format "base struct constructor doesn't exist")))))

;; (-> symbol? (or/c struct-type/c #f) (listof symbol?) boolean?
;;     struct-type/c)
(define (make-struct key super field-names mutable?)
  (λ field-values
    (define which-container
      (if mutable? make-hash make-immutable-hash))
    (define pivot (length field-names))
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
    (define super-predicate (first super-instance))
    (define super-accessors (second super-instance))
    (define super-mutators (third super-instance))
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
       ((fourth super-instance))
       (for/list ([field-name field-names])
         (hash-ref this-instance field-name))))
    (define this-constructor
      (make-struct key super field-names mutable?))
    (list this-predicate
          this-accessors
          this-mutators
          this-values
          this-constructor)))

(define-for-syntax (build-tmp-id name)
  (datum->syntax
   name
   (gensym (syntax->datum name))))


(define-for-syntax (build-id name kind)
  (λ (field-name)
    (cond [(equal? kind 'accessor)
           (datum->syntax
            name
            (string->symbol
             (format "~a-~a" (syntax->datum name) (syntax->datum field-name))))]
          [(equal? kind 'mutator)
           (datum->syntax
            name
            (string->symbol
             (format "set-~a-~a!" (syntax->datum name) (syntax->datum field-name))))]
          [(equal? kind 'predicate)
           (datum->syntax
            name
            (string->symbol
             (format "~a?" (syntax->datum name))))])))  

(define-syntax (struct/h stx)
  (syntax-parse stx
    [(_ name:id super:expr (field-name:id ...) mutable?:expr)
     (with-syntax ([(accessor ...)
                    (map (build-id #'name 'accessor) 
                         (syntax->list #'(field-name ...)))]
                   [(mutator ...)
                    (map (build-id #'name 'mutator) 
                         (syntax->list #'(field-name ...)))]
                   [predicate ((build-id #'name 'predicate) #f)]
                   [key (gensym (syntax->datum #'name))]
                   [tmp-id (build-tmp-id #'name)])
       #`(begin

           #;(define name
             (make-struct 'key super '(field-name ...) mutable?))

           (define tmp-id
             (make-struct 'key super '(field-name ...) mutable?))

           
           (define-match-expander name
             (λ (stx)
               (syntax-parse stx
                 [(_ ~rest field-pat)
                  #'(? predicate
                       (app
                        struct->list
                        (list #,@#'field-pat)))]))
             (λ (stx)
               (syntax-parse stx
                 [stx:id #'tmp-id]
                 [(_ ~rest args)
                  #`(tmp-id #,@#'args)])))           
         
           (define (predicate a-struct) ((first a-struct) 'key))

           (define (accessor a-struct) ((second a-struct) 'field-name))
           ...

           #,(when (syntax->datum #'mutable?)
               #`(begin
                   (define (mutator a-struct v)
                     ((third a-struct) 'field-name v))
                   ...))))]))



(define-syntax (new-struct stx)
  (syntax-parse stx
    [(_ name:id (field-name:id ...))
     #'(struct/h name #f (field-name ...) #f)]
    [(_ name:id super:expr (field-name:id ...))
     #'(struct/h name super (field-name ...) #f)]
    [(_ name:id (field-name:id ...) (~datum #:mutable))
     #'(struct/h name #f (field-name ...) #t)]
    [(_ name:id super:expr (field-name:id ...) (~datum #:mutable))
     #'(struct/h name super (field-name ...) #t)]))


(module+ test

  (require rackunit
           syntax/parse/define)

  (define-simple-macro (check-error? test msg)
    (check-exn exn:fail? (thunk test) msg))

  (new-struct foo (f1 f2))
  (new-struct boo (b1 b2))
  (new-struct m+foo (f1 f2) #:mutable)
  (new-struct m+boo (b1 b2) #:mutable)
  (new-struct s+foo boo (f1 f2))
  (new-struct m-s+foo m+boo (f1 f2))
  (new-struct m-s+m+foo m+boo (f1 f2) #:mutable)

  

  (check-equal?
   (let ([an-instance (foo 42 3)])
     (foo-f2 an-instance))
   3
   "immutable struct construction and access")

  (check-error?
   (foo 1)
   "immutable struct construction with missing field values")
  
  (check-error?
   (foo 1 2 3)
   "immutable struct construction with extra field values")


  (check-equal?
   (let ([an-instance (m+foo 1 3)])
     (m+foo-f2 an-instance))
   3
   "mutable struct construction and access")

  (check-equal?
   (let ([an-instance (m+foo 1 3)])
     (set-m+foo-f2! an-instance 42)
     (m+foo-f2 an-instance))
   42
   "mutable struct construction, mutate and access mutated field")

  (check-equal?
   (let ([an-instance (m+foo 1 3)])
     (set-m+foo-f2! an-instance 42)
     (m+foo-f1 an-instance))
   1
   "mutable struct construction, mutate and access unmutated field")

  (check-error?
   (s+foo 1 2 3 4 5)
   "immutable struct construction with super and extra field values")

  (check-error?
   (s+foo 1 2 3)
   "immutable struct construction with super and missing field values")

  (check-equal?
   (let ([an-instance (s+foo 1 2 3 4)])
     (boo-b1 an-instance))
   1
   "immutable struct construction and access of super field")

  (check-equal?
   (let ([an-instance (s+foo 1 2 3 4)])
     (s+foo-f1 an-instance))
   3
   "immutable struct construction and access of own field")

  (check-equal?
   (let ([an-instance (m-s+m+foo 1 2 3 4)])
     (begin
       (set-m+boo-b1! an-instance 42)
       (boo-b1 an-instance)))
   42
   "immutable struct construction, mutation and access of super field")

  (check-equal?
   (let ([an-instance (foo 1 2)])
     (struct->list an-instance))
   '(1 2 )
   "immutable struct construction and list conversion")

  (check-equal?
   (let ([an-instance (m-s+foo 1 2 3 4)])
     (set-m+boo-b2! an-instance 42)
     (struct->list an-instance))
   '(1 42 3 4)
   "mutable struct construction, super mutation and list conversion")

  (check-true
   (let* ([an-instance (m-s+foo 1 2 3 4)]
          [construct (struct->constructor an-instance)]
          [new-instance (construct 41 42 43 44)])
     (m-s+foo? new-instance))
   "mutable struct construction with super, constructor extraction, reconstruction and successful predicate check")

  (check-true
   (let ([an-instance (m-s+foo 1 2 3 4)])
     (and (m-s+foo? an-instance) (m+boo? an-instance)))
   "mutable struct construction with super and successful predicate check")

  (check-false
   (let ([an-instance (m-s+foo 1 2 3 4)])
     (or (foo? an-instance)
         (m+foo? an-instance)
         (m-s+m+foo? an-instance)
         (boo? an-instance)
         (s+foo? an-instance)))
   "mutable struct construction with super and failed predicate check")

  (check-equal?
   (let ([an-instance (m-s+foo 1 2 3 4)])
     (set-m+boo-b2! an-instance 42)
     (struct->list an-instance))
   '(1 42 3 4)
   "mutable struct construction, super mutation and list conversion")

  (check-equal?
   (let ([an-instance (m-s+foo 1 2 3 4)])
     (match an-instance
       [(m-s+foo x y z h) (list y z)]))
   '(2 3)
   "mutable struct construction and match"))

