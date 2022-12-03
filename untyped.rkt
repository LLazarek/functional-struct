#lang racket

(provide (rename-out [new-struct struct])
         struct-copy
         struct->list
         struct->predicate
         struct->generic-accessor
         struct->generic-mutator
         struct->field-names
         struct->constructor)

(require "struct-lib.rkt"
         (for-syntax racket
                     syntax/parse
                     "util.rkt"))

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

           (define (predicate a-struct) ((struct->predicate a-struct) 'key))

           (define (accessor a-struct) ((struct->generic-accessor a-struct) 'field-name))
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
   "mutable struct construction and match")

  (check-equal? (let ([an-instance (m-s+foo 1 2 3 4)])
                  (struct->list
                   (struct-copy m-s+foo
                                an-instance
                                [f1 5]
                                [b2 7])))
                '(1 7 5 4)))
