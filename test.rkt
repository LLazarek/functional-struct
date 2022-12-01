#lang typed/racket

(require "typed.rkt")

(struct foo ([f1 : Natural]
             [f2 : String])
  #:type-name Foo)

(: make-the-foo (-> Foo))
(define (make-the-foo)
  (foo 5 "7"))

(: use-it (-> Foo Natural))
(define (use-it a-foo)
  (add1 (foo-f1 a-foo)))

(use-it (make-the-foo))

