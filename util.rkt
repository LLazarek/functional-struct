#lang racket

(provide build-id
         build-tmp-id)

(define (build-tmp-id name)
  (datum->syntax
   name
   (gensym (syntax->datum name))))


(define (build-id name kind)
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
