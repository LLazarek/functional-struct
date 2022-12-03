#lang racket

(provide build-id
         build-tmp-id)

(require racket/syntax)

(define (build-tmp-id name)
  (datum->syntax
   name
   (gensym (syntax->datum name))))


(define (build-id name kind)
  (λ ([field-name #f])
    (match kind
      ['accessor
       (format-id name
                  "~a-~a"
                  name
                  field-name)]
      ['mutator
       (format-id name
                  "set-~a-~a!"
                  name
                  field-name)]
      ['predicate
       (format-id name
                  "~a?"
                  name)]
      ['struct-info-id
       (format-id name
                  "~a:"
                  name)])))
