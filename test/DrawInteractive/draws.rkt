#lang racket

(provide draws
         get-draw
         draws-append)

;数据结构描述：===========================================
;图形列表：
(define draws '())

;取得图形：
(define (get-draw n)
  (if (and (< n (length draws))
           (>= n 0))
      (list-ref draws n)
      #f))

;添加图形到图形列表：
(define (draws-append value)
  (set! draws (cons value draws)))
