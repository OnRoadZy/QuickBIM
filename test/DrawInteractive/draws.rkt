#lang racket

(require "draw-data.rkt")

(provide draws

         init-draws
         get-draw
         draws-append
         draw-pels)

;数据结构描述：===========================================
;图形列表：
(define draws '())

;初始化draws：
(define (init-draws)
  void)

;取得图形：
(define (get-draw n)
  (if (and (< n (length draws))
           (>= n 0))
      (list-ref draws n)
      #f))

;添加图形到图形列表：
(define (draws-append draw)
  (set! draws (cons draw draws)))

;画图形列表内图形：
(define (draw-pels dc)
  (map
   (lambda (draw)
     (draw-pel dc draw))
   draws))
