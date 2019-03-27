#lang racket

(require "pels.rkt")

(provide draw-pels)

;定义图元列表：========================================
(define pels
 (list
  ;两点线：
  (line/2p
   (point 10 40)
   (point 100 100))

;正方形：
  (square
   (point 100 30)
   50)
;矩形：
  (rectangle/2p
   (point 170 30)
   (point 240 50))

  (rectangle/len
   (point 250 30)
   60 100)

;圆：
  (circle/cp
   (point 350 100)
   (point 400 150))

  (circle/2p
   (point 420 100)
   (point 560 150))

  (circle/radius
   (point 600 100)
   60)

  (circle/3p
   (point 10 200)
   (point 10 260)
   (point 100 200))

;圆弧：
  (arc/angle
   (point 120 200)
   40 10 45)

  (arc/2p
   (point 200 200)
   (point 250 240)
   (point 220 160))

  (arc/3p
   (point 200 200)
   (point 250 240)
   (point 220 160))))

  
;绘制单个图元测试：==================================
(define (draw-pels dc)
  (rec-pels dc pels))
  
(define (rec-pels dc ls)
  (when (not (null? ls))
    (draw-pel dc (car ls))
    (rec-pels dc (cdr ls))))

(define (draw-pel dc pel)
  (cond
    [(line/2p? pel)
     (draw-line/2p dc pel)]
    [(square? pel)
     (draw-square dc pel)]
    [(rectangle/2p? pel)
     (draw-rectangle/2p dc pel)]
    [(rectangle/len? pel)
     (draw-rectangle/len dc pel)]))

      