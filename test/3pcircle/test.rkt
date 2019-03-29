;test.rkt

#lang racket/gui

(require racket/draw)
(require "3p-circle.rkt")

;测试：========================================
;定义点：
(define sp (point 498 116))
(define mp (point 181 280))
(define ep (point 290 458))
;定义视图：
(define main-frame
  (new frame%
       [label "3点定圆测试"]
       [width 800]
       [height 600]
       [border 5]))
(define canvas
  (new canvas%
       [parent main-frame]
       [paint-callback
        (lambda (canvas dc)
          (draw-circle/3p dc))]))
;显示视图：
(send main-frame show #t)

;绘圆：
(define (draw-circle/3p dc)
  (let-values ([(cp r)
                (3p-circle sp mp ep)])
    (send dc set-background "black")
    (send dc clear)
    ;画圆：
    (send dc set-pen "white" 1 'solid)
    (send dc set-brush "white" 'transparent)
    (let* ([x (- (point-x cp) r)]
           [y (- (point-y cp) r)]
           [w (* r 2)])
      (send dc draw-ellipse x y w w))
    ;画标志点：
    (send dc set-pen "white" 0 'transparent)
    (send dc set-brush "red" 'solid)
    (draw-3ps dc)
    (send dc set-pen "red" 1 'solid)
    (draw-center dc cp)))

;画三个点：
(define (draw-3ps dc)
  (draw-1p dc sp)
  (draw-1p dc mp)
  (draw-1p dc ep))

;画单个点：
(define (draw-1p dc p)
  (send dc draw-ellipse
        (point-x p)
        (point-y p)
        6 6))

;画圆心十字叉：
(define (draw-center dc cp)
  (let ([x (point-x cp)]
        [y (point-y cp)])
    (send dc draw-line
          x (- y 6)
          x (+ y 6))
    (send dc draw-line
          (- x 6) y
          (+ x 6) y)))
