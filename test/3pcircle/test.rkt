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
    (let* ([x (- (point-x cp) r)]
           [y (- (point-y cp) r)]
           [w (* r 2)])
      (send dc set-background "black")
      (send dc clear)
      (send dc set-pen "white" 2 'solid)
      (send dc set-brush "white" 'transparent)
      (send dc draw-ellipse x y w w))))
