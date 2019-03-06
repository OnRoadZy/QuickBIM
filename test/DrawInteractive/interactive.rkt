#lang racket

(require racket/gui)
(require "draw.rkt")

(include "interactive-canvas.rkt")

;定义主界面=======================
(define main-frame
  (new frame%
       [label "QuickBIM"]
       [width 800]
       [height 600]
       [border 5]))

(send main-frame show #t)

;定义视图框架分割区域：========================
;总容器：
(define panel/all
  (new vertical-pane%
       [parent main-frame]
       [alignment (list 'left 'top)]))

;视图区：
(define panel/view
  (new horizontal-pane%
       [parent panel/all]
       [alignment (list 'left 'bottom)]))

;画布：
(define canvas
  (new interactive-canvas%
       [parent panel/view]
       [style '(border)]
       [paint-callback
        (lambda (canvas dc) (paint canvas dc))]))

;视图-交互间隔：
(define panel/separator
  (new vertical-pane%
       [parent panel/all]
       [alignment (list 'left 'bottom)]
       [min-height 3]
       [stretchable-height #f]))

;交互区：
(define panel/interactive
  (new vertical-panel%
       [parent panel/all]
       [style '(border)]
       [alignment (list 'left 'bottom)]
       [stretchable-height #f]))

;信息显示区：
(define textfield/information
  (new text-field% 
       [parent panel/interactive]
       [label #f]
       [style '(multiple)]
       [init-value "信息显示区……"]))

;命令编辑区：
(define textfield/command
  (new text-field%
       [parent panel/interactive]
       [label "命令："]))
