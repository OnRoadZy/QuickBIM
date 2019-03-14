#lang racket

(require racket/gui)

(provide main-frame
         canvas)

(include "canvas.rkt")

;定义主界面=======================
(define main-frame
  (new frame%
       [label "交互绘图程序-interactive"]
       [width 800]
       [height 600]
       [border 5]))

;定义菜单：==================================
(define menu-bar
  (new menu-bar%
       [parent main-frame]))

;定义菜单项：
;文件菜单：
(define menu/file
  (new menu%
       [parent menu-bar]
       [label "文件(&F)"]))
(define menu/file/exit
  (new menu-item%
       [parent menu/file]
       [label "退出(&X)"]
       [callback
        (lambda (item event)
          (exit-app item event))]))

;绘图菜单：
(define menu/draw
  (new menu%
       [parent menu-bar]
       [label "画图(&D)"]))

;画两点线：
(define menu/draw/2p-line
  (new menu-item%
       [parent menu/draw]
       [label "两点线(&L)"]
       [shortcut #\L]
       [callback void]))

;画多段线：
(define menu/draw/poly-line
  (new menu-item%
       [parent menu/draw]
       [label "多段线(&P)"]
       [shortcut #\P]
       [callback void]))

;画半径圆：
(define menu/draw/radius-circle
  (new menu-item%
       [parent menu/draw]
       [label "半径圆(&R)"]
       [shortcut #\R]
       [callback void]))

;画圆弧：
(define menu/draw/arc
  (new menu-item%
       [parent menu/draw]
       [label "圆弧(&A)"]
       [shortcut #\A]
       [callback void]))

;帮助：
(define menu/help
  (new menu%
       [parent menu-bar]
       [label "帮助(&H)"]))
;关于：
(define menu/help/about
  (new menu-item%
       [parent menu/help]
       [label "关于(&A)"]
       [callback
        (lambda (item event)
          (about item event main-frame))]))


;定义视图框架分割区域：========================
;总容器：
(define pane/all
  (new vertical-pane%
       [parent main-frame]
       [alignment (list 'left 'top)]))

;工具栏：---------------------
(define pane/toolbar
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'top)]
       [stretchable-height #f]))

;画两点线：
(define tb/2p-line
  (new button%
       [parent pane/toolbar]
       [label "两点线"]
       [callback void]))

;画多段线：
(define tb/poly-line
  (new button%
       [parent pane/toolbar]
       [label "多段线"]
       [callback void]))

;画圆弧：
(define tb/arc
  (new button%
       [parent pane/toolbar]
       [label "圆弧"]
       [callback void]))

;画半径圆：
(define tb/radius-circle
  (new button%
       [parent pane/toolbar]
       [label "半径圆"]
       [callback void]))

;视图区：
(define pane/view
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]))

;画布：
(define canvas
  (new interactive-canvas%
       [parent pane/view]
       [paint-callback
        (lambda (canvas dc)
          (paint canvas dc))]))

;视图-交互间隔：
(define panel/separator
  (new vertical-panel%
       [parent pane/all]
       [alignment (list 'left 'bottom)]
       [min-height 3]
       [stretchable-height #f]))

;交互区：
(define pane/interactive
  (new vertical-pane%
       [parent pane/all]
       [border 1]
       [alignment (list 'left 'bottom)]
       [stretchable-height #f]))

;对话列表显示框：
(define interactive-list
  (new text-field% 
       [parent pane/interactive]
       [label #f]
       [style (list 'multiple)]
       [init-value "命令交互准备就绪！"]
       [min-height 80]
       [stretchable-height #f]))

;对话行编辑框：
(define interactive-line
  (new message%
       [parent pane/interactive]
       [label "命令："]))

;状态栏：
(define pane/statusbar
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]
       [stretchable-height #f]))

;信息
(define status/mouse-position
  (new message%
       [parent pane/statusbar]
       [label "程序准备就绪！"]
       [auto-resize #t]))

;插入视图控制程序：
(include "view-control.rkt")