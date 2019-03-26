#lang racket

(require racket/gui)

(provide main-frame)

(require "pels.rkt")

;定义主界面=======================
(define main-frame
  (new frame%
       [label "绘制图元程序-DrawPels"]
       [width 800]
       [height 600]
       [border 2]))

;定义菜单:==================================
(define menu-bar
  (new menu-bar%
       [parent main-frame]))

;定义菜单项:
;文件菜单:
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

;帮助:
(define menu/help
  (new menu%
       [parent menu-bar]
       [label "帮助(&H)"]))
;关于:
(define menu/help/about
  (new menu-item%
       [parent menu/help]
       [label "关于(&A)"]
       [callback
        (lambda (item event)
          (about item event main-frame))]))

;定义视图框架分割区域:========================
;总容器:
(define pane/all
  (new vertical-pane%
       [parent main-frame]
       [alignment (list 'left 'top)]))

;工具栏:---------------------
(define pane/toolbar
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'top)]
       [stretchable-height #f]))

;画两点线:
(define tb/2p-line
  (new button%
       [parent pane/toolbar]
       [label "两点线"]
       [callback void]))

;画多段线:
(define tb/poly-line
  (new button%
       [parent pane/toolbar]
       [label "多段线"]
       [callback void]))

;画圆弧:
(define tb/arc
  (new button%
       [parent pane/toolbar]
       [label "圆弧"]
       [callback void]))

;画半径圆:
(define tb/radius-circle
  (new button%
       [parent pane/toolbar]
       [label "半径圆"]
       [callback void]))

;视图区:
(define pane/view
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]))

;画布:
(define canvas
  (new canvas%
       [parent pane/view]
       [paint-callback
        (lambda (canvas dc)
          (paint canvas dc))]))
;状态栏:
(define pane/statusbar
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]
       [stretchable-height #f]))

;信息
(define status/mouse-position
  (new message%
       [parent pane/statusbar]
       [label "程序准备就绪!"]
       [auto-resize #t]))

;退出程序:
(define (exit-app item event)
  (send main-frame on-exit))

;关于:
(define (about item event)
  (message-box  "关于本程序" "本程序旨在演示绘制基本图元。\n提供了两点线、直线、正方形、矩形、正多边形、多边形、点圆、两点圆、半径圆、三点圆、多段线、角圆弧、两点圆弧、三点圆弧、椭圆、椭圆弧、spline线、B样条线、nurbs线。"
   main-frame
   (list 'ok)))

;在画布画图:
(define (paint canvas dc)
  ;初始化画布:
  (send canvas set-canvas-background (make-object color% 0 0 0 1.0))
  ;绘制欢迎文字:
  (send dc set-text-foreground "white")
  (send dc set-scale 1 1)
  (send dc draw-text "画布(Canvas)准备就绪!" 0 0)
  
  ;设置画笔：
  (send dc set-pen
        (make-object color%
          255 0 255)
        2
        'solid)
  ;设置画刷：
  (send dc set-brush
        (make-object color%
          0 255 255)
        'transparent)
  ;测试绘制图元:
  (draw-pels dc))
