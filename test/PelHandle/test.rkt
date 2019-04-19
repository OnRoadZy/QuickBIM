#lang racket

(require racket/gui)

(require "pel.rkt")

;测试数据：================================


;绘图：===================================
;测试画单个控制点：
(define (draw-handler cp)
  (let ([hd
         (new handler% [cp cp])]
        [dc (send canvas get-dc)])
    (send hd draw-handler dc)))

(define (test-handler)
  (draw-handler (point 100 100)))

;清除绘图内容：
(define (canvas-reset)
  (send canvas refresh-now))

;定义主界面=================================
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
          (about item event))]))

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

;工具按钮通用宏：
(define-syntax-rule (toolbutton p lb cb)
  (new button%
       [parent p]
       [label lb]
       [callback
        (lambda (b e) cb)]))
;定义按钮：
(define tb/clear
  (toolbutton pane/toolbar "清除" (canvas-reset)))
(define tb/handler
  (toolbutton pane/toolbar "测试控制点" (test-handler)))

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
  (message-box  "关于本程序" "本程序旨在演示绘制Spline线、B-Spline线并显示其控制点。线的描述用类来定义。"
   main-frame
   (list 'ok)))

;在画布画图:
(define (paint canvas dc)
  ;初始化画布:
  (send canvas set-canvas-background
        (make-object color% 0 0 0 1.0))
  ;启用抗锯齿：
  (send dc set-smoothing 'aligned)
  ;绘制欢迎文字:
  (send dc set-text-foreground "white")
  (send dc set-scale 1 1)
  (send dc draw-text "画布(Canvas)准备就绪!" 0 0)
  
  ;设置画笔：
  (send dc set-pen
        (make-object color%
          255 0 255)
        1
        'solid)
  ;设置画刷：
  (send dc set-brush
        (make-object color%
          0 255 255)
        'transparent)
  ;设置文字前景色：
  (send dc set-text-foreground
        (make-object color% 255 0 255)))

;显示框架：=========================
;显示主框架视图：
(send main-frame show #t)
