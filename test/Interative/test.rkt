#lang racket

(require racket/gui)

(require "canvas.rkt"
         "interactive.rkt")

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

;工具按钮通用宏：
(define-syntax-rule (toolbutton p lb cb)
  (new button%
       [parent p]
       [label lb]
       [callback
        (lambda (b e) cb)]))

;定义按钮：
(define tb/line-2p
  (toolbutton pane/toolbar "两点线" void))
(define tb/line-poly
  (toolbutton pane/toolbar "多段线" void))
(define tb/arc-a
  (toolbutton pane/toolbar "圆弧" void))
(define tb/circle-r
  (toolbutton pane/toolbar "半径圆" void))

;视图区：
(define pane/view
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]))

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
(define interactive-info
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

;定义会话对象：===============================
(define ic
  (new interactive-context%
       [msg-mouse-pos status/mouse-position]
       [interactive-line interactive-line]
       [interactive-info interactive-info]))

;定义画布：==================================
;画布：
(define canvas
  (new interactive-canvas%
       [parent pane/view]
       [ic ic]
       [paint-callback
        (lambda (canvas dc)
          (paint canvas dc))]))

;事件回调函数：===============================
;退出程序：
(define (exit-app item event)
  (send main-frame on-exit))

;关于：
(define (about item event)
  (message-box  "关于本程序" "本程序旨在演示交互绘图。\n提供了鼠标、键盘交互功能，可在此程序基础上作扩展即可实现实际使用的交互绘图程序。\n绘图主要考虑不同的交互输入设置了画两点线（演示键盘及鼠标左键点输入）、多段线（演示不确定数量点输入）、半径圆（演示点及长度输入）、圆弧（演示选项分支交互）。"
   main-frame
   (list 'ok)))

;在画布画图：
(define (paint canvas dc)
  ;初始化画布：
  (send canvas set-canvas-background
        (make-object color% 0 0 0 1.0))
  ;绘制欢迎文字：
  (send dc set-text-foreground "white")
  (send dc set-scale 1 1)
  (send dc draw-text "画布（Canvas）准备就绪!" 0 0))

;显示程序主框架：===================================
(send main-frame show #t)
(send canvas focus)