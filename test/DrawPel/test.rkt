#lang racket

(require racket/gui)

(provide main-frame)

(require "pels.rkt")

;定义实例图元：==========================
(require "pels.rkt")

;定义图元列表：
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
  (circle/1p
   (point 350 100)
   (point 400 150))

  (circle/2p
   (point 420 100)
   (point 560 150))

  (circle/r
   (point 600 100)
   60)

  (circle/3p
   (point 150 200)
   (point 100 260)
   (point 200 400))

;椭圆：
  (ellipse/cp
   (point 350 400)
   70 30)

  (ellipse/2p
   (point 400 300)
   (point 800 600))

;圆弧：
  (arc/a
   (point 120 200)
   40
   (degrees->radians 10)
   (degrees->radians 180))

  (arc/2p
   (point 200 200)
   (point 250 240)
   (point 220 160))

  (arc/3p
   (point 200 200)
   (point 250 240)
   (point 220 160))

  ;多边形：
  (polygon/pts
   (point 200 300)
   (list
    (point 20 70) 
    (point 40 90) 
    (point 60 110) 
    (point 90 110) 
    (point 130 90) 
    (point 100 70)))

  (polygon/n
   (point 300 350)
   200 5)
  ))

;绘制实例图元：===========================
;绘制所有图元：
(define (draw-pels)
  (rec-pels (send canvas get-dc) pels))
  
(define (rec-pels dc ls)
  (when (not (empty? ls))
    (draw-pel dc (car ls))
    (rec-pels dc (cdr ls))))

;绘制判断函数确定图元：
(define (draw-one-pel just-struct)
  (draw-pel (send canvas get-dc)
            (find pels just-struct)))

;图元判断函数，查找指定结构的图元：
;just-struct是结构判断函数，如：arc/3p？等等
(define (find  ls just-struct)
  (if (empty? ls)
      #f
      (if (just-struct (car ls))
          (car ls)
          (find (cdr ls) just-struct))))

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
;通用组框定义：
(define-syntax-rule (toolgroup lb)
  (new group-box-panel%
       [parent pane/toolbar]
       [label lb]
       [alignment (list 'left 'top )]
       [stretchable-width #f]
       [stretchable-height #f]))
;定义组框：
(define tg/manage (toolgroup "管理"))
(define tg/line (toolgroup "画线"))
(define tg/rectangle (toolgroup "画矩形"))
(define tg/circle (toolgroup "画圆"))
(define tg/arc (toolgroup "画圆弧"))
(define tg/polygon (toolgroup "画多边形"))

;工具栏组框集装箱定义：
(define-syntax-rule (group-pane p)
  (new horizontal-pane%
       [parent p]
       [alignment (list 'left 'top)]))
(define gp/manage (group-pane tg/manage))
(define gp/line (group-pane tg/line))
(define gp/rectangle (group-pane tg/rectangle))
(define gp/circle (group-pane tg/circle))
(define gp/arc (group-pane tg/arc))
(define gp/polygon (group-pane tg/polygon))

;工具按钮通用宏：
(define-syntax-rule (toolbutton p lb cb)
  (new button%
       [parent p]
       [label lb]
       [callback
        (lambda (b e) cb)]))
;定义按钮：
(define tb/clear
  (toolbutton gp/manage "清除" (canvas-reset)))
(define tb/all
  (toolbutton gp/manage "全部绘制" (draw-pels)))
(define tb/line-2p
  (toolbutton gp/line "两点线" (draw-one-pel line/2p?)))
(define tb/square
  (toolbutton gp/rectangle "正方形" (draw-one-pel square?)))
(define tb/rectangle-2p
  (toolbutton gp/rectangle "两点矩形" (draw-one-pel rectangle/2p?)))
(define tb/rectangle-len
  (toolbutton gp/rectangle "长度矩形" (draw-one-pel rectangle/len?)))
(define tb/circle-r
  (toolbutton gp/circle "半径圆" (draw-one-pel circle/r?)))
(define tb/circle-1p
  (toolbutton gp/circle "中心圆" (draw-one-pel circle/1p?)))
(define tb/circle-2p
  (toolbutton gp/circle "两点圆" (draw-one-pel circle/2p?)))
(define tb/circle-3p
  (toolbutton gp/circle "三点圆" (draw-one-pel circle/3p?)))
(define tb/ellipse-cp
  (toolbutton gp/circle "圆心椭圆" (draw-one-pel ellipse/cp?)))
(define tb/ellipse-2p
  (toolbutton gp/circle "两点椭圆" (draw-one-pel ellipse/2p?)))
(define tb/arc-a
  (toolbutton gp/arc "角圆弧" (draw-one-pel arc/a?)))
(define tb/arc-2p
  (toolbutton gp/arc "两点圆弧" (draw-one-pel arc/2p?)))
(define tb/arc-3p
  (toolbutton gp/arc "三点圆弧" (draw-one-pel arc/3p?)))
(define tb/polygon-pts
  (toolbutton gp/polygon "多点多边形" (draw-one-pel polygon/pts?)))
(define tb/polygon-n
  (toolbutton gp/polygon "边数正多边形" (draw-one-pel polygon/n?)))

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
        1
        'solid)
  ;设置画刷：
  (send dc set-brush
        (make-object color%
          0 255 255)
        'transparent))

;显示框架：=========================
;显示主框架视图：
(send main-frame show #t)
