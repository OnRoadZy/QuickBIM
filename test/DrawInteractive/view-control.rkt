;view-control.rkt
;定义main-frame.rkt的视图控制程序，
;用include载入frame-main.rkt使用。

;#lang racket

;(require racket/gui)

(provide clear-intertactive-line)

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
  (send canvas set-canvas-background (make-object color% 0 0 0 1.0))
  ;绘制欢迎文字：
  (send dc set-text-foreground "white")
  (send dc set-scale 1 1)
  (send dc draw-text "画布（Canvas）准备就绪!" 0 0))

;重置对话行：
(define (clear-intertactive-line)
  (send interactive-line set-label "命令："))