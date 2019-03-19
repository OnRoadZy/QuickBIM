#lang racket

(require "view.rkt"
         "interactive-data.rkt")

;重置会话环境：
(reset-interactive-context)

;显示程序主框架：
(send main-frame show #t)
(send canvas focus)

;初始化会话行：
;(reset-intertactive-line)
