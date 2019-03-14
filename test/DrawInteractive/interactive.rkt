#lang racket

(require "view.rkt"
         "interactive-data.rkt")

;初始化会话环境：
(define interactive-context void)

;显示程序主框架：
(send main-frame show #t)
(send canvas focus)
;初始化命令行：
(clear-intertactive-line)

;重置对话环境：
(define (reset-interactive-context)
  (set! interactive-context void))

