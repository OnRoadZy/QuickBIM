#lang racket

(require racket/gui
         "interactive.rkt")

(provide interactive-canvas%)

;扩展控件类：========================================
;扩展画布类：
(define interactive-canvas%
  (class canvas%
    (super-new)

    (init-field ic)
    
    ;重定义鼠标事件：----------------------------------
    (define/override (on-event event) ;mouse-event%
      (send ic mouse-event event))

    ;重定义键盘事件：---------------------------------
    (define/override (on-char event) ;key-event%
      (send ic key-event event))

    ))

;通用函数=======================================
#|
;检查键盘字符是否为会话字符(在会话行允许输入):
;包含:字符、数字、括号、@、逗号(,)、负号(-),角号(<)
(define (interactive-char? key)
  (and (char? key)
       (or (char-alphabetic? key)
           (char-numeric? key)
           (char=? key #\()
           (char=? key #\))
           (char=? key #\<)
           (char=? key #\,)
           (char=? key #\.)
           (char=? key #\-)
           (char=? key #\@))))








;进行下一段交互：
(define (next-interactive)
  (save-interactive-line)
  (add-current-int)
  (if
   ;当前会话不是最后一个会话：
   (< (get-current-int)
      (get-prompt-number))
   (show-prompt)
   (end-interactive)))

;结束交互：
(define (end-interactive)
  ;保存图形数据：
  (draws-append cur-draw)
  ;(send canvas refresh-now)
  (draw-pels (send canvas get-dc))
  ;显示结束语：
  (show-end-str))

;显示会话结束语：
(define (show-end-str)
  (send interactive-list set-value
        (format "~a\n~a"
                (send interactive-list get-value)
                (get-end-str)))
  (reset-interactive-line))



;显示交互提示：
(define (show-prompt)
  (send interactive-line set-label
        (get-prompt (get-current-int))))
|#