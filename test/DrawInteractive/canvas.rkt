;#lang racket

;(require racket/gui)

(provide interactive-canvas%)

;扩展控件类：======================
;扩展画布类：
(define interactive-canvas%
  (class canvas%
    ;重定义鼠标事件：----------------------------------
    (define/override (on-event event) ;mouse-event%
      (let ([type (send event get-event-type)])
        (cond
          ;点击左键：
          [(equal? type 'left-down)
           (click-left-button event)]
          ;点击右键：
          [(equal? type 'right-down)
           (click-right-button event)]
          ;把鼠标位置显示在状态栏：
          [else (show-mouse-pos event)])))

    ;重定义键盘事件：---------------------------------
    (define/override (on-char event) ;key-event%
      (let ([key (send event get-key-code)])
        (cond
          ;Esc键：
          [(equal? key 'escape)
           void]
          ;Backspace键：
          [(equal? key #\backspace)
           void]
          ;回车：
          [(or (equal? key #\return )
               (equal? key 'numpad-enter ))
           void]
          ;为命令字符：
          [(interactive-char? key)
           (add-char-to-interactive-line key)])))

    (super-new)))

;处理鼠标事件函数：=============================
;处理鼠标左键事件：-----------------------------
(define (click-left-button event)
  void)

;处理鼠标右键事件：----------------------------
(define (click-right-button event)
  void)

;处理鼠标其它事件：----------------------------
;在状态栏坐标区显示鼠标位置：
(define (show-mouse-pos event)
  (send status/mouse-position set-label
        (get-pos-from-event event)))

;从鼠标事件取得鼠标位置：
(define (get-pos-from-event event)
  (format "~a,~a"
          (send event get-x)
          (send event get-y)))

;处理键盘事件函数：==============================
;检查键盘字符是否为会话字符（在会话行允许输入）：
;包含：字符、数字、括号、@、逗号（,）、负号（-）,角号（<）
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

;添加字符到交互行：
(define (add-char-to-interactive-line key)
  (send interactive-line set-label
        (format "~a~a"
         (send interactive-line get-label)
         key)))

