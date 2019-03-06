;#lang racket

;(require racket/gui)
(require graphics/graphics)

;(provide interactive-canvas%)

(define interactive-canvas%
  (class canvas%
    (define/override (on-event event)
      (send textfield/command set-value
            (string-append "画布点击鼠标事件，当前坐标点："
                     (format "(~a,~a)"
                             (send event get-x)
                             (send event get-y))))) ;mouse-event%

    (define/override (on-char event)
      (send textfield/information set-value
            (string-append "画布键盘事件，当前按键："
                      (format "~a" (send event get-key-code)))));key-event%

    (super-new)))