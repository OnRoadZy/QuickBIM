#lang racket

(provide interactive-context%)

;定义交互环境类：
(define interactive-context%
  (class object%
    (super-new)

    (init-field msg-mouse-pos
                interactive-line
                interactive-info)

    ;交互环境字段：
    (field [interactive/style void] ;会话类型。包括:两点线2p-line,多段线poly-line,半径圆‘circle-radius,圆弧arc
           [interactive/object void] ;当前交互对象。
           [interactive/vector void] ;当前会话向量。
           [interactive/n 0]) ;当前会话向量序号。

    ;定义交互栈,每当产生选项时,当前交互环境状态入栈。
    ;入栈的环境字段：vector、n。
    ;当结束命令时清空栈；当结束选项对话时回退之前交互内容。
    (define stack empty)
    (define line-prompt "") ;交互行提示内容
    (define line-val "") ;交互行值
    (define interactive-history empty) ;交互历史记录

    (define/public (mouse-event event) ;mouse-event%
      (let ([type (send event get-event-type)])
        (cond
          ;点击左键:
          [(equal? type 'left-down)
           (click-left-button event)]
          ;点击右键:
          [(equal? type 'right-down)
           (click-right-button event)]
          ;把鼠标位置显示在状态栏:
          [else (show-mouse-pos event)])))

    (define/public (key-event event) ;key-event%
      (let ([key (send event get-key-code)])
        (cond
          ;为命令字符:
          [(interactive-char? key)
           (add-str-to-interactive-line
            (string key))]
          ;Esc键:
          [(equal? key 'escape)
           (escape-interactive)]
          ;Backspace键:
          [(equal? key #\backspace)
           (backspace-interactive-line)]
          ;回车:
          [(or (equal? key #\return )
               (equal? key 'numpad-enter ))
           (enter-interactive)])))

    ;处理鼠标事件函数:---------------------------
    ;处理鼠标左键事件:
    (define/private (click-left-button event)
      void)

    ;处理鼠标右键事件:
    (define/private (click-right-button event)
      void)

    ;处理鼠标其它事件:
    ;在状态栏坐标区显示鼠标位置:
    (define/private (show-mouse-pos event)
      (send msg-mouse-pos set-label
            (get-pos-from-event event)))

    ;从鼠标事件取得鼠标位置:
    (define/private (get-pos-from-event event)
      (format "~a,~a"
              (send event get-x)
              (send event get-y)))

    ;处理键盘事件函数:---------------------------
    ;添加字符串到交互行:
    (define/private (add-str-to-interactive-line str)
      (send interactive-line set-label
            (string-append
             (send interactive-line get-label)
             str)))
    
    ;放弃会话:
    (define/private (escape-interactive)
      ;重置交互环境:
      (reset-interactive-context)
      ;添加提示放弃信息:
      (when (non-empty-string? (get-answer-from-interactive-line))
        (add-str-to-interactive-line "*取消*"))
      ;保存交互行信息:
      (save-interactive-line)
      ;重置交互行:
      (reset-interactive-line))

    ;回退交互行文本:
    (define/private (backspace-interactive-line)
      (let* ([str (send interactive-line get-label)]
             [ls-str (regexp-split #rx":" str)]
             [len (string-length
                   (list-ref ls-str 1))])
        (when (> len 0)
          (send interactive-line set-label
                (substring str 0 (- (string-length str) 1))))))

    ;确认会话:
    (define/private (enter-interactive)
      (let ([str (get-answer-from-interactive-line)])
        (if (command? str) ;为命令
            (set-interactive-context str) ;设置交互环境
            (
              [(value? str) ;为值
               void]
              [(equal? style 'point) ;为坐标
                 (begin
                   (save-point str)
                   (next-interactive))])))))

    ;通用方法：--------------------------------------------
    ;检查键盘字符是否为会话字符(在会话行允许输入):
    ;包含:字符、数字、括号、@、逗号(,)、负号(-),角号(<)
    (define/private (interactive-char? key)
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

    ;重置会话环境:
    (define/private (reset-interactive-context)
      (set! style void)
      (set! prompt-int 0))

    ;取得交互回答字符串:
    (define/private (get-answer-from-interactive-line)
      (string-trim
       (list-ref
        (string-split
         (send interactive-line get-label)
         ":")
        1)))

    ;保存交互行内容:
    (define/private (save-interactive-line)
      (when (non-empty-string?
             (get-answer-from-interactive-line))
        (send interactive-info set-value
              (format "~a\n~a"
                      (send interactive-info get-value)
                      (send interactive-line get-label)))))

    ;重置交互行:
    (define (reset-interactive-line)
      (send interactive-line set-label "命令:"))

    ;检查会话环境重置状态:
    (define (interactive-context-reset?)
      (and (equal? style void)
           (= prompt-int 0)))

    ;为命令字串?
    (define (command-str? str)
      (string=?
       (car (regexp-match #px"[0-9a-z]*" str))
       str))

    #|
    ;进行绘图交互:
    (define (interactive/command str)
      ;设置交互环境:
      (when (set-interactive-context str)
        ;设置当前绘图类型:
        (init-cur-draw)
        ;保存命令:
        (save-interactive-line)
        ;显示交互提示:
        (show-prompt)))

    ;取得会话提示类型:
    (define (get-prompt-style current-int)
      (send interactive-object get-prompt-style prompt-int))

    ;保存点值:
    (define (save-point str)
      (when (value/point? str)
        (set-draw-ls! cur-draw
                      (cons
                       (draw-value
                        'point
                        (analyze-answer str))
                       (draw-ls cur-draw)))))

    ;进行下一段交互:
    (define (next-interactive)
      (save-interactive-line)
      (add-current-int)
      (if
       ;当前会话不是最后一个会话:
       (< (get-current-int)
          (get-prompt-number))
       (show-prompt)
       (end-interactive)))

    ;设置交互环境:
    (define (set-interactive-context str)
      (if (command/draw? str)
          (set! interactive-context
                (hash-ref interactive-hash
                          (hash-ref draw-style-hash str)))
          #f))

    ;设置绘图结构类型:
    (define (init-cur-draw)
      (set! cur-draw
            (draw (get-style) null)))

    ;显示交互提示:
    (define (show-prompt)
      (send interactive-line set-label
            (get-prompt (get-current-int))))
    |#
    ))
    
    