#lang racket

(require "draws.rkt")

(provide interactive-context%)

;简化命令散列表：
(define simple-command
  (hash "l" "line"
        "sp" "spline"
        "be" "bezier"))

;命令散列表：
(define full-command
  (list "line"
        "spline"
        "bezier"))

;定义交互环境类：
(define interactive-context%
  (class object%
    (super-new)

    (init-field msg-mouse-pos
                interactive-line
                interactive-info)

    ;交互环境字段：
    (field [objects '()]
           [interactive/old-pt void]
           [interactive/dc void]
           [interactive/object void] ;当前交互对象。
           [interactive/n 0]) ;当前会话向量序号。

    ;定义交互栈,每当产生选项时,当前交互环境状态入栈。
    ;入栈的环境字段：vector、n。
    ;当结束命令时清空栈；当结束选项对话时回退之前交互内容。
    (define stack empty)
    (define line-prompt "命令：") ;交互行提示内容
    (define line-val "") ;交互行值
    (define interactive-history "命令交互准备就绪!") ;交互历史记录

    (define/public (key-event event) ;key-event%
      (let ([key (send event get-key-code)])
        (cond
          ;为命令字符:
          [(interactive-char? key)
           (add-interactive-str (string key))]
          ;Esc键:
          [(equal? key 'escape)
           (escape-interactive)]
          ;Backspace键:
          [(equal? key #\backspace)
           (backspace-interactive-val)]
          ;回车:
          [(or (equal? key #\return )
               (equal? key 'numpad-enter ))
           (enter-interactive)])))

    ;处理鼠标事件函数:---------------------------
    ;处理鼠标左键事件:
    (define/public (click-left-button event)
      (cond
      ;处理鼠标事件获取坐标值：
      [(and
             (not (interactive-context-reset?))
             (equal?
              (send interactive/object
                    get-value-style interactive/n)
              'point))
       (set-mouse-point event)]))

    ;处理鼠标右键事件:
    (define/public (click-right-button event)
      void)

    ;处理鼠标移动事件：
    (define/public (mouse-moving event dc)
      ;显示鼠标位置：
      (show-mouse-pos event)
      ;绘制动态图：
      (unless (equal? interactive/object void)
        ;绘制即时图形：
        (send interactive/object
              immediatly-draw
              interactive/dc
              interactive/old-pt
              (point (send event get-x)
                     (send event get-y)))
        ;保存当前鼠标位置：
        (set! interactive/old-pt
              (point (send event get-x)
                     (send event get-y)))))

    ;处理鼠标其它事件:
    ;在状态栏坐标区显示鼠标位置:
    (define/public (show-mouse-pos event)
      (send msg-mouse-pos set-label
            (get-pos-from-event event)))

    ;取得鼠标点：
    (define (set-mouse-point event)
      (set! line-val (get-pos-from-event event))
      (set-interactive-line)
      (enter-interactive))

    ;从鼠标事件取得鼠标位置:
    (define/private (get-pos-from-event event)
      (format "~a,~a"
              (send event get-x)
              (send event get-y)))

    ;处理键盘事件函数:---------------------------
    ;添加字符串到交互行:
    (define (add-interactive-str str)
      ;保存到交互行值字段：
      (set! line-val (string-append line-val str))
      (set-interactive-line))
    
    ;放弃会话:
    (define (escape-interactive)
      ;显示会话提示信息:
      (show-interactive-msg "取消")
      ;重置交互环境:
      (reset-interactive-context))

    ;回退交互行文本:
    (define (backspace-interactive-val)
      (let* ([len (string-length line-val)])
        (when (> len 0)
          (set! line-val
                (substring line-val
                           0
                           (- (string-length line-val)
                              1)))
          (set-interactive-line))))

    ;确认会话:
    (define (enter-interactive)
      (if (interactive-context-reset?)
          ;交互环境已重置，需要输入命令：
          (let ([command (command? line-val)])
            (if command ;为命令
                (start-new-interactive command)
                (unless (equal? line-val "")
                  (begin
                    (show-interactive-msg "不是正确的命令")
                    (reset-interactive-line)))))
          ;处于交互状态：
          (let ([value-style
                 (send interactive/object
                       get-value-style interactive/n)])
            (cond ;各分支采用宏简化代码。
              ;需要点：
              [(equal? value-style 'point)
               (check-value-style is-point?
                                  string->point
                                  "需要提供点坐标")]
              ;需要值:
              [(equal? value-style 'value)
               (check-value-style is-value?
                                  string->value
                                  "需要提供一个数值")]
              ;需要选项：
              [(equal? value-style 'select)
               (check-value-style is-select?
                                  string->select
                                  "需要提供选项")]))))

    ;需求值验证宏：
    (define-syntax-rule (check-value-style style? convert msg)
      (if (style? line-val)
          ;交互行值满足需求类型：
          (begin
            (send interactive/object
                  set-value (convert line-val))
            (when (is-point? line-val)
                  (set! interactive/old-pt (convert line-val)))
            (save-interactive-line) ;保存交互行。
            (interactive-n-prompt
             (+ interactive/n 1)))
          ;交互行值不满足需求类型，重新显示交互提示：
          (begin
            (show-interactive-msg msg)
            (set-n-prompt interactive/n))))

    ;字符串转换为点坐标值：
    (define (string->point str)
      (if (is-point? str)
          (let* ([ls-str (regexp-split #rx"[<,]" str)]
                 [first-str (list-ref ls-str 0)]
                 [second-str (list-ref ls-str 1)])
            (point (string->number first-str)
                   (string->number second-str)))
          #f))
             
    ;字符串转换为点坐标值:
    (define (string->value str)
      (if (is-value? str)
          (string->number str)
          #f))

    ;字符串转换为选项值：
    (define (string->select str)
      (if (is-select? str)
          str
          #f))

    ;通用方法：--------------------------------------------
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

    ;是否是命令字符串？是，返回命令字串；否返回#f。
    (define (command? str)
        (if (hash-has-key? simple-command str)
          (hash-ref simple-command str)
          (if (index-of full-command str)
              str
              #f)))

    ;为坐标点？
    ;坐标格式有两种:(number,number),(number<degree)
    (define (is-point? val)
      (if (or
           (regexp-match #rx"," val)
           (regexp-match #rx"<" val))
          (let* ([ls-str (regexp-split #rx"[<,]" val)]
                 [first-str (list-ref ls-str 0)]
                 [second-str (list-ref ls-str 1)])
            (and (string->number first-str)
                 (string->number second-str)))
          #f))

    ;为数值？
    (define (is-value? val)
      (string->number val))

    ;为选项？
    (define (is-select? val)
      #f)

    ;启动交互环境：
    (define/public (begin-interactive-context)
      (send interactive-info
            set-value interactive-history))
    
    ;重置会话环境:
    (define (reset-interactive-context)
      (set! interactive/object void)
      (set! interactive/n 0)
      (set! line-val "")
      (reset-interactive-line))

    ;检查会话环境重置状态:
    (define (interactive-context-reset?)
      (equal? interactive/object void))

    ;开始一个新的会话：
    (define (start-new-interactive command)
      ;设置会话环境：
      (set! interactive/object
            (create-interactive-object command))
      (set! interactive/n 0)
      ;保存交互行：
      (save-interactive-line)
      ;启动命令对象的第一段会话：
      (set! line-prompt
            (send interactive/object
                  get-prompt interactive/n))
      (set! line-val "")
      (set-interactive-line))

    ;进行第n段交互提示：
    (define (interactive-n-prompt n)
      (set! interactive/n n)
      (if (< interactive/n
              (send interactive/object
                    prompt-count))
          ;当前序号的会话未超出会话总数,进行该会话：
          (set-n-prompt interactive/n)
          ;序号超出会话总数，栈不为空，回退；栈为空，结束会话：
          (if (empty? stack)
              (end-interactive)
              ;暂用#f占位
              #f)))

    ;设置第n段提示：
    (define (set-n-prompt n)
      (set! line-prompt
            (send interactive/object
                  get-prompt interactive/n))
      (set! line-val "")
      (set-interactive-line))
    
    ;结束会话：
    (define (end-interactive)
      (append-interactive-history
       (send interactive/object get-end-prompt))
      ;保存交互对象：
      (set! objects
            (cons interactive/object objects))
      (reset-interactive-context))
       
    ;创建交互对象：
    (define (create-interactive-object command)
      (cond
        [(equal? command "spline")
         (new spline%)]))

    ;显示会话提醒信息:
    (define (show-interactive-msg msg)
      ;保存会话行内容：
      (save-interactive-line)
      ;显示指定信息：
      (append-interactive-history
       (string-append "*" msg "*")))

    ;保存交互行内容到交互历史信息：
    (define (save-interactive-line)
      (append-interactive-history
       (string-append line-prompt
                      line-val)))

    ;保存字符串到交互历史信息:
    (define (append-interactive-history str)
      (set! interactive-history
            (string-append
             interactive-history
             "\n"
             str))
      (send interactive-info
            set-value interactive-history))

    ;重置交互行:
    (define (reset-interactive-line)
      (set! line-prompt "命令：")
      (set! line-val "")
      (set-interactive-line))

    ;设置交互行内容：
    (define (set-interactive-line)
      (send interactive-line set-label
            (string-append line-prompt line-val)))

    ;设置交互DC：
    (define/public (set-dc dc)
      (set! interactive/dc dc))

    ;取得交互DC：
    (define/public (get-dc)
      interactive/dc)
    
    ;绘制全部交互对象：
    (define/public (draw-objects dc)
      (for/list ([ob objects])
        (cond
          [(equal? (send ob get-style) 'spline)
           (send ob draw dc)])))

    ))
    