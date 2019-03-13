#lang racket

(require racket/gui
         graphics/graphics)

;会话及绘图===============================
(define (paint cv dc)
  (send dc set-scale 1 1)
  (send cv set-canvas-background (make-object color% 0 0 0 1.0))
  (send dc set-text-foreground "white")
  (send dc draw-text "快速BIM系统（QuickBIM）!" 0 0)
  (send cv on-event (new mouse-event% [event-type (list 'left-down)])))

;扩展控件类：======================
;扩展画布类：
(define interactive-canvas%
  (class canvas%
    ;重定义鼠标事件：
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

    ;重定义键盘事件：
    (define/override (on-char event) ;key-event%
      (let ([key (send event get-key-code)])
        (cond
          ;Esc键：
          [(equal? key 'escape)
           (add-str-to-commang-line "取消")
           (save-command)]
          ;Backspace键：
          [(equal? key #\backspace)
           (backspace-command-line)]
          ;回车：
          [(or (equal? key #\return )
               (equal? key 'numpad-enter ))
           (save-command)]
          ;取可作为命令的编辑字符加入命令行：
          [else (add-str-to-commang-line
                 (get-char-from-event key))])))

    (super-new)))

;处理鼠标右键事件：
(define (click-right-button event)
  (save-command))

;处理鼠标左键事件：
(define (click-left-button event)
  ;检查是否存在绘图环境：
  (unless (equal? interactive-context void)
    ;仅在会话的绘图值为点（point）时，才接受鼠标点值：
    (let ([current-int (get-current-int)])
      (when (equal?
             (get-interactive-value-style current-int)
             'point)
        (get-mouse-point
         current-int
         (get-pos-from-event event))))))

;接受鼠标点值：
(define (get-mouse-point current-int point)
  ;设置当前会话绘图值：
  (set-interactive-value current-int point)
  ;添加命令显示列表：
  (add-str-to-commang-line point)
  (save-command)
  (refresh-command current-int))

;从键盘事件取得字符：
(define (get-char-from-event key)
  (if (and (char? key)
           (or (char-alphabetic? key)
               (char-numeric? key)
               (char=? key #\()
               (char=? key #\))
               (char=? key #\<)
               (char=? key #\,)
               (char=? key #\.)
               (char=? key #\-)
               (char=? key #\@)))
      (format "~a" key)
      ""))

;回退命令行文本：
(define (backspace-command-line)
  (let* ([str (send command-line get-label)]
         [ls-str (regexp-split #rx"：" str)]
         [len (string-length
               (list-ref ls-str 1))])
    (when (> len 0)
           (send command-line set-label
                 (substring str 0 (- (string-length str) 1))))))
           
;添加字串到命令行文本：
(define (add-str-to-commang-line str)
  (when (> (string-length str) 0)
    (send command-line set-label
          (string-append
           (send command-line get-label)
           str))))

;从命令行取得回答文本：
(define (get-answer-from-command-line)
  (let ([str (send command-line get-label)])
    (string-trim
     (list-ref
      (regexp-split #rx"：" str)
      1))))

;检查回答文本合法性：
(define (answer-rightful? str)
  (let ([value-style
         (get-interactive-value-style (get-current-int))])
    (cond
      [(equal? value-style 'point)
       (regexp-match #px"[0-9]*[<,][0-9]*" str)]
      [(equal? value-style 'number)
       (real? (string->number str))])))

;是否为坐标格式，
;坐标格式有两种：（number,number）,(number<degree)
(define (value/point? str)
  (when (or
         (regexp-match #rx"," str)
         (regexp-match #rx"<" str))
    (let* ([ls-str (regexp-split #rx"[<,]" str)]
           [first-str (list-ref ls-str 0)]
           [second-str (list-ref ls-str 1)])
      (and (string->number first-str)
           (string->number second-str)))))
;解析回答文本：
(define (analyze-answer str)
  (let ([value-style
         (get-interactive-value-style (get-current-int))])
    (cond
      [(equal? value-style 'point) (get-answer/point str)]
      [(equal? value-style 'number) (string->number str)]
      [(equal? value-style 'string) str])))

;解析点字串为点结构：
(define (get-answer/point str)
  ;点对形式：
  (if (regexp-match #rx"," str)
      (analyze-number-point str) ;数值点形式
      (analyze-angle-point str)));角度点形式

;解析数值点：
(define (analyze-number-point str)
  (let* ([ls-str (regexp-split #rx"," str)]
         [v1 (string->number (string-ref str 0))]
         [v2 (string->number (string-ref str 1))])
    (point-struct v1 v2)))

;解析角度点：
(define (analyze-angle-point str)
  (let* ([ls-str (regexp-split #rx"<" str)]
         [v1 (string->number (string-ref str 0))]
         [v2 (string->number (string-ref str 1))])
    (values v1 v2)))

;添加交互记录：
(define (save-command)
  (when (non-empty-string? (get-answer-from-command-line))
    (send command-line-list set-value
          (format "~a\n~a"
                  (send command-line-list get-value)
                  (send command-line get-label)))
    (clear-command-line)))

;清空命令行：
(define (clear-command-line)
  (send command-line set-label "命令："))

;刷新命令提示：
(define (refresh-command current-int)
  (send command-line set-label
        (get-interactive-value-prompt current-int)))
  
;从鼠标事件取得鼠标位置：
(define (get-pos-from-event event)
  (format "~a,~a"
          (send event get-x)
          (send event get-y)))

;在状态栏显示鼠标位置：
(define (show-mouse-pos event)
  (send status/mouse-position set-label
        (get-pos-from-event event)))

;根据输入命令确定初始化绘图环境：
(define (cond-draw str)
  (cond
    ;初始化画线（line）绘图环境：
    [(equal? str "line") (set-context/line)]))

;定义绘图函数：======================
;定义会话环境结构，以备判断：
(struct interactive-context-struct
  (style value-vector current-int end-prompt) #:mutable)
;定义会话值结构：
;style：'point,'number,'string
(struct interactive-value-struct
  (style prompt (value #:mutable)))

;初始化会话环境：
(define interactive-context void)

;取得当前会话序号：
(define (get-current-int)
  (interactive-context-struct-current-int interactive-context))
   
;取得会话值：
(define (get-interactive-context-value current-int)
  (vector-ref
    (interactive-context-struct-value-vector interactive-context)
    current-int))
 
;取得绘图值：
(define (get-interactive-value current-int)
  (interactive-value-struct-value
   (get-interactive-context-value current-int)))

;取得绘图值类型：
(define (get-interactive-value-style current-int)
  (interactive-value-struct-style
   (get-interactive-context-value current-int)))

;取得绘图值的会话提示：
(define (get-interactive-value-prompt current-int)
  (string-append
   (interactive-value-struct-prompt
   (get-interactive-context-value current-int))
   "："))
  
;设置绘图环境值并将当前会话序号增加1：
(define (set-interactive-value current-int value)
  (vector-set!
   (interactive-context-struct-value-vector interactive-context)
   current-int
   value)
  (set-interactive-context-struct-current-int! (+ current-int 1)))

;取得对话次数：
(define (get-interactive-times)
  (vector-length
   (interactive-context-struct-value-vector interactive-context)))

;画线
;设置对话上下文：
(define (set-context/line)
  (set-interactive-context-struct-style! 'line)
  (set-interactive-context-struct-value-vector!
   (vector
    (interactive-value-struct
     'point
     "请输入直线的第一个点"
     void)
    (interactive-value-struct
     'point
     "请输入直线的第二个点"
     void))
    0
    "画线……"))

;点数据结构：
(struct point-struct (x y))

;线段数据结构：
(struct line-struct (p1 p2))

(define (draw-line dc)
  (let ([x1 (point-struct-x (get-interactive-value 0))]
        [y1 (point-struct-y (get-interactive-value 0))]
        [x2 (point-struct-x (get-interactive-value 1))]
        [y2 (point-struct-y (get-interactive-value 1))])
  (send dc draw-line
        x1 y1
        x2 y2)))

;定义主界面=======================
(define main-frame
  (new frame%
       [label "QuickBIM"]
       [width 800]
       [height 600]
       [border 5]))

(send main-frame show #t)

;定义视图框架分割区域：========================
;总容器：
(define pane/all
  (new vertical-pane%
       [parent main-frame]
       [alignment (list 'left 'top)]))

;视图区：
(define pane/view
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]))

;画布：
(define canvas
  (new interactive-canvas%
       [parent pane/view]
       [paint-callback
        (lambda (canvas dc) (paint canvas dc))]))

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

;已用命令列表显示框：
(define command-line-list
  (new text-field% 
       [parent pane/interactive]
       [label #f]
       [style (list 'multiple)]
       [init-value "准备就绪！"]
       [min-height 80]
       [stretchable-height #f]))

;命令编辑框：
(define command-line
  (new message%
       [parent pane/interactive]
       [label "命令："]))

;状态栏：
(define pane/status
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]
       [stretchable-height #f]))

;信息
(define status/mouse-position
  (new message%
       [parent pane/status]
       [label "程序准备就绪！"]
       [auto-resize #t]))
