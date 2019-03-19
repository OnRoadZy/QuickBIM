#lang racket

(require "draw-data.rkt"
         "draws.rkt")

(provide interactive-context
         cur-draw

         reset-interactive-context
         interactive-context-reset?
         set-interactive-context
         get-prompt
         get-prompt-style
         get-prompt-vector
         get-prompt-number
         get-current-int
         add-current-int
         get-end-str
         analyze-answer
         save-point
         init-cur-draw)

;数据结构============================================
;会话环境全局变量:
(define interactive-context void)
;定义绘图对象：
(define cur-draw void)

;会话结构：
(struct interactive
  (style ;会话类型。包括：两点线2p-line，多段线poly-line，半径圆‘circle-radius，圆弧arc
   current-int ;当前会话序号。对应会话向量序号
   prompt-vector ;会话提示向量
   end-str) #:mutable) ;会话结束语。如：“线段绘制结束。”

;会话提示结构：
(struct interactive-prompt
  (style ;需要取得的值类型。包括：点'point，选项'select，数值'number
   prompt)) ;提示内容

;绘图结构：
(struct draw
  (style ;绘图类型。同会话结构的style项
   ls) #:mutable) ;绘图值向量

;绘图值类型：
(struct draw-value
  (style ;值类型。包括：点'point，角度'angle，长度'length
   value)) ;绘图值

;交互数据：=============================================
;绘图命令与绘图交互环境类型对应表：
(define draw-style-hash
  (hash "line" 'line/2p
        "poly-line" 'line/poly
        "radius-circle" 'circle/radius
        "3p-arc" 'arc/3p))

;定义交互环境散列表：
(define interactive-hash
  (hash
   ;两点线交互环境：
   'line/2p
   (interactive
    'line/2p
    0
    (vector
     (interactive-prompt
      'point
      "请输入线段的第一点")
     (interactive-prompt
      'point
      "请输入线段的第二点"))
    "绘制两点线结束。")
   ;多段线交互环境：
   'line/poly-context
   (interactive
    'line/poly
    0
    (vector
     (interactive-prompt
      'point
      "请输入多段线的起点")
     (interactive-prompt
      'select
      "请选择多段线当前线的类型（L两点线/A圆弧）"))
    "绘制多段线结束。")
   ;圆交互环境：
   'circle/radius-context
   (interactive
    'circle/radius
    0
    (vector
     (interactive-prompt
      'point
      "请输入圆心坐标")
     (interactive-prompt
      'number
      "请输入半径"))
    "绘制圆结束。")
   ;三点圆弧交互环境：
   'arc/3p-context
   (interactive
    'arc/3p
    0
    (vector
     (interactive-prompt
      'point
      "请输入圆弧第一点")
     (interactive-prompt
      'point
      "请输入圆弧第二点")
     (interactive-prompt
      'point
      "请输入圆弧第三点"))
    "绘制圆弧结束。")))

;数据操作===============================================
;重置会话环境:
(define (reset-interactive-context)
  (set! interactive-context void))

;检查会话环境重置状态：
(define (interactive-context-reset?)
  (equal? interactive-context void))

;是否为会话输入状态：
;interactive-context?->boolean?
(define (command-char/command? context)
  context)

;为画图命令？
(define (command/draw? str)
  (hash-ref draw-style-hash str #f))

;设置交互环境：
(define (set-interactive-context str)
  (if (command/draw? str)
    (set! interactive-context
          (hash-ref interactive-hash
                    (hash-ref draw-style-hash str)))
    #f))

;取得当前会话序号：
(define (get-current-int)
  (interactive-current-int interactive-context))

;增加当前会话序号：
(define (add-current-int)
  (set-interactive-current-int!
   interactive-context
   (+ (get-current-int) 1)))

;取得会话总次数：
(define (get-prompt-number)
  (vector-length
   (interactive-prompt-vector interactive-context)))

;取得会话结束语：
(define (get-end-str)
  (interactive-end-str interactive-context))

;取得交互类型：
(define (get-style)
  (interactive-style interactive-context))

;取得会话提示类型：
(define (get-prompt-style current-int)
  (interactive-prompt-style
   (get-prompt-vector current-int)))

;取得会话提示向量表：
(define (get-prompt-vector current-int)
  (if
   (< (get-current-int)
      (get-prompt-number))
   (vector-ref
    (interactive-prompt-vector interactive-context)
    current-int)
   #f))

;取得会话提示：
(define (get-prompt current-int)
  (string-append
   (interactive-prompt-prompt
    (get-prompt-vector current-int))
   "："))

;设置绘图结构类型：
(define (init-cur-draw)
  (set! cur-draw
   (draw (get-style) null)))

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
         (get-prompt-style (get-current-int))])
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
  (let* ([ls-str (string-split str ",")]
         [v1 (string->number (list-ref ls-str 0))]
         [v2 (string->number (list-ref ls-str 1))])
    (point v1 v2)))

;解析角度点：
(define (analyze-angle-point str)
  (let* ([ls-str (string-split str "<")]
         [v1 (string->number (list-ref ls-str 0))]
         [v2 (string->number (list-ref ls-str 1))])
    (values v1 v2)))

;保存点值：
(define (save-point str)
  (when (value/point? str)
    (set-draw-ls! cur-draw
                  (cons
                   (draw-value
                    'point
                    (analyze-answer str))
                   (draw-ls cur-draw)))))

;画图元：
(define (draw-pel dc value)
  (let ([style (get-style)])
    (cond
      [(equal? style 'line/2p)
       (draw-line/2p dc value)])))