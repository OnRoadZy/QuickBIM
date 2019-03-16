#lang racket

(provide interactive-context
         
         interactive-struct
         interactive-prompt-struct
         draw-struct
         draw-value-struct
         
         reset-interactive-context
         interactive-context-reset?)

;数据结构============================================
;会话环境全局变量:
(define interactive-context void)

;会话结构：
(struct interactive-struct
  (style ;会话类型。包括：两点线2p-line，多段线poly-line，半径圆‘circle-radius，圆弧arc
   current-int ;当前会话序号。对应会话向量序号
   prompt-vector ;会话提示向量
   end-str)) ;会话结束语。如：“线段绘制结束。”

;会话提示结构：
(struct interactive-prompt-struct
  (style ;需要取得的值类型。包括：点'point，选项'select，数值'number
   prompt)) ;提示内容

;绘图结构：
(struct draw-struct
  (style ;绘图类型。同会话结构的style项
   value-vector)) ;绘图值向量

;绘图值类型：
(struct draw-value-struct
  (style ;值类型。包括：点'point，角度'angle，长度'length
   value)) ;绘图值

;交互数据：=============================================
;绘图命令与绘图交互环境类型对应表：
(define command-style-hash
  (hash "line" 'line/2p
        "poly-line" 'line/poly
        "radius-circle" 'circle/radius
        "3p-arc" 'arc/3p))

;两点线交互环境：
(define line/2p-context
  (interactive-struct
   'line/2p
   0
   (vector (interactive-prompt-struct
            ('point
             "请输入线段的第一点"))
           (interactive-prompt-struct
            ('point
             "请输入线段的第二点")))
   "绘制两点线结束。"))

;多段线交互环境：
(define line/poly-context
  (interactive-struct
   'line/poly
   0
   (vector (interactive-prompt-struct
            ('point
             "请输入多段线的起点"))
           (interactive-prompt-struct
            ('select
             "请选择多段线当前线的类型（L两点线/A圆弧）")))
   "绘制多段线结束。"))

;圆交互环境：
(define circle/radius-context
  (interactive-struct
   'circle/radius
   0
   (vector (interactive-prompt-struct
            ('point
             "请输入圆心坐标"))
           (interactive-prompt-struct
            ('number
             "请输入半径")))
   "绘制圆结束。"))

;三点圆弧交互环境：
(define arc/3p-context
  (interactive-struct
   'arc/3p
   0
   (vector (interactive-prompt-struct
            ('point
             "请输入圆弧第一点"))
           (interactive-prompt-struct
            ('point
             "请输入圆弧第二点"))
           (interactive-prompt-struct
            ('point
             "请输入圆弧第三点")))
   "绘制圆弧结束。"))

;数据操作===============================================
;重置会话环境:
(define (reset-interactive-context)
  (set! interactive-context void))

;检查重置状态：
(define (interactive-context-reset?)
  (equal? interactive-context void))

;是否为会话输入状态：
;interactive-context-struct?->boolean?
(define (command-char/command? context)
  context)





