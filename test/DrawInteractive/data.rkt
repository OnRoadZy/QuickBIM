#lang racket

(provide interactive-struct
         interactive-prompt-struct
         draw-struct
         draw-value-struct)

;数据结构============================================
;会话结构：
(struct interactive-struct
  (style ;会话类型。包括：两点线2p-line，多段线poly-line，半径圆‘circle-radius，圆弧arc
   current-int ;当前会话序号。对应会话向量序号
   prompt-vector ;会话提示向量
   end-str)) ;回话结束语。如：“线段绘制结束。”

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

;数据操作===============================================
;是否为命令输入状态：
;interactive-context-struct?->boolean?
(define (command-char/command? context)
  context)





