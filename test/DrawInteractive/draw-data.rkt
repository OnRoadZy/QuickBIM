#lang racket

;数据结构描述：===========================================
;绘图结构：
(struct draw-struct
  (style ;绘图类型。如：两点线2p-line
   value-vector)) ;绘图值向量

;绘图值结构：
(struct draw-value-struct
  (style ;值类型。包括：点'point，角度'angle，长度'length
   value)) ;绘图值

;点结构：
(struct point-struct (x y))

;两点线结构：
(struct line/2p-struct
  (start ;起点。点结构
   end)) ;终点。点结构

;多段线结构：
(struct line/poly-struct
  (value-vector)) ;线图元矢量。包含两点线、圆弧等

;半径圆结构：
(struct circle/radius-struct
  (center ;圆心。点结构
   radius)) ;半径

;角圆弧结构：
(struct arc/angle-struct
  (center ;圆心。点结构
   radius ;半径
   start ;起始角
   end)) ;结束角。

;3点圆弧结构：
(struct arc/3p-struct
  (start ;起始点
   middle ;中间点
   end)) ;结束点

;2点圆弧结构：
(struct arc/2p-struct
  (center ;圆心。点结构
   start ;起始点
   end)) ;结束点

;数据操作========================================
;2点圆弧->角圆弧：
(define (arc/2p->arc/angle value)
  (let ([center
         (arc/2p-struct-center value)]
        [radius
         (2p-length (arc/2p-struct-center
                         arc/2p-struct-start))]
        [start
         (2p-angle (arc/2p-struct-center
                        arc/2p-struct-start))]
        [end
         (2p-angle (arc/2p-struct-center
                        arc/2p-struct-end))])
    (arc/angle-struct center radius start end)))

;通用数学函数：=====================================
;计算两点距离：
(define (2p-length p1 p2)
  (sqrt (+
         (expt
          (- (point-struct-x p2)
             (point-struct-x p1)) 2))
        (expt
         (- (point-struct-y p2)
            (point-struct-y p1)) 2)))

;计算两点角度：
(define (2p-angle p1 p2)
  (atan (/
         (- (point-struct-y p2)
            (point-struct-y p1))
         (- (point-struct-x p2)
            (point-struct-x p1)))))