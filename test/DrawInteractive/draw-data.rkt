#lang racket

(provide point
         draw-pel
         draw-line/2p)

;数据结构描述：===========================================
;绘图结构：
(struct draw
  (style ;绘图类型。如：两点线2p-line
   value)) ;绘图值向量

;点结构：
(struct point (x y))

;两点线结构：
(struct line/2p
  (start ;起点。点结构
   end)) ;终点。点结构

;多段线结构：
(struct line/poly
  (value/v)) ;线图元矢量。包含两点线、圆弧等

;半径圆结构：
(struct circle/radius
  (center ;圆心。点结构
   radius)) ;半径

;角圆弧结构：
(struct arc/angle
  (center ;圆心。点结构
   radius ;半径
   start ;起始角
   end)) ;结束角。

;3点圆弧结构：
(struct arc/3p
  (start ;起始点
   middle ;中间点
   end)) ;结束点

;2点圆弧结构：
(struct arc/2p
  (center ;圆心。点结构
   start ;起始点
   end)) ;结束点

;图元绘制：========================================
;画一个图元：
(define (draw-pel dc value)
  (cond
    [(equal? (draw-style value) '2p-line)
     (draw-line/2p dc (draw-value value))]))

;画两点线：
(define (draw-line/2p dc value)
  (let ([start (line/2p-start value)]
        [end (line/2p-end value)])
    (send dc draw-line
          (point-x start) (point-y start)
          (point-x end) (point-y end))))

;画半径圆：
(define (draw-circle/radius dc value)
  (let* ([center (circle/radius-center value)]
        [radius (circle/radius-radius value)]
        [x (- point-x radius)]
        [y (- point-y radius)]
        [width (+ radius radius)]
        [height width])
    (send dc draw-ellipse
          x y
          width height)))

;画角圆弧：
(define (draw-arc/angle dc value)
  (let* ([center (arc/angle-center value)]
        [radius (arc/angle-radius value)]
        [start (arc/angle-start value)]
        [end (arc/angle-end value)]
        [x (- ((point-x center) radius))]
        [y (- ((point-y center) radius))]
        [width (+ radius radius)]
        [height width])
    (send dc draw-arc
          x y
          width height
          start end)))

;数据转换：========================================
;2点圆弧->角圆弧：
         (define (arc/2p->arc/angle value)
           (let* ([center (arc/2p-center value)]
                  [start (arc/2p-start value)]
                  [end (arc/2p-end value)]
                  [radius (2p-length center start)]
                  [a/start (2p-angle center start)]
                  [a/end (2p-angle center end)])
             (arc/angle center radius a/start a/end)))

;通用数学函数：=====================================
;计算两点距离：
(define (2p-length p1 p2)
  (sqrt (+
         (expt
          (- (point-x p2)
             (point-x p1)) 2)
        (expt
         (- (point-y p2)
            (point-y p1)) 2))))

;计算两点角度：
(define (2p-angle p1 p2)
  (atan (/
         (- (point-y p2)
            (point-y p1))
         (- (point-x p2)
            (point-x p1)))))