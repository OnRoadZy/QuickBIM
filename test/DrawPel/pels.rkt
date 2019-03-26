#lang racket

(require racket/draw)

(provide draw-pels)

(define draws null)

;定义点结构：
(struct point (x y))

;定义图原结构：==============================
;两点线：
(struct line/2p (sp ep))

;多段线：

;正方形：
(struct square (bp l))

;长方形：
(struct rectangle/2p (sp ep))
(struct rectangle/len (bp l w))

;正多边形：

;多边形：

;点圆：
(struct circle/cp (cp ep))
;两点圆：
(struct circle/2p (sp ep))
;半径圆：
(struct circle/radius (cp r))
;三点圆：
(struct circle/3p (sp mp ep))

;角圆弧：
(struct arc/qngle (cp sa ea))
;两点圆弧：
(struct arc/2p (cp sp ep))
;三点圆弧：
(struct arc/3p (sp mp ep))

;椭圆：
(struct ellipse (cp hl vl))
;椭圆弧：
;(struct elliptic-arc (cp hl vl sa ea))

;spline线：

;B样条线：

;nurbs线：

;定义图元：========================================
;两点线：
(define cur-line/2p
  (line/2p
   (point 10 40)
   (point 100 100)))

;正方形：
(define cur-square
  (square
   (point 100 30)
   50))
;矩形：
(define cur-rectangle/2p
  (rectangle/2p
   (point 170 30)
   (point 240 50)))

;绘制图元：========================================
(define (draw-pels dc)
  (draw-line/2p dc cur-line/2p)
  (draw-square dc cur-square)
  (draw-rectangle/2p dc cur-rectangle/2p))

;画两点线（draw-line）：
(define (draw-line/2p dc l2p)
  (let-values
      ([(x1 y1 x2 y2)
        (line/2p->draw/line l2p)])
    (send dc draw-line x1 y1 x2 y2)))

;draw-arc
;draw-bitmap
;draw-bitmap-section
;draw-ellipse
;draw-lines
;draw-path
;draw-point
;draw-polygon
;绘制正方形（draw-rectangle）：
(define (draw-square dc sq)
  (let-values
      ([(x y width height)
        (square->draw/rectangle sq)])
    (send dc draw-rectangle
          x y width height)))

;绘制两点矩形（draw-rectangle）：
(define (draw-rectangle/2p dc r2p)
  (let-values
      ([(x y width height)
        (rectangle/2p->draw/rectangle r2p)])
    (send dc draw-rectangle
          x y width height)))
                           
;draw-rounded-rectangle
;draw-spline
;draw-text

;通用函数：========================================
;两点线结构转化为画线结构：
(define (line/2p->draw/line l2p)
  (values
   (point-x (line/2p-sp l2p))
   (point-y (line/2p-sp l2p))
   (point-x (line/2p-ep l2p))
   (point-y (line/2p-ep l2p))))

;正方形结构转化为画矩形结构：
(define (square->draw/rectangle sq)
  (values
   (point-x (square-bp sq))
   (point-y (square-bp sq))
   (square-l sq)
   (square-l sq)))

;矩形结构转化为画矩形结构：
(define (rectangle/2p->draw/rectangle r2p)
  (let ([xs (point-x (rectangle/2p-sp r2p))]
        [ys (point-y (rectangle/2p-sp r2p))]
        [xe (point-x (rectangle/2p-ep r2p))]
        [ye (point-y (rectangle/2p-ep r2p))])
    (if (< xs xe)
        (if (< ys ye)
            (values xs ys
                    (- xe xs)
                    (- ye ys))
            (values xs ye
                    (- xe xs)
                    (- ys ye)))
        (if (< ys ye)
            (values xe ys
                    (- xs xe)
                    (- ye ys))
            (values xe ye
                    (- xs xe)
                    (- ys ye))))))
