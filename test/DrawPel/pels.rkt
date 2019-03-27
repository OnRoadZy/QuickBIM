#lang racket

(require racket/draw)

(provide
 ;图元结构：
 (struct-out point)
 (struct-out line/2p)
 (struct-out square)
 (struct-out rectangle/2p)
 (struct-out rectangle/len)
 (struct-out circle/cp)
 (struct-out circle/2p)
 (struct-out circle/radius)
 (struct-out circle/3p)
 (struct-out arc/angle)
 (struct-out arc/2p)
 (struct-out arc/3p)
 (struct-out ellipse)
 ;图元绘制：
 draw-line/2p
 draw-square
 draw-rectangle/2p
 draw-rectangle/len)
  

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
(struct arc/angle (cp r sa ea))
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

;绘制单个图元：========================================
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

;绘制两点矩形（draw-rectangle）：
(define (draw-rectangle/len dc rlen)
  (let-values
      ([(x y width height)
        (rectangle/len->draw/rectangle rlen)])
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
    (values xs ys
            (- xe xs)
            (- ye ys))))

(define (rectangle/len->draw/rectangle rlen)
  (values
   (point-x (rectangle/len-bp rlen))
   (point-y (rectangle/len-bp rlen))
   (rectangle/len-w rlen)
   (rectangle/len-l rlen)))

;圆结构转化为画圆结构：
(define (circle/cp->draw/circle ccp)
  void)
