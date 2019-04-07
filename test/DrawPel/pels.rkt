#lang racket

(require racket/draw)

(provide
 ;图元结构：
 (struct-out point)
 (struct-out line/2p)
 (struct-out line/ploy)
 (struct-out square)
 (struct-out rectangle/2p)
 (struct-out rectangle/len)
 (struct-out circle/cp)
 (struct-out circle/2p)
 (struct-out circle/r)
 (struct-out circle/3p)
 (struct-out arc/a)
 (struct-out arc/2p)
 (struct-out arc/3p)
 (struct-out ellipse)
 ;通用图元绘制：
 draw-pel
 ;图元绘制：
 draw-line/2p
 draw-square
 draw-rectangle/2p
 draw-rectangle/len
 draw-circle/cp
 draw-circle/r
 draw-circle/2p
 draw-circle/3p)
  

;定义点结构：
(struct point (x y))

;定义图原结构：==============================
;两点线：
(struct line/2p (sp ep))

;多段线：
(struct line/ploy (sp points))
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
(struct circle/r (cp r))
;三点圆：
(struct circle/3p (sp mp ep))

;角圆弧：
(struct arc/a (cp r sa ea))
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

;draw-point

;draw-line
;画两点线：
(define (draw-line/2p dc l2p)
  (let-values
      ([(x1 y1 x2 y2)
        (line/2p->draw/line l2p)])
    (send dc draw-line x1 y1 x2 y2)))

;draw-lines

;draw-rectangle
;绘制正方形：
(define (draw-square dc sq)
  (let-values
      ([(x y width height)
        (square->draw/rectangle sq)])
    (send dc draw-rectangle
          x y width height)))

;绘制两点矩形：
(define (draw-rectangle/2p dc r2p)
  (let-values
      ([(x y width height)
        (rectangle/2p->draw/rectangle r2p)])
    (send dc draw-rectangle
          x y width height)))

;绘制两点矩形：
(define (draw-rectangle/len dc rlen)
  (let-values
      ([(x y width height)
        (rectangle/len->draw/rectangle rlen)])
    (send dc draw-rectangle
          x y width height)))
                           
;draw-rounded-rectangle
;draw-polygon

;draw-ellipse
;画中心圆：
(define (draw-circle/cp dc pel)
  (let-values
      ([(x y w h) (circle/cp->draw/circle pel)])
    (send dc draw-ellipse x y w h)))

;画半径圆：
(define (draw-circle/r dc pel)
  (let-values
      ([(x y w h) (circle/r->draw/circle pel)])
    (send dc draw-ellipse x y w h)))

;画两点圆：
(define (draw-circle/2p dc pel)
  (let-values
      ([(x y w h) (circle/2p->draw/circle pel)])
    (send dc draw-ellipse x y w h)))

;画三点圆：
(define (draw-circle/3p dc pel)
  (let-values
      ([(x y w h) (circle/3p->draw/circle pel)])
    (send dc draw-ellipse x y w h)))

;draw-arc
;画角度圆弧：
(define (draw-arc/a dc pel)
  (let-values
      ([(x y w h sa ea) (arc/a->draw/arc pel)])
    (send dc draw-arc x y w h sa ea)))

;画两点圆弧：
(define (draw-arc/2p dc pel)
  (let-values
      ([(x y w h sa ea) (arc/2p->draw/arc pel)])
    (send dc draw-arc x y w h sa ea)))

;画三点圆弧：
(define (draw-arc/3p dc pel)
  (let-values
      ([(x y w h sa ea) (arc/3p->draw/arc pel)])
    (send dc draw-arc x y w h sa ea)))

;draw-bitmap
;draw-bitmap-section

;draw-text
;draw-spline
;draw-path

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
  (let* ([cp (circle/cp-cp ccp)]
         [ep (circle/cp-ep ccp)]
         [xc (point-x cp)]
         [yc (point-y cp)]
         [xe (point-x ep)]
         [ye (point-y ep)]
         [r
          (sqrt
           (+
            (expt (- xe xc) 2)
            (expt (- ye yc) 2)))]
         [width (* 2 r)])
    (values (- xc r) (- yc r) width width)))

(define (circle/2p->draw/circle c2p)
  (let* ([sp (circle/2p-sp c2p)]
         [ep (circle/2p-ep c2p)]
         [sx (point-x sp)]
         [sy (point-y sp)]
         [ex (point-x ep)]
         [ey (point-y ep)]
         [cp (point
              (/ (+ sx ex) 2)
              (/ (+ sy ey) 2))]
         [ccp (circle/cp cp ep)])
    (circle/cp->draw/circle ccp)))

(define (circle/r->draw/circle cr)
  (let* ([cp (circle/r-cp cr)]
         [cx (point-x cp)]
         [cy (point-y cp)]
         [r (circle/r-r cr)])
    (values (- cx r) (- cy r) (+ r r) (+ r r))))

(define (circle/3p->draw/circle c3p)
  (let ([sp (circle/3p-sp c3p)]
        [mp (circle/3p-mp c3p)]
        [ep (circle/3p-ep c3p)])
    (let-values ([(cp r)
                  (3p-circle sp mp ep)])
      (circle/r->draw/circle
       (circle/r cp r)))))

;圆弧结构转换为画圆弧结构：
(define (arc/a->draw/arc aa)
  (let ([cp (arc/a-cp aa)]
        [r (arc/a-r aa)]
        [sa (arc/a-sa aa)]
        [ea (arc/a-ea aa)])
    (values
     (- (point-x cp) r)
     (- (point-y cp) r)
     (+ r r) (+ r r)
     sa ea)))

(define (arc/2p->draw/arc a2p)
  (let* ([cp (arc/2p-cp a2p)]
         [sp (arc/2p-sp a2p)]
         [ep (arc/2p-ep a2p)]
         [r (len-2p cp sp)]
         [sa (angle-2p cp sp)]
         [ea (angle-2p cp ep)]
         [aa (arc/a cp r sa ea)])
    (arc/a->draw/arc aa)))

(define (arc/3p->draw/arc a3p)
  (let ([sp (arc/3p-sp a3p)]
        [mp (arc/3p-mp a3p)]
        [ep (arc/3p-ep a3p)])
    (let-values
        ([(cp r) (3p-circle sp mp ep)])
      (arc/2p->draw/arc
       (arc/2p cp sp ep)))))

;通用函数：===============================
;两点之间的距离：
(define (len-2p sp ep)
  (let ([sx (point-x sp)]
        [sy (point-y sp)]
        [ex (point-x ep)]
        [ey (point-y ep)])
    (sqrt
     (+ (expt (- ex sx) 2)
        (expt (- ey sy) 2)))))

;两点之间的角度（弧度）：
(define (angle-2p sp ep)
  (let ([sx (point-x sp)]
        [sy (point-y sp)]
        [ex (point-x ep)]
        [ey (point-y ep)])
    (atan (/ (- ey sy)
             (- ex sx)))))

;三点定圆：
(define (3p-circle sp mp ep)
  (let* ([spx (point-x sp)]
         [spy (point-y sp)]
         [mpx (point-x mp)]
         [mpy (point-y mp)]
         [epx (point-x ep)]
         [epy (point-y ep)]
         [smx (- spx mpx)]
         [smy (- spy mpy)]
         [sex (- spx epx)]
         [sey (- spy epy)]
         [smxy (/
               (+
                (- (expt spx 2)
                   (expt mpx 2))
                (- (expt spy 2)
                   (expt mpy 2)))
               2)]
        [sexy (/
               (+
                (- (expt spx 2)
                   (expt epx 2))
                (- (expt spy 2)
                   (expt epy 2)))
               2)]
        [cpy (/
              (- (* sexy smx)
                 (* smxy sex))
              (- (* smx sey)
                 (* sex smy)))]
        [cpx (/
              (- smxy (* smy cpy))
              smx)]
        [r (sqrt
            (+
             (expt (- cpx epx) 2)
             (expt (- cpy epy) 2)))])
    (values (point cpx cpy) r)))

(define (draw-pel dc pel)
  (cond
    ;线：
    [(line/2p? pel)
     (draw-line/2p dc pel)]
    ;框：
    [(square? pel)
     (draw-square dc pel)]
    [(rectangle/2p? pel)
     (draw-rectangle/2p dc pel)]
    [(rectangle/len? pel)
     (draw-rectangle/len dc pel)]
    ;圆：
    [(circle/cp? pel)
     (draw-circle/cp dc pel)]
    [(circle/r? pel)
     (draw-circle/r dc pel)]
    [(circle/2p? pel)
     (draw-circle/2p dc pel)]
    [(circle/3p? pel)
     (draw-circle/3p dc pel)]
    ;圆弧：
    [(arc/a? pel)
     (draw-arc/a dc pel)]
    [(arc/2p? pel)
     (draw-arc/2p dc pel)]
    [(arc/3p? pel)
     (draw-arc/3p dc pel)]))