#lang racket

(require racket/draw)

(provide
 ;图元结构：
 (struct-out point)
 
 (struct-out line/2p)
 
 (struct-out square)
 (struct-out rectangle/2p)
 (struct-out rectangle/len)
 
 (struct-out circle/1p)
 (struct-out circle/2p)
 (struct-out circle/r)
 (struct-out circle/3p)

 (struct-out ellipse/cp)
 (struct-out ellipse/2p)
 
 (struct-out arc/a)
 (struct-out arc/2p)
 (struct-out arc/3p)

 (struct-out polygon/pts)
 (struct-out polygon/n)

 (struct-out lines/pts)
 (struct-out line/ploy)
 
 (struct-out text/style)
 (struct-out text/width)

 (struct-out bitmap/bp)

 (struct-out spline/2p)
 (struct-out bezier)
 
 ;通用图元绘制：
 draw-pel
 ;图元绘制：
 draw-line/2p

 draw-rectangle/2p
 draw-rectangle/len
 draw-square
 
 draw-circle/r
 draw-circle/1p
 draw-circle/2p
 draw-circle/3p

 draw-ellipse/cp
 draw-ellipse/2p

 draw-polygon/pts
 draw-polygon/n

 draw-lines/pts

 draw-text/style
 draw-text/width

 draw-bitmap/bp

 draw-spline/2p

 ;判断函数：
 text/left?
 text/right?
 text/middle?
 text/center?)
  
;定义点结构：
(struct point (x y))

;定义图元结构：==============================
;两点线：
(struct line/2p (sp ep))

;长方形：
(struct rectangle/2p (sp ep))
(struct rectangle/len (bp w h))

;正方形：
(struct square (bp l))

;半径圆：
(struct circle/r (cp r))
;点圆：
(struct circle/1p (cp ep))
;两点圆：
(struct circle/2p (sp ep))
;三点圆：
(struct circle/3p (sp mp ep))

;圆心椭圆：
(struct ellipse/cp (cp rh rv))
;两点椭圆：
(struct ellipse/2p (sp ep))

;角圆弧：
(struct arc/a (cp r sa ea))
;两点圆弧：
(struct arc/2p (cp sp ep))
;三点圆弧：
(struct arc/3p (sp mp ep))

;多点多边形：
(struct polygon/pts (bp pts))
;边数正多边形：
(struct polygon/n (bp r num))

;多线段：
(struct lines/pts (bp pts))
;多段线：
(struct line/ploy (bp pts))
;spline线：
(struct spline/2p (sp cp ep))
;B样条线：
(struct bezier (sp csp ep cep))
;nurbs线：

;单点文字：
;style：'left，'right，'middle，'center
(struct text/style (text bp style))
;定宽文字：
(struct text/width (text bp width))

;单点位图：
(struct bitmap/bp (source bp scale))

;填充：
(struct fill/pts (pts))

;绘制单个图元：========================================

;draw-point

;draw-line
;画两点线：
(define (draw-line/2p dc l2p)
  (let-values
      ([(x1 y1 x2 y2)
        (line/2p->draw/line l2p)])
    (send dc draw-line x1 y1 x2 y2)))

;draw-rectangle
;draw-rounded-rectangle
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

;draw-ellipse
;画中心圆：
(define (draw-circle/1p dc pel)
  (let-values
      ([(x y w h) (circle/1p->draw/circle pel)])
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

;画圆心椭圆：
(define (draw-ellipse/cp dc ecp)
  (let-values
      ([(x y w h) (ellipse/cp->draw/ellipse ecp)])
    (send dc draw-ellipse x y w h)))

;画两点椭圆：
(define (draw-ellipse/2p dc e2p)
  (let-values
      ([(x y w h) (ellipse/2p->draw/ellipse e2p)])
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

;draw-polygon
;画多点多边形：
(define (draw-polygon/pts dc ppts)
  (let-values ([(bp pts)
                (polygon/pts->draw/polygon ppts)])
    (send dc draw-polygon
          pts
          (point-x bp)
          (point-y bp))))

;画n边正多边形：
(define (draw-polygon/n dc pn)
  (let-values ([(cp pts)
                (polygon/n->draw/polygon pn)])
    (send dc draw-polygon
          pts
          (point-x cp)
          (point-y cp))))

;draw-lines
;画多线段：
(define (draw-lines/pts dc lpts)
  (let-values ([(bp pts)
                (lines/pts->draw/lines lpts)])
    (send dc draw-lines
          pts
          (point-x bp) (point-y bp))))

;draw-text
;绘制定位类型文字：
(define (draw-text/style dc ts)
  (let-values ([(txt x y scale-x)
                (text/style->draw/text dc ts)]
               [(scale-x/old scale-y/old)
                (send dc get-scale)])
    (send dc set-scale scale-x 1)
    (send dc draw-text txt x y)
    (send dc set-scale scale-x/old scale-y/old)))

;绘制指定宽度文字：
(define (draw-text/width dc tw)
  (let-values ([(txt x y scale-x)
                (text/width->draw/text dc tw)]
               [(scale-x/old scale-y/old)
                (send dc get-scale)])
    (send dc set-scale scale-x 1)
    (send dc draw-text txt x y)
    (send dc set-scale scale-x/old scale-y/old)))

;draw-bitmap
;draw-bitmap-section
;绘制位图：
(define (draw-bitmap/bp dc bbp)
  (let-values ([(src x y scale)
                (bitmap/bp->draw/bitmap bbp)]
               [(scale-x/old scale-y/old)
                (send dc get-scale)])
    (let ([bmp (read-bitmap src)])
      (send dc set-scale scale scale)
      (send dc draw-bitmap bmp x y)
      (send dc set-scale scale-x/old scale-y/old))))

;draw-spline
;绘制spline线：
(define (draw-spline/2p dc s2p)
  (let-values ([(spx spy cepx cepy
                     epx epy)
                (spline/2p->draw/spline s2p)])
    (send dc draw-spline spx spy cepx cepy epx epy)))
                     
   
;draw-path

;通用函数：========================================
;两点线结构转化为画线结构：
(define (line/2p->draw/line l2p)
  (values
   (point-x (line/2p-sp l2p))
   (point-y (line/2p-sp l2p))
   (point-x (line/2p-ep l2p))
   (point-y (line/2p-ep l2p))))

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
   (rectangle/len-h rlen)
   (rectangle/len-w rlen)))

;正方形结构转化为画矩形结构：
(define (square->draw/rectangle sq)
  (values
   (point-x (square-bp sq))
   (point-y (square-bp sq))
   (square-l sq)
   (square-l sq)))

;圆结构转化为画圆结构：
(define (circle/r->draw/circle cr)
  (let* ([cp (circle/r-cp cr)]
         [cx (point-x cp)]
         [cy (point-y cp)]
         [r (circle/r-r cr)])
    (values (- cx r) (- cy r) (+ r r) (+ r r))))

(define (circle/1p->draw/circle ccp)
  (let* ([cp (circle/1p-cp ccp)]
         [ep (circle/1p-ep ccp)]
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
         [ccp (circle/1p cp ep)])
    (circle/1p->draw/circle ccp)))

(define (circle/3p->draw/circle c3p)
  (let ([sp (circle/3p-sp c3p)]
        [mp (circle/3p-mp c3p)]
        [ep (circle/3p-ep c3p)])
    (let-values ([(cp r)
                  (3p-circle sp mp ep)])
      (circle/r->draw/circle
       (circle/r cp r)))))

;圆心椭圆结构转换为画椭圆结构：
(define (ellipse/cp->draw/ellipse ecp)
  (let ([cp (ellipse/cp-cp ecp)]
        [rh (ellipse/cp-rh ecp)]
        [rv (ellipse/cp-rv ecp)])
    (values
     (- (point-x cp) rh)
     (- (point-y cp) rv)
     ( * 2 rh) (* 2 rv))))

(define (ellipse/2p->draw/ellipse e2p)
  (let* ([sp (ellipse/2p-sp e2p)]
         [ep (ellipse/2p-ep e2p)]
         [sp-x (point-x sp)]
         [sp-y (point-y sp)]
         [ep-x (point-x ep)]
         [ep-y (point-y ep)]
         [bp-x (if (< sp-x ep-x)
                   sp-x
                   ep-x)]
         [bp-y (if (< sp-y ep-y)
                   sp-y
                   ep-y)]
         [dh (abs (- ep-x sp-x))]
         [dv (abs (- ep-y sp-y))])
    (values bp-x bp-y dh dv)))

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

;多边形结构转换为画多边形结构：
(define (polygon/pts->draw/polygon ppts)
  (let* ([bp (polygon/pts-bp ppts)]
         [pts (polygon/pts-pts ppts)])
    (if (< (length pts) 3)
        #f
        (values
         bp
         (add-points-to-list null pts)))))

(define (polygon/n->draw/polygon pn)
  (let* ([cp (polygon/n-bp pn)]
         [r (polygon/n-r pn)]
         [num (polygon/n-num pn)]
         [a (/ (* 2 pi) num)]
         [ls null])
    (values
     cp
     (add-ap-to-list a r ls 0 num))))

;多线段结构转换为画多线段结构：
(define (lines/pts->draw/lines lpts)
  (let ([bp (lines/pts-bp lpts)]
        [pts (lines/pts-pts lpts)])
    (values
     bp
     (add-points-to-list null pts))))

;文字结构转换为画文字结构：
(define (text/style->draw/text dc ts)
  (let ([txt (text/style-text ts)]
        [bp (text/style-bp ts)]
        [style (text/style-style ts)])
    (let-values ([(w h bh eh)
                  (send dc get-text-extent txt)])
      (cond
        [(eq? style 'left)
         (let ([y (- (point-y bp) h)])
           (values txt (point-x bp) y 1))]
        [(eq? style 'right)
         (let ([x (- (point-x bp) w)]
               [y (- (point-y bp) h)])
           (values txt x y 1))]
        [(eq? style 'middle)
         (let ([x (- (point-x bp) (/ w 2))]
               [y (- (point-y bp) h)])
           (values txt x y 1))]
        [(eq? style 'center)
         (let ([x (- (point-x bp) (/ w 2))]
               [y (- (point-y bp) (/ h 2))])
           (values txt x y 1))]))))

(define (text/width->draw/text dc tw)
  (let ([txt (text/width-text tw)]
        [bp (text/width-bp tw)]
        [width (text/width-width tw)])
    (let-values ([(w h bh eh)
                  (send dc get-text-extent txt)])
      (let ([scale-x (/ width w)]
            [x (point-x bp)]
            [y (- (point-y bp) h)])
        (values txt x y scale-x)))))

;位图结构转换为绘制位图结构：
(define (bitmap/bp->draw/bitmap bbp)
  (let ([src (bitmap/bp-source bbp)]
        [bp (bitmap/bp-bp bbp)]
        [scale (bitmap/bp-scale bbp)])
    (values src
            (point-x bp) (point-y bp)
            scale)))

;两点spline结构转换为绘制spline结构：
(define (spline/2p->draw/spline s2p)
  (let ([sp (spline/2p-sp s2p)]
        [cp (spline/2p-cp s2p)]
        [ep (spline/2p-ep s2p)])
    (values (point-x sp)
            (point-y sp)
            (point-x cp)
            (point-y cp)
            (point-x ep)
            (point-y ep))))

;判断绘制文字类型：===============================
#|(define-syntax-rule (just-text style)
  (define (text/`style,? pel)
    (if (text/style? pel)
        (if (eq? '`style, (text/style-style pel))
            #t #f)
        #f)))|#
(define (text/left? pel)
  (if (text/style? pel)
      (if (eq? 'left (text/style-style pel))
          #t #f)
      #f))
(define (text/right? pel)
  (if (text/style? pel)
      (if (eq? 'right (text/style-style pel))
          #t #f)
      #f))
(define (text/middle? pel)
  (if (text/style? pel)
      (if (eq? 'middle (text/style-style pel))
          #t #f)
      #f))
(define (text/center? pel)
  (if (text/style? pel)
      (if (eq? 'center (text/style-style pel))
          #t #f)
      #f))

;通用函数：===============================
;添加角度对应点到列表：
(define (add-ap-to-list a r result i num)
  (if (= i num)
      result
      (let ([x (* r (cos (* a i)))]
            [y (* r (sin (* a i)))])
        (add-ap-to-list
         a r
         (cons (cons x y)
                   result)
         (+ i 1) num))))

;添加点到列表：
(define (add-points-to-list result pts)
  (if (empty? pts)
      result
      (let* ([pt (car pts)]
             [x (point-x pt)]
             [y (point-y pt)])
        (add-points-to-list
         (cons (cons x y) result)
         (cdr pts)))))

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
    [(circle/r? pel)
     (draw-circle/r dc pel)]
    [(circle/1p? pel)
     (draw-circle/1p dc pel)]
    [(circle/2p? pel)
     (draw-circle/2p dc pel)]
    [(circle/3p? pel)
     (draw-circle/3p dc pel)]
    ;椭圆：
    [(ellipse/cp? pel)
     (draw-ellipse/cp dc pel)]
    [(ellipse/2p? pel)
     (draw-ellipse/2p dc pel)]
    ;圆弧：
    [(arc/a? pel)
     (draw-arc/a dc pel)]
    [(arc/2p? pel)
     (draw-arc/2p dc pel)]
    [(arc/3p? pel)
     (draw-arc/3p dc pel)]
    ;多边形：
    [(polygon/pts? pel)
     (draw-polygon/pts dc pel)]
    [(polygon/n? pel)
     (draw-polygon/n dc pel)]

    ;多线段：
    [(lines/pts? pel)
     (draw-lines/pts dc pel)]

    ;文字：
    [(text/style? pel)
     (draw-text/style dc pel)]
    [(text/width? pel)
     (draw-text/width dc pel)]

    ;位图：
    [(bitmap/bp? pel)
     (draw-bitmap/bp dc pel)]

    ;spline:
    [(spline/2p? pel)
     (draw-spline/2p dc pel)]
    ))