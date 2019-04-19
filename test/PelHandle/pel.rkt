#lang racket

(require racket/draw)

(provide
 ;图元结构：
 (struct-out point)
 
 handler%)

 
;定义点结构：
(struct point (x y))

;B样条线：
(struct bezier/4p (sp csp cep ep))
;spline线类：
;(class spline/3p (sp cp ep))

;定义控制点类：
(define handler%
  (class object%
    (super-new)
    
    (init-field cp
                (width 4))
    
    (field [hw (/ width 2)])

    (define/public (draw-handler dc)
      (let ([old-brush (send dc get-brush)]
            [old-pen (send dc get-pen)])
        (send dc set-pen "red" 0 'transparent)
        (send dc set-brush "red" 'solid)
        (send dc draw-rectangle
              (- (point-x cp) hw)
              (- (point-y cp) hw)
              width width)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen)))
  ))

