#lang racket

(require racket/draw)

(provide
 ;图元结构：
 (struct-out point)

 pels
 
 handler%
 spline%
 bezier%)

;图元管理：=====================================
(define pels empty)

;会话提示结构:
(struct interactive-prompt
  (style ;需要取得的值类型。包括:点'point,选项'select,数值'number
   prompt)) ;提示内容

;图元定义：=====================================
;定义点结构：
(struct point (x y))

;定义控制点类：
(define handler%
  (class object%
    (super-new)
    
    (init-field cp
                (width 4))
    
    (field [hw (/ width 2)])

    (define/public (draw dc)
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

;spline线类：
(define spline%
  (class object%
    (super-new)

    (init-field sp
                cp
                ep)

    (field [create-prompt
            (vector
             (interactive-prompt
              'point
              "请输入起点")
             (interactive-prompt
              'point
              "请输入控制点")
             (interactive-prompt
              'point
              "请输入结束点"))]
           [end-prompt "绘制Spline线结束。"])

    (define/public (draw dc)
      (send dc draw-spline
            (point-x sp) (point-y sp)
            (point-x cp) (point-y cp)
            (point-x ep) (point-y ep)))

    (define/public (draw-handler dc)
      (let ([hsp (new handler% [cp sp])]
            [hcp (new handler% [cp cp])]
            [hep (new handler% [cp ep])])
        (send hsp draw dc)
        (send hcp draw dc)
        (send hep draw dc)
        (draw-handler-line dc sp cp)
        (draw-handler-line dc cp ep)))
    ))

;B样条线类：
(define bezier%
  (class object%
    (super-new)

    (init-field sp
                csp
                cep
                ep)

    (define/public (draw dc)
      (let ([dc-path
             (new dc-path%)])
        (send dc-path move-to
              (point-x sp) (point-y sp))
        (send dc-path curve-to
              (point-x csp) (point-y csp)
              (point-x cep) (point-y cep)
              (point-x ep) (point-y ep))
        (send dc draw-path dc-path)))

    (define/public (draw-handler dc)
      (let ([hsp (new handler% [cp sp])]
            [hcsp (new handler% [cp csp])]
            [hcep (new handler% [cp cep])]
            [hep (new handler% [cp ep])])
        (send hsp draw dc)
        (send hcsp draw dc)
        (send hcep draw dc)
        (send hep draw dc)
        (draw-handler-line dc sp csp)
        (draw-handler-line dc csp cep)
        (draw-handler-line dc cep ep)))
    ))

;通用函数：=========================================
;画控制线：
(define (draw-handler-line dc sp ep)
  (let ([old-pen
         (send dc get-pen)])
    (send dc set-pen "red" 1 'dot)
    (send dc draw-line
          (point-x sp) (point-y sp)
          (point-x ep) (point-y ep))
    (send dc set-pen old-pen)))