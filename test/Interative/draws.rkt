#lang racket

(require racket/draw)

(provide
 ;图元结构：
 (struct-out point)
 (struct-out interactive-prompt)

 pels
 
 handler%
 spline%
 bezier%)

;图元管理：=====================================
(define pels empty)

;会话提示结构:
;由两部分组成：需求值类型，提示。
(struct interactive-prompt
  (style ;需求值类型。包括:点'point,选项'select,数值'number
   prompt)) ;提示

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

;line直线类：
(define line%
  (class object%
    (super-new)

    (field [vals '()]
           [style 'line]
           [create-prompt
            (vector
             (interactive-prompt
              'point
              "请输入起点:")
             (interactive-prompt
              'point
              "请输入下一点:"))]
           [end-prompt "绘制Line线结束。"])

    ;绘制:
    #|(define/public (draw dc)
      (send dc draw-lines vals))|#
    ))

;spline线类：
(define spline%
  (class object%
    (super-new)

    (field [vals '()]
           [style 'spline]
           [create-prompt
            (vector
             (interactive-prompt
              'point
              "请输入起点：")
             (interactive-prompt
              'point
              "请输入控制点：")
             (interactive-prompt
              'point
              "请输入结束点："))]
           [end-prompt "绘制Spline线结束。"])

    ;为字段填充值使之具体：
    (define/public (make-object sp cp ep)
      (set! vals
            (cons sp
                  (cons cp
                        (cons ep values))))
      this)
    
    ;绘制：
    (define/public (draw dc)
      (let-values ([(sp cp ep) (get-values)])
        (send dc draw-spline
              (point-x sp) (point-y sp)
              (point-x cp) (point-y cp)
              (point-x ep) (point-y ep))))

    ;绘制控制点：
    (define/public (draw-handler dc)
      (let-values ([(sp cp ep) (get-values)])
        (let ([hsp (new handler% [cp sp])]
              [hcp (new handler% [cp cp])]
              [hep (new handler% [cp ep])])
          (send hsp draw dc)
          (send hcp draw dc)
          (send hep draw dc)
          (draw-handler-line dc sp cp)
          (draw-handler-line dc cp ep))))

    ;绘制交互即时图：
    (define/public (immediatly-draw dc old-pt cur-pt)
      ;根据v向量值的个数绘制图形：
      ;(send dc set-pen "red" 1 'xor)
      (cond
        [(= (length vals) 1)
         (let ([sp (list-ref vals 0)])
           (send dc set-pen (send dc get-background) 1 'xor)
           (send dc draw-line
                 (point-x sp) (point-y sp)
                 (point-x old-pt) (point-y old-pt))

           (send dc set-pen "red" 1 'xor)
           (send dc draw-line
                 (point-x sp) (point-y sp)
                 (point-x cur-pt) (point-y cur-pt)))]
        [(= (length vals) 2)
         (let ([cp (list-ref vals 0)]
               [sp (list-ref vals 1)])
           (send dc set-pen (send dc get-background) 1 'xor)
           (send dc draw-spline
                 (point-x sp) (point-y sp)
                 (point-x cp) (point-y cp)
                 (point-x old-pt) (point-y old-pt))

           (send dc set-pen "red" 1 'xor)
           (send dc draw-spline
                 (point-x sp) (point-y sp)
                 (point-x cp) (point-y cp)
                 (point-x cur-pt) (point-y cur-pt)))]))
    
    ;提示数量：
    (define/public (prompt-count)
      (vector-length create-prompt))

    ;取得提示内容：
    (define/public (get-prompt n)
      (interactive-prompt-prompt
       (vector-ref create-prompt n)))

    ;取得需求值类型：
    (define/public (get-value-style n)
      (interactive-prompt-style
       (vector-ref create-prompt n)))

    ;取得会话结束提示：
    (define/public (get-end-prompt)
      end-prompt)

    ;取得图形类型：
    (define/public (get-style)
      style)
      
    ;设置绘图值：
    (define/public (set-value v)
      (set! vals (cons v vals)))

    ;取得绘图值：
    (define (get-values)
      (values (list-ref vals 2)
              (list-ref vals 1)
              (list-ref vals 0)))
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