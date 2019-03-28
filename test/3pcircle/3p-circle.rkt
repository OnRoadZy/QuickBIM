;3p-circle.rkt
;三点确定圆：

#lang racket

(provide
 (struct-out point)
 3p-circle)

;数学推理：===========================================
;如何通过三个点确定一个圆？
;这是一个有趣的几何问题，同时也是计算机图形学要处理的问题。

;这里给出一个求解办法：
;给出三个点：(spx,spy)，(mpx,mpy)，(epx,epy)
;求解圆。
;根据圆半径（r）、圆心(cpx,cpy)、圆上的一点(x,y)的关系：
;(x - cpx)^2 + (y - cpy)^2 = r^2

;得出三个等式：
;1）：(spx - cpx)^2 + (spy - cpy)^2 = r^2
;2）：(mpx - cpx)^2 + (mpy - cpy)^2 = r^2
;3）：(epx - cpx)^2 + (epy - cpy)^2 = r^2

;解开等式，得：
;1）：spx^2 + cpx^2 - 2 * spx * cpx + spy^2 + cpy^2 - 2 * spy * cpy = r^2
;2）：mpx^2 + cpx^2 - 2 * mpx * cpx + mpy^2 + cpy^2 - 2 * mpy * cpy = r^2
;3）：epx^2 + cpx^2 - 2 * epx * cpx + epy^2 + cpy^2 - 2 * epy * cpy = r^2

;执行等式相减1）-2），1）-3），得到：
;4）：cpx * (spx - mpx) - cpy * (spy - mpy) =[(spx^2 - mpx^2) + (spy^2 - mpy^2)] / 2
;5）：cpx * (spx - epx) - cpy * (spy - epy) =[(spx^2 - epx^2) + (spy^2 - epy^2)] / 2

;进行代数替代，设：
;smx = spx - mpx
;smy = spy - mpy
;sex = spx - epx
;sey = spy - epy
;smxy = [(spx^2 - mpx^2) + (spy^2 - mpy^2)] / 2
;sexy = [(spx^2 - epx^2) + (spy^2 - epy^2)] / 2

;简化等式为：
;4）：smx * cpx - smy * cpy = smxy
;5）：sex * cpx - sey * cpy = sexy

;再计算得到cpx和cpy：
;cpx = (sey * smxy - smy * sexy) / (smx * sey - smy * sex)
;cpy = (sex * smxy - smx * sexy) / (smx * sey - smy * sex)

;最后计算r值：
;r = [(spx - cpx)^2 + (spy - cpy)^2]^(1/2)

;根据以上推导，实现如下：
;Racket实现：===================================
;点结构：
(struct point (x y))
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
        [cpx (/
               (- (* sey smxy)
                  (* smy sexy))
               (- (* smx sey)
                  (* smy sex)))]
        [cpy (/
               (- (* sex smxy)
                  (* smx sexy))
               (- (* smx sey)
                  (* smy sex)))]
        [r (sqrt
            (+
             (expt (- cpx epx) 2)
             (expt (- cpy epy) 2)))])
    (values
     (point cpx cpy)
     r)))
