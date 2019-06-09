#lang racket

;行r列c对应单元值：
(define (cell-value m r c)
  (vector-ref
   (vector-ref m r)
   c))

;检查单元值是否为0：
(define (cell-zero? m r c)
  (zero? (cell-value m r c)))

;取得最大列数：
(define (column-max m)
  (vector-length (vector-ref m 0)))

;找r行以下c列不为0的行：
(define (find-nzero-row m r c)
  (if (= r (vector-length m))
      r
      (if (not (cell-zero? m r c))
          r
          (find-nzero-row m (+ r 1) c))))

;交换r行与r-c行：
(define (swap-row! m r r-c)
  (let ([t (vector-ref m r)])
    (vector-set! m r (vector-ref m r-c))
    (vector-set! m r-c t)))

;对r行进行简化：
(define (simple-row! v c val)
  (if (= c (vector-length v))
      v
      (if (zero? val)
          (if (zero? (vector-ref v c))
              (simple-row! v (+ c 1) val)
              (simple-row!
               (vector-set! v c 1)
               (+ c 1)
               (vector-ref v c)))
          (simple-row!
           (vector-set! v c
                        (/ (vector-ref v c) val))
           (+ c 1)
           val))))

;对r行c列后的每一列求值：
(define (eval-row! vr vr-c c val)
  (if (= c (vector-length vr))
      vr-c
      (begin
        (vector-set! vr-c c
                     (+ (* (vector-ref vr c) val)
                        (vector-ref vr-c c)))
        (eval-row! vr vr-c (+ c 1) val))))

;计算行消除乘积数：
(define (row-clear-val m r r-c c)
  (* -1
     (/ (cell-value m r-c c)
        (cell-value m r c))))


;递归对矩阵进行行消除：
;m：矩阵；r行；c列；cr：当前行；val：值
(define (forward-row-clear m r c r-c)
  (if (or (= r (vector-length m))
          (= c (column-max m)))
      ;为行列式结束：
      m
      (if (= r-c (vector-length m))
          ;c列消除完：
          (forward-row-clear m (+ r 1) (+ c 1)
                             (+ r 1))
          ;消除c列的r-c行：
          (if (= r r-c)
              (if (cell-zero? m r-c c)
                  ;r行c列首行为0：
                  (let ([r-nz (find-nzero-row m r-c c)])
                    (if (= r-nz (vector-length m))
                        ;后边该列均为0：
                        (forward-row-clear m r (+ c 1)
                                           r)
                        ;找到该列不为0的行：
                        (begin
                          (swap-row! m r-c r-nz)
                          (forward-row-clear m r c
                                             (+ r 1)))))
                    (forward-row-clear m r c
                                       (+ r 1)))
              (begin
                (when (not (cell-zero? m r-c c))
                  (vector-set! m r-c
                               (eval-row!
                                (vector-ref m r)
                                (vector-ref m r-c)
                                c
                                (row-clear-val m r r-c c))))
                (forward-row-clear m r c
                                   (+ r-c 1)))))))
        
;单元测试：
(module+ test
  ;测试样本：
  (define m (vector
             (vector 1 -2 1 0)
             (vector 0 0 -2 8)
             (vector 4 -1 3 -6)))

  ;测试(cell-value m r c)：
  (format "(1,2)=~a" (cell-value m 1 2))

  ;测试(cell-zero? m r c)：
  (format "(1,2)=0?->~a" (cell-zero? m 1 2))
  (format "(0,3)=0?->~a" (cell-zero? m 0 3))

  ;测试(column-max m)：
  (format "矩阵最大~a行，~a列。" (vector-length m) (column-max m))

  ;测试(find-nzero-row m r c)：
  (format "~a行~a列以下不为0的行为：~a" 0 0 (find-nzero-row m 0 0))
  (format "~a行~a列以下不为0的行为：~a" 1 2 (find-nzero-row m 1 2))

  ;测试(swap-row! m r r-c)：
  (let ([t (swap-row! m 0 1)])
    (format "交换0行与1行：~a" t))

  (forward-row-clear m 0 0 0)
  )