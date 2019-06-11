#lang racket

;单元测试：
(module+ test
  ;测试样本：
  (define m (vector
             (vector 0 1 -3 2)
             (vector 1 2 -5 0)
             (vector 0 -3 9 5))))
;单元测试：
(module+ test
  ;测试m回显：
  (display
   (format "m原始值为："))
  m)

;行r列c对应单元值：
(define (cell-value m r c)
  (vector-ref
   (vector-ref m r)
   c))

;单元测试：
(module+ test
  ;测试(cell-value m r c)：
  (display
   (format "\n(1,2)=~a" (cell-value m 1 2))))

;检查单元值是否为0：
(define (cell-zero? m r c)
  (zero? (cell-value m r c)))

;单元测试：
(module+ test
  ;测试(cell-zero? m r c)：
  (display
   (format "\n(1,2)=0?~a" (cell-zero? m 1 2))))

;取得最大列数：
(define (column-max m)
  (vector-length (vector-ref m 0)))

;单元测试：
(module+ test
  ;测试(column-max m)：
  (display
   (format "\n矩阵最大~a行，~a列。"
          (vector-length m)
          (column-max m))))

;找r行以下c列不为0的行：
(define (find-nzero-row m r c)
  (if (= r (vector-length m))
      r
      (if (not (cell-zero? m r c))
          r
          (find-nzero-row m (+ r 1) c))))

;单元测试：
(module+ test
  ;测试(find-nzero-row m r c)：
  (display
   (format "\n~a行~a列以下不为0的行为：~a"
          0 0
          (find-nzero-row m 0 0)))
  (display
   (format "\n~a行~a列以下不为0的行为：~a"
          1 2
          (find-nzero-row m 1 2))))

;交换r行与r-c行：
(define (swap-row! m r r-c)
  (let ([t (vector-ref m r)])
    (vector-set! m r (vector-ref m r-c))
    (vector-set! m r-c t)))

;单元测试：
(module+ test
  ;测试(swap-row! m r r-c)：
  (display
   (format "\n交换0行与1行前后：\n"))
  m
  (swap-row! m 0 1)
  m)

;对r行进行简化：
(define (simple-row v c val)
  (if (= c (vector-length v))
      v
      (if (zero? (vector-ref v c))
          (simple-row v (+ c 1) val)
          (if (zero? val)
              (begin
                (vector-set! v c 1)
                (simple-row
                 v (+ c 1)
                 (vector-ref v c)))
              (begin
                (vector-set! v c
                             (/ (vector-ref v c) val))
                (simple-row v (+ c 1) val))))))

;单元测试：
(module+ test
  ;测试(simple-row! v c val)：
  (display
   (format "\n对~a行简化之前为：~a\n简化之后为：~a"
          2
          (vector-ref m 2)
          (simple-row (vector-ref m 2) 0 0))))

;对r行c列后的每一列求值：
(define (row-clear vr vr-c c val)
  (if (= c (vector-length vr))
      vr-c
      (begin
        (vector-set! vr-c c
                     (+ (* (vector-ref vr c) val)
                        (vector-ref vr-c c)))
        (row-clear vr vr-c (+ c 1) val))))

;单元测试：
(module+ test
  ;测试(row-clear vr vr-c c val)：
  (display(format "\n对~a行消除之前为：~a\n行消除之后为：~a"
          1
          (vector-ref m 1)
          (row-clear
           (vector-ref m 1)
           (vector-ref m 0)
           1 2))))

;计算行消除乘积数：
(define (row-clear-val m r r-c c)
  (* -1
     (/ (cell-value m r-c c)
        (cell-value m r c))))

;以r行c列为基准向上对矩阵进行多行消除：
(define (rows-up-clear m r r-c c)
  (if (< r-c 0)
      m
      (begin
        (vector-set!
         m r-c
         (row-clear
          (vector-ref m r)
          (vector-ref m r-c)
          c
          (row-clear-val m r r-c c)))
        (rows-up-clear m r (- r-c 1) c))))

;单元测试：
(module+ test
  ;测试(rows-up-clear m r r-c c)：
  (define vc (vector
              (vector 1 -2 0 2)
              (vector 0 -4 3 8)
              (vector 0 0 3 1)))
  (display
   (format "\n向上多行消除前后结果为：\n"))
  vc
  (rows-up-clear vc 2 1 2))

;确定非零列：
(define (nzero-column m r c)
  (if (or (= c (column-max m))
          (not (cell-zero? m r c)))
      c
      (nzero-column m r (+ c 1))))

;单元测试：
(module+ test
  ;测试(nzero-column m r c)：
  (display
   (format "\n~a行的非0列为：~a"
           0
           (nzero-column m 0 0))))

;递归对矩阵进行向前行消除：
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
                               (row-clear
                                (vector-ref m r)
                                (vector-ref m r-c)
                                c
                                (row-clear-val m r r-c c))))
                (forward-row-clear m r c
                                   (+ r-c 1)))))))

;单元测试：
(module+ test
  ;测试(forward-row-clear m r c r-c):
  (display
   (format "\n对矩阵向前行消除前后：\n"))
   m
   (forward-row-clear m 0 0 0))

;用递归对每一行进行向后行消除(同时进行行简化)：
(define (backward-row-clear m r)
  (let ([c (nzero-column m r 0)])
    ;检查矩阵是否有解：
    (if (and
         (= r (- (vector-length m) 1))
         (cell-zero? m r (- (column-max m) 2)))
        #f
        (if (= c (- (column-max m) 1))
            (backward-row-clear m (- r 1))
            (begin
              ;对本行进行行简化：
              (vector-set!
               m r
               (simple-row (vector-ref m r)
                           c 0))
              (if (= r 0)
                  m
                  ;非第一行，进行向后行消除：
                  (backward-row-clear
                   (rows-up-clear m r (- r 1) c)
                   (- r 1))))))))

;单元测试：
(module+ test
  ;测试(backward-row-clear m r)：
  (display
   (format "\n对矩阵行简化并向后行消除前后，#f表示矩阵无解：\n"))
  m
  (backward-row-clear
   m
   (- (vector-length m) 1)))

;单元测试：
(module+ test
  ;测试样本：
  (define m1 (vector
              (vector 0 1 -3 2)
              (vector 1 2 -5 0)
              (vector 0 -3 9 5)))
  (define m2 (vector
              (vector 0 1 -3 2)
              (vector 9 2 -5 0)
              (vector 2 -3 9 5)))
  (define m3 (vector
              (vector 0  1  -3 2  5 3  1)
              (vector 9  2  -5 0  2 7  0)
              (vector 2 -3   9 5  1 9 -5)
              (vector 7  4   8 2  5 1  5)
              (vector 5  3  -7 0 -5 2  6)))
  (define m4 (vector
              (vector 0  1  -3)
              (vector 9  2  -5)
              (vector 2 -3   9)
              (vector 7  4   8)
              (vector 5  3  -7)))
  (define m5 (vector
              (vector 0  1  -3  2)
              (vector 3  2  -5 -2)
              (vector 0 -3   0  0)
              (vector 5  0  -1 -5)
              (vector -2  3 -7 0)))
  ;综合测试：
  (define mz m2)
  
  (display
   (format "综合测试，矩阵求解前后："))
  mz
  (let* ([mf (forward-row-clear mz 0 0 0)]
         [mr (backward-row-clear
              mf
              (- (vector-length mz) 1))])
    (if mr
        mr
        (begin
          (display "本矩阵无解。\n")
          mf))))
