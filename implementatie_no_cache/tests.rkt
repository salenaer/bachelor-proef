(test (with-heap #(free free free)
                 (n-free-blocks? 0 2))
      #t)
(test (with-heap #(free free free)
                 (n-free-blocks? 0 3))
      #t)
(test (with-heap #(free free free)
                 (n-free-blocks? 0 4))
      #f)
(test (with-heap #(free free free)
                 (n-free-blocks? 2 1))
      #t)
(test (with-heap #(free free free)
                 (n-free-blocks? 2 2))
      #f)

(test (with-heap #(free free free)
                 (find-free-space 0 1))
      0)
(test (with-heap #(pair free free)
                 (find-free-space 0 1))
      1)
(test (with-heap #(pair free free)
                 (find-free-space 0 2))
      1)
(test (with-heap #(pair free free)
                 (find-free-space 0 3))
      #f)

(test (let ([v (make-vector 12 'x)])
        (with-heap v (init-allocator))
        v)
      (make-vector 12 'free))

(test (with-heap (vector 'free 'free 'free 'flat 14 'free 'free)
                 (gc:deref 3))
      14)

(test (with-heap (vector 'free 'flat 3 'pair 0 1)
                 (gc:first 3))
      0)

(test (with-heap (vector 'free 'flat 3 'pair 0 1)
                 (gc:rest 3))
      1)

(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:flat? 2))
      #f)

(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:flat? 5))
      #t)

(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:cons? 2))
      #t)
(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:cons? 5))
      #f)

(test (let ([v (vector #f #t '() 0 1 2 3 4 5 6 'pair 0 1 'flat 14 'pair 0 1 'flat 12)])
        (with-heap v (free! (list 10 18)))
        v)
      (vector #f #t '() 0 1 2 3 4 5 6 'free 'free 'free 'flat 14 'pair 0 1  'free 'free))
      

(test (add-in '(13 14) '(100 102) '(13 14 104 105))
      '(14 13 100 102))

(test (add-in '(13 14) '(100 102) '(13 104 105))
      '(13 100 102))

(test (with-heap (vector #f #t '() 0 1 2 3 4 5 6 'pair 0 1 'flat 14 'pair 0 1 'flat 12)
                 (get-all-records 0))
      (list 10 13 15 18))

(test (with-heap (make-vector 10 'free) (gc:alloc-flat #f))
      0)

(test (with-heap (make-vector 10 'free) (gc:alloc-flat #t) (gc:alloc-flat #f))
      2)

(test (let ([v (vector 'flat 0 'flat 1)])
        (with-heap v (collect-garbage-help (list) 
                                           (get-all-records 0)))
        v)
      (vector 'free 'free 'free 'free))

(test (let ([v (vector 'flat 0 'flat 1)])
        (with-heap v (collect-garbage-help (list 0) 
                                           (remove 0 (get-all-records 0))))
        v)
      (vector 'flat 0 'free 'free))

(test (let ([v (vector 'flat 0 'flat 1)])
        (with-heap v (collect-garbage-help (list 2) 
                                           (remove 2 (get-all-records 0))))
        v)
      (vector  'free 'free 'flat 1))

(test (let ([v (vector 'flat 0 'flat 1 'pair 0 2)])
        (with-heap v (collect-garbage-help (list 4) 
                                           (remove 4 (get-all-records 0))))
        v)
      (vector 'flat 0 'flat 1 'pair 0 2))

(test (let ([v (vector 'flat 0 'flat 1 'pair 0 0)])
        (with-heap v (collect-garbage-help (list 4) 
                                           (remove 4 (get-all-records 0))))
        v)
      (vector 'flat 0 'free 'free 'pair 0 0))

(test (let ([v (vector 'flat 0 'flat 1 'pair 4 4)])
        (with-heap v (collect-garbage-help (list 4) 
                                           (remove 4 (get-all-records 0))))
        v)
      (vector 'free 'free 'free 'free 'pair 4 4))

(test (with-heap (make-vector 50)
                 (with-roots (list 1 2 3)
                             (get-root-set)))
      (list 1 2 3))

(test (with-heap (make-vector 50)
                 (with-roots (list 1 2 3)
                             (with-roots (list 4 5 6)
                                         (sort (get-root-set) <))))
      (list 1 2 3 4 5 6))