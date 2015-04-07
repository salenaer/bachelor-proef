#lang scheme/gui
(require plai/private/gc-core)
(provide heap-viz%)

(define row-size 10)
(define b-size #f)
(define s-size #f)

(define heap-viz<%> (interface () update-view))

(define horizontal-axis-height 0)
(define vertical-axis-width 0)
(define label-line-size 2)
(define cell-horizontal-padding 6)
(define cell-vertical-padding 4)
(define vertical-axis-spacer 2)

(define show-highlighted-cells? #f)

(define heap-canvas%
  (class* canvas% (heap-viz<%>)
    
    (init-field heap-vec)
    
    (define column-widths (make-vector 
                           (cond
                             [(<= (vector-length heap-vec) 300) 10]
                             [else 20])
                           0))
    (define row-heights (make-vector (ceiling (/ (vector-length heap-vec) 
                                                 (vector-length column-widths)))
                                     0))
    
    (define highlighted-cells '())
    
    (define/public (update-view)
      (setup-min-width/height)
      (redraw-offscreen)
      (recompute-highlighted-cells)
      (on-paint))
    
    (inherit get-dc get-client-size refresh min-width min-height)
    
    (define/private (setup-min-width/height)
      (fill-in-min-sizes)
      (min-width (ceiling (inexact->exact (+ vertical-axis-width (vector-sum column-widths)))))
      (min-height (ceiling (inexact->exact (+ horizontal-axis-height (vector-sum row-heights)))))
      (compute-sizes))
    
    (define/private (compute-sizes)
      (fill-in-min-sizes)
      (let-values ([(w h) (get-client-size)])
        
        (define (distribute-extras sizes avail-size)
          (let ([min-size (vector-sum sizes)])
            (cond
              [(< avail-size min-size) 
               ;; just give up here; we'll draw outside the frame and get clipped
               ;; could try to shrink the bigger columns or something, tho.
               (void)]
              [else
               ;; distribute the extra width evenly to all the columns
               (let ([extra-space (/ (- avail-size min-size)
                                     (vector-length sizes))])
                 (for ([i (in-range 0 (vector-length sizes))])
                   (vector-set! sizes
                                i
                                (+ (vector-ref sizes i)
                                   extra-space))))])))
        
        (distribute-extras column-widths (- w vertical-axis-width))
        (distribute-extras row-heights (- h horizontal-axis-height))))
    
    (define/private (fill-in-min-sizes)
      (let ([dc (get-dc)])
        (let-values ([(w h d a) 
                      (send dc get-text-extent (format "~a" (vector-length heap-vec)))])
          (set! vertical-axis-width (+ w label-line-size vertical-axis-spacer)))
        (let-values ([(w h d a) (send dc get-text-extent "1")])
          (set! horizontal-axis-height (+ h label-line-size))))
      
      (for ([i (in-range 0 (vector-length heap-vec))])
        (let ([column (remainder i (vector-length column-widths))]
              [row (quotient i (vector-length column-widths))])
          (let-values ([(cw ch) (cell-min-size (vector-ref heap-vec i))])
            (vector-set! row-heights 
                         row
                         (max (+ ch cell-vertical-padding)
                              #;(vector-ref row-heights row)))
            (vector-set! column-widths 
                         column
                         (max (+ cw cell-horizontal-padding) 
                              #;(vector-ref column-widths column)))))))
    
    (define/private (cell-min-size obj)
      (let ([dc (get-dc)])
        (let-values ([(w h d a) (send dc get-text-extent (val->string obj))])
          (values w h))))
    
    (define/private (val->string obj)
      (cond
        [(boolean? obj) (if obj "#t" "#f")]
        [(number? obj) (format "~a" obj)]
        [(procedure? obj)
         (if (object-name obj)
             (format "~a" (object-name obj))
             "#<proc>")]
        [(symbol? obj) (format "'~s" obj)]
        [(null? obj) "empty"]
        [else (error 'val->string "unknown object, expected a heap-value.")]))
    
    (define/override (on-paint)
      (unless offscreen (redraw-offscreen))
      (let ([dc (get-dc)])
        (send dc set-origin 0 0)
        (send dc draw-bitmap offscreen 0 0)
        (send dc set-origin vertical-axis-width horizontal-axis-height)
        (for-each (λ (i) (draw-cell dc i #t))
                  highlighted-cells)))
    
    (define offscreen #f)
    
    (define/private (redraw-offscreen)
      (let-values ([(w h) (get-client-size)])
        (when (or (not offscreen)
                  (not (equal? w (send offscreen get-width)))
                  (not (equal? h (send offscreen get-height))))
          (set! offscreen (make-object bitmap% w h)))
        (let ([dc (make-object bitmap-dc% offscreen)])
          (send dc set-smoothing 'aligned)
          (send dc clear)
          
          (send dc set-origin 0 0)
          
          ;; draw lines
          (let-values ([(w h) (get-client-size)])
            (send dc set-pen "navy" label-line-size 'solid)
            (send dc draw-line 
                  (- vertical-axis-width (/ label-line-size 2)) 0
                  (- vertical-axis-width (/ label-line-size 2)) h)
            (send dc draw-line 
                  0 (- horizontal-axis-height (/ label-line-size 2))
                  w (- horizontal-axis-height (/ label-line-size 2))))
          
          (send dc set-origin vertical-axis-width horizontal-axis-height)
          
          ;; draw x axis
          (let ([y (- 0 horizontal-axis-height label-line-size)])
            (for/fold ([x 0])
              ([i (in-range 0 (vector-length column-widths))])
              (let ([str (format "~a" i)])
                (let-values ([(w h d a) (send dc get-text-extent str)])
                  (setup-colors dc (+ i 1) 0 #f)
                  (send dc draw-rectangle x y
                        (vector-ref column-widths i)
                        horizontal-axis-height)
                  (send dc draw-text str 
                        (+ x (- (/ (vector-ref column-widths i) 2)
                                (/ w 2)))
                        y)
                  (+ x (vector-ref column-widths i))))))
          
          ;; draw y axis
          (for/fold ([y 0])
            ([i (in-range 0 (vector-length row-heights))])
            (let ([str (format "~a" (* i (vector-length column-widths)))])
              (let-values ([(w h d a) (send dc get-text-extent str)])
                (let ([x (- 0 label-line-size w vertical-axis-spacer)])
                  (setup-colors dc 0 (+ i 1) #f)
                  (send dc draw-rectangle 
                        (- vertical-axis-width)
                        y 
                        (- vertical-axis-width label-line-size)
                        (vector-ref row-heights i))
                  (send dc draw-text str 
                        x
                        (+ y (- (/ (vector-ref row-heights i) 2)
                                (/ h 2))))
                  (+ y (vector-ref row-heights i))))))
          
          ;; draw cells (this is O(n^2), but it seems unlikely to ever matter
          ;; to fix, one would have to precompute the partial sums of the widths
          ;; and heights of the columns)
          (for ([i (in-range 0 (round-up-to-even-multiple 
                                (vector-length heap-vec)
                                (vector-length column-widths)))])
            (draw-cell dc i #f))
          
          (send dc set-bitmap #f))))
    
    (define/private (draw-cell dc i highlighted?)
      (let-values ([(cell-x cell-y cell-w cell-h) (cell->ltwh i)])
        (let* ([column (remainder i (vector-length column-widths))]
               [row (quotient i (vector-length column-widths))])
          (setup-colors dc (+ column 1) (+ row 1) highlighted?)
          (send dc draw-rectangle cell-x cell-y cell-w cell-h)
          (when (< i (vector-length heap-vec))
            (let-values ([(ow oh) (cell-min-size (vector-ref heap-vec i))])
              (send dc draw-text 
                    (val->string (vector-ref heap-vec i))
                    (+ cell-x (- (/ cell-w 2) (/ ow 2)))
                    (+ cell-y (- (/ cell-h 2) (/ oh 2)))))))))
    
    (define (cell->ltwh i)
      (let* ([column (remainder i (vector-length column-widths))]
             [row (quotient i (vector-length column-widths))]
             [cell-x (vector-sum column-widths column)]
             [cell-y (vector-sum row-heights row)])
        (values cell-x cell-y 
                (vector-ref column-widths column)
                (vector-ref row-heights row))))
    
    (define/override (on-size w h)
      (compute-sizes)
      (redraw-offscreen)
      (refresh))
    
    (define/private (mouse-xy->ij mx my)
      (define (find-index start vec m-coord)
        (let loop ([coord start]
                   [i 0])
          (cond
            [(< i (vector-length vec))
             (cond
               [(<= coord m-coord (+ coord (vector-ref vec i)))
                i]
               [else
                (loop (+ coord (vector-ref vec i))
                      (+ i 1))])]
            [else #f])))
      (values (find-index vertical-axis-width column-widths mx)
              (find-index horizontal-axis-height row-heights my)))
    
    (define/override (on-event evt)
      (cond
        [(or (send evt moving?)
             (send evt entering?))
         (set! mouse-x (send evt get-x))
         (set! mouse-y (send evt get-y))]
        [else
         (set! mouse-x #f)
         (set! mouse-y #f)])
      (recompute-highlighted-cells))
    
    (define mouse-x #f)
    (define mouse-y #f)
    
    (define/private (recompute-highlighted-cells)
      (cond
        [(and mouse-x mouse-y)
         (let-values ([(i j) (mouse-xy->ij mouse-x mouse-y)])
           (let ([index (and i j (+ (* j (vector-length column-widths)) i))])
             (cond
               [(and index (< index (vector-length heap-vec)))
                (update-highlighted-cells (cons index (index->nexts index)))]
               [else
                (update-highlighted-cells '())])))]
        [else
         (update-highlighted-cells '())]))
    
    (define/private (index->nexts index)
      (if (< index (vector-length heap-vec))
          (let ([n (vector-ref heap-vec index)])
            (cond
              [(and (exact-integer? n)
                    (<= 0 n)
                    (< n (vector-length heap-vec)))
               (list n)]
              [(procedure? n)
               (map read-root (procedure-roots n))]
              [else
               '()]))
          '()))
    
    (define/private (update-highlighted-cells new)
      (when show-highlighted-cells?
        (unless (equal? new highlighted-cells)
          (set! highlighted-cells new)
          (refresh))))
    
    (super-new)
    
    (setup-min-width/height)
    (send (get-dc) set-smoothing 'aligned)))

(define (round-up-to-even-multiple n cols)
  (let ([%% (remainder n cols)])
    (cond
      [(zero? %%) n]
      [else (+ n (- cols %%))])))

(define highlighted-color "forestgreen")

(define (setup-colors dc i j highlighted-cell?)
  (send dc set-pen "black" 1 'transparent)
  (send dc set-brush (ij->background-color i j) 'solid)
  (send dc set-text-foreground (send the-color-database find-color (ij->text-color i j))))

(define (ij->background-color i j)
  (if (or (zero? i)(zero? j))
      "white"
      (let ((x (+ (- i 1) (* 10 (- j 1)))))
        (cond 
          ((zero? (modulo x s-size))
           "black")
          ((zero? (modulo x b-size))
           "blue")
          ((even? (quotient x b-size))
           "white")
          (else
           "gray")))))

(define (ij->text-color i j)
  (let ([bkg (ij->background-color i j)])
    (cond
      [(equal? bkg "black")
       "white"]
      [else 
       "black"])))

(define (vector-sum v [cap (vector-length v)])
  (for/fold ((sum 0))
    ((i (in-range cap)))
    (+ sum (vector-ref v i))))


(define heap-viz%
  (class* object% (heap-viz<%>)
    (init heap-vec block-size set-size)
    (define eventspace (make-eventspace))
    (define frame
      (parameterize ([current-eventspace eventspace])
        (new frame% [label "Cache"])))
    (define canvas (new heap-canvas% [parent frame] [heap-vec heap-vec] [style '(no-autoclear)]))
    (set! b-size block-size)
    (set! s-size (* block-size set-size))
    (new grow-box-spacer-pane% [parent frame])
    (send frame show #t)
    
    ;; protects 'queued'
    (define queued-sema (make-semaphore 1))
    (define queued #f)
    
    (define/public (update-view #:location loc) 
      (semaphore-wait queued-sema)
      (cond
        [queued
         (semaphore-post queued-sema)]
        [else
         (set! queued #t)
         (semaphore-post queued-sema)
         (parameterize ([current-eventspace eventspace])
           (queue-callback
            (λ () 
              (semaphore-wait queued-sema)
              (set! queued #f)
              (semaphore-post queued-sema)
              ;; we might get others queued while this happens, but that seems ok
              (send canvas update-view))
            ;; low priority, so that mouse movements and window resizes
            ;; take priority (important in the case that the mutator is 
            ;; running a tight loop that changes the heap)
            #f))]))
    
    (super-new)))
