#lang racket

(require racket/cmdline)

(define computeThreads (make-parameter 1))
(define computeDepth (make-parameter 37))
(define iters (make-parameter 10))
(define sleepTime (make-parameter 1))
(define gcThreads (make-parameter 1))
(define permLength (make-parameter 9))
(define showGCStats   (make-parameter #f))

(command-line #:program "perm9"
    #:once-each
    [("-t" "--compute-threads") i "Compute Threads"  (computeThreads (string->number i))]
    [("-d" "--compute-depth") i "Compute Depth"  (computeDepth (string->number i))]
    [("-i" "--iterations") i "Compute/GC Iterations"  (iters (string->number i))]
    [("-s" "--compute-sleep") i "Compute Sleep"  (sleepTime (string->number i))]
    [("-g" "--gc-threads") i "GC Threads"  (gcThreads (string->number i))]
    [("-e" "--perm-depth") i "Size of list to generate permutations of"  (permLength (string->number i))]
    [("-S" "--gc-stats") "Print GC stats"  (showGCStats #t)]
)

(define (parallel thunks)
        (map thread-wait (map (lambda (s) (thread s)) thunks))
)

(define (range min max)
    (if (<= min max)
        (cons min (range (+ min 1) max))
        '()
    )
)

(define (permutations x)
  (let ((x x)
        (perms (list x)))
    (define (P n)
      (if (> n 1)
          (do ((j (- n 1) (- j 1)))
              ((zero? j)
               (P (- n 1)))
              (P (- n 1))
              (F n))
    '()
    ))
    (define (F n)
      (set! x (revloop x n (list-tail x n)))
      (set! perms (cons x perms)))
    (define (revloop x n y)
      (if (zero? n)
          y
          (revloop (cdr x)
                   (- n 1)
                   (cons (car x) y))))
    (define (list-tail x n)
      (if (zero? n)
          x
          (list-tail (cdr x) (- n 1))))
    (P (length x))
    perms))

(define (sumlists x)
  (do ((x x (cdr x))
       (sum 0 (do ((y (car x) (cdr y))
                   (sum sum (+ sum (car y))))
                  ((null? y) sum))))
      ((null? x) sum)))

(define (factorial n)
    (if (zero? n)
        1
        (* n (factorial (- n 1)))
    )
)

(define (fib n)
    (if (< n 2)
        1
        (+ (fib (- n 2)) (fib (- n 1)))
    )
)

(define (pconsume)
    (display (thread-receive)) (pconsume)
)

(define (gcFunc n iters atomicPrint id)
    (map
        (lambda (i)
            (atomicPrint (format "gc:start:~a:~a:~a\n" id i (inexact->exact (truncate (current-inexact-milliseconds)))))
            (= (sumlists (permutations (range 1 n))) (* (quotient (* n (+ n 1)) 2) (factorial n)))
            (atomicPrint (format "gc:stop:~a:~a:~a\n" id i (inexact->exact (truncate (current-inexact-milliseconds)))))
        )
        (range 1 iters)
    )
)

(define (compute n iters atomicPrint id sleepTime)
    (map
        (lambda (i)
            (atomicPrint (format "compute:start:~a:~a:~a\n" id i (inexact->exact (truncate (current-inexact-milliseconds)))))
            (fib n)
            (atomicPrint (format "compute:stop:~a:~a:~a\n" id i (inexact->exact (truncate (current-inexact-milliseconds)))))
            (sleep sleepTime)
        )
        (range 1 iters)
    )
)

(let ((printThd (thread pconsume)))
    (define (atomicPrint s) (thread-send printThd s))
    (parallel (append
        (map
            (lambda (id)
                (lambda () (gcFunc (permLength) (iters) atomicPrint id))
            )
            (range 1 (gcThreads))
        )
        (map
            (lambda (id)
                (lambda () (compute (computeDepth) (iters) atomicPrint id (sleepTime)))
            )
            ;(range (+ (gcThreads) 1) (+ (gcThreads) (computeThreads)))
            (range 1 (computeThreads))
        )
    ))
    (if (showGCStats) (dump-memory-stats) (void))
    (void)
)
