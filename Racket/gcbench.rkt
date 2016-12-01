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
    [("-e" "--tree-depth") i "Depth of tree to allocate"  (permLength (string->number i))]
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

(struct node (x left right) #:transparent)

(define (makeTree iDepth)

    (let* ()

        (define (make-empty-node) '())
        (define (make-node l r) (node 0 l r))

        ;  Build tree bottom-up
        (if (<= iDepth 0)
            (make-empty-node)
            (make-node (makeTree (- iDepth 1)) (makeTree (- iDepth 1)))
        )
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

(define (gcFunc depth iters atomicPrint id)

    (let (
        (longLivedTree (makeTree depth))
        (longLivedArray (make-vector 5000 0.0))
        (dummy 0))
          (do ((i 0 (+ i 1)))
              ((>= i (/ 5000 2)))
            (vector-set! longLivedArray i (/ 1.0 (+ i 1))))

        (map
            (lambda (i)
                    (atomicPrint (format "gc:start:~a:~a:~a\n" id i (inexact->exact (truncate (current-inexact-milliseconds)))))
                    ;(printf "~a:~v\n" i (makeTree 10))
                    (makeTree depth)
                    (atomicPrint (format "gc:stop:~a:~a:~a\n" id i (inexact->exact (truncate (current-inexact-milliseconds)))))
                    (sleep 0) ; A zero value for secs simply acts as a hint to allow other threads to execute. For print thread
                )
            (range 1 iters)
        )
        ;  fake reference to LongLivedTree
        ;  and array
        ;  to keep them from being optimized away
        (if (or (eq? longLivedTree '())
                (let ((n (min 1000
                              (- (/ (vector-length longLivedArray)
                                      2)
                                 1))))
                  (not (= (vector-ref longLivedArray n)
                          (/ 1.0 (+ n 1))))))
            (begin (displayln "Failed")) (void))
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

; Allow time to flush print thread queue
(sleep 0.2)
