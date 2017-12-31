#lang racket/base

(provide make-modern-readtable
         wrap-reader
         )

(require racket/match
         syntax/srcloc
         syntax/parse/define
         sweet-exp/util
         sweet-exp/modern/process-curly
         )
(module+ test
  (require rackunit))

(define-simple-macro (readtable* orig-rt:expr [key:expr mode:expr action:expr] ...)
  (let* ([rt orig-rt]
         [rt (make-readtable rt key mode action)]
         ...)
    rt))

(define (wrap-reader p)
  (lambda args
    (parameterize ([current-readtable (make-modern-readtable)])
      (apply p args))))

(define (make-modern-readtable [orig-rt (current-readtable)])

  ; for (f)(x) -> ((f) x) and [f](x) -> ([f] x)
  (define paren-proc (make-paren-proc orig-rt))

  ; for {a + b} -> (+ a b) and {f}(x) -> ({f} x)
  (define curly-proc (make-curly-proc orig-rt))

  ; for f(x) -> (f x)
  (define else-proc (make-else-proc orig-rt))
  
  (readtable* orig-rt
    [#\( 'terminating-macro paren-proc]
    [#\[ 'terminating-macro paren-proc]
    [#\{ 'terminating-macro curly-proc]
    [#f 'non-terminating-macro else-proc]))

(define (read-else/no-process-tail c in src orig-rt)
  (read-syntax/recursive src in c orig-rt))

(define (read-paren/no-process-tail c in src orig-rt)
  (read-else/no-process-tail c in src orig-rt))

(define (∆ a b)
  (- b a))

(define (process-tail stx in src orig-rt)
  (cond
    [(eof-object? stx) stx]
    [else
     (define prefix (syntax-e stx))
     (define stx-start (syntax-position stx))
     (define stx-end (source-location-end stx))

     (define c2 (peek-char in))
     (define cur-pos (port-pos in))

     (cond [(not (or (symbol? prefix) (pair? prefix))) stx]
           [(eof-object? c2) stx]
           [(< stx-end cur-pos) stx]
           [(char=? c2 #\( )
            (define args (read-paren/no-process-tail #f in src orig-rt))
            (define args-end (source-location-end args))
            (process-tail
             (datum->syntax stx (cons stx args)
               (update-source-location stx #:span (∆ stx-start args-end))
               (paren-shape orig-stx #\( ))
             in src orig-rt)]
           [(char=? c2 #\[ )
            (define args (read-paren/no-process-tail #f in src orig-rt))
            (define args-end (source-location-end args))
            (process-tail
             (datum->syntax stx (cons stx args)
               (update-source-location stx #:span (∆ stx-start args-end))
               (paren-shape orig-stx #\[ ))
             in src orig-rt)]
           [(char=? c2 #\{ )
            (define arg (read-curly/no-process-tail #f in src orig-rt))
            (define arg-end (source-location-end arg))
            (process-tail
             (datum->syntax stx (list stx arg)
               (update-source-location stx #:span (∆ stx-start arg-end)))
             in src orig-rt)]
           [else stx])]))

(define (make-paren-proc orig-rt)
  (define (proc c in src ln col pos)
    (process-tail
     (read-paren/no-process-tail c in src orig-rt)
     in src orig-rt))
  proc)

(define (read-curly/no-process-tail c in src orig-rt)
  (process-curly (read-paren/no-process-tail c in src orig-rt)))

(define (make-curly-proc orig-rt)
  (define (proc c in src ln col pos)
    (process-tail
     (read-curly/no-process-tail c in src orig-rt)
     in src orig-rt))
  proc)

(define (make-else-proc orig-rt)
  (define (proc c in src ln col pos)
    (process-tail
     (read-else/no-process-tail c in src orig-rt)
     in src orig-rt))
  proc)

;; ------------------------------------------------------------------------

(module+ test
  (define orig-rt (current-readtable))
  (define (rd str)
    (read (open-input-string str)))
  (define (rd* str)
    (for/list ([x (in-port read (open-input-string str))])
      x))

  (define-syntax-rule (should-be-consistent-under-read-cdot body ...)
    (begin
      (define (run-body) (test-begin body ...))
      (define read-cdot
        (dynamic-require 'racket/base 'read-cdot (λ () #f)))
      (cond
        [(parameter? read-cdot)
         (parameterize ([read-cdot #false]) (run-body))
         (parameterize ([read-cdot #true]) (run-body))]
        [else
         (run-body)])))
  
  (parameterize ([current-readtable (make-modern-readtable orig-rt)])
    (check-equal? (rd "f(x)") '(f x))
    (check-equal? (rd "f(x y)") '(f x y))
    (check-equal? (rd "f(x)(y)") '((f x) y))

    (should-be-consistent-under-read-cdot
     (check-equal? (rd* "f(x)") (list '(f x)))
     (check-equal? (rd* "f (x)") (list 'f '(x)))
     (check-equal? (rd* "f\t(x)") (list 'f '(x)))
     (check-equal? (rd* "f\n(x)") (list 'f '(x)))
     (check-equal? (rd* "f(x y)") (list '(f x y)))
     (check-equal? (rd* "f (x y)") (list 'f '(x y)))
     (check-equal? (rd* "f(x)(y)") (list '((f x) y)))
     (check-equal? (rd* "f (x)(y)") (list 'f '((x) y)))
     (check-equal? (rd* "f(x) (y)") (list '(f x) '(y)))
     (check-equal? (rd* "f (x) (y)") (list 'f '(x) '(y))))

    (should-be-consistent-under-read-cdot
     (check-equal? (rd "(list f(x) g(x))") '(list (f x) (g x)))
     (check-equal? (rd "(list f(x)g(x))") '(list (f x) (g x)))
     (check-equal? (rd "(list f (x) g(x))") '(list f (x) (g x)))
     (check-equal? (rd "(list f (x) g (x))") '(list f (x) g (x)))
     (check-equal? (rd "(list f (x)   (x))") '(list f (x) (x)))
     (check-equal? (rd "(list f (x)(x))") '(list f ((x) x)))
     (check-equal? (rd "(list f(x)(x))") '(list ((f x) x)))
     (check-equal? (rd "list(f(x) g(x))") '(list (f x) (g x)))
     (check-equal? (rd "list(f (x) g(x))") '(list f (x) (g x)))
     (check-equal? (rd "list(f (x)(x))") '(list f ((x) x)))
     (check-equal? (rd "list(f(x)(x))") '(list ((f x) x))))
    
    (check-equal? (rd "'f(x)") '(quote (f x)))
    (check-equal? (rd "'f(x)(y)") '(quote ((f x) y)))
    (check-equal? (rd "f(g(x))") '(f (g x)))

    (should-be-consistent-under-read-cdot
     (check-equal? (rd* "'f(x) (g y)") (list '(quote (f x)) '(g y)))
     (check-equal? (rd* "'f(x)(y) (g z)") (list '(quote ((f x) y)) '(g z)))
     (check-equal? (rd* "f(g(x)) (h y)") (list '(f (g x)) '(h y))))
    
    (check-equal? (rd "{a + b}") '(+ a b))
    (check-equal? (rd "f{a + b}") '(f (+ a b)))
    (check-equal? (rd "f{g(a) + h(a)}") '(f (+ (g a) (h a))))
    )
  (check-equal? (syntax->datum
                 (process-tail (datum->syntax #f 'f '(str 1 0 1 1)) (open-input-string "(x)") 'str orig-rt))
                '(f x))
  )
