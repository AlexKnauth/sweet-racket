(module reader racket/base
  (require syntax/module-reader
           (only-in "../reader.rkt" make-modern-readtable wrap-reader))

  (provide (rename-out [modern-read read]
                       [modern-read-syntax read-syntax]
                       [modern-get-info get-info]))

  (define lang-reader-module-paths
    (dynamic-require
     'syntax/module-reader
     'lang-reader-module-paths
     (lambda ()
       (lambda (bstr)
         (let* ([str (bytes->string/latin-1 bstr)]
                [sym (string->symbol str)])
           (and (module-path? sym)
                (vector
                 ;; try submod first:
                 `(submod ,sym reader)
                 ;; fall back to /lang/reader:
                 (string->symbol (string-append str "/lang/reader")))))))))

  (define-values [modern-read modern-read-syntax modern-get-info]
    (make-meta-reader
     'sweet-exp2/modern
     "language path"
     lang-reader-module-paths
     wrap-reader
     wrap-reader
     (lambda (proc)
       (lambda (key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (case key
           [else (fallback)]))))))
