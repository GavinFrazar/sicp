;; data-directed dispatch setup
(define dispatch-table (rkt:make-hash))

(install-special-form `(quote ,(lambda (exp env)
                                 (text-of-quotation exp))))
(install-special-form `(set! ,eval-assignment))
(install-special-form `(define ,eval-definition))
(install-special-form `(if ,eval-if))
(install-special-form `(lambda ,(lambda (exp env)
                                  (make-procedure (lambda-parameters exp)
                                                  (lambda-body exp)
                                                  env))))
(install-special-form `(begin ,(lambda (exp env)
                                 (eval-sequence (begin-actions exp) env))))
(install-special-form `(cond ,(lambda (exp env)
                                (eval (cond->if exp) env))))

;; repl setup
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define the-global-environment (setup-environment))

