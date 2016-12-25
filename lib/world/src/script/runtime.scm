;;;===============
;;;  WorldScheme
;;;===============
;;;
;;;; Script Dialect Runtime
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 2012-2016
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):


(unit protected script.runtime


;;;
;;;; Dialect
;;;


(jazz:define-class jazz:Script-Dialect jazz:Dialect (constructor: jazz:allocate-script-dialect)
  ())


(define (jazz:new-script-dialect name)
  (jazz:allocate-script-dialect name (%%make-table test: eq?) (%%make-table test: eq?)))


(jazz:define-method (jazz:dialect-walker (jazz:Script-Dialect dialect))
  (jazz:new-script-walker))


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Script-Walker jazz:Jazz-Walker (constructor: jazz:allocate-script-walker)
  ())


(define (jazz:new-script-walker)
  (jazz:allocate-script-walker #f #f '() '() '() (jazz:new-queue) (jazz:new-queue) (%%make-table test: eq?) (%%make-table test: eq?) '()))


(jazz:define-method (jazz:jazz-walker-supports-tilde? (jazz:Script-Walker walker))
  #f)

(jazz:define-method (jazz:jazz-walker-supports-composite? (jazz:Script-Walker walker))
  #f)


(jazz:define-method (jazz:walker-declarations (jazz:Script-Walker walker))
  (let ((table (%%make-table test: eq?)))
    (define (add dialect symbol #!optional (as #f))
      (%%table-set! table (or as symbol) (%%table-ref (jazz:get-dialect-declarations (jazz:get-dialect dialect)) symbol)))
    
    (add 'foundation 'import '%%import)
    (add 'scheme 'define)
    (%%list table)))


(jazz:define-method (jazz:walker-bindings (jazz:Script-Walker walker))
  (let ((table (%%make-table test: eq?)))
    (define (add dialect symbol #!optional (as #f))
      (%%table-set! table (or as symbol) (%%table-ref (jazz:get-dialect-bindings (jazz:get-dialect dialect)) symbol)))
    
    (add 'foundation 'import '%%import)
    (add 'scheme 'and)
    (add 'scheme 'begin)
    (add 'scheme 'case)
    (add 'scheme 'cond)
    (add 'scheme 'define)
    (add 'scheme 'delay)
    (add 'scheme 'do)
    (add 'scheme 'if)
    (add 'scheme 'lambda)
    (add 'scheme 'let)
    (add 'scheme 'let*)
    (add 'scheme 'letrec)
    (add 'scheme 'or)
    (add 'scheme 'quasiquote)
    (add 'scheme 'quote)
    (add 'scheme 'receive)
    (add 'scheme 'set!)
    (add 'jazz 'declare '%%declare)
    (%%list table)))


;;;
;;;; Register
;;;


(jazz:define-dialect script
  (jazz:new-script-dialect 'script)))
