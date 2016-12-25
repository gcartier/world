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
  (cons (jazz:get-dialect-declarations (jazz:get-dialect 'jazz))
        (nextmethod walker)))


(jazz:define-method (jazz:walker-bindings (jazz:Script-Walker walker))
  (cons (jazz:get-dialect-bindings (jazz:get-dialect 'jazz))
        (nextmethod walker)))


;;;
;;;; Register
;;;


(jazz:define-dialect script
  (jazz:new-script-dialect 'script)))
