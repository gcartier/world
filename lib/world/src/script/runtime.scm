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


(jazz:define-method (jazz:dialect-wrap (jazz:Script-Dialect dialect) body)
  `((declare (proper-tail-calls))
    (import (script.syntax (phase syntax))
            (world.script.syntax (phase syntax))
            (world.scriptum))
    (%%define me (%%current-instance))
    (%%define %%poll-script? #t)
    ,@body))


;;;
;;;; Walker
;;;


(jazz:define-class jazz:Script-Walker jazz:Jazz-Walker (constructor: jazz:allocate-script-walker)
  ())


(define (jazz:new-script-walker)
  (jazz:allocate-script-walker #f #f '() '() '() (jazz:new-queue) (jazz:new-queue) (%%make-table test: eq?) (%%make-table test: eq?) '()))


(jazz:define-method (jazz:jazz-walker-supports-selfdot? (jazz:Script-Walker walker))
  #f)

(jazz:define-method (jazz:jazz-walker-supports-composite? (jazz:Script-Walker walker))
  #f)


(jazz:define-method (jazz:walker-declarations (jazz:Script-Walker walker))
  (let ((table (jazz:get-dialect-declarations (jazz:get-dialect 'script))))
    (define (add dialect symbol)
      (%%table-set! table symbol (%%table-ref (jazz:get-dialect-declarations (jazz:get-dialect dialect)) symbol)))
    
    (add 'foundation 'import)
    (add 'foundation 'export)
    (add 'foundation '%%syntax)
    
    (add 'scheme '%%define)
    
    (%%list table)))


(jazz:define-method (jazz:walker-bindings (jazz:Script-Walker walker))
  (let ((table (jazz:get-dialect-bindings (jazz:get-dialect 'script))))
    (define (add dialect symbol)
      (%%table-set! table symbol (%%table-ref (jazz:get-dialect-bindings (jazz:get-dialect dialect)) symbol)))
    
    (add 'foundation 'import)
    (add 'foundation 'export)
    (add 'foundation '%%syntax)
    (add 'foundation 'walk-failed?)
    
    (add 'scheme 'and)
    (add 'scheme 'begin)
    (add 'scheme 'case)
    (add 'scheme 'cond)
    (add 'scheme '%%define)
    (add 'scheme 'delay)
    (add 'scheme '%%do)
    (add 'scheme 'if)
    (add 'scheme '%%lambda)
    (add 'scheme '%%let)
    (add 'scheme 'let*)
    (add 'scheme 'letrec)
    (add 'scheme 'or)
    (add 'scheme 'quasiquote)
    (add 'scheme 'quote)
    (add 'scheme 'receive)
    (add 'scheme 'set!)
    
    ;; until all available functionaly
    (add 'jazz 'declare)
    (add 'jazz 'cast)
    
    (%%list table)))


;;;
;;;; %%Syntax
;;;


(jazz:define-class jazz:%%Syntax-Declaration jazz:Syntax-Declaration (constructor: jazz:allocate-%%syntax-declaration)
  ())


(define (jazz:new-%%syntax-declaration name type access compatibility attributes parent signature syntax-form)
  (let ((new-declaration (jazz:allocate-%%syntax-declaration name type #f access compatibility attributes #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:outline-extract (jazz:%%Syntax-Declaration declaration) meta)
  `(%%syntax ,@(jazz:outline-generate-access-list declaration) ,(jazz:get-lexical-binding-name declaration)))


(define (jazz:walk-%%syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (let ((name (jazz:source-code (%%car rest))))
      (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                 (jazz:new-%%syntax-declaration name jazz:Any access compatibility '() declaration #f #f))))
        (jazz:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


;;;
;;;; Register
;;;


(jazz:define-dialect script
  (jazz:new-script-dialect 'script))


(jazz:define-walker-declaration import       foundation jazz:walk-import-declaration jazz:walk-import)
(jazz:define-walker-declaration export       foundation jazz:walk-export-declaration jazz:walk-export)
(jazz:define-walker-declaration %%syntax     foundation jazz:walk-%%syntax-declaration jazz:walk-syntax)
(jazz:define-walker-special     walk-failed? foundation jazz:walk-walk-failed)

(jazz:define-walker-declaration %%define     scheme jazz:walk-define-declaration jazz:walk-define)
(jazz:define-walker-special     %%lambda     scheme jazz:walk-lambda)
(jazz:define-walker-special     %%let        scheme jazz:walk-let)
(jazz:define-walker-special     %%do         scheme jazz:walk-do)

(jazz:define-walker-special     declare      jazz jazz:walk-declare))
