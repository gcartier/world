;;;===============
;;;  WorldScheme
;;;===============
;;;
;;;; Script Dialect Runtime
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 2012-2018
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

#; ;; at the moment this breaks literals
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
    
    ;; going with the clos oo
    (add 'jazz 'generic)
    (add 'jazz '%class)
    (add 'jazz '%interface)
    (add 'jazz '%slot)
    (add 'jazz '%property)
    (add 'jazz 'method)
    
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
    (add 'scheme 'server)
    (add 'scheme 'client)
    
    ;; until all available functionaly
    (add 'jazz 'declare)
    (add 'jazz 'cast)
    
    ;; going with the clos oo
    (add 'jazz 'generic)
    (add 'jazz 'specific)
    (add 'jazz 'class)
    (add 'jazz '%class)
    (add 'jazz '%interface)
    (add 'jazz 'slot)
    (add 'jazz 'property)
    (add 'jazz '%slot)
    (add 'jazz '%property)
    (add 'jazz 'method)
    (add 'jazz 'with-self)
    
    (%%list table)))


;;;
;;;; %%Syntax
;;;


(jazz:define-class jazz:%%Syntax-Declaration jazz:Syntax-Declaration (constructor: jazz:allocate-%%syntax-declaration)
  ())


(define (jazz:new-%%syntax-declaration name type access compatibility modifiers attributes parent signature syntax-form)
  (let ((new-declaration (jazz:allocate-%%syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (jazz:setup-declaration new-declaration)
    new-declaration))


(jazz:define-method (jazz:outline-extract (jazz:%%Syntax-Declaration declaration) meta)
  `(%%syntax ,@(jazz:outline-generate-access-list declaration) ,(jazz:get-lexical-binding-name declaration)))


(define (jazz:walk-%%syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (jazz:parse-modifiers walker resume declaration jazz:syntax-modifiers (%%cdr (jazz:source-code form-src)))
    (let ((name (jazz:source-code (%%car rest))))
      (let ((new-declaration (or (jazz:find-declaration-child declaration name)
                                 (jazz:new-%%syntax-declaration name jazz:Any access compatibility modifiers '() declaration #f #f))))
        (jazz:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (jazz:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (jazz:expand-server walker resume declaration environment form-src)
  (if (jazz:feature-satisfied? 'server)
      (%%cons 'begin (%%cdr (jazz:source-code form-src)))
    '(unspecified)))


(define (jazz:expand-client walker resume declaration environment form-src)
  (if (jazz:feature-satisfied? 'client)
      (%%cons 'begin (%%cdr (jazz:source-code form-src)))
    '(unspecified)))


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
(jazz:define-walker-syntax      server       scheme jazz:expand-server)
(jazz:define-walker-syntax      client       scheme jazz:expand-client)

(jazz:define-walker-special     declare      jazz jazz:walk-declare)

;; going with the clos oo
(jazz:define-walker-declaration generic      jazz jazz:walk-generic-declaration jazz:walk-generic)
(jazz:define-walker-special     specific     jazz jazz:walk-specific)
(jazz:define-walker-syntax      class        jazz jazz:expand-class)
(jazz:define-walker-declaration %class       jazz jazz:walk-%class-declaration jazz:walk-%class)
(jazz:define-walker-syntax      interface    jazz jazz:expand-interface)
(jazz:define-walker-declaration %interface   jazz jazz:walk-%interface-declaration jazz:walk-%interface)
(jazz:define-walker-syntax      slot         jazz jazz:expand-slot)
(jazz:define-walker-syntax      property     jazz jazz:expand-property)
(jazz:define-walker-declaration %slot        jazz jazz:walk-%slot-declaration jazz:walk-%slot)
(jazz:define-walker-declaration %property    jazz jazz:walk-%slot-declaration jazz:walk-%slot)
(jazz:define-walker-declaration method       jazz jazz:walk-method-declaration jazz:walk-method)
(jazz:define-walker-special     with-self    jazz jazz:walk-with-self))
