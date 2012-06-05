;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; World Product
;;;


(unit world.product


;;;
;;;; Build
;;;


(cond-expand
  (windows
    (define jazz:world-units
      (let ((glew-include-path (jazz:quote-jazz-pathname "foreign/opengl/glew/include"))
            (glew-lib-path     (jazz:quote-jazz-pathname "foreign/opengl/glew/lib")))
        `((world.foreign cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -lopengl32 -lglew32"))))))
  (else
    (define jazz:world-units
      '())))


(define (jazz:build-world descriptor . rest)
  (let ((unit-specs jazz:world-units))
    (apply jazz:custom-compile/build unit-specs rest)
    (apply jazz:build-product-descriptor descriptor rest)))


;;;
;;;; Register
;;;


(jazz:register-product 'world.client
  build: jazz:build-world))
