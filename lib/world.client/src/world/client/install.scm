;;;===============
;;;  WorldScheme
;;;===============
;;;
;;;; World Client Install
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 2012-2018
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):


(unit world.client.install


(cond-expand
  (windows
    (jazz:register-foreign-libraries 'world.foreign 'opengl32 'glu32 'glew32))
  (cocoa
    (jazz:register-foreign-libraries 'world.foreign 'OpenGL 'glew))
  (else)))
