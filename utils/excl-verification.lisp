#+allegro(in-package :sys)
#-allegro(in-package :common-lisp)


(eval-when (compile load eval)
  (pushnew :clim *features*)
  (pushnew :clim-2 *features*)
  (pushnew :clim-2.1 *features*)
  (pushnew :silica *features*)
  (pushnew :ansi-90 *features*))

(provide :clim)
