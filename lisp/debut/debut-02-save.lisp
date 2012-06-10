;;; @(#)$Id: //proj/evcl3/mainline/lisp/debut/debut-02-save.lisp#1 $

(in-package :si)

(dolist (name '(#:asm #:regex #+(and nil win32 x86) #:win32))
  (ds:load-system name :config :release)
  (.collect-garbage) )

(setq *print-array*
    (lambda ()
        (let ((*image-save-time* (get-universal-time))
              (*finalizations* nil) )
          (setq *print-array* nil)
          (internal-save-image "evcl3.image") )) )

(setq *debug-output* nil)

(.collect-garbage :age 4)
(.collect-garbage :age 4)

(funcall *print-array*)
