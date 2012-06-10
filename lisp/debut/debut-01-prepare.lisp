;;; @(#)$Id: //proj/evcl3/mainline/lisp/debut/debut-01-prepare.lisp#1 $

(dolist (name '(#:asm #:regex))
  (ds:clean-system name :config :release)
  (ds:compile-system name :config :release) )

#+nil (progn
#+(and win32 x86)
(progn
  (ds:load-system    '#:asm   :config :release)
  #+nil (ds:clean-system  '#:win32 :config :release)
  (ds:compile-system '#:win32 :config :release) )
 )