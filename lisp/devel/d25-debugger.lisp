;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DEVEL; Base: 10 -*-
;;;;
;;;; evcl - devel - 25 Debugger
;;; devl/d25-debugger.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-debugger.lisp#2 $
;;;
;;; Description:
;;;  This file contains functions for development:
;;;     debugger
;;;     dump-object
;;;     show-control-frames     debug support
;
(in-package :devel)


;;;; debugger
;
(defun debugger (&optional condition)
    (declare (type (or condition null) condition))
    (declare (values nil))
  (labels (
    ;; pretty-symbol-p
    ;;  Returns t if symbol may have meaning to user, otherwise returns nil.
    (pretty-symbol-p (sym)
      (and sym
         (symbolp sym)
         (find-symbol (symbol-name sym))
         (null (member (symbol-package sym)
                       '(#.(find-package :si) #.(find-package :c)) ))) )

    ;; interest-function-p
    (interest-function-p (fn)
      (let ((fname (ext:function-name fn)))
        (case fname
          ((:compile-toplevel) t)
          ((error) nil)
          ((invoke-debugger) nil)
          (otherwise
            (when (consp fname)
              (setq fname (rest fname))
            (when (and (consp fname) (null (rest fname)))
              (setq fname (first fname)) ))
          (pretty-symbol-p fname) ) ) ) )

    ;; show-condition
    (show-condition ()
      (let ((fn
             (loop
               for frame = (si::.next-frame nil)
                      then (si::.next-frame frame)
               while frame do
                 (let ((fn (si:record-ref frame 1)))
                   (when (interest-function-p fn)
                     (return (ext:function-name fn)) ) )
               finally
                 (let ((frame (si::.next-frame (si::.next-frame nil))))
                   (return (si:record-ref frame 1)) )) ))

        ;; Report error
        (let ((package *package*))
          (with-standard-io-syntax
            (let ((*print-circle* t)
                  (*print-pretty* t)
                  (*package* package)
                  (s *error-output*) )
              (terpri s)
              (pprint-logical-block (s nil :per-line-prefix "; ")
                    (format s "~A: "
                        (etypecase condition
                          (error "Error")
                          (style-warning "Style-warning")
                          (warning "Warning")
                          (condition "Condition") ))
                (if (not (typep condition 'simple-condition))
                    (format s "~A~%" condition)
                  (let ((control
                          (format nil "~~@<~A~~:@>~%"
                             (simple-condition-format-control condition) ) ))
                    (apply #'format s
                          control
                          (simple-condition-format-arguments condition) ) ))
                (format s " while in the function ~S.~%" fn) )
              (fresh-line s) ) ) ) ) )
    )
    ;;
    (let (;; Rebind I/O streams
          (*standard-input*  *cmdl-standard-input*)
          (*standard-output* *cmdl-standard-output*)

          ;; Reset printer control variables, which prevent user to see
          ;; messages
          (*print-lines* nil)
          (*print-right-margin* nil) )
      (clear-input)
      (force-output)
      (when condition (show-condition))
      (let ((*condition* condition)
            (*restarts* (compute-restarts)) )
        (show-restarts)
        (command-loop)
        (abort) ) ) ) )


;;;; get-nth-frame
;;;     => fn, ip, fp, sp, cp
;
(defun get-nth-frame (nth)
    (declare (values (or function null) fixnum fixnum fixnum fixnum))
  (loop
    for frame = (si::.next-frame nil) then (si::.next-frame frame)
    repeat (1+ nth) do
     (when (null frame) (return nil))
    finally
      (if (null frame)
          (return (values nil 0 0 0 0))
      (return
        (values
            (si:record-ref frame 1)    ; fn
            (si:record-ref frame 2)    ; ip
            (si:record-ref frame 3)    ; fp
            (si:record-ref frame 4)    ; sp
            0 ))) ) )


;;;; pretty-function-name
;
(defun pretty-function-name (fn)
  (let ((name (ext:function-name fn)))
    (when (null name) (return-from pretty-function-name fn))
    (when (and (consp name)
               (eq (first name) :discriminator)
               (typep fn 'si:byte-code-closure) )
      (loop
        for i from 1 below (si::byte-code-function-length fn)
        for x = (si::object-ref fn i) do
          (when (typep x 'generic-function)
            (setq name (clos:generic-function-name x))
            (return) )))
    name ) )


;;;; backtrace
;
(devel:defcommand backtrace (&optional (nframes 10) (min *frame-index*)) 
  "Show call stack."
  :alias bt
  (labels (
    (indent (name)
      (typecase name
        (null "    ")
        (symbol
          (if (find-symbol (symbol-name name)) "" "    ") )
        (cons
          (case (car name)
            (setf    (indent (second name)))
            (method  "  ")
            (otherwise "    ") ) )
        (otherwise "    ") ) )
    )
    (let ((frame (si::.next-frame nil)))
      (loop repeat min do
        (when (null frame) (return))
        (setq frame (si::.next-frame frame)) )

      (loop
         for nth = min then (1+ nth)
         repeat nframes do
          (when (null frame) (return))
          (let ((name (pretty-function-name (si:record-ref frame 1))))
            (format t "; ~C~2D ~A~S~%"
                    (if (= nth *frame-index*) #\> #\Space)
                    nth
                    (indent name)
                    name )
            (setq frame (si::.next-frame frame)) )) ) ) )


;;;; down
;
(defcommand down (&optional (n 1))
  "Down current frame toword to callee."
  :alias dn
  (setq n (- *frame-index* n))
  (let ((fn (and (plusp n) (get-nth-frame n))))
    (cond
      ((minusp n)
        (format t "; You are at the top most function frame.~%") )
      ((not fn)
        (format t "; No such function frame ~D.~%" n) )
      (t
        (setq *frame-index* n)
        (remember-value fn t) )) ) )


;;;; frame
;;;
;;; Description:
;;;  Show contents of function frame.
;
(devel:defcommand frame (&optional (nth *frame-index*))
  "Show NTH function frame."
  :alias fr

  (multiple-value-bind (fn ip fp sp cp)
      (get-nth-frame nth)

    (when (null fn)
      (format t "; No such function frame ~D.~%" nth)
      (return-from frame) )

    (format t "~&; Function frame [~D]: ~S~%"
        nth
        (pretty-function-name fn) )

    (format t "; ~4Tip=#x~X fp=#x~X, sp=#x~X, cp=#x~X~%"
           ip
           (ash fp 2)
           (ash sp 2)
           (ash cp 2) )

    (loop
      for nth = 0 then (1+ nth) do
        (multiple-value-bind (name value)
            (function-frame-variable fn ip fp sp nth)
          (when (null name)
            (when (plusp nth) (format t ";~%"))
            (return) )
          (when (zerop nth) (format t ";~%; Variables:~%"))
          (let ((*print-lines* 1)
                (*print-escape* t) )
            (format t ";  ~2:D: ~S ~14T ~:W" nth name value) )
          (terpri) ))

    (setq *frame-index* nth)
    (remember-value fn t) ) )


;;;; up
;
(defcommand up (&optional (n 1))
  "Up current frame toword to caller."
  (setq n (+ *frame-index* n))
  (let ((fn (get-nth-frame n)))
    (if (not fn)
        (format t "; No such function frame ~D.~%" n)
      (progn
        (setq *frame-index* n)
        (remember-value fn t) )) ) )


;;;; var
;
(defcommand var (nth)
  "Show nth variable in current function frame."
  (multiple-value-bind (fn ip fp sp)
      (get-nth-frame *frame-index*)
    (when (null fn) (return-from var))
    (multiple-value-bind (name value)
        (function-frame-variable fn ip fp sp nth)
      (if (null name)
          (format t "; No such variable: ~D~%" nth)
        (remember-value value t) ) ) ) )


;;;; show-function-frames
;
(defun show-function-frames (&optional (nth 5))
  (setq nth (min nth *frame-index*))
  (loop
    (multiple-value-bind (fn ip fp sp cp)
         (get-nth-frame nth)
         (declare (ignore ip fp sp cp))
      (when (null fn) (return))
      (let ((name
              (or (si::.byte-code-function-name fn) fn) ))
        (if (= nth *frame-index*)
            (format t "; >~2D ~S~%" nth name)
          (format t ";  ~2D ~S~%" nth name) )
        (incf nth) ) ) ) )


;;;; Show Function Frame
;;;
;;; Layout of function frame:
;;;
;;;                 ... callee's frame...
;;;
;;;       -nargs      arg[0]
;;;       -nargs+1    arg[1]
;;;                     ...
;;;       -3          arg[nargs-1]
;;;       -2          caller'
;;;       -1          ip'
;;; fp =>  0          fp'
;;;        1          loc[0]
;;;        2          loc[1]
;;;        ...
;;;        nlocs      loc[nlocs-1]
;
(defun show-function-frame (&optional (nth *frame-index*))
  (multiple-value-bind (fn ip fp sp cp)
      (get-nth-frame nth)
    (unless fn (return-from show-function-frame (values)))

    (multiple-value-bind (min max nrest nlocs)
        (function-arity fn)
      (format t "; ~D ~S ip=#x~4,'0X, nreqs=~D nopts=~D nrest=~D nlocs=~D~%"
              nth fn ip
              min  (- max min) nrest
              nlocs )
    (values fn ip fp sp cp) ) ) )


;;; block
;;;     0   cp'
;;;     1   block
;;;     2   proxy
;;;     3   vip
;;;     4   lbp
;;;     5   vfn
;;;
;;; catch
;;;     0   cp'
;;;     1   block
;;;     2   proxy
;;;     3   vip
;;;     4   lbp
;;;     5   vfn
;;;     6   tag
;;;
;;; special
;;;     value-cell-1
;;;     value-1
;;;     ...
;;;     value-cell-n
;;;     value-n
;;;     cp'
;;;     special
;;;     n * 2
;;;
;;; tagbody
;;;     0   cp'
;;;     1   tagobdy
;;;     2   proxy
;;;     3   vip
;;;     4   lbp
;;;     5   vfn
;;;     6   ntags
;;;
;;; tag
;;;     0 0
;;;     1 go
;;;     2 proxy
;;;     3 vip
;;;     4 tagbody



;;;; show-control-frames
;
#+nil
(defun show-control-frames (&optional (nth 5))
  (setq nth (min nth *frame-index*))
  (let (fn ip fp sp cp)
  (multiple-value-setq (fn ip fp sp cp)
    (get-nth-frame nth) )

  (loop
    (format t ";~%")
    (when (zerop cp) (return))

    (let ((frame-next (address-ref cp))
          (frame-type (address-ref (1+ cp))) )

      (case frame-type
        (block
        (format t ";   ~8,'0X BLOCK~%"  (ash cp 2))
          (let* ((proxy  (address-ref (+ 2 cp)))
                 (offset (address-ref (+ 3 cp)))
                 (local  (address-ref (+ 4 cp)))
                 (owner  (address-ref (+ 5 cp))) )
            (format t ";          ~S~%"
                    proxy ) ) )

        (catch
          (format t ";   ~8,'0X CATCH~%"  (ash cp 2))
          (let* ((proxy  (address-ref (+ 2 cp)))
                 (offset (address-ref (+ 3 cp)))
                 (local  (address-ref (+ 4 cp)))
                 (owner  (address-ref (+ 5 cp)))
                 (tag    (address-ref (+ 6 cp))) )
            (format t ";          ~S ~S~%"
                    tag
                    proxy ) ) )

        (special
          (format t ";   ~8,'0X BIND~%"  (ash cp 2))
          (let* ((nbinds*2 (address-ref (+ 2 cp)))
                 (addr     (- cp nbinds*2)) )
            (loop
              (when (= addr cp) (return))
              (format t ";        ~S ~40T~S~%"
                      (si::.value-cell-name (address-ref addr))
                      (address-ref (1+ addr)) )
              (incf addr 2) ) ) )

        (tagbody
          (let* ((proxy  (address-ref (+ 2 cp)))
                 (ntags  (address-ref (+ 3 cp)))
                 (local  (address-ref (+ 4 cp)))
                 (owner  (address-ref (+ 5 cp)))
                 (addr   (- cp (* ntags 3))) )
          (format t ";   ~8,'0X TAGBODY~%" (ash cp 2))
          (loop
            for nth below ntags
            do (format t ";          [~D] ip=~4,'0X ~S~%"
                       nth
                       (address-ref (+ 2 addr))
                       (address-ref (+ 0 addr)) )) ) )

        (unwind-protect
          (format t ";   ~8,'0X FINALLY~%"  (ash cp 2))
          (let* ((local   (address-ref (+ 2 cp)))
                 (finally (address-ref (+ 3 cp))) )
            (format t ";    ~X ~S~%" local finally) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Native Bind
        ;;
        (#.(ash #x4C44540 -2)
          (format t ";   ~8,'0X BIND~%"  (ash cp 2))
          (let* ((size   (address-ref (+ 2 cp)))
                 (nbinds (ash (- size 4) -1))
                 (addr   (+ 4 cp)) )
            (loop repeat nbinds do
              (format t ";        ~S ~40T ~S~%"
                      (si::.value-cell-name (address-ref addr))
                      (address-ref (1+ addr)) )
              (incf addr 2) ) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Native Block
        ;;
        (#.(ash #x424C4B0 -2)
          (let ((exit-point (address-ref (+ 3 cp))))
            (format t ";   ~8,'0X BLOCK ~S~%"
                   (ash cp 2)
                   exit-point ) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Native Catch
        ;;
        (#.(ash #x5341450 -2)
          (let ((tag (address-ref (+ 3 cp))))
            (format t ";   ~8,'0X CATCH ~S~%"
                    (ash cp 2)
                    tag ) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Native Finally
        ;;
        (#.(ash #x434C4E0 -2)
          (let ((finally (address-ref (+ 3 cp))))
            (format t ";   ~8,'0X FINALLY ~S~%"
                    (ash cp 2)
                    finally ) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Native Tagbody
        ;;
        (#.(ash #x5442590 -2)
          (let ((size (address-ref (+ 3 cp))))
            (format t ";   ~8,'0X TAGBODY ~S~%"
                    (ash cp 2)
                    size ) ) )

        (otherwise
          (format t ";  Invalid cp frame ~S at #x~X.~%"
                    frame-type
                    cp )
          (return-from show-cp-frames) ))

      (setq cp frame-next) )) ) )


;;;; xd - hexadcimal dump
;
(defcommand xd (&optional (object * object-p) (unit 1))
  "Displays hexadecimal dump of specified object."
  (labels (
    (dump (object size unit)
      (let* ((start* (ext:make-pointer object :data-type '(unsigned-byte 8)))
             (end*   (ext:copy-pointer start*)) )
        (ext:pointer-inc end* size)

        (format t "; Dump of ~S at #x~X, ~D byte~%"
          object
          (si::.pointer-address start*)
          size )

        (ecase unit
          ((1) (dump-1 start* end*))
          ((4) (dump-4 start* end*)) ) ) )

    ;; dump-1
    (dump-1 (start* end*)
        (declare (type (ext:pointer (unsigned-byte 8)) start* end*))
        (declare (values ext:unspecified))
      (loop
        with ascii = (make-string 16)
        for runner* = (ext:copy-pointer start*)
                     then (ext:pointer-inc runner*)
        while (ext:pointer< runner* end*) do
          (let ((index (ext:pointer-diff runner* start*)))
            (cond
              ((zerop (logand index 15))
                (unless (zerop index)
                  (format t "~60T~A~%" ascii) )
                (format t "; ~4,'0X -" index) )
              ((zerop (logand index 7))
                (write-string " -") ))

            (let ((datum (ext:pointer-ref runner*)))
              (format t " ~2,'0X" datum)
              (setf (schar ascii (logand index 15))
                (if (<= #x20 datum #x7E) (code-char datum) #\.) ) ) )
        finally
          (ext:pointer-dec runner*)
          (let ((index (ext:pointer-diff runner* start*)))
            (unless (zerop (logand index 15))
              (format t "~60T~A~%" (subseq ascii 0 (logand index 15))) ) )) )

    ;; dump-4
    (dump-4 (start* end*)
        (declare (type (ext:pointer (unsigned-byte 8)) start* end*))
        (declare (values ext:unspecified))
      (loop
        for runner* = (ext:copy-pointer start* '(unsigned-byte 32))
                     then (ext:pointer-inc runner*)
        while (< (si::.pointer-diff runner* end*) 0) do
          (let ((index (si::.pointer-diff runner* start*)))
            (cond
              ((zerop (logand index 15))
                (when (plusp index) (terpri))
                (format t "; ~4,'0X -" index) ))

            (let ((datum (ext:pointer-ref runner*)))
              (format t " ~8,'0X" datum) ) )
        finally
          (ext:pointer-dec runner*)
          (let ((index (si::.pointer-diff runner* start*)))
            (unless (zerop (logand index 15))
              (terpri) ) )) )

    ;; size-of
    ;;  Returns byte size of object.
    ;; BUGBUG: We assume 32bit pointer.
    (size-of (object)
        (declare (values ext:sequence-index))
      (typecase object
        (double-float 16)
        (si:byte-code-function
          (ash (si::.byte-code-function-length object) 2) )
        (si:instance
          (ash (ash (si::instance-length object) 2) 2) )
        (si:native-code-function
          (ash (si::.native-code-function-length object) 2) )
        (simple-bit-vector
          (ash (+ (ceiling (length object) 8) 2) 2) )
        (simple-string
          (ash (+ (ceiling (length object) 2) 2) 2) )
        (simple-vector
          (ash (+ (length object) 2) 2) )
        (structure-object
          (ash (si::record-length object) 2) )
        (symbol 16)
        ((or si:signed-byte-16-vector si:unsigned-byte-16-vector)
          (ash (+ (ceiling (length object) 2) 2) 2) )
        ((or si:signed-byte-32-vector si:unsigned-byte-32-vector)
          (ash (+ (ceiling (length object) 1) 2) 2) )
        ((or si:signed-byte-8-vector si:unsigned-byte-8-vector)
          (ash (+ (ceiling (length object) 4) 2) 2) )
        (otherwise 8) ) )
    )
    ;;
    (block nil
      (typecase object
        (fixnum    (dump object 256 unit))
        (character (format t "; Can't dump character.~%") (return))
        (simple-string
          (dump object (size-of object) unit) )
        ((or si:signed-byte-16-vector si:unsigned-byte-16-vector
             si:signed-byte-8-vector si:unsigned-byte-8-vector )
          (dump object (size-of object) unit) )
        (otherwise
          (dump object (size-of object) 4) ))
      (when object-p (remember-value object nil)) ) ) )
