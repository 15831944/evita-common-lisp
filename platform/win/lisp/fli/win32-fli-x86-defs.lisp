(in-package :fli)

;;;; call
(defmacro call (proc-name &rest args)
  (let ((fname (intern (format nil "C ~A" proc-name) :fli)))
   `(,fname ,.args) ) )


;;;; define
(defmacro define (lib-name proc-name rety (&rest params))
  (labels (
    ;; make-lisp-params
    (make-lisp-params (flparams)
      (loop
        with start-end = nil
        with params = '()
        for fltype in flparams do
          (when (eq fltype 'string)
            (when start-end
              (error "Can't handle multiple strings") )
              (setq start-end '(sequence-index sequence-end)) )
          (push 't params)
        finally
          (return (nreconc params start-end)) ) )

    ;; process-param
    (process-param (nth param)
      (let* ((reg (or (nth nth '($r0 $r1 $r2 $r3 $r4)) '$rn))
             (forms (process-param-aux reg param)) )
        (if (eq reg '$rn)
            `((mov $rn ($rtcb ,(+ 56 (* 4 nth))))
              ,.forms
              (mov ($rsp ,(ash nth 2)) $rn) )
          (nconc forms `((mov ($rsp ,(ash nth 2)) ,reg))) ) ) )

    ;; process-param-aux
    (process-param-aux (reg param)
      (case (or (find-symbol (symbol-name param) :fli) param)
        ((COLORREF)
          `((sar ,reg 2)) )
        ((fixnum))
        ((HANDLE HBRUSH HDC HFONT HGDIOBJ HPEN)
          `((mov ,reg (,reg #.(- (sizeof 'bignum) (tagof 'bignum))))) )
        ((HWND)
          `((mov ,reg (,reg (offsetof window hwnd)))
            (shr ,reg 2) ) )
        ((int int32)
          `((sar ,reg 2)) )
        ((string)
          `((lea ,reg (,reg #.(- (sizeof 'simple-string)
                                 (tagof 'simple-string) ))))  )
        ((void*)
          `((shr ,reg 2)) )
        ((uint uint32)
          `((shr ,reg 2)) )
        (otherwise
          (let ((class (find-class param)))
            (unless (eq (class-of class) (find-class 'foreign-class))
              (error "~S isn't foreign-class." class) ) )
          `((mov ,reg (,reg (offsetof foreign-pointer value)))
            (sar ,reg 2) ) )) )

    ;; process-params
    (process-params ()
      (loop
        with forms = '()
        for nth from 0
        for param in params do
         (setq forms (nconc forms (process-param nth param)))
        finally (return forms) ) )

    ;; process-rety
    (process-rety (cbArgs)
      (ecase (or (find-symbol (symbol-name rety) :fli) rety)
        ((BOOL)
          `((mov $r1 'nil) (test $r0 $r0) (cmove $r0 $r1)) )
        ((COLORREF)
          `((sal $r0 2)) )
        ((fixnum)
          nil )
        ((HANDLE HBRUSH HFONT HGDIOBJ HPEN)
          `((call #'si::.box-int)) )
        ((int int32)
          `((call #'si::.box-int)) )
        ((LastError)
          `((test $r0 $r0)
            (mov $r0 'nil)
            (jne done)
            (sub $rsp ,cbArgs)
            (call ((:dll "kernel32" "GetLastError")))
            (add $rsp ,cbArgs)
            (shl $r0 2)
            done ) )
        ((LRESULT)
          `((call #'si::.box-int)) )
        ((uint uint32)
          `((call #'si::.box-uint)) )
        ((void*)
          `((shl $r0 2)) )
        ((VOID)
          `((mov $r0 'nil)) )) )
    )
    ;;
    (let* ((fname (intern (format nil "C ~A" proc-name) :fli))
           (cbArgs  (ash (length params) 2))
           (cbLocs  0)
           (cbFrame (+ cbArgs cbLocs (ash 3 2))) )
     `(progn
        (declaim (ftype (function ,(make-lisp-params params) ,rety) ,fname))
        (x86:defasm ,fname ()
          (frame ,(+ cbFrame 4))
          (sub $rsp ,cbFrame)

          ,.(process-params)

          ;; Push ToKernel frame
          (lea $r0 ($rsp ,cbArgs))
          (mov $rn ($rtcb #x2c))
          (mov ($rsp ,(+ cbArgs 0)) $rn)
          (mov ($rsp ,(+ cbArgs 4)) #x4b724e00)
          (mov ($rsp ,(+ cbArgs 8)) ,cbArgs)
          (mov ($rtcb #x2c) $r0)

          (call ((:dll ,lib-name ,proc-name)))

          ;; Restor ESP for STDCALL
          (sub $rsp ,cbArgs)

          ;; Pop ToKernel Frame
          (mov $rn ($rsp ,cbArgs))
          (mov ($rtcb #x2c) $rn)

          ,.(process-rety cbArgs)

          ;; Pop ToKernel Frame
          (add  $rsp ,cbFrame)
          (ret) ) ) ) ) )
