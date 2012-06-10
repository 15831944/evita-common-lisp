(in-package :win32)


(defmacro define-api (name rety (&rest params) library proc-name)
  (labels (
    ;; process-param
    (process-param (nth param)
      (let* ((reg (or (nth nth '($r0 $r1 $r2 $r3 $r4)) '$rn))
             (forms `(,.(process-param-aux reg param) (push ,reg))) )
        (if (eq reg '$rn)
            `((mov $rn ($rtcb ,(+ 56 (* 4 nth)))) ,.forms)
          forms ) ) )

    ;; process-param-aux
    (process-param-aux (reg param)
      (case param
        ((COLORREF)
          `((sar ,reg 2)) )
        ((fixnum))
        ((HBRUSH HFONT HPEN)
          `((mov ,reg (,reg (offsetof graphic-object handle)))
            (mov ,reg (,reg (offsetof si::handle si::value))) ) )
        ((HDC)
          `((mov ,reg (,reg (offsetof drawable hdc)))
            (mov ,reg (,reg (offsetof si::handle si::value))) ) )
        ((HGDIOBJ)
          `((mov ,reg (,reg (offsetof si::handle si::value)))) )
        ((HWND)
          `((mov ,reg (,reg (offsetof window hwnd)))
            (shr ,reg 2) ) )
        ((int int32)
          `((sar ,reg 2)) )
        ((LPCTSTR LPMSG LPRECT LPTSTR)
          `((lea ,reg (,reg #.(- (sizeof 'simple-string)
                                 (tagof 'simple-string) ))))  )
        ((LPVOID)
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
         (setq forms (nconc (process-param nth param) forms))
        finally (return forms) ) )

    ;; process-rety
    (process-rety (cbArgs)
      (ecase rety
        ((BOOL)
          `((mov $r1 'nil) (test $r0 $r0) (cmove $r0 $r1)) )
        ((COLORREF)
          `((sal $r0 2)) )
        ((fixnum)
          nil )
        ((HFONT HGDIOBJ)
          `((call #'!make-handle)) )
        ((int)
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
        ((LPVOID LRESULT)
          `((call #'si::.box-int)) )
        ((VOID)
          `((mov $r0 'nil)) )) )
    )
    ;;
    (let* ((cbArgs  (ash (length params) 2))
           (cbFrame (+ (ash (+ 1 3) 2) cbArgs)) )
     `(x86:defasm ,name ()
        (frame ,cbFrame)

        ;; Push ToKernel Frame
        (push ,cbArgs)
        (push #x4b724e00) ; Frame::Type_ToKernel
        (push ($rtcb #x2c))
        (mov  ($rtcb #x2c) $rsp)

        ,.(process-params)
        (call ((:dll ,library ,proc-name)))

        ,.(process-rety cbArgs)

        ;; Pop ToKernel Frame
        (pop  ($rtcb #x2c))
        (add  $rsp 8)
        (ret) ) ) ) )
