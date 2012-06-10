;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 22 Printer - Pretty-Printer
;;; macro/m22-pprint.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m22-pprint.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;
;;;     pprint-logical-block-aux        internal
;;;
;;;     pprint-exit-if-list-exhausted   22.4.4
;;;     pprint-logical-block            22.4.7
;;;     pprint-pop                      22.4.9
;
(in-package :xc)

;;;; pprint-logical-block-aux
;;;
;;; Called by:
;;;   pprint-logical-block
;;;   ~<...~:> derective
;
(defmacro pprint-logical-block-aux ((var-stream form-stream
                                     object
                                     prefix per-line-prefix-p
                                     suffix )
                                    &body body )
(let ((var-scan    (gensym "scan"))
      (blk-block   (gensym "pprint"))
      (var-counter (gensym "counter"))
      (fn-pop      (gensym "pop")) )
  (setq body
    `(block ,blk-block
       (let ((,var-counter 0))
           (declare (type (integer 0 #.most-positive-fixnum) ,var-counter))
           (declare (ignorable ,var-counter))
        (labels (
          (,fn-pop ()
            ,@(when object
                `((unless (listp ,var-scan)
                    (write-string ". " ,var-stream)
                    (si::write-object ,var-scan ,var-stream)
                    (return-from ,blk-block) )) )

            (when (eql *print-length* ,var-counter)
              (write-string "..." ,var-stream)
              (return-from ,blk-block) )

            ,@(when object
               `((when si::*printer-label-table*
                   (let ((label (gethash ,var-scan si::*printer-label-table*)))
                     (cond
                       ((null label))
                       ((eq 't label)
                         (when (plusp ,var-counter)
                           (setf (gethash ,var-scan
                                           si::*printer-label-table*)
                                           0 )
                           (return-from ,blk-block) ) )
                       ((zerop label)
                         (return-from ,blk-block) )
                       ((and (plusp label) (plusp ,var-counter))
                         (write-string ". #" ,var-stream)
                         (si::print-fixnum-aux label ,var-stream 10)
                         (write-char #\# ,var-stream)
                         (return-from ,blk-block) )) ))))

            (incf ,var-counter)

            ,(if object
                 `(pop ,var-scan)
               (make-ignorable-form nil) ) )
          )
          (declare (ignorable #',fn-pop))
          ;;
          (macrolet (
            (pprint-exit-if-list-exhausted ()
              ,(if object
                   `'(when (null ,var-scan) (return-from ,blk-block))
                 `'(return-from ,blk-block) ) )
            (pprint-pop () '(,fn-pop))
            )
            ;;
            ,@body ) )) ))
  ;; Expansion
  ;;
 `(si::pprint-logical-block-function
    ,form-stream
    ,object
    ,prefix
    ,per-line-prefix-p
    ,suffix
    #'(lambda (,var-stream ,var-scan)
       (declare (ext:lambda-name (pprint-logical-block ,var-stream)))
       ,@(unless object `((declare (ignore ,var-scan))))
       ,body
       ,(make-ignorable-form nil) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public Macros
;;;


;;;; 22.4.4 pprint-exit-if-list-exhausted
(defmacro cl:pprint-exit-if-list-exhausted ()
  (error 'invalid-local-macro-call
         :name   'cl:pprint-exit-if-list-exhausted
         :parent 'cl:pprint-logical-block ) )


;;;; 22.4.7 pprint-logical-block
;;;
;;; Syntax:
;;;   pprint-logical-block ((stream-symbol object
;;;                          &key prefix per-line-prefix suffix )
;;;    {decl}* {form}* => nil
;;
;;; Expansion:
;;;  (with-pp-stream (var-stram form-stream)
;;;    (let ((var-scan object))
;;;      (if (not (listp var-scan))
;;;          (write-object var-scan var-stream)
;;;        (macrolet (
;;;          (pprint-exit-if-list-exhausted () ...)
;;;          (pprint-pop () (#:pprint-pop))
;;;          )
;;;          (labels ((#:pprint-pop () ...))
;;;            {decl}*
;;;            {form}*
;;;            nil ) )) ) )
;
(defmacro cl:pprint-logical-block ((stream-symbol object
                                    &key prefix per-line-prefix suffix )
                                   &body body )
  (multiple-value-bind (var-stream form-stream)
      (cond
        ((eq 'nil stream-symbol)
          (values '*standard-output* '*standard-output*) )

        ((eq 't stream-symbol)
          (values '*terminal-io* '*terminal-io*) )

        (t
          (values stream-symbol `(si::ensure-output-stream ,stream-symbol)) ))

    (when per-line-prefix
      (when prefix
        (error 'simple-program-error
               :format-control   "Cannot specify both a ~S and ~S."
               :format-arguments (list :prefix :per-line-prefix) ))
      (setq prefix per-line-prefix)
      (setq per-line-prefix t) )

    (unless prefix (setq prefix ""))

    `(pprint-logical-block-aux (,var-stream ,form-stream
                                ,object
                                ,(or prefix "") ,per-line-prefix
                                ,(or suffix "") )
       ,@body ) ) )


;;;; 22.4.9 pprint-pop
(defmacro cl:pprint-pop ()
  (error 'invalid-local-macro-call
         :name   'cl:pprint-pop
         :parent 'cl:pprint-logical-block ) )
