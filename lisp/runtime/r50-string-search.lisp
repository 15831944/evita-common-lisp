;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; Extension - String Search
;;; lisp/regex/r50-string-search.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r50-string-search.lisp#3 $
;;;
;;; Description:
;;;  This file contains fast string search function using "Quick Search"
;;;  algorithm.
;;;
;;;  Quick Search algorithm is the most efficent for large alphabets and
;;;  small pattern [Lecroq95]. It is faster than Boyer-Moore, Turbo-BM,
;;;  BM-Horspool.
;;;
;;; Features [Charras97]:
;;;  o Simplification of the Boyer-Moore algorithm; 
;;;  o Uses only the bad-character shift; 
;;;  o Easy to implement; 
;;;  o Preprocessing phase in O(m+s) time and O(s) space complexity; 
;;;  o Searching phase in O(mn) time complexity; 
;;;  o Very fast in practice for short patterns and large alphabets
;;;
;;;  where m is length of pattern, n is length of test, s = size of alphabet.
;;;
;;; Reference:
;;; [Charras04] Christian Charras and Thierry Lecroq, Handbook of Exact String
;;; Matching Algorithms, King's College London Publications, 2004,
;;; http://www-igm.univ-mlv.fr/~lecroq/string/
;;;
;;; [Sunday90] Sunday D.M., 1990, A very fast substring search algorithm,
;;; Communications of the ACM . 33(8):132-142.
;;;
;;; [Lecroq95] Lecroq, T., 1995, Experimental results on string matching
;;; algorithms, Software - Practice & Experience 25(7):727-765,
;;; http://www-igm.univ-mlv.fr/~lecroq/articles/spe95.pdf
;;;
;;; [Michailidis99]P.D. Michailidis and K.G. Margaritis, On-line String
;;; Matching Algorithms: Survey and Experimental Results, International
;;; Journal of Computer Mathematics, 76(4), pp. 411-434, 2001,
;;; http://macedonia.uom.gr/~panosm/pdfs/jpaper001.pdf
;
;
(in-package :si)

#+nil
(progn
  (import 'make-string-search-function :ext)
  (export 'make-string-search-function :ext) )

;;;; make-string-search-function
;;;
;;; Description:
;;;  Returns function which search string by using Turbo-BM algorithm.
;
(defun make-string-search-function (string &key (start 0) end ignore-case from-end)
    (declare (type string string))
    (declare (type sequence-index index))
    (declare (type sequence-end end))
    (declare (type t ignore-case))
    (declare (type t from-end))
    (declare (values function))
  (let* ((pat
           (multiple-value-bind (string start end)
               (string-data string start end)
             #+nil
             (when (eql start end)
               (error "Pattern must not be an empty-string.") )
             (subseq string start end) ) )
         (bad-char-vec #()) )
    (declare (type simple-string pat))
    (declare (type simple-vector bad-char-vec))
  (labels (
    ;; backward-search-empty
    (backward-search-empty ()
      (lambda (string &optional (start 0) end)
          (declare (lambda-name backward-search-empty))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (ignore string start))
          end ) ) )

    ;; forward-search-empty
    (forward-search-empty ()
      (lambda (string &optional (start 0) end)
          (declare (lambda-name forward-search-empty))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (ignore string end))
          start ) ) )

    ;; char-index
    (char-index (char)
        (declare (type character char))
        (declare (values sequence-index))
      (when ignore-case (setq char (char-upcase char)))
      (logand (char-code char) 127) )

    ;; prepare
    ;;  Computes occurence shift vector (bad-character shift).
    (prepare (BcV &aux (m (length pat)))
        (declare (type simple-vector BcV))
        (declare (values unspecified))
        (declare (type sequence-index m))
      (loop
        with x-key  = (if ignore-case #'char-upcase #'identity)
        initially (fill BcV (1+ m))
        for pos   of-type sequence-index from 0 below m
        for char  of-type character = (funcall x-key (schar pat pos))
        for index of-type sequence-index     = (char-index char) do
          (setf (svref BcV index) (- m pos)) ) )
    )
  (macrolet (
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Backward search
    ;;

    ;; backward-search-template
    (backward-search-template (name y-key)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop named search
            with BcV of-type simple-vector = bad-char-vec
            with x = pat
            with m = (length x)
            with y = string
            with start+m = (+ start m)
            with j of-type sequence-index = end
            while (>= j start+m) do
              (loop
                for i below m
                for j+i   = (1- j) then (1- j+i)
                for x_i   = (schar x i)
                for y_j+i = (,y-key (schar y j+i)) do
                  (unless (eql x_i y_j+i) (return))
                finally (return-from search (- j m)) )

              (when (eql j start+m) (return-from search nil))

              (let ((y_j+m (char-code (,y-key (schar y (- j m 1))))))
                (setq y_j+m (logand y_j+m 127))
                (decf j (svref BcV y_j+m)) )) ) ) )

    ;; backward-search-1-template
    (backward-search-1-template (name cmp-fn)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop
            for pos from (1- end) downto start
            for char = (schar string pos) do
              (when (,cmp-fn char pat-char) (return pos)) ) ) ) )

    ;; backward-search-xx-template
    ;;  Backward seach of two characters string where both characters are
    ;;  different, e.g. xx.
    ;;
    ;;  abc-x--xx       xx-abc-d    xx--abc-d
    ;;         1        4 3 2 1     54 3 2 1
    ;;
    (backward-search-xx-template (name cmp-fn)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop
            with pos of-type sequence-index = (- end 2)
            while (>= pos start) do
              (let ((char-2 (schar string pos)))
                (if (not (,cmp-fn char-2 pat-1))
                    (decf pos 2)
                  (let ((char-1 (schar string (1+ pos))))
                    (when (,cmp-fn char-1 pat-1) (return pos))
                    (decf pos) )) )) ) ) )

    ;; backward-search-xy-template
    ;;  Backward seach of two characters string where both characters are
    ;;  different, e.g. xy.
    ;;
    ;;  abc-x--xy       xy-ayc-d    xy--ayc-d
    ;;         1         4 32 1     5 4 32 1
    ;;
    ;; Note: pat-1='y', pat-2='x'
    ;;
    (backward-search-xy-template (name cmp-fn)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop
            with pos of-type sequence-index = (- end 2)
            while (>= pos start) do
              (let ((char-2 (schar string pos)))
                (cond
                  ((,cmp-fn char-2 pat-2)
                    (when (,cmp-fn (schar string (1+ pos)) pat-1)
                      (return pos) )
                    (decf pos 2) )
                  ((,cmp-fn char-2 pat-1)
                    (decf pos 1) )
                  (t
                    (decf pos 2) )) ) ) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Forward search
    ;;

    ;; forward-search-template
    (forward-search-template (name y-key)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop named search
            with BcV of-type simple-vector = bad-char-vec
            with x = pat
            with m = (length x)
            with y = string
            with end-m = (- end m)
            with j of-type sequence-index = start
            while (<= j end-m) do
              (loop
                for i below m
                for j+i   = j then (1+ j+i)
                for x_i   = (schar x i)
                for y_j+i = (,y-key (schar y j+i)) do
                  (unless (eql x_i y_j+i) (return))
                finally (return-from search j) )

              (when (eql j end-m) (return-from search nil))

              (let ((y_j+m (char-code (,y-key (schar y (+ j m))))))
                (setq y_j+m (logand y_j+m 127))
                (incf j (svref BcV y_j+m)) )) ) ) )

    ;; forward-search-1-template
    (forward-search-1-template (name cmp-fn)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop
            for pos from start below end
            for char = (schar string pos) do
              (when (,cmp-fn char pat-char) (return pos)) ) ) ) )

    ;; forward-search-xx-template
    ;;  Forward seach of two characters string where both characters are
    ;;  different, e.g. xx.
    ;;
    ;;  abc-x--xx       abc--x--xx  abcx-yxx
    ;;   1 2 3 45        1 2 3 4 5  1 2  3 4
    ;;
    (forward-search-xx-template (name cmp-fn)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop
            with pos of-type sequence-index = (1+ start)
            while (< pos end) do
              (let ((char-2 (schar string pos)))
                (if (not (,cmp-fn char-2 pat-1))
                    (incf pos 2)
                  (let ((char-1 (schar string (1- pos))))
                    (when (,cmp-fn char-1 pat-1) (return (1- pos)))
                    (incf pos) )) )) ) ) )

    ;; forward-search-xy-template
    ;;  Forward seach of two characters string where both characters are
    ;;  different, e.g. xy.
    ;;
    ;;  abc-x--xy       abc--x--xy  abyy-yxy
    ;;   1 2 3 45        1 2 3 4 5   1 2 3 4
    ;;
    (forward-search-xy-template (name cmp-fn)
     `(lambda (string &optional (start 0) end)
          (declare (lambda-name ,name))
          (declare (type string string))
          (declare (type sequence-index start))
          (declare (type sequence-end end))
        (multiple-value-bind (string start end) (string-data string start end)
            (declare (type simple-string string))
            (declare (type sequence-index start end))
          (loop
            with pos of-type sequence-index = (1+ start)
            while (< pos end) do
              (let ((char-2 (schar string pos)))
                (cond
                  ((,cmp-fn char-2 pat-2)
                    (when (,cmp-fn (schar string (1- pos)) pat-1)
                      (return (1- pos)) )
                    (incf pos 2) )
                  ((,cmp-fn char-2 pat-1)
                    (incf pos 1) )
                  (t
                    (incf pos 2) )) ) ) ) ) )
    )
    ;;
    (when ignore-case
      (if (some #'both-case-p pat)
          (setq pat (nstring-upcase pat))
        (setq ignore-case nil) ))

    (when from-end
      (setq pat (nreverse pat)) )

    (case (length pat)
      ((0)
        (if from-end (backward-search-empty) (forward-search-empty)) )
      ((1)
        (let ((pat-char (schar pat 0)))
          (cond
            ((and from-end ignore-case)
              (backward-search-1-template search-backward-ci char-equal) )
            (from-end
              (backward-search-1-template search-backward-cs char=) )
            (ignore-case
              (forward-search-1-template search-backward-ci char-equal) )
            (t
              (forward-search-1-template search-backward-cs char=) )) ) )
      ((2)
        (let ((pat-1 (schar pat 0))
              (pat-2 (schar pat 1)) )
          (cond
            ((and from-end ignore-case)
              (if (char-equal pat-1 pat-2)
                  (backward-search-xx-template
                        search-backward-ci char-equal )
                  (backward-search-xy-template
                        search-backward-ci char-equal )) )
            (from-end
              (if (char-equal pat-1 pat-2)
                  (backward-search-xx-template search-backward-cs char=)
                  (backward-search-xy-template search-backward-cs char=) ) )
            (ignore-case
              (if (char-equal pat-1 pat-2)
                  (forward-search-xx-template search-forward-ci char-equal)
                  (forward-search-xy-template search-forward-ci char-equal) ) )
            (t
              (if (char-equal pat-1 pat-2)
                  (forward-search-xx-template search-forward-cs char=)
                  (forward-search-xy-template search-forward-cs char=) ) )) ) )
      (otherwise
        (setq bad-char-vec (make-array 128))
        (prepare bad-char-vec)
        (cond
          ((and from-end ignore-case)
            (backward-search-template search-backward-ci char-upcase) )
          (from-end
            (backward-search-template search-backward-cs identity) )
          (ignore-case
            (forward-search-template search-forward-ci char-upcase) )
          (t
            (forward-search-template search-forward-cs identity) )) )) ) ) ) )


;;;; test
;;;
;;; Description:
;;;  Test driver for make-string-search-function function.
;
#+nil
(defun test (x y)
  (labels (
    (test-1 (&rest options &key from-end &allow-other-keys)
      (format t "~2%; ~70~~%; Search ~S ~S~%" x options)
      (loop
        with fn = (apply #'make-string-search-function x options)
        with end = (length y)
        with m = (length x)
        with start = 0
        for count = 0 then (1+ count)
        for pos = (funcall fn y start end)
        while pos do
          (format t "; ~A~%; ~VT~V~ ~D ~%" y (+ pos 2) m pos)
          (if from-end
              (setq end pos)
            (setq start (+ pos m)) )
        collect pos into occurs
        finally
          (format t "; Occurs~S: ~S~%" options occurs)
          (return occurs) ) )
    )
    (if (zerop (length x))
        (format t "Empty strng.")
      (progn
        (test-1 :ignore-case nil)
        (test-1 :ignore-case t)
        (test-1 :ignore-case t :from-end t) )) ) )

; (test "GCAGAGAG" "GCATCGCAGAGAGTATACAGTACG")
; BcV: A C G
;      2 7 1
; Occurence: 5

; (test "foo" "xfoo bar Foo xfoo barFoo")
; suff = #(0 1 3)
; GsV  = #(3 1 2)
; BcV: f o
;      2 1
; Occurence(ignore-case nil): 1   14
; Occurence(ignore-case t):   1 9 14 21
