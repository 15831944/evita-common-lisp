;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printer - Pretty Printer
;;; runtime/r22-pprint.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-pprint.lisp#12 $
;;;
;;; Description:
;;;  This file implements following functions:
;;; Description:
;;;  This file implements following functions:
;;;     pp-fit-line-p
;;;     pp-last-section
;;;     pp-line-break
;;;     pp-line-breaks
;;;     pp-misering-p
;;;     pp-prepare
;;;     pp-pop-op
;;;     pp-push-op
;;;     pp-section-column
;;;     pp-stream-handler
;;;     pp-write-char
;;;     pp-write-string
;;;
;;; Internal API:
;;;     pprint-logical-block-function
;;;     pprint-block-end
;;;     pprint-block-start
;;;     pprint-end
;;;     pprint-newline-aux
;;;     pprint-object
;;;     pprint-start
;;;
;;; Pretty Printer:
;;;     pprint-array
;;;     pprint-simple-vector
;;;
;;; Public Functions:
;;;     copy-pprint-dispatch    22.4.1
;;;     pprint-dispatch         22.4.3
;;;     pprint-fill             22.4.5
;;;     pprint-indent           22.4.6
;;;     pprint-linear           22.4.5
;;;     pprint-newline          22.4.8
;;;     pprint-tab              22.4.10
;;;     pprint-tabular          22.4.5
;;;     set-pprint-dispatch     22.4.13
;;;
;;; See Also: r22-defs.lisp, r22-defvar.lisp
;;;
;;; BUGBUG: REVIEW: Is it better to cached prefix using prefix buffer and
;;; start column stack like XP?
;;; BUGBUG: pprint-start vs. pp-start-stream. See: format-start-stream
;;; BUGBUG: pprint-block-start vs. pprint-start-block 
;;; BUGBUG: PERF: write-string. compiler-macro %write-string?
;;; BUGBUG: PERF: pprint-dispatch: Use compiled dispatcher function instead of
;;; table scanning at runtime.
;
(in-package :si)

#+debug-pprint
(defun dformat (cs &rest args)
  (let ((*print-pretty* nil))
    (apply #'.format *debug-output* cs args) ) )

#+debug-pprint
(defun dump-pp-op (stream ptr)
  (let ((heap (ref pp-stream heap (ref instance storage stream))))
    (ecase (pp-op-type ptr)
      ((:block)
        (dformat "[~D] ~S parent=[~D] cindex=~D n=~D~%"
            ptr
            (pp-op-type ptr)
            (pp-op-parent-ptr ptr)
            (pp-op-column ptr)
            (pp-op-cindex ptr)
            (pp-op-n ptr) ) )
      ((:current)
        (dformat "[~D] ~S parent=[~D] cidx=~D n=~D col=~D~%"
            ptr
            (pp-op-type ptr)
            (pp-op-parent-ptr ptr)
            (pp-op-cindex ptr)
            (pp-op-n ptr)
            (pp-op-column ptr) ) )
      ((:end)
        (dformat "[~D] ~S parent=[~D] start=[~D]~%"
            ptr
            (pp-op-type ptr)
            (pp-op-parent-ptr ptr)
            (pp-op-start-ptr  ptr) ) )
      ((:fill :linear :miser)
        (dformat "[~D] ~S parent=[~D] scol=~D cidx=~D ecol=~D~%"
            ptr
            (pp-op-type ptr)
            (pp-op-parent-ptr ptr)
            (pp-op-start-col ptr)
            (pp-op-cindex ptr)
            (pp-op-end-col ptr) ) )
      ((:start)
        (dformat "[~D] ~S parent=[~D] scol=~D cidx=~D ecol=~D sec-col=~D sec-row=~D indent=~D prefix=~S suffix=~S~%"
            ptr
            (pp-op-type ptr)
            (pp-op-parent-ptr ptr)
            (pp-op-start-col ptr)
            (pp-op-cindex ptr)
            (pp-op-end-col ptr)
            (pp-op-section-col ptr)
            (pp-op-section-row ptr)
            (pp-op-indent ptr)
            (pp-op-prefix ptr)
            (pp-op-suffix ptr) ) )) ) )


;;;; pp-clear-output
;;;
;;; Called by:
;;;   pp-line-break
;;;
;;; Description:
;;;  Clears output buffer of pp-stream. This function will be called when
;;;  line abbreviation is happend.
;
(defun pp-clear-output (stream)
    (declare (type pp-stream stream))
  (let ((st (ref instance storage stream)))
    (setf (ref pp-stream block-ptr st) 1)
    (setf (ref pp-stream scan-ptr  st) 0)
    (setf (ref pp-stream cindex    st) 0) ) )


;;;; pp-fit-line-p
;;;
;;; Description:
;;;  Returns true when specified column is less than or equal to right-margin.
(defun pp-fit-line-p (stream column)
    (declare (type stream    stream))
    (declare (type ext:sequence-index column))
  (let* ((st (ref instance storage stream))
         (right-margin (ref pp-stream right-margin st)) )
    (when (= 1 (ref pp-stream nlines st))
      (decf right-margin 3)     ; 3 for " .."
      (decf right-margin (ref pp-stream suffix-len st)) )
    (<= column right-margin) ) )


;;;; pp-line-break
;;;
;;; Syntax:
;;;   pp-line-break stream entry-ptr literal-p
;;;
;;; Called by:
;;;   pp-process
;;;
;;; Description:
;;;  1. Sends preceding section without trailing space.
;;;  2. Sends newline
;;;  3. Sends indentation and per-line-prefix
;;;
;;;  entry-ptr points section where line-break is occured or zero if
;;;  pp-line-breaks calls.
;;;
;;;  S-1 S-2 S-3 S-4
;;;          ^
;;;          |
;;;          entry-ptr (=line-break)
;;;
(defun pp-line-break (stream entry-ptr literal-p)
    (declare (type pp-stream stream))
    (declare (type ext:sequence-index entry-ptr))
    (declare (values unspecified))

  (let* ((st (ref instance storage stream))
         (heap    (ref pp-stream heap st))
         (bstream (ref pp-stream stream st))
         (column  0) )

    (setf (ref pp-stream scan-ptr st) entry-ptr)

    #+debug-pprint
    (dformat "; line-break: entry=~D ~S~%"
      entry-ptr
      (and (plusp entry-ptr) (pp-op-type entry-ptr)) )

    ;; Send buffer
    (let ((cend (if (zerop entry-ptr)
                    (ref pp-stream cindex st)
                  (pp-op-cindex entry-ptr) )) )
      (pp-send-buffer stream cend literal-p) )

    (decf (ref pp-stream nlines st))

    ;; Check line abbreviation
    (when (zerop (ref pp-stream nlines st))
      (write-string " .." bstream)

      (when (plusp entry-ptr)
        (loop
          with head-ptr = (ref pp-stream head-ptr st)
          for  scan-ptr = entry-ptr then (pp-op-prev-ptr scan-ptr)
          until (eq scan-ptr head-ptr) do
            (when (eq :start (pp-op-type scan-ptr))
              (write-string (pp-op-suffix scan-ptr) bstream) )))

      (pp-clear-output stream)
      (throw 'line-abbreviation t) )

    ;; Send newline
    (write-char #\Newline bstream)

 #+debug-pprint
 (progn
   (dformat "; line-break: entry=~D block=~D head=~D~%"
      entry-ptr
      (ref pp-stream block-ptr st)
      (ref pp-stream head-ptr st) )
   (loop
      with head-ptr = (ref pp-stream head-ptr st)
      for  scan-ptr = head-ptr then (pp-op-next-ptr scan-ptr)
      until (zerop scan-ptr) do
        (dump-pp-op stream scan-ptr) ))

    ;; Send per-line-prefix
    (loop
      with suffix-len = 0
      with head-ptr = (ref pp-stream head-ptr st)
      for  scan-ptr = head-ptr then (pp-op-next-ptr scan-ptr)
      until (eq scan-ptr entry-ptr) do
        (when (eq (pp-op-type scan-ptr) :start)
          (incf suffix-len (length (pp-op-suffix scan-ptr)))

          (let* ((prefix     (pp-op-prefix scan-ptr))
                 (start-col  (pp-op-start-col scan-ptr))
                 (prefix-len (length prefix)) )

            (when (and literal-p
                       #+nil (zerop (pp-op-next-ptr scan-ptr))
                       (zerop prefix-len) )
              (setq start-col column) )

            (write-chars #\Space (- start-col prefix-len column) bstream)
            (write-string prefix bstream)
            (setq column start-col) ))
      finally
        (loop until (zerop scan-ptr) do
          (when (eq (pp-op-type scan-ptr) :start)
            (incf suffix-len (length (pp-op-suffix scan-ptr))) )
          (setq scan-ptr (pp-op-next-ptr scan-ptr)) )
        (setf (ref pp-stream suffix-len st) suffix-len) )

    ;; Indentation
    (unless literal-p
      (let ((block-ptr
              (if (zerop entry-ptr)
                  (ref pp-stream block-ptr st)
                (pp-op-parent-ptr entry-ptr) ) ))
        (incf column (pp-op-indent block-ptr))
        (write-chars #\Space (pp-op-indent block-ptr) bstream) ))

    #+debug-pprint
    (dformat "; line-break: col=~D cindex=~D ~S~%"
      column
      (ref pp-stream cindex st)
      (subseq (ref pp-stream cbuffer st) 0 (ref pp-stream cindex st)) )

    ;; Set column for next output.
    (setf (ref pp-stream column st) (+ column (ref pp-stream cindex st)))

    ;; Update start-col and end-col
    (loop
      with start-col = column
      for scan-ptr = entry-ptr then (pp-op-next-ptr scan-ptr)
      while (plusp scan-ptr) do
           #+debug-pprint (progn
             (dformat "; pp-line-break[3]: ")
             (dump-pp-op stream scan-ptr) )

        (ecase (pp-op-type scan-ptr)
          ((:block))

          ((:current)
            (setq column (+ start-col (pp-op-cindex scan-ptr)))

            (setf (pp-op-column scan-ptr)
                  (+ start-col (pp-op-cindex scan-ptr)) ) )

          ((:end))

          ((:fill :linear :miser :start)
           (setq column (+ start-col (pp-op-cindex scan-ptr)))

           (let ((block-ptr
                   (if (eq :start (pp-op-type scan-ptr))
                       scan-ptr
                     (pp-op-parent-ptr scan-ptr) ) ))
             (setf (pp-op-section-row block-ptr) (ref pp-stream nlines st))
             (setf (pp-op-section-col block-ptr) column) )

            (if (minusp (pp-op-end-col scan-ptr))
                (setf (pp-op-start-col scan-ptr) column)
              (let ((nchars (- (pp-op-end-col scan-ptr)
                               (pp-op-start-col scan-ptr) )))
                (assert (not (minusp nchars)))
                (setf (pp-op-start-col scan-ptr) column)
                (incf column nchars)
                (setf (pp-op-end-col scan-ptr) column) )) ))) ) )


;;;; pp-line-breaks
;;;
;;; Syntax:
;;;  pp-line-breaks stream kind
;;;
;;; Called by:
;;;  pprint-newline-aux
;;;
;;; Description:
;;;  Sends pending outputs and newline.
;
(defun pp-line-breaks (stream literal-p)
    (declare (type pp-stream stream))
    (declare (values unspecified))

  (let ((st (ref instance storage stream)))
    ;; Sends preceding sections
    (loop
      with scan-ptr = (ref pp-stream scan-ptr st)
      while (plusp scan-ptr) do
        (setq scan-ptr (pp-process stream scan-ptr)) )

    (setf (ref pp-stream scan-ptr st) 0)

    ;; Line break
    (pp-line-break stream 0 literal-p) ) )


;;;; pp-misering-p
;;;
;;; Called by:
;;;  pprint-indent
;;;  pprint-newline-aux
;;;
;;; Description:
;;;  Returns true when miser mode.
;;;
;;;  Miser mode ::= Starting column of the logical block is less than or equal
;;;                 to *print-miser-width* columns from the end of the line.
;
(defun pp-misering-p (stream block-ptr)
    (declare (type pp-stream stream))
    (declare (type ext:sequence-index block-ptr))
  (let* ((st (ref instance storage stream))
         (heap (ref pp-stream heap st)) )
    (and *print-miser-width*
         (<= (- (ref pp-stream right-margin st) (pp-op-start-col block-ptr))
             *print-miser-width* )) ) )


;;;; pp-pop-op
;;;
;;; Called by:
;;;   pp-process
;
(defun pp-pop-op (stream heap entry-ptr)
    (declare (type pp-stream      stream))
    (declare (type simple-vector  heap))
    (declare (type ext:sequence-index entry-ptr))
    (declare (values ext:sequence-index))
    (assert (plusp entry-ptr))
  (labels (
    ;; free-op
    (free-op ()
        (declare (values unspecified))
      (let* ((next-ptr-ptr 0)
             (entry-size   (pp-op-size (pp-op-type entry-ptr)))
             (next-ptr     (+ entry-ptr entry-size)) )
          (declare (type ext:sequence-index next-ptr-ptr))
          (declare (type ext:sequence-index entry-size))
          (declare (type ext:sequence-index next-ptr))
        (loop
          (let* ((scan-ptr  (svref heap next-ptr-ptr))
                 (scan-size (svref heap (1+ scan-ptr))) )

            ;; No more free chunk. --SSSS--eeee--
            (when (zerop scan-ptr)
              (setf (svref heap entry-ptr) 0)
              (setf (svref heap (1+ entry-ptr)) entry-size)

              (setf (svref heap next-ptr-ptr) entry-ptr)
              (return) )

            (when (= (+ scan-ptr scan-size) entry-ptr)
              ;; --SSSSeeee--
              (incf (svref heap (1+ scan-ptr)) entry-size)

              (when (= (svref heap scan-ptr) next-ptr)
                ;; --SSSSeeeeNNNN--
                (incf (svref heap (1+ scan-ptr)) (svref heap (1+ next-ptr)))
                (setf (svref heap scan-ptr)       (svref heap next-ptr)) )

              (return) )

            (when (= next-ptr scan-ptr)
              ;; --eeeeSSSS--
              (setf (svref heap entry-ptr) (svref heap scan-ptr))

              (setf (svref heap (1+ entry-ptr))
                (+ entry-size (svref heap (1+ scan-ptr))) )

              (setf (svref heap next-ptr-ptr) entry-ptr)

              (return) )

            (when (< entry-ptr scan-ptr)
              ;; --eeee--SSSS--
              (setf (svref heap entry-ptr)      scan-ptr)
              (setf (svref heap (1+ entry-ptr)) entry-size)

              (setf (svref heap next-ptr-ptr) entry-ptr)

              (return) )

            (setq next-ptr-ptr scan-ptr) )) ) )
    )
    ;;
    ;; pp-pop-op
    ;;
    (let ((next-ptr (pp-op-next-ptr entry-ptr))
          (prev-ptr (pp-op-prev-ptr entry-ptr))
          (st (ref instance storage stream)) )

      (if (plusp next-ptr)
          (setf (pp-op-prev-ptr next-ptr) prev-ptr)
        (setf (ref pp-stream tail-ptr st) prev-ptr) )

      (if (plusp prev-ptr)
          (setf (pp-op-next-ptr prev-ptr) next-ptr)
        (setf (ref pp-stream head-ptr st) next-ptr) )

      (free-op)
      next-ptr ) ) )


;;;; pp-prepare
;;;
;;; Called by:
;;;  pp-write-char
;;;  pp-write-string-aux
;;;
;;; Description:
;;;  Make sure current line can have nchars. If not, this function calls
;;;  pp-process to make room.
(defun pp-prepare (stream nchars)
    (declare (type pp-stream stream))
    (declare (type ext:sequence-index nchars))
  (loop
    with st = (ref instance storage stream)
    with scan-ptr = (ref pp-stream scan-ptr st) do
     (when (pp-fit-line-p stream (+ (ref pp-stream column st) nchars))
       (setf (ref pp-stream scan-ptr st) scan-ptr)
       (return) )

     (when (zerop scan-ptr)
       (setf (ref pp-stream scan-ptr st) 0)
       (return) )
     (setq scan-ptr (pp-process stream scan-ptr)) ) )


;;;; pp-process
;;;
;;; Called by:
;;;   pp-line-breaks
;;;   pp-prepare
;;;
;;; Description:
;;;  Processes pending output.
;;;
;;; Note: When called by pprint-end, :linear is enclosed in block.
;
(defun pp-process (stream scan-ptr)
    (declare (type pp-stream stream))
    (declare (type ext:sequence-index scan-ptr))
  (let* ((st (ref instance storage stream))
         (heap (ref pp-stream heap st)) )
    #+debug-pprint
      (progn
        (dformat "; pp-process: ")
        (dump-pp-op stream scan-ptr) )
    (ecase (pp-op-type scan-ptr)
      ((:block)
        (unless (pp-misering-p stream (pp-op-parent-ptr scan-ptr))
          (setf (pp-op-indent (pp-op-parent-ptr scan-ptr))
                (max (pp-op-n scan-ptr) 0) ))
        (pp-pop-op stream heap scan-ptr) )

      ((:current)
        (unless (pp-misering-p stream (pp-op-parent-ptr scan-ptr))
          (let* ((block-ptr (pp-op-parent-ptr scan-ptr))
                 (new-col   (+ (pp-op-column scan-ptr) (pp-op-n scan-ptr))) )
            (setf (pp-op-indent block-ptr)
                  (max (- new-col (pp-op-start-col block-ptr)) 0) ) ))
         (pp-pop-op stream heap scan-ptr) )

      ((:end)
        ;; Free entry from end to start.
        (loop
          with next-ptr  = (pp-op-next-ptr  scan-ptr)
          with start-ptr = (pp-op-start-ptr scan-ptr)
          for  prev-ptr  = (pp-op-prev-ptr  scan-ptr) do
            (pp-pop-op stream heap scan-ptr)
            (when (eq start-ptr scan-ptr) (return next-ptr))
            (setq scan-ptr prev-ptr) ) )

      ((:fill)
       (let ((next-col   (pp-op-end-col scan-ptr))
             (parent-ptr (pp-op-parent-ptr scan-ptr)) )
         (when (or (minusp next-col)
                   (/= (ref pp-stream nlines st)
                       (pp-op-section-row parent-ptr) )
                   (pp-misering-p stream parent-ptr)
                   (not (pp-fit-line-p stream (1- next-col))) )
               (pp-line-break stream scan-ptr nil) )
         (pp-pop-op stream heap scan-ptr) ) )

      ((:linear)
       (pp-line-break stream scan-ptr nil)
       (pp-pop-op stream heap scan-ptr) )

      ((:miser)
        (when (pp-misering-p stream (pp-op-parent-ptr scan-ptr))
          (pp-line-break stream scan-ptr nil) )
        (pp-pop-op stream heap scan-ptr) )

      ((:start)
        (let ((next-col (pp-op-end-col scan-ptr))
              (next-ptr (pp-op-next-ptr scan-ptr)) )
          (when (and (not (minusp next-col))
                     (pp-fit-line-p stream next-col) )
            (setq next-ptr scan-ptr)

            (loop
              (when (and (eq :end     (pp-op-type      next-ptr))
                         (eq scan-ptr (pp-op-start-ptr next-ptr)) )
                (setq next-ptr (pp-pop-op stream heap next-ptr))
                (return) )
              (setq next-ptr (pp-pop-op stream heap next-ptr)) ))

          next-ptr ) )) ) )


;;;; pp-push-op
;;;
;;; Description:
;;;  Makes room for operation on pp-stream-heap.
(defun pp-push-op (stream op entry-size)
    (declare (type pp-stream stream))
    (declare (type pp-operation op))
    (declare (values simple-vector ext:sequence-index))
  (let* ((st (ref instance storage stream))
         (heap         (ref pp-stream heap st))
         (scan-ptr     (svref heap 0))
         (next-ptr-ptr 0) )
    (loop
      (when (zerop scan-ptr)
        ;; Extend heap
        (let* ((old-size (length heap))
               (new-size (ceiling (* 4 (+ old-size entry-size)) 3))
               (new-heap (make-array new-size)) )
          (replace new-heap heap)
          (setf (ref pp-stream heap st) new-heap)
          (setq heap new-heap)

          ;; Put new free chunk at head of free-chunk-list.
          (setf (svref heap 0) old-size)

          (setf (svref heap old-size)      next-ptr-ptr)
          (setf (svref heap (1+ old-size)) (- new-size old-size))

          ;; Start over
          (setq next-ptr-ptr 0)
          (setq scan-ptr old-size) ))

      (let ((next-ptr  (svref heap scan-ptr))
            (scan-size (svref heap (1+ scan-ptr))) )

        (when (>= scan-size entry-size)
          ;; Split free chunk: --FFFFFF-- => --UUUUFF--
          (when (> scan-size (+ entry-size 2))
            (let ((free-ptr (+ scan-ptr entry-size)))
              (setf (svref heap free-ptr)      next-ptr)
              (setf (svref heap (1+ free-ptr)) (- scan-size entry-size))
              (setq next-ptr free-ptr) ))
          (setf (svref heap next-ptr-ptr) next-ptr)
          (return) )

        ;; Try next free chunk
        (setq next-ptr-ptr scan-ptr)
        (setq scan-ptr next-ptr) ))

    ;; Initialize pp-op slots
    (let ((tail-ptr (ref pp-stream tail-ptr st)))
      (setf (pp-op-type     scan-ptr) op)
      (setf (pp-op-next-ptr scan-ptr) 0)
      (setf (pp-op-prev-ptr scan-ptr) tail-ptr)

      (setf (pp-op-parent-ptr scan-ptr) (ref pp-stream block-ptr st))
      (setf (pp-op-start-col  scan-ptr) (ref pp-stream column st))

      (when (>= entry-size 7)
        (setf (pp-op-cindex  scan-ptr) (ref pp-stream cindex st))
        (setf (pp-op-end-col scan-ptr) -1) )

      (when (plusp tail-ptr)
        (setf (pp-op-next-ptr tail-ptr) scan-ptr) )

      (setf (ref pp-stream tail-ptr st) scan-ptr) )

    (when (zerop (ref pp-stream scan-ptr st))
      (setf (ref pp-stream scan-ptr st) scan-ptr) )

    (values heap scan-ptr) ) )


;;;; pp-section-column
;;;
;;; Called by:
;;;  pprint-tab
;;;
;;; Description:
;;;  Returns start column of current section.
;
(defun pp-section-column (stream)
    (declare (values sequence-index))
    (declare (type pp-stream stream))
  (let* ((st (ref instance storage stream))
         (heap (ref pp-stream heap st)) )
    (pp-op-section-col (ref pp-stream block-ptr st)) ) )


;;;; pp-send-buffer
;;;
;;; Called by:
;;;   pp-line-break
;;;
;;; Description:
;;;  Sends cbuffer to base stream.
;
(defun pp-send-buffer (stream cend literal-p)
    (declare (values unspecified))
    (declare (type pp-stream stream))
    (declare (type ext:sequence-index cend))

  (let ((st (ref instance storage stream)))

    (assert (<= 0 cend (ref pp-stream cindex (ref instance storage stream))))

  (when (not (plusp cend))
    (return-from pp-send-buffer) )

  (let ((bstream (ref pp-stream stream  st))
        (cbuffer (ref pp-stream cbuffer st))
        (climit  cend) )

      (unless literal-p
        (loop
          with index = cend do
            (decf index)
            (when (minusp index) (return))
            (when (char/= #\Space (schar cbuffer index))
              (setq climit (1+ index))
              (return) )) )

      (write-string cbuffer bstream :end climit)

      (replace cbuffer cbuffer
               :start2 cend
               :end2   (ref pp-stream cindex st) )

      (decf (ref pp-stream cindex st) cend) )

    ;; Update pp-op-cindex
    (loop
      with heap     = (ref pp-stream heap     st)
      for  scan-ptr = (ref pp-stream scan-ptr st)
                    then (pp-op-next-ptr scan-ptr)
      while (plusp scan-ptr) do
        (ecase (pp-op-type scan-ptr)
          ((:block :end))
          ((:current :fill :linear :miser :start)
            (decf (pp-op-cindex scan-ptr) cend) ))) ) )


;;;; pp-write-char
;;;
;;; Description:
;;;  Puts one character on buffer with checking line overflow if character
;;;  isn't newline.
;
(defun pp-write-char (char stream)
    (declare (type character char))
    (declare (type pp-stream stream))
    (declare (values unspecified))
  (if (char= #\Newline char)
      (pprint-newline-aux :literal stream)
    (progn
      (pp-prepare stream 1)

      (let* ((st (ref instance storage stream))
             (cbuffer (ref pp-stream cbuffer st))
             (cbufsiz (length cbuffer))
             (cindex  (ref pp-stream cindex st)) )

        (when (= cindex cbufsiz)
          (write-string cbuffer (ref pp-stream stream st))
          (setq cindex 0) )

        (setf (schar cbuffer cindex) char)
        (setf (ref pp-stream cindex st) (1+ cindex))
        (incf (ref pp-stream column st)) )))
   char )


;;;; pp-write-string
;;;
;;; Description:
;;;   Process substrings separated by #\Newline.
;
(defun pp-write-string (str stream start end)
    (declare (type string           str))
    (declare (type pp-stream        stream))
    (declare (type ext:sequence-index        start))
    (declare (type ext:sequence-end end))
    (declare (values string))
  (multiple-value-bind (string start end) (string-data str start end)
    (when (= end start)
      (return-from pp-write-string str) )

    #+debug-pprint
    (dformat ";   write-string: col=~D ~S~%"
        (ref pp-stream column (ref instance storage stream))
        (subseq string start end) )

    ;; Write substrings separated by newline.
    (loop
      for index from start below end do
        (when (char= #\Newline (schar string index))
          (pp-write-string-aux string stream start index)
          (setq start (1+ index))
          (pprint-newline-aux :literal stream) ))

    (when (> end start)
      (pp-write-string-aux string stream start end) )

    str ) )


;;;; pp-write-string-aux
;;;
;;; Description:
;;;  Puts string into cbuffer.
;
(defun pp-write-string-aux (string stream start end)
    (declare (type simple-string  string))
    (declare (type pp-stream      stream))
    (declare (type ext:sequence-index      start))
    (declare (type ext:sequence-index      end))

  (let ((nchars (- end start)))
    (when (zerop nchars)
      (return-from pp-write-string-aux nil) )

    (pp-prepare stream (- end start))

    (let* ((nrests  nchars)
           (st (ref instance storage stream))
           (cbuffer (ref pp-stream cbuffer st))
           (cbufsiz (length cbuffer))
           (cstart  (ref pp-stream cindex st))
           (cend    (min cbufsiz (+ cstart nrests))) )

        (replace cbuffer string
                 :start1 cstart
                 :end1   cend
                 :start2 start )

        (decf nrests (- cend cstart))

        (when (plusp nrests)
          (write-string cbuffer (ref pp-stream stream st))
          (incf start (- cend cstart))

          (multiple-value-bind (q r) (truncate nrests cbufsiz)
            (when (plusp q)
              (write-string string (ref pp-stream stream st)
                            :start start
                            :end   (* q cbufsiz) )
              (incf start (* q cbufsiz)) )
           (replace cbuffer string
                    :start2 start
                    :end2  (+ start r) )
           (setq cend r) ) )

    (setf (ref pp-stream cindex st) cend)
    (incf (ref pp-stream column st) nchars) ) ) )

#|
;;; Extending buffer version.
;;; This version requires uses buffer which size is greater than line width.
;
(defun pp-write-char (char stream)
    (declare (type character char))
    (declare (type pp-stream stream))
  (if (char= #\Newline char)
      (pp-line-breaks stream t)
    (progn
      (pp-prepare stream 1)

      (let* ((cbuffer (ref pp-stream cbuffer st))
             (cbufsiz (length cbuffer))
             (cindex  (ref pp-stream cindex st)) )

        (when (= cindex cbufsiz)
          (let ((new-buffer (make-string (ceiling (* cbufsiz 4) 3))))
            (replace new-buffer cbuffer)
            (free-pooled-format-string cbuffer)
            (setq cbuffer new-buffer)
            (setf (ref pp-stream cbuffer st) cbuffer) ) )

        (setf (schar cbuffer cindex) char)
        (setf (ref pp-stream cindex st) (1+ cindex))
        (incf (ref pp-stream column st)) ))) )


(defun pp-write-string-aux (string stream start end)
    (declare (type simple-string  string))
    (declare (type pp-stream      stream))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-index end))

  (let ((nchars (- end start)))
    (when (zerop nchars)
      (return-from pp-write-string-aux nil) )

    (pp-prepare stream (- end start))

    (let* ((cbuffer (ref pp-stream cbuffer st))
           (cbufsiz (length cbuffer))
           (cstart  (ref pp-stream cindex st))
           (cend    (+ cstart nchars)) )

      (when (> cend cbufsiz)
        (let ((new-buffer (make-string (ceiling (* cend 4) 3))))
          (replace new-buffer cbuffer)
          (free-pooled-format-string cbuffer)
          (setq cbuffer new-buffer)
          (setf (ref pp-stream cbuffer st) cbuffer) ))

      (replace cbuffer string
               :start1 cstart
               :end1   cend
               :start2 start )

    (setf (ref pp-stream cindex st) cend)
    (incf (ref pp-stream column st) nchars) ) ) )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Internal API
;;;

;;;; pprint-logical-block-function
;;;
;;; See Also: pprint-object
;
(defun pprint-logical-block-function (stream object
                                      prefix per-line-prefix-p suffix
                                      body-fn )
    (declare (values null))
    (declare (type stream   stream))
    (declare (type string   prefix))
    (declare (type string   suffix))
    (declare (type function body-fn))
  (labels (
    ;; need-lable-p
    (need-label-p (object)
      (typecase object
        (null nil)
        (vector (eq (array-element-type object) 't))
        (otherwise t) ) )

    ;; process-logical-block
    ;;
    (process-logical-block (stream)
      (if (not (typep stream 'pp-stream))
          (start-new-pp-stream stream)
        (progn
          (pprint-block-start stream prefix per-line-prefix-p suffix)
          (funcall body-fn stream object)
          (pprint-block-end stream) )) )

    ;; start-new-pp-stream
    ;;
    (start-new-pp-stream (stream)
      (let* ((pp-stream (pprint-start stream))
             (*printer-stream* pp-stream) )
        (catch 'line-abbreviation
          (pprint-block-start pp-stream prefix per-line-prefix-p suffix)
          (funcall body-fn pp-stream object)
          (pprint-block-end pp-stream) )
        (pprint-end pp-stream) ) )
    )
    ;;
    ;; pprint-logical-block-function
    ;;
    (unless (listp object)
      (write-object object stream)
      (return-from pprint-logical-block-function) )

    (when (and *print-level* (>= *printer-level* *print-level*))
      (write-char #\# stream)
      (return-from pprint-logical-block-function nil) )

    (let ((*printer-level* (1+ *printer-level*)))
      (cond
        ((not *print-circle*)
          (process-logical-block stream) )

        ((eq *printer-stream* stream)
          (process-logical-block stream) )

        ((and (typep stream 'pp-stream)
              (eq *printer-stream*
                  (ref pp-stream stream (ref instance storage stream) )) )
          (let ((*printer-stream* stream))
            (process-logical-block stream) ) )

        (t
          (let* ((*printer-label*       nil)
                 ;; BUGBUG: NYI: Should use pooled hash-table.
                 (label-table           (make-hash-table :test #'eq))
                 (*printer-label-table* label-table) )

            ;; Register objects.
            (when (need-label-p object)
              (setf (gethash object label-table) t)
              (start-new-pp-stream (make-broadcast-stream)) )

            ;; Pretty print object with labels.
            (if (not (eql 0 (gethash/eq object label-table)))
                (setq *printer-label* 0)
              (progn
                (setq *printer-label* 1)
                (setf (gethash object label-table) 1)
                (write-string "#1=" stream) ))
            (let ((*printer-stream* stream))
              (process-logical-block stream) ) ) ))
      nil ) ) )


;;;; pprint-block-end
;;;
;;; Called by:
;;;  pprint-logical-block
;;;
;;; Description:
;;;  Ends logical block.
;;;
;;;  1. Outputs suffix.
;;;  2. Sets cindex of block end at :start entry.
;;;  3. Push :end entry.
;;;  4. Pop block.
;
(defun pprint-block-end (stream)
    (declare (type pp-stream stream))

  (let* ((st (ref instance storage stream))
         (heap       (ref pp-stream heap st))
         (block-ptr  (ref pp-stream block-ptr st))
         (parent-ptr (pp-op-parent-ptr block-ptr)) )
     ;(assert (eq :start (pp-op-type block-ptr)))

    (let ((suffix (pp-op-suffix block-ptr)))
      (pp-write-string suffix stream 0 nil)
      (decf (ref pp-stream suffix-len st) (length suffix)) )

    #+debug-pprint
    (dformat "; pp-block-end: col=~D~%"
      (ref pp-stream column st) )

    (multiple-value-bind (heap entry-ptr)
        (pp-push-op stream :end #.(pp-op-size :end))
      (setf (pp-op-start-ptr  entry-ptr) block-ptr)
      (setf (pp-op-parent-ptr entry-ptr) parent-ptr)

      (setf (ref pp-stream block-ptr st) parent-ptr) ) ) )


;;;; pprint-block-start
;;;
;;; Called by:
;;;  pprint-logical-block
;;;
;;; Description:
;;;  Starts logical block.
(defun pprint-block-start (stream prefix per-line-prefix-p suffix)
    (declare (type pp-stream stream))
    (declare (type string    prefix))
    (declare (type string    suffix))

  (pp-write-string prefix stream 0 nil)

  (let ((st (ref instance storage stream)))
    (incf (ref pp-stream suffix-len st) (length suffix))

    #+debug-pprint
     (dformat "; pprint-block-start: ~S sec-col=~D sec-row=~D~%"
       (subseq (ref pp-stream cbuffer st) 0 (ref pp-stream cindex st))
       (ref pp-stream column st)
       (ref pp-stream nlines st) )

    (multiple-value-bind (heap entry-ptr)
        (pp-push-op stream :start #.(pp-op-size :start))
      (setf (pp-op-section-row  entry-ptr) (ref pp-stream nlines st))
      (setf (pp-op-section-col  entry-ptr) (ref pp-stream column st))
      (setf (pp-op-prefix       entry-ptr) (if per-line-prefix-p prefix ""))
      (setf (pp-op-suffix       entry-ptr) suffix)
      (setf (pp-op-indent       entry-ptr) 0)
      (setf (pp-op-end-col      entry-ptr) -1)

      (setf (ref pp-stream block-ptr st) entry-ptr) ) ) )


;;;; pprint-end
;;;
;;; Description:
;;;  This function is called at end of pretty printing.
;;;   1. End outer most logical-block pushed by pprint-start
;;;   2. Set end column of the last section.
;;;   3. Process pending operations.
;;;   4. Free pp-stream.
;
(defun pprint-end (stream)
    (declare (type pp-stream stream))
  (pprint-block-end stream)

  (let ((st (ref instance storage stream)))

    ;; Sends preceding sections
    (loop
      with heap     = (ref pp-stream heap     st)
      with scan-ptr = (ref pp-stream scan-ptr st)
      while (plusp scan-ptr) do
        #+debug-pprint
          (progn (dformat "; pprint-end: ") (dump-pp-op stream scan-ptr))
        (case (pp-op-type scan-ptr)
          ((:fill :linear :miser :start)
            (when (minusp (pp-op-end-col scan-ptr))
              (setf (pp-op-end-col scan-ptr) (ref pp-stream column st)) ) ))
        (setq scan-ptr (pp-process stream scan-ptr)) )

    (write-string (ref pp-stream cbuffer st) (ref pp-stream stream st)
                  :end (ref pp-stream cindex st) )

    (free-pooled-pp-stream stream) ) )


;;;; pprint-newline-aux
;
(defun pprint-newline-aux (kind stream)
    (declare (type pp-newline kind))
    (declare (type pp-stream stream))
    (declare (values unspecified))

  ;; Set end column of the last sections of current level and
  ;; lesser level of sections.
  (loop
    with st        = (ref instance storage stream)
    with heap      = (ref pp-stream heap      st)
    with block-ptr = (ref pp-stream block-ptr st)
    with level     = 0
    with col       = (ref pp-stream column    st)
    with tail-ptr  = (ref pp-stream tail-ptr  st)
    for  scan-ptr  = tail-ptr then (pp-op-prev-ptr scan-ptr)
    initially
      (setf (pp-op-section-col block-ptr) col)
      #+debug-pprint (dformat "; pp-newline-aux: block-ptr=~D~%" block-ptr)

    while (plusp scan-ptr) do
        #+debug-pprint
          (progn
            (dformat "; pp-newline-aux: level=~D " level)
            (dump-pp-op stream scan-ptr) )

      (ecase (pp-op-type scan-ptr)
        ((:block))
        ((:current))
        ((:end)
          (let ((start-ptr (pp-op-start-ptr scan-ptr)))
            (when (minusp (pp-op-end-col start-ptr))
              (setf (pp-op-end-col start-ptr) col) ) )
          (incf level) )
        ((:fill :linear :miser)
          (when (and (not (minusp level)) (minusp (pp-op-end-col scan-ptr)))
            (setf (pp-op-end-col scan-ptr) col) )
          (when (eql (pp-op-parent-ptr scan-ptr) block-ptr)
            (return) ) )
        ((:start)
          (when (eql block-ptr scan-ptr)
            (assert (zerop level))
            (return) )
          (decf level) )))

  (ecase kind
    ((:fill)
      (pp-push-op stream :fill #.(pp-op-size :fill)) )

    ((:linear)
     (pp-push-op stream :linear #.(pp-op-size :linear)) )

    ((:literal)
      (pp-line-breaks stream t) )

    ((:mandatory)
      (pp-line-breaks stream nil) )

    ((:miser)
      (pp-push-op stream :miser #.(pp-op-size :miser)) )) )


;;;; pprint-object
;;;
;;; Called by:
;;;  write-object
;;;
;;; See Also: pprint-logical-block-function
(defun pprint-object (object stream)
    (declare (type stream stream))
  (let ((pprinter (pprint-dispatch object)))
    (if (typep stream 'pp-stream)
        (funcall pprinter stream object)
      (let ((pp-stream (pprint-start stream)))
        (catch 'line-abbreviation
          (funcall pprinter pp-stream object) )
        (pprint-end pp-stream) )) ) )


;;;; pprint-start
;;;
;;; Description:
;;;  Initialize pp-stream based on base-stream.
;;;
;;; BUGBUG: NYI: get line-width from stream.
(defun pprint-start (bstream)
    (declare (type (and stream  (not pp-stream)) bstream))
    (declare (values pp-stream))

  #+debug-pprint
  (dformat "; pprint-start: ~S col=~D~%"
    bstream
    (stream-line-column bstream ))

  (let* ((stream (make-pooled-pp-stream))
         (st (ref instance storage stream)) )
    (setf (ref pp-stream stream st) bstream)

    (setf (ref pp-stream nlines st)
          (or (and (not *print-readably*) *print-lines*)
              #.(ash 1 20) ))

    (setf (ref pp-stream column st)
          (or (stream-line-column bstream) 0) )

    ;; Allocate line buffer
    (let ((right-margin (or *print-right-margin* 72))
          (cbuffer (make-pooled-format-string)) )
      (when (< (length cbuffer) right-margin)
        (free-pooled-format-string cbuffer)
        (setq cbuffer (make-string (* 16 (ceiling right-margin 16)))) )

      (setf (ref pp-stream cbuffer      st) cbuffer)
      (setf (ref pp-stream right-margin st) right-margin) )

    ;; Ininitlaize heap
    (let ((heap (make-pooled-pp-chunk)))
      (setf (ref pp-stream heap st) heap)

      ;; Set heap[1] as the first free block
      (setf (svref heap 0) 1)

      ;; Initialize free block at heap@1
      (setf (svref heap 1) 0)                       ; next free block
      (setf (svref heap 2) (1- (length heap))) )    ; size of free block

    ;; Push :start operation
    (multiple-value-bind (heap entry-ptr)
        (pp-push-op stream :start #.(pp-op-size :start))

      (setf (pp-op-start-col entry-ptr) (ref pp-stream column st))
      (setf (pp-op-end-col   entry-ptr) -1)

      (setf (pp-op-section-row entry-ptr) (ref pp-stream nlines st))
      (setf (pp-op-section-col entry-ptr) (ref pp-stream column st))

      (setf (pp-op-cindex    entry-ptr) 0)

      (setf (pp-op-indent    entry-ptr) 0)

      (setf (pp-op-prefix    entry-ptr) "")
      (setf (pp-op-suffix    entry-ptr) "")

      (setf (ref pp-stream head-ptr  st) entry-ptr)
      (setf (ref pp-stream scan-ptr  st) entry-ptr)
      (setf (ref pp-stream tail-ptr  st) entry-ptr)
      (setf (ref pp-stream block-ptr st) entry-ptr)

     stream ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Pretty Printers
;;;


;;;; pprint-array
;
(defun pprint-array (stream array)
    (declare (type stream stream))
    (declare (type array  array))
    (declare (values unspecified))
  (labels (
    ;; pprint-array-like-list
    ;;
    (pprint-array-like-list (stream array)
        (declare (type stream stream))
        (declare (type array array))
        (declare (values unspecified))
      (format stream "#~Da" (array-rank array))
      (multiple-value-bind (vector start) (array-data array)
        (pprint-array-contents stream
                               vector start (array-dimensions array) 0 ) ) )

    ;; pprint-array-contents
    ;;
    (pprint-array-contents (stream vector start dimensions level)
      (incf level)
      (cond
        ((null dimensions)
          (write-object (aref vector start) stream) )

        ((plusp (first dimensions))
          ;; Note: body of pprint-local-block is executed twice when
          ;; *print-circle* is true, so we avoid to modifiy variable
          ;; dimensions.
          (pprint-logical-block (stream nil :prefix "(" :suffix ")")
             (loop
               with dims  = dimensions
               with dim   = (pop dims)
               with kind  = (if dims :linear :fill)
               with step  = (reduce #'* dims)
               with count = 0
               for  index = start then (+ index step) do
                 (pprint-pop)
                 (pprint-array-contents stream vector index dims level)
                 (incf count)
                 (when (= count dim) (return))
                 (write-char #\Space stream)
                 (pprint-newline kind stream) ) ) )
        (t
          (write-string "()" stream) )) )

    ;; pprint-array-readably
    ;;
    (pprint-array-readably (stream array)
      (cond
        ((not *read-eval*)
         (error 'print-not-readable :object array) )

        ((array-displacement array)
          (error 'print-not-readable :object array) )

        (t
          (let ((form
                  (loop
                    for index from 0 below (array-total-size array)
                      collect `(setf (row-major-aref array ,index)
                                     ',(row-major-aref array index)) )) )
            (setq form
              `(let ((array (make-array ',(array-dimensions array)
                              :adjustable ',(adjustable-array-p array)
                              :element-type ',(array-element-type array) )))
                 ,@form ))
            (pprint form stream) ) )) )
    )
    ;;
    ;; pprint-array
    ;;
    (cond
      (*print-readably*
        (pprint-array-readably stream array) )

      (*print-array*
        (pprint-array-like-list stream array) )

      (t
        ;; Note: print-array function takes array and stream in this order.
        ;; This is different from pprint function.
        (print-array-unreadably array stream) ) )) )


;;;; pprint-vector
;
(defun pprint-vector (stream vector)
    (declare (type stream stream))
    (declare (type vector  vector))
    (declare (values unspecified))
  (labels (
    ;; pprint-vector-like-list
    ;;
    (pprint-vector-like-list (stream vector)
      (pprint-logical-block (stream nil :prefix "#(" :suffix ")")
        (loop
          with space = ""
          for elt across vector do
            (write-string space stream)
            (pprint-newline :fill stream)
            (write-object elt stream)
            (pprint-pop)
            (setq space " ") ) ) )

    ;; pprint-vector-readably
    ;;
    (pprint-vector-readably (stream vector)
      (cond
        ((not *read-eval*)
         (error 'print-not-readable :object vector) )

        ((array-displacement vector)
          (error 'print-not-readable :object vector) )

        (t
          (pprint
           `(make-array (array-total-size vector)
               :element-type ',(array-element-type vector)
               :adjustable ',(adjustable-array-p vector)
               ,@(when (array-has-fill-pointer-p vector)
                 `((:fill-pointer (fill-pointer vector))) )
               :initial-element ',(coerce vector 'list) )
            stream ) )) )
    )
    ;;
    ;; pprint-vector
    ;;
    (cond
      ((stringp vector)
        (print-object vector stream) )

      ((not *print-array*)
        ;; Note: print-array function takes array and stream in this order.
        ;; This is different from pprint function.
        (if *print-readably*
            (pprint-vector-readably stream vector)
          (print-vector-unreadably vector stream) ) )

      ((simple-vector-p vector)
        (pprint-vector-like-list stream vector) )

      ((bit-vector-p vector)
        (print-object vector stream) )

      (*print-readably*
        (pprint-vector-readably stream vector) )

      (t
        (pprint-vector-like-list stream vector) )) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Function
;;;;

;;;; 22.4.1 copy-pprint-dispatch
;
(defun cl:copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
    (declare (type (or pp-dispatch-table null) table))
    (declare (values pp-dispatch-table))
  (unless table (setq table *standard-pprint-dispatch*))

  (let ((new-table (make-pp-dispatch-table)))
    (setf (pp-dispatch-table-entries new-table)
          (copy-list (pp-dispatch-table-entries table) ))
    (loop
      with entries     = (pp-dispatch-table-cons-entries table)
      with new-entries = (pp-dispatch-table-cons-entries new-table)
      for key being each hash-key in entries using (hash-value val) do
        (setf (gethash key new-entries) (copy-list val)) )

    new-table ) )


;;;; 22.4.3 pprint-dispatch
;;;
;;; Syntax
;;;   pprint-dispatch object &optional table => function, found-p
;
(defun cl:pprint-dispatch (object &optional (table *print-pprint-dispatch*))
    (declare (type (or null pp-dispatch-table) table))
    (declare (values function-designator boolean))
  (unless table (setq table *standard-pprint-dispatch*))

  (loop
    with found = (and (consp object)
                      (symbolp (first object))
                      (gethash/eq (first object)
                                  (pp-dispatch-table-cons-entries table) ))
    for entry in (pp-dispatch-table-entries table) do
      (when (typep object (first entry))
        (cond
          ((not found) (setq found entry))
          ((not (second entry)))
          ((not (second found)) (setq found entry))
          ((< (second found) (second entry)) (setq found entry)) ))
    finally
      (if found
          (return (values (third found) t))
        (return (values (labels ((default-use-print-object (s o)
                                   (print-object o s) ))
                          #'default-use-print-object )
                        nil )))) )


;;;; 22.4.5 pprint-fill
;;;; 22.4.5 pprint-linear
;;;; 22.4.5 pprint-tabular
;
(macrolet (
  (define-pprint-list (name newline tabular-p)
    `(defun ,name (stream list &optional (colon-p t) at-sign-p
                   ,@(when tabular-p '((tabsize))) )
          (declare (ignore at-sign-p))
       ,@(when tabular-p
          '((unless tabsize (setq tabsize 16))) )
       (pprint-logical-block (stream list
                              :prefix (if colon-p "(" "")
                              :suffix (if colon-p ")" "") )
         (pprint-exit-if-list-exhausted)
         (loop
           (write (pprint-pop) :stream stream)
           (pprint-exit-if-list-exhausted)
           (write-char #\Space stream)
           ,@(when tabular-p
              '((pprint-tab :section-relative 0 tabsize stream)) )
           (pprint-newline ,newline stream) )) ) )
    )
    ;;
    ;;
    (define-pprint-list cl:pprint-fill    :fill nil)
    (define-pprint-list cl:pprint-linear  :linear nil)
    (define-pprint-list cl:pprint-tabular :fill t)
 ) ; macrolet


;;;; 22.4.6 pprint-indent
;
(defun cl::pprint-indent (relative-to n &optional stream)
    (declare (type (member :block :current) relative-to))
    (declare (type real n))
    (declare (type stream-designator stream))
    (declare (values null))
  (setq stream (ensure-output-stream stream))
  (when (and *print-pretty* (typep stream 'pp-stream))
    (multiple-value-bind (heap entry-ptr)
        (pp-push-op stream relative-to  #.(pp-op-size :block))
      (setf (pp-op-n entry-ptr) n) ))
  nil )


;;;; 22.4.8 pprint-newline
;
(defun cl:pprint-newline (kind &optional stream)
    (declare (type (member :fill :linear :literal :mandatory :miser) kind))
    (declare (type stream-designator stream))
    (declare (values null))
  (setq stream (ensure-output-stream stream))
  (when (and *print-pretty* (typep stream 'pp-stream))
    (pprint-newline-aux kind stream) )
  nil )


;;;; 22.4.10 pprint-tab
;
(defun cl::pprint-tab (kind column colinc &optional stream)
    (declare (type (member :line :line-relative :secion :section-relative)
                   kind ))
    (declare (type unsigned-byte column))
    (declare (type unsigned-byte colinc))
    (declare (type stream-designator stream))
    (declare (values null))

  (setq stream (ensure-output-stream stream))
  (when (and *print-pretty* (typep stream 'pp-stream))
    (multiple-value-bind (indented-p relative-p)
        (ecase kind
          (:line             (values nil nil))
          (:line-relative    (values nil t))
          (:section          (values t   nil))
          (:section-relative (values t   t)) )
      (let* ((col (ref pp-stream column (ref instance storage stream)))
             (cur (if (not indented-p)
                      col
                    (- col (pp-section-column stream)) ) )
             (new
               (cond
                 ((zerop colinc)
                   (if relative-p (+ cur column) (max column cur)) )
                 (relative-p
                   (* colinc (ceiling (+ cur column) colinc)) )
                 ((> column cur)
                   column )
                 (t
                   (+ column
                      (* colinc
                        (ceiling (- cur column) colinc) )) )) )
             (nchars (- new cur)) )
        #+debug-pprint
        (dformat "; pprint-tab: col=~D sec=~D cur=~D new=~D column=~D colinc=~D~%"
            col
            (pp-section-column stream)
            cur new
            column colinc )
        (loop repeat nchars do (pp-write-char #\Space stream)) ) ))
  nil )


;;;; 22.4.13 set-pprint-dispatch
;;;
;;; BUGBUG: NYI: We should use parse-type for validating type specifier.
;
(defun cl:set-pprint-dispatch (type function
                               &optional (priority 0)
                                         (table *print-pprint-dispatch*) )
    (declare (type ext:type-specifier type))
    (declare (type (or function symbol) function))
    (declare (type (or null pp-dispatch-table) table))
    (declare (type real priority))
    (declare (values null))

  (unless table (setq table *standard-pprint-dispatch*))

  (labels ((one-argument-form-p (form operator)
            (and (consp form)
                 (eq operator (car form))
                 (null (cddr form)) ) ))

  (if (and (one-argument-form-p type 'cons)
           (or (one-argument-form-p (second type) 'eql)
               (one-argument-form-p (second type) 'member) )
           (symbolp (second (second type))) )
      (let ((name    (second (second type)))
            (entries (pp-dispatch-table-cons-entries table)) )
        (if (not function)
            (remhash name entries)
          (setf (gethash name entries)
                (list name priority function) )) )
    (progn
      (setf (pp-dispatch-table-entries table)
            (delete type (pp-dispatch-table-entries table)
                    :test #'equal
                    :key  #'first ))

      (when function
        (let ((prev nil)
              (scan (pp-dispatch-table-entries table)) )

          (loop
            (when (null scan) (return))
            (let ((present (first scan)))
              (when (subtypep type (first present)) (return))
              (when (not (second present))          (return))
              (when (< (second present) priority)   (return))
              (setq prev scan)
              (setq scan (rest scan)) ))

          (let ((entry (list type priority function)))
            (if (not prev)
                (setf (pp-dispatch-table-entries table) (cons entry scan))
              (setf (cdr prev) (cons entry scan)) ) ) ))))
  nil ) )
