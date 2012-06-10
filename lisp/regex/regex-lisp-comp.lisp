;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - lisp-code compiler
;;; lisp/regex/regex-lisp-comp.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-lisp-comp.lisp#2 $
;;;
;;; Description:
;;;  This file contains regex lisp-code compiler.
;
(in-package :si)

;;;; regex-lisp-compile
;;;
;;; Description:
;;;  Compiles regular expression into lisp form and returns regex object.
;
(defun regex-lisp-compile (source &rest options)
  (multiple-value-bind (expr name-vector from-end modifiers)
      (apply #'parse-regex source options)

  (multiple-value-bind (scanner-form min-len flags hint)
      (regex-lisp-compile-main expr from-end)

  (multiple-value-bind (scanner-fn warnings-p failure-p)
          (compile nil scanner-form)
          #+nil (declare (ignore warnings-p))
    (when (or warnings-p failure-p)
      (error "Regex compiler generates malformed code.") )

    (make-instance 'regex
        :function    scanner-fn
        :min-length  min-len
        :flags       flags
        :scan-hint   hint
        :name-vector name-vector
        :source      source
        :modifiers   modifiers ) ) ) ) )


;;;; regex-lisp-compile-inline
;
(defun regex-lisp-compile-inline (source options)
  (remf options :start)
  (remf options :end)
  (multiple-value-bind (expr name-vector from-end modifiers)
      (apply #'parse-regex source options)
      (declare (ignore modifiers))
  (multiple-value-bind (scanner-form min-len flags hint)
      (multiple-value-bind (flags hint)
          (regex-study-scan expr from-end)
        (regex-lisp-compile-scanner expr flags hint) )
      (declare (ignore min-len flags hint))
  (multiple-value-bind (matcher-form max-local-index)
      (regex-lisp-compile-matcher expr from-end nil)
  (let ((capture-count (1+ (length name-vector))))
    `(lambda (string string-start string-end)
        (declare (lambda-name regex-inline-match))
        (declare (type string string-start string-end))
        (declare (values (or sequence-index null)))
      (multiple-value-bind (string string-start string-end)
          (string-data string string-start string-end)
          (declare (type simple-string string))
          (declare (type sequence-index string-start string-end))
        ,(regex-lisp-compile-aux
            scanner-form
            matcher-form
            from-end
            max-local-index
            'string-start
            'string-end
            'string
            (if from-end 'string-end 'string-start)
            `(make-array ,capture-count :initial-element nil)
            `(make-array ,capture-count :initial-element nil) ) ) ) ) ) ) ) )


;;;; regex-lisp-compile-main
;
(defun regex-lisp-compile-main (expr from-end)
  (multiple-value-bind (scanner-form min-len flags hint)
      (multiple-value-bind (flags hint)
          (regex-study-scan expr from-end)
        (regex-lisp-compile-scanner expr flags hint) )

    (when *regex-debug* (format t "~&; Scanner:~%~:W~%" scanner-form))

  (multiple-value-bind (matcher-form max-local-index)
      (regex-lisp-compile-matcher expr from-end 'match)

    (when *regex-debug* (format t "~&; Matcher:~%~:W~%" matcher-form))

  (let ((form
         `(lambda (match)
            (declare (lambda-name regex-function))
            (declare (type regex-match match))
            (declare (values t))
          ,(regex-lisp-compile-aux
                scanner-form
                matcher-form
                from-end
                max-local-index
                '(slot-value match 'start)
                '(slot-value match 'end)
                '(slot-value match 'string)
                '(slot-value match 'position)
                '(slot-value match 'start-vector)
                '(slot-value match 'end-vector) ) ) ))
    (values form min-len flags hint) ) ) ) )


;;;; regex-lisp-compile-aux
;
(defun regex-lisp-compile-aux (
        scanner-form
        matcher-form
        from-end
        max-local-index
        string-start
        string-end
        string
        position
        start-vector
        end-vector )
 `(let* ((string-start ,string-start)
         (string-end   ,string-end)
         (string       ,string)
         (scan-start   ,position)

         (start-vec    ,start-vector)
         (end-vec      ,end-vector)

         (loop-limit-pos
           ,(if from-end
                '(1- string-start)
                '(1+ string-end) ))
         (cstack
           (let ((context (regex-get-context)))
             (slot-value context 'cstack) ) ))

        (declare (type simple-string string))
        (declare (type sequence-index string-start string-end scan-start))
        (declare (type sequence-index loop-limit-pos))
        (declare (type simple-vector start-vec end-vec))
        (declare (ignorable cstack loop-limit-pos))
      (labels (
        (execute (pos)
            (declare (type sequence-index pos))
          ,matcher-form )
        )
        (fill start-vec nil)
        (fill end-vec   nil)

        (when (<= string-start scan-start string-end)
          (when (< (length cstack) ,max-local-index)
            (setq cstack (make-array ,(1+ max-local-index))) )

           ,scanner-form ) ) ) )


;;;; regex-lisp-compile-matcher
;;;
;;; Description:
;;;  Compiles regex parse tree into lisp-code and returns regex object.
;;;
;;; fdef*
;;;  Contains list of (name lambda-list form*).
;;;
;;; need-boundary-p
;;;  True when expr contains :BOUNDARY expression.
;
(defun regex-lisp-compile-matcher (expr from-end match)
    (declare (type regex-expr expr))
    (declare (type t from-end))
    (declare (values form sequence-index))
  (let* ((backward-p        from-end)

         (local-index       0)
         (max-local-index   0)

         (loop-depth        0)
         #+nil (max-loop-depth 0)

         (need-boundary-p   nil)
         (need-capture.b-p  nil)
         (need-capture.f-p  nil)

         (form-counter      0)

         (fetch-form        '(schar string pos))

         (fdef*             '()) )
    (declare (type sequence-index loop-depth))
    (declare (type sequence-index local-index))

  (macrolet (
    (with-stack-slots ((&rest spec*) &rest decl*-form*)
     `(let ((local-index/save local-index)
            ,@(loop
                for spec in spec*
                for bind =
                  (etypecase spec
                    (symbol
                      `(,spec (incf local-index)) )
                    (cons
                      (destructuring-bind (name cond) spec
                        `(,name (when ,cond (incf local-index))) )))
                  collect bind ) )
         (setq max-local-index (max max-local-index local-index))
         (prog1
             (locally ,@decl*-form*)
           (setq local-index local-index/save) ) ) )
    )
  (labels (
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Tree Walkers
    ;;

    ;; compile-1
    (compile-1 (expr &optional rest* (min-rest 0))
      `(progn ,@(compile-1* expr rest* min-rest)) )

    ;; compile-1*
    (compile-1* (expr &optional rest* (min-rest 0))
      (let ((form-id (incf form-counter))
            (form (compile-1-aux expr rest* min-rest)) )
        (if (and (consp form) (eq (first form) 'progn))
            `((DEBUG-TRACE ,form-id pos ,expr) ,@(rest form))
            `((DEBUG-TRACE ,form-id pos ,expr) ,form) ) ) )

    ;; compile-1-aux
    (compile-1-aux (expr &optional rest* (min-rest 0))
        (declare (type t expr))
        (declare (type sequence-index min-rest))
        (declare (type list rest*))
        (declare (values form))

      (REGEX-DEBUG "; compile-1: min-rest=~D ~S~%" min-rest expr)

      (etypecase expr
        (character
          (compile-char-test expr rest*) )
        (fixnum
          (compile-char-test expr rest*) )
        (string
          (compile-string 'string= expr rest*) )
        (symbol
          (compile-symbol expr rest*) )
        (cons
          (ecase (first expr)
            ((=)
              (compile-ref 'string= (second expr) rest*) )
            ((and)
              (compile-and (rest expr) rest* min-rest) )
            ((atom)
              (compile-atom (second expr) rest* min-rest) )
            ((bit-vector)
              (compile-char-test expr rest*) )
            ((capture)
              (destructuring-bind (nth subexpr) (rest expr)
                (compile-capture nth subexpr rest* min-rest) ) )
            ((category)
              (compile-char-test expr rest*) )
            ((equal)
              (compile-ref 'string-equal (second expr) rest*) )
            ((if)
              (destructuring-bind (cond then else) (rest expr)
                (compile-if cond then else rest* min-rest) ) )
            ((min)
              (destructuring-bind (min max subexpr) (rest expr)
                (compile-min min max subexpr rest* min-rest) ) )
            ((max)
              (destructuring-bind (min max subexpr) (rest expr)
                (compile-max min max subexpr rest* min-rest) ) )
            ((not)
              (compile-char-test expr rest*) )
            ((or)
              (compile-or (rest expr) rest* min-rest) )
            ((:range)
              (compile-char-test expr rest*) )
            ((reverse)
              (compile-reverse (second expr) rest* min-rest) )
            ((string-equal)
              (compile-string 'string-equal (second expr) rest*) )
            ((union)
              (compile-char-test expr rest*) )
            ((unless)
              (compile-unless (second expr) rest*) )
            ((when)
              (compile-when (second expr) rest*) )
            ) ) ) ); ecase cons

    ;; compile-rest*-to-fun
    (compile-rest*-to-fun (rest*)
      (when rest*
        (let ((fname (gensym "rest*_")))
          (push `(,fname (pos) ,@rest*) fdef*)
          `(,fname pos) )) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; For each node
    ;;

    ;; compile-and
    (compile-and (subexprs rest* min-rest)
      (loop
        with submin-rest =
          (+ (loop for subexpr in subexprs sum (compute-min-len subexpr))
             min-rest )
        for subexpr = (pop subexprs)
        while subexpr do
          (decf submin-rest (compute-min-len subexpr))

          (unless (cont-next-p subexpr)
            (when subexprs
              (let ((rest*-form (compile-and subexprs rest* min-rest)))
                (setq rest* (list rest*-form)) ))

            (let ((form (compile-1 subexpr rest* submin-rest)))
              (when before* (setq form `(and ,@before* ,form)))
              (return form) ) )

          collect (compile-1 subexpr nil  submin-rest) into before*
        finally
          (cond
            ((and before* rest*)
              (return `(and ,@before* ,(compose-single 'progn rest*))) )
            (before*
              (return (compose-single 'and before*)) )
            (rest*
              (return (compose-single 'progn rest*)) )
            (t
              (return 'pos) ))) )

    ;; compile-atom
    ;;  Compiles subexpr without continuation then call continuation.
    (compile-atom (subexpr rest* min-rest)
        (declare (ignore min-rest))
      `(progn
         (setq pos ,(compile-1 subexpr nil 0))
        ,(if (null rest*)
             'pos
           `(when pos ,@rest*) ) ) )

    ;; compile-capture
    (compile-capture (nth subexpr rest* min-rest)
      (with-stack-slots (idx)
        (if backward-p
            (progn
              (setq need-capture.b-p t)
              (setq rest*
                `((when pos (set-capture.b ,nth ,idx pos) ,@rest*)) ))
            (progn
              (setq need-capture.f-p t)
              (setq rest*
                `((when pos (set-capture.f ,nth ,idx pos) ,@rest*)) )))

       `(let ((start/save (svref start-vec ,nth))
              (end/save   (svref end-vec   ,nth))
              (tmp/save   (svref cstack    ,idx)) )
            (setf (svref cstack ,idx) pos)
            (or ,(compile-1 subexpr rest* min-rest)
                (progn
                  (REGEX-DEBUG ";~4T Restore capture[~D]~%" ,nth)
                  (setf (svref start-vec ,nth) start/save)
                  (setf (svref end-vec   ,nth) end/save)
                  (setf (svref cstack    ,idx) tmp/save)
                  nil )) ) ) )

    ;; compile-char-test
    (compile-char-test (expr rest*)
      (let ((form (compose-char-test expr)))
          (assert form)
        (ecase (regex-study-length expr)
          ((0) (compose-test form rest*))
          ((1) (compose-advance-1 form rest*)) ) ) )

    ;; compile-if
    (compile-if (cond then else rest* min-rest)
      (let ((test-form
              (ecase (first cond)
                ((boundp)
                  `(match-start match ,(second cond)) )
                ((unless when)
                  `(let ((pos pos)) ,(compile-1 cond)) )) ))
        (if (null rest*)
           `(if ,test-form
                   ,(compile-1 then)
                   ,(compile-1 else) )
          (let* ((rest*-forms `((setq pos ,(compile-rest*-to-fun rest*)))))
           `(if ,test-form
                ,(compile-1 then rest*-forms min-rest)
                ,(compile-1 else rest*-forms min-rest) ) )) ) )

    ;; compile-max
    (compile-max (m n subexpr rest* min-rest)
        (declare (type fixnum m n))
        (declare (type t subexpr))
        (declare (values form))
      (cond
        ((or (and (eql m 0) (eql n 0)) (eq subexpr :void))
          (compose-single 'progn rest*) )
        ((and (eql m 0) (eql n 1))
          (compile-max/0-1 subexpr rest* min-rest) )
        ((compile-max/simple m n subexpr rest* min-rest))
        ((eql m n)
          (compile-max/n m subexpr rest* min-rest) )
        ((compile-max/capture m n subexpr rest* min-rest))
        (t
          (compile-max/m-n m n subexpr rest* min-rest) )) )

    ;; compile-max/0-1
    ;;  Branch[1] = Try subexpr, Try rest
    ;;  Branch[2] = Try rest
    (compile-max/0-1 (subexpr rest* min-rest)
      (let ((rest*-form (compile-rest*-to-fun rest*)))
        (if (null rest*-form)
           `(let ((pos/save pos))
              (or ,(compile-1 subexpr nil min-rest)
                  (setq pos pos/save)) )
         `(let ((pos/save pos))
            (or
              ,(compile-1 subexpr `((setq pos ,rest*-form)) min-rest)
              (progn
                (setq pos pos/save)
                (setq pos ,rest*-form) )) )) ) )

    ;; compile-max/capture
    ;;   Factor capturing out when r is deterministic.
    ;;      (r)*     => (?:r*(r))?
    ;;      (r)+     => (?:r*(r))
    ;;      (r){n}   => (?:r{n-1}(r))
    ;;      (r){0,n} => (?:r{0,n-1}(r))?
    ;;      (r){m,n} => (?:r{m-1,n-1}(r))
    (compile-max/capture (m n subexpr rest* min-rest)
        (assert (>= n m))
      (unless (and (consp subexpr) (eq (first subexpr) 'capture))
        (return-from compile-max/capture nil) )

      (let ((subsubexpr (third subexpr)))
        (multiple-value-bind (min max cap-p det-p)
            (regex-study-length subsubexpr)
            (declare (ignore cap-p))
          (unless (and det-p (eql min max))
            (return-from compile-max/capture nil) )

          (unless (eql n regex-infinity)
            (decf n) )

          (let ((expr `(and (max ,(max (1- m) 0) ,n ,subsubexpr)
                            ,subexpr ) ))
            (when (eql m 0) (setq expr `(max 0 1 ,expr)))
            (compile-1 expr rest* min-rest) ) ) ) )

    ;; compile-max/m-n
    (compile-max/m-n (m n subexpr rest* min-rest)
      (prog2
        (incf loop-depth)
        (compile-max/m-n-aux m n subexpr rest* min-rest)
        (decf loop-depth) ) )

    ;; compile-max/m-n-aux
    (compile-max/m-n-aux (m n subexpr rest* min-rest)
        (declare (type fixnum m n))
        (declare (type t subexpr))
        (declare (type list rest*))
        (declare (type sequence-index min-rest))
        (declare (values form))
      (with-stack-slots (
            (counter-index (not (and (zerop m) (eql n regex-infinity))))
            (pos-index (zerop (compute-min-len subexpr))) )
      (let ((loop-fname (make-symbol (format nil "max_~D" form-counter)))
            (match-rest nil) )
        (push `(,loop-fname (pos)
         (prog (,@(when counter-index
                    `((counter (svref cstack ,counter-index))) )
                 ,@(when pos-index
                    `((last-loop-pos (svref cstack ,pos-index))) ))

            ,(if counter-index
               `(DEBUG-TRACE ',loop-fname pos counter)
               `(DEBUG-TRACE ',loop-fname pos nil) )

            ,@(compose-check-min-rest loop-fname min-rest)

            ,@(when pos-index
                (setq match-rest '(match-rest))
              `((when (eql last-loop-pos pos)
                 ,(if counter-index
                     `(REGEX-DEBUG ";~4T ~S[~D]: Empty match => No more loop~%"
                        ',loop-fname
                        counter )
                     `(REGEX-DEBUG ";~4T ~S: Empty match => No more loop~%"
                        ',loop-fname ))
                  (go match-rest) )))

            ,@(unless (eql n regex-infinity)
                (setq match-rest '(match-rest))
                `((when (eql counter ,n)
                    (REGEX-DEBUG ";~4T ~S[~D]: No more loop~%"
                      ',loop-fname
                      ,n )
                    (go match-rest) )) )

            ,@(when counter-index
                `((setf (svref cstack ,counter-index) (1+ counter))) )

            ,@(when pos-index
               `((setf (svref cstack ,pos-index) pos)) )

            (let ((pos/save pos))
              (setq pos ,(compile-1 subexpr 
                                    `((setq pos (,loop-fname pos)))
                                    min-rest ))
              (when pos
                ,(if counter-index
                    `(REGEX-DEBUG
                          ";~4T ~S[~D]: Subexpr and rest* are matched.~%"
                          ',loop-fname
                          counter )
                    `(REGEX-DEBUG
                          ";~4T ~S: Subexpr and rest* are matched.~%"
                          ',loop-fname ))
                (return pos) )

              (setq pos pos/save) )

            ,@(when counter-index
                `((REGEX-DEBUG ";*** backtrack: ~S: restore counter[~D]=~D to ~D~%"
                    ',loop-fname
                    ,counter-index
                    (svref cstack ,counter-index)
                    counter )
                  (setf (svref cstack ,counter-index) counter) ))

            ,@(when pos-index
                `((REGEX-DEBUG ";*** backtrack: ~S: restore loop-start[~D] ~D to ~D~%"
                    ',loop-fname
                    ,pos-index
                    (svref cstack ,pos-index)
                    last-loop-pos )
                  (setf (svref cstack ,pos-index) last-loop-pos)) )

            ,@(cond
                ((eql m 0)
                  `((REGEX-DEBUG ";*** backtrack: ~S: continue~%"
                        ',loop-fname )) )
                ((eql m n)
                  `((return nil)) )
                (counter-index
                  `((when (< counter ,m) (return nil))) )
                (t
                  (REGEX-INTERNAL-ERROR "Bad loop") ))

          ,@match-rest
            (setq pos ,(compose-single 'progn rest*))

           ,(if counter-index
               `(REGEX-DEBUG ";~4T ~S[~D]: rest* pos=~D~%"
                    ',loop-fname
                    counter
                    pos )
               `(REGEX-DEBUG ";~4T ~S: rest* pos=~D~%"
                    ',loop-fname
                    pos ))

            (return pos) )) fdef* )

       ;; Greedy repetition form
       `(let (
          ,@(when counter-index
              `((counter/save (shiftf (svref cstack ,counter-index) 0))) )
          ,@(when pos-index
              `((pos/save (shiftf (svref cstack ,pos-index) -1))) ) )
            (setq pos (,loop-fname pos))
            ,@(when counter-index
                `((setf (svref cstack ,counter-index) counter/save)) )
            ,@(when pos-index
                `((setf (svref cstack ,pos-index) pos/save)) )
            pos ) ) ) )

    ;; compile-max/n
    ;;  (r){n} => (?:r{n-1}(r))
    (compile-max/n (n subexpr rest* min-rest)
      (case n
        ((0) (compose-single 'progn rest*))
        ((1) (compile-1 subexpr rest* min-rest))
        (otherwise
          (if (not (and (consp subexpr) (eq (first subexpr) 'capture)))
              (compile-max/m-n n n subexpr rest* min-rest)
            (let ((expr
                    (let ((n (1- n))
                          (subsubexpr (third subexpr)) )
                     (if (eql n 1)
                         `(and ,subsubexpr ,subexpr)
                       `(and (max ,n ,n ,subsubexpr) ,subexpr) ) ) ))
              (compile-1 expr rest* min-rest) )) )) )

    ;; compile-max/simple
    ;;  Compiles into tight loop when subexpr is deterministic and
    ;;  fixed-length.
    ;;
    ;;  Note: Regex /foo|bar/ is fixed-length, but isn't deterministic.
    ;;
    (compile-max/simple (m n subexpr rest* min-rest)
      (cond
        ((eq subexpr :any)
          (compile-max/simple/any m n rest* min-rest) )
        ((stringp subexpr)
          (let* ((pattern subexpr)
                 (patlen     (length pattern)) )
            (compile-max/simple/fixed m n rest* min-rest
                `(string= string ,pattern :start1 pos :end1 (+ pos ,patlen))
                patlen ) ) )
        ((and (consp subexpr) (eq (first subexpr) 'string-equal))
          (let* ((pattern (second subexpr))
                 (patlen  (length pattern)))
            (compile-max/simple/fixed m n rest* min-rest
                `(string-equal string ,pattern
                        :start1 pos
                        :end1   (+ pos ,patlen) )
                patlen ) ) )
        (t
          (let ((form (compose-char-test subexpr)))
            (when (and form (eql (regex-study-length subexpr) 1))
              (compile-max/simple/fixed m n rest* min-rest form 1) ) ) )) )

    ;; compile-max/simple/any
    ;;  Tries rest* from pos+n to pos+m.
    (compile-max/simple/any (m n rest* min-rest)
      (cond
        ((eql m n)
          (compile-max/simple/any/n n rest* min-rest) )
        ((and (eql m 0) (eql n 1))
          (compile-max/simple/any/0-1 rest* min-rest) )
        (t
          (compile-max/simple/any/m-n m n rest* min-rest) )) )

    ;; compile-max/simple/any/0-1
    (compile-max/simple/any/0-1 (rest* min-rest)
      (cond
        ((and backward-p rest*)
           `(loop
              with end-pos = (max (1- pos) (+ string-start ,min-rest))
              with start-pos = pos
              for loop-pos from end-pos to start-pos do
                (setq pos loop-pos)
                (setq pos ,(compose-single 'progn rest*))
                (when pos (return pos)) ) )
        (backward-p
           `(let ((end-pos (max (1- pos) (+ string-start ,min-rest))))
              (when (>= pos end-pos)
                (setq pos end-pos) ) ) )
        (rest*
           `(loop
              with end-pos = (min (1+ pos) (- string-end ,min-rest))
              with start-pos = pos
              for loop-pos from pos downto start-pos do
                (setq pos loop-pos)
                (setq pos ,(compose-single 'progn rest*))
                (when pos (return pos)) ) )
        (t
           `(let ((end-pos (min (1+ pos) (- string-end ,min-rest))))
              (when (<= pos end-pos)
                (setq pos end-pos) ) ) )) )

    ;; compile-max/simple/any/n
    (compile-max/simple/any/n (n rest* min-rest)
      (if backward-p
           `(let ((next-pos (- pos ,n)))
              (when (>= next-pos (+ string-start ,min-rest))
                (setq pos next-pos)
                ,@rest* ) )
           `(let ((next-pos (+ pos ,n)))
              (when (<= next-pos (- string-end ,min-rest))
                (setq pos next-pos)
                ,@rest* ) )) )

    ;; compile-max/simple/any/m-n
    ;;  Tries rest* from pos+max to pos+min.
    (compile-max/simple/any/m-n (m n rest* min-rest)
      (multiple-value-bind (cmp advance loop-pos end-pos)
          (cond
            ((and backward-p (eql n regex-infinity))
              (values '<=
                      '1+
                      `(+ string-start ,min-rest)
                      `(- pos ,m) ) )
            (backward-p
              (values '<=
                      '1+
                      `(n (- pos ,n) (+ string-start ,min-rest))
                      `(- pos ,m) ) )
            ((eql n regex-infinity)
              (values '>=
                      '1-
                      `(- string-end ,min-rest)
                      `(+ pos ,m) ) )
            (t
              (values '>=
                      '1-
                      `(m (+ pos ,n) (- string-end ,min-rest))
                      `(+ pos ,m) ) ))
        (cond
          ((and (null rest*) (eql m 0))
           `(setq pos ,loop-pos) )
          ((null rest*)
           `(let ((next-pos ,loop-pos))
              (when (,cmp next-pos ,end-pos)
                (setq pos next-pos) ) ) )
          (t
           `(loop
              with end-pos   = ,end-pos
              for loop-pos   = ,loop-pos then (,advance loop-pos)
              while (,cmp loop-pos end-pos) do
                (setq pos loop-pos)
                (DEBUG-TRACE ,form-counter pos start-pos)
                (when ,(compose-single 'progn rest*)
                  (return pos) )) )) ) )

    ;; compile-max/simple/fixed
    ;;  Tries rest* from pos+n to pos+m.
    (compile-max/simple/fixed (m n rest* min-rest cmp-form delta)
      (cond
        ((eql m n)
           (compile-max/simple/fixed/n m rest* min-rest cmp-form delta) )
        ((and backward-p (eql m 0))
           `(block repeat
              ,(compile-max/simple/fixed/backward/max
                    n rest* min-rest cmp-form delta )) )
        (backward-p
           `(block repeat
              ,(compile-max/simple/fixed/backward/min
                    m min-rest cmp-form delta )
              ,(compile-max/simple/fixed/backward/max
                    (if (eql n regex-infinity) n (- n m))
                    rest* min-rest cmp-form delta )) )
        ((eql m 0)
           `(block repeat
              ,(compile-max/simple/fixed/forward/max
                    n rest* min-rest cmp-form delta )) )
        (t
           `(block repeat
              ,(compile-max/simple/fixed/forward/min
                    m min-rest cmp-form delta )
              ,(compile-max/simple/fixed/forward/max
                    (if (eql n regex-infinity) n (- n m))
                    rest* min-rest cmp-form delta )) )) )

    ;; compile-max/simple/fixed/n
    (compile-max/simple/fixed/n (n rest* min-rest cmp-form delta)
      (let ((form (compile-max/simple/fixed/n-aux n min-rest cmp-form delta)))
        (if rest*
            `(when ,form ,@rest*)
          form ) ) )

    ;; compile-max/simple/fixed/n-aux
    (compile-max/simple/fixed/n-aux (n min-rest cmp-form delta)
        (assert (plusp n))
      (cond
        ((and backward-p (<= n 5))
         `(let ((next-pos (- pos ,(* n delta))))
            (when (>= next-pos (+ string-start ,min-rest))
              (block nil
                ,@(loop repeat n
                    collect `(decf pos ,delta)
                    collect `(unless ,cmp-form (return nil)) ))) ) )
        (backward-p
         `(let ((next-pos (- pos ,(* n delta))))
            (when (>= next-pos (+ string-start ,min-rest))
              (loop repeat ,n do
                (decf pos ,delta)
                (unless ,cmp-form (return nil)) )) ) )
        ((<= n 5)
         `(let ((next-pos (+ pos ,(* n delta))))
            (when (<= next-pos (- string-end ,min-rest))
              (block nil
                ,@(loop repeat n
                    collect `(unless ,cmp-form (return nil))
                    collect `(incf pos ,delta) ))) ) )
        (t
         `(let ((next-pos (+ pos ,(* n delta))))
            (when (<= next-pos (- string-end ,min-rest))
              (loop repeat ,n do
                (unless ,cmp-form (return nil))
                (incf pos ,delta) )) ) )) )

    ;; compile-max/simple/fixed/backward/min
    ;;  Try to match min characters.
    (compile-max/simple/fixed/backward/min (min min-rest cmp-form delta)
        (assert (plusp min))
       `(let ((next-pos (- pos ,(* min delta))))
          (when (< next-pos (+ string-start ,min-rest))
            (return-from repeat nil) )
          (loop while (> pos next-pos) do
            (decf pos ,delta)
            (unless ,cmp-form (return-from repeat nil)) ) ) )

    ;; compile-max/simple/fixed/backward/max
    ;;  1. Try to match max characters.
    ;;  2. Down to start-pos until rest* returns true.
    (compile-max/simple/fixed/backward/max
        (max rest* min-rest cmp-form delta)
      (if (null rest*)
         `(loop
            with end-pos of-type sequence-index =
                ,(if (eql max regex-infinity)
                     `(+ string-start ,min-rest)
                     `(max (+ string-start ,min-rest)
                           (- pos ,(* max delta)) ))
            while (> pos end-pos) do
              (DEBUG-TRACE ,form-counter pos end-pos)
              (decf pos ,delta)
              (unless ,cmp-form (incf pos ,delta) (loop-finish))
            finally (return pos) )
         `(loop
            with start-pos of-type sequence-index = pos
            with end-pos   of-type sequence-index =
                ,(if (eql max regex-infinity)
                     `(+ string-start ,min-rest)
                     `(max (+ string-start ,min-rest)
                           (- pos ,(* max delta))) )
            while (> pos end-pos) do
              (decf pos ,delta)
              (unless ,cmp-form (incf pos ,delta) (loop-finish))
            finally
              (REGEX-DEBUG ";~4T Try rest* from ~D to ~D.~%" pos start-pos)
              (loop for loop-pos from pos to start-pos by ,delta do
                (setq pos loop-pos)
                (when ,(compose-single 'progn rest*)
                  (return-from repeat pos) )))) )

    ;; compile-max/simple/fixed/forward/min
    ;;  Try to match min characters.
    (compile-max/simple/fixed/forward/min (min min-rest cmp-form delta)
        (assert (plusp min))
       `(let ((next-pos (+ pos ,(* min delta))))
          (when (> next-pos (- string-end ,min-rest))
            (return-from repeat nil) )
          (loop while (< pos next-pos) do
            (unless ,cmp-form (return-from repeat nil))
            (incf pos ,delta) ) ) )

    ;; compile-max/simple/fixed/forward/max
    ;;  1. Try to match max characters.
    ;;  2. Down to start-pos until rest* returns true.
    (compile-max/simple/fixed/forward/max (max rest* min-rest cmp-form delta)
      (if (null rest*)
         `(loop
            with end-pos of-type sequence-index =
                ,(if (eql max regex-infinity)
                     `(- string-end ,min-rest)
                     `(min (- string-end ,min-rest) (+ pos ,(* max delta))) )
            while (< pos end-pos) do
              (unless ,cmp-form (loop-finish))
              (incf pos ,delta)
            finally (return pos) )
         `(loop
            with start-pos of-type sequence-index = pos
            with end-pos   of-type sequence-index =
                ,(if (eql max regex-infinity)
                     `(- string-end ,min-rest)
                     `(min (- string-end ,min-rest) (+ pos ,(* max delta))) )
            while (< pos end-pos) do
              (unless ,cmp-form (loop-finish))
              (incf pos ,delta)
            finally
              (REGEX-DEBUG ";~4T Try rest* from ~D to ~D.~%" pos start-pos)
              (loop for loop-pos from pos downto start-pos by ,delta do
                (setq pos loop-pos)
                (when ,(compose-single 'progn rest*)
                  (return-from repeat pos) )))) )

    ;; compile-min
    ;;  Note: a{3}? == a{3}
    (compile-min (m n subexpr rest* min-rest)
        (declare (type fixnum min max))
        (declare (type t subexpr))
        (declare (values form))
      (cond
        ((or (and (eql m 0) (eql n 0)) (eq subexpr :void))
          (compose-single 'progn rest*) )
        ((or (eql m n) (null rest*))
          (compile-max/n m subexpr rest* min-rest) )
        ((and (eql m 0) (eql n 1))
          (compile-min/0-1 subexpr rest* min-rest) )
        (t
          (compile-min/m-n m n subexpr rest* min-rest) )) )

    ;; compile-min/0-1
    ;;  Branch[1] - Try rest
    ;;  Branch[2] - Try subexpr, Try rest
    (compile-min/0-1 (subexpr rest* min-rest)
      (if (null rest*)
          'pos
        (let ((rest*-form (compile-rest*-to-fun rest*)))
         `(block nil
            (let ((pos/save pos))
              ;; Branch[1]
              (setq pos ,rest*-form)
              (when pos (return pos))

              ;; Branch[2]
              (setq pos pos/save)
              (setq pos ,(compile-1 subexpr nil min-rest))
              (when pos
                (setq pos ,rest*-form)
                (when pos (return pos)) )
              (return nil) )) )) )

    ;; compile-min/m-n
    ;;  repeat min times subexpr
    ;;  Branch[1] = rest
    ;;  Branch[2] = subexpr, rest
    ;;  Branch[3] = subexpr x 2, rest
    ;;  ...
    ;;  Branch[max-min] = subexpr x (max - min), rest
    ;;
    (compile-min/m-n (min max subexpr rest* min-rest)
        (assert rest*)
      (prog2
        (incf loop-depth)
        (compile-min/m-n-aux min max subexpr rest* min-rest)
        (decf loop-depth) ) )

    ;; compile-min/m-n-aux
    (compile-min/m-n-aux (m n subexpr rest* min-rest)
        (assert rest*)
      (with-stack-slots (
            (counter-index (not (and (zerop m) (eql n regex-infinity))))
            (pos-index (zerop (compute-min-len subexpr))) )
      (let* ((loop-fname (gensym "min_"))
             (loop-form* `((setq pos (,loop-fname pos))))
             (rest*-form (compile-rest*-to-fun rest*)) )

        (push `(,loop-fname (pos)
          (prog (,@(when counter-index
                      `((counter (svref cstack ,counter-index))) )
                   ,@(when pos-index
                      `((last-loop-pos (svref cstack ,pos-index))) ))

            ,(if counter-index
               `(DEBUG-TRACE ',loop-fname pos counter)
               `(DEBUG-TRACE ',loop-fname pos nil) )

            ,@(compose-check-min-rest loop-fname min-rest)

            ,@(when pos-index
               `((when (eql last-loop-pos pos)
                   (REGEX-DEBUG ";~4T ~S: Match empty. No more loop.~%"
                        ',loop-fname )
                   (return ,rest*-form) )))

            ,@(unless (eql n regex-infinity)
               `((when (eql counter ,n)
                   (REGEX-DEBUG ";~4T ~S: No more loop.~%"
                        ',loop-fname )
                   (return ,rest*-form) )) )

            ,@(cond
                ((eql m 0)
                 `((let ((next-pos ,rest*-form))
                    (when next-pos
                      (REGEX-DEBUG ";~4T Rest is matched.~%")
                      (return next-pos) ) )) )
                (counter-index
                  `((when (>= counter ,m)
                      (let ((next-pos ,rest*-form))
                        (when next-pos
                          (REGEX-DEBUG ";~4T ~S: Rest is matched.~%"
                            ',loop-fname )
                          (return next-pos) ) )
                      (REGEX-DEBUG ";~4T ~S: Loop more counter=~D~%"
                        ',loop-fname
                        counter ))) )
                (t (REGEX-INTERNAL-ERROR "Bad counter")) )

            ,@(when counter-index
                `((setf (svref cstack ,counter-index) (1+ counter))) )

           ,(if (and (null counter-index) (null pos-index))
                `(return ,(compile-1 subexpr loop-form* min-rest))
              `(let ((pos ,(compile-1 subexpr loop-form* min-rest)))
                  ,@(when counter-index
                      `((decf (svref cstack ,counter-index))) )
                  ,@(when pos-index
                      `((setf (svref cstack ,pos-index) last-loop-pos)) )
                  (return pos) )) )) fdef* )

       ;; Lazy repetition form
       `(let (
          ,@(when counter-index
              `((counter/save (shiftf (svref cstack ,counter-index) 0))) )
          ,@(when pos-index
              `((pos/save (shiftf (svref cstack ,pos-index) -1))) ) )
            (setq pos (,loop-fname pos))
            ,@(when counter-index
                `((setf (svref cstack ,counter-index) counter/save)) )
            ,@(when pos-index
                `((setf (svref cstack ,pos-index) pos/save)) )
            pos ) ) ) )

    ;; compile-min/simple
    ;; BUGBUG: We should specify when we can use this template.
    ;; (TEST-CASE "perl-evita/122" "^(b+?|a){1,2}?c" "bbbc" nil '("bbbc" "bb")
    ;; was failed.
    #+nil
    (compile-min/simple (m n subexpr rest* min-rest)
        (assert (not (eql m n)))
        (assert rest*)
      (multiple-value-bind (min max cap-p det-p)
          (regex-study-length subexpr)
          (declare (ignore cap-p))
        (when (and det-p (not (eql min 0)))
          (assert (eql min max))
          (let ((rest*-form (compile-rest*-to-fun rest*)))
           `(prog ((counter ,(- n m -1))
                   (pos 0) )
              ,@(when (eql m 0)
                  (list (compile-max/n m subexpr rest* min-rest)) )

                (go rest)

              loop
                (setq pos pos/save)
                (setq pos ,(compile-1 subexpr nil min-rest))
                (when (null pos) (return nil))

              rest
                (setq pos/save pos)
                (setq pos ,rest*-form)
                (when pos (return pos))

                (setq pos pos/save)
                (decf counter)
                (unless (eql counter 0) (go loop)) ) )) ) )

    ;; compile-or
    ;;  (or)        => nothing
    ;;  (or expr)   => expr
    ;;  (or char* expr char*)   => (or (union char*) expr (union char*))
    (compile-or (subexprs rest* min-rest)
      (cond
        ((null subexprs)
          (REGEX-INTERNAL-ERROR "Empty OR-expr") )
        ((null (rest subexprs))
          (compile-1 (first subexprs) rest* min-rest) )
        (t
          (loop
            with place = nil
            with union = nil
            with prev  = nil
            with all-p = t
            for runner on subexprs
            for subexpr = (first runner) do
              (cond
                ((not (or (char-expr-p subexpr) (char-class-expr-p subexpr)))
                  (setq place nil)
                  (setq prev  nil)
                  (setq all-p nil) )
                ((null place)
                  (setq place runner)
                  (setq prev  runner)
                  (setq union subexpr) )
                (t
                  (setf (rest prev) (rest runner))
                  (setq union `(union ,subexpr ,union))
                  (setf (first place) union) ))
            finally
              (let ((form
                      (if (and all-p union)
                          (compose-advance-1 (compose-union subexprs) rest*)
                        (compile-or-aux   subexprs rest* min-rest) ) ))
                (return form) )) )) )

    ;; compile-or-aux
    (compile-or-aux (subexprs rest* min-rest)
        (assert subexprs)
      (when rest*
        (setq rest* `((setq pos ,(compile-rest*-to-fun rest*)))) )
      (loop
        for subexpr in subexprs
        collect
          (if (eq subexpr :void)
              (compose-single 'progn rest*)
           `(let ((pos/save pos))
              (let ((pos-or-nil ,(compile-1 subexpr rest* min-rest)))
                (if pos-or-nil
                    (setq pos pos-or-nil)
                  (progn
                    (setq pos pos/save)
                    nil )) ) ) )
          into forms
        finally (return `(or ,@forms)) ) )

    ;; compile-ref
    (compile-ref (cmp-fn nth rest*)
      `(let ((pattern (match-string match ,nth)))
         (when pattern
          ,(if backward-p
              `(let ((next-pos (- pos (length pattern))))
                 (when (and (>= next-pos string-start)
                            (,cmp-fn string pattern
                                    :start1 next-pos :end1 pos ))
                   (setq pos next-pos)
                   ,@rest* ) )
              `(let ((next-pos (+ pos (length pattern))))
                 (when (and (<= next-pos string-end)
                            (,cmp-fn string pattern
                                  :start1 pos :end1 next-pos) )
                   (setq pos next-pos)
                   ,@rest* ) ))) ) )

    ;; compile-reverse
    (compile-reverse (subexpr rest* min-rest)
      (setq backward-p (not from-end))
      (prog1
          (compile-1 subexpr rest* min-rest)
        (setq backward-p from-end) ) )

    ;; compile-string
    (compile-string (cmp-fn pattern rest*)
        (declare (type string string))
        (declare (values form))
      (if backward-p
          `(let ((next-pos (- pos ,(length pattern))))
             (when (and (>= next-pos string-start)
                        (,cmp-fn string ,pattern :start1 next-pos :end1 pos) )
               (setq pos next-pos)
               ,@rest* ) )
        `(let ((next-pos (+ pos ,(length pattern))))
           (when (and (<= next-pos string-end)
                      (,cmp-fn string ,pattern :start1 pos :end1 next-pos) )
             (setq pos next-pos)
             ,@rest* ) )) )

    ;; compile-symbol
    (compile-symbol (name rest*)
      (ecase name
        ((:any)
          (if backward-p
              `(when (> pos string-start) (decf pos) ,@rest*)
              `(when (< pos string-end)   (incf pos) ,@rest*) ) )
        ((:bos)
          (compose-test '(eql pos string-start) rest*) )
        ((:boundary)
          (setq need-boundary-p t)
          (compose-test '(boundary-p pos) rest*) )
        ((:eos)
          (compose-test '(eql pos string-end) rest*) )
        ((:mbol)
          (compose-test '(or (eql pos string-start)
                          (and (not (eql pos string-end))
                                (eql (schar string (1- pos)) #\Newline) ))
                     rest* ) )
        ((:meol)
          (assert (not backward-p))
          (compose-test '(or (eql pos string-end)
                          (eql (schar string pos) #\Newline) )
                     rest* ) )
        ((:pos)
          (compose-test '(eql pos scan-start) rest*) )
        ((:seol)
          (assert (not backward-p))
          (compose-test '(or (eql pos string-end)
                          (and (eql (1+ pos) string-end)
                               (eql (schar string pos) #\Newline) ))
                      rest* ) )
        ((:void)
          (compose-single 'progn rest*) ) ) )

    ;; compile-unless
    ;;  Localizes POS.
    (compile-unless (subexpr rest*)
      (compose-test `(let ((pos pos)) (not ,(compile-1 subexpr))) rest*) )

    ;; compile-when
    ;;  Localizes POS.
    (compile-when (subexpr rest*)
      (compose-test `(let ((pos pos)) ,(compile-1 subexpr)) rest*) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Composeters
    ;;

    ;; compose-check-min-rest
    (compose-check-min-rest (loop-fname min-rest)
      (when (eql min-rest 0) (return-from compose-check-min-rest nil))
      (cond
        ((and backward-p (eql loop-depth 1))
          `((let ((next-pos (- pos ,min-rest)))
              (cond
                ((< next-pos string-start)
                  (return nil) )
                ((< next-pos loop-limit-pos)
                  (REGEX-DEBUG ";~4T ~S: Already checked=~D<=~D~%~40~~%"
                      ',loop-fname
                      next-pos
                      loop-limit-pos )
                  (return nil) )
                ((eql (1- next-pos) loop-limit-pos)
                  (REGEX-DEBUG ";~4T ~S: Update loop-limit to ~D=~D+~D~%"
                      ',loop-fname
                      next-pos pos ,min-rest ) 
                  (setq loop-limit-pos next-pos) )) )) )
        (backward-p
         `((let ((next-pos (- pos ,min-rest)))
             (unless (and (>= next-pos string-start)
                          (>  next-pos loop-limit-pos) )
                (return nil) ) )) )
        ((eql loop-depth 1)
          `((let ((next-pos (+ pos ,min-rest)))
              (REGEX-DEBUG ";~30T pos=~D ~S LAST min-rest=~D~%"
                pos ',loop-fname ,min-rest )
              (cond
                ((> next-pos string-end)
                  (return nil) )
                ((> next-pos loop-limit-pos)
                  (REGEX-DEBUG ";~4T ~S: Already checked=~D>=~D~%~40~~%"
                      ',loop-fname
                      next-pos
                      loop-limit-pos )
                  (return nil) )
                ((eql (1+ next-pos) loop-limit-pos)
                  (REGEX-DEBUG ";~4T ~S: Update loop-limit to ~D=~D+~D~%"
                      ',loop-fname
                      next-pos pos ,min-rest ) 
                  (setq loop-limit-pos next-pos) )) )) )
        (t
         `((let ((next-pos (+ pos ,min-rest)))
             (REGEX-DEBUG ";~30T pos=~D ~S REST min-rest=~D~%"
                pos ',loop-fname ,min-rest )
             (unless (<= next-pos string-end)
                (return nil) )
             (unless (<= next-pos loop-limit-pos)
                (return nil) ) )) )) )

    ;; compose-advance-1
    (compose-advance-1 (cmp-form rest*)
      ;; BUGBUG: Should study when we can remove pos range check.
      (if backward-p
          (setq cmp-form `(and (>= pos string-start) ,cmp-form))
          (setq cmp-form `(and (<  pos string-end)   ,cmp-form)) )
      (cond
        ((and backward-p rest*)
          `(progn (decf pos) (when ,cmp-form ,@rest*)) )
        (backward-p
          `(progn (decf pos) (when ,cmp-form pos)) )
        (rest*
          `(when ,cmp-form (incf pos) ,@rest*) )
        (t
          `(when ,cmp-form (incf pos)) )) )

    ;; compose-char-test
    (compose-char-test (expr)
      (typecase expr
        (character
          `(char= ,fetch-form ,expr) )
        (fixnum
          (let ((char (code-char expr)))
            (if (both-case-p char)
                `(char-equal ,fetch-form ,char)
              `(char= ,fetch-form ,char) ) ) )
        (symbol
          (ecase expr
            ((:any)
              '(setq pos pos) )
            ((:boundary)
              (setq need-boundary-p t)
              '(boundary-p pos) )) )
        (cons
          (case (first expr)
            ((bit-vector)
              (destructuring-bind (op min-code bitvec) (rest expr)
                `(let* ((bv    ,bitvec)
                        (char  ,fetch-form)
                        (code  (char-code char))
                        (index (- code ,min-code)) )
                    (and (<= 0 index ,(1- (length bitvec)))
                         (,op (sbit bv index) 1) ) ) ) )
            ((category)
              (destructuring-bind (min-cat &optional max-cat) (rest expr)
                (if max-cat
                    `(<= ,min-cat
                         (char-category (schar string pos))
                         ,max-cat )
                  `(eql (char-category (schar string pos))
                        ,min-cat )) ) )
            ((not)
              (let ((form (compose-char-test (second expr))))
                  (assert form)
                (case (first form)
                  ((char=)          (setf (first form) 'char/=))
                  ((char/=)         (setf (first form) 'char=))
                  ((char-equal)     (setf (first form) 'char-not-equal))
                  ((char-not-equal) (setf (first form) 'char-equal))
                  ((not)            (setq form (second form)))
                  (otherwise        (setq form `(not ,form))) )
                form ) )
            ((:range)
              (destructuring-bind (min-char max-char) (rest expr)
                (compose-range min-char max-char) ))
            ((union)
              (compose-union (rest expr)) )) )) )

    ;; compose-range
    (compose-range (min max)
        (declare (type (or character character-code) min max))
      (cond
        ((eql min max)
          (compose-char-test min) )
        ((and (characterp min) (characterp max))
          `(char<= ,min ,fetch-form ,max) )
        ((and (integerp min) (integerp max))
          (compose-union `((:range ,min ,max))) )
        (t
          (REGEX-INTERNAL-ERROR "Bad range: ~S" expr) )) )

    ;; compose-union
    (compose-union (subexprs)
      (multiple-value-bind (positives negatives) (regex-study-union subexprs)
        (cond
          ((find :any negatives)
            nil )
          ((find :any positives)
            t )
          (t
            (let ((p-forms
                    (loop for subexpr in positives
                      collect (compose-char-test subexpr) ) )
                  (n-forms
                    (loop for subexpr in negatives
                      collect (compose-char-test `(not ,subexpr)) ) ))
              (cond
                ((and positives negatives)
                  `(and (or ,@p-forms) ,@n-forms) )
                (positives
                  (compose-single 'or p-forms) )
                (negatives
                  (compose-single 'and n-forms) )
                (t
                  (REGEX-INTERNAL-ERROR "Invalid union expr: ~S"
                    `(union ,@subexprs) ) )) ) )) ) )

    ;; compose-single
    (compose-single (op form*)
      (cond
        ((null form*) 'pos)
        ((rest form*) `(,op ,@form*))
        (t (first form*)) ) )

    ;; compose-test
    (compose-test (test-form rest*)
      (if (null rest*)
          `(when ,test-form pos)
        `(when ,test-form ,@rest*) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Utility functions
    ;;

    ;; char-class-expr-p
    (char-class-expr-p (expr)
      (and (consp expr) (member (first expr) '(not :range union))) )

    ;; char-expr-p
    (char-expr-p (expr)
      (or (characterp expr)
          (and (typep expr 'fixnum) (not (minusp expr))) ) )

    ;; compute-min-len
    (compute-min-len (expr)
      (values (regex-study-length expr)) )

    ;; cont-next-p
    (cont-next-p (expr)
        (declare (values t))
      (etypecase expr
        (character t)
        (fixnum t)
        (string t)
        (symbol t)
        (cons
          (ecase (first expr)
            ((=) t)
            ((and) (every #'cont-next-p (rest expr)))
            ((atom) t)
            ((bit-vector) t)
            ((capture) nil)
            ((category) t)
            ((equal) t)
            ((if) nil)
            ((max min) nil)
            ((not) t)
            ((or) nil)
            ((:range) t)
            ((reverse) (cont-next-p (second expr)))
            ((string-equal) t)
            ((union) t)
            ((unless) t)
            ((when) t) ))) )
    )
    ;;
    (push `(match-aux (pos) ,(compile-1 expr '() 0)) fdef*)

    (push
      `(match ()
          (REGEX-DEBUG "~70~~%")
          (let ((start pos)
                (end   (match-aux pos)) )
            (if end
                (progn
                  (REGEX-DEBUG ";*** Matched!~%")
                  ,@(if from-end
                        '((setf (svref end-vec   0) start)
                          (setf (svref start-vec 0) end)
                          (when (eql start end) (decf end)) )
                        '((setf (svref start-vec 0) start)
                          (setf (svref end-vec   0) end)
                          (when (eql start end) (incf end)) ))
                  ,(if match
                       `(setf (slot-value ,match 'position) end)
                       't ))
              (progn
                (REGEX-DEBUG ";**** Not matched!~%")
                ;; We must reset captures for next scanning.
                ;; Note: When regex contains one of atomic grouping, if,
                ;; or lookaround, captures may not be restored.
                (fill start-vec nil)
                (fill end-vec   nil)
                nil )) ) )
        fdef* )

      (when need-capture.b-p
        (push
         '(set-capture.b (nth idx pos)
            (let ((start pos)
                  (end   (svref cstack idx)) )
              (setf (svref start-vec nth) start)
              (setf (svref end-vec   nth) end)
              (REGEX-DEBUG ";~4T capture[~D]=~S~%"
                  nth (subseq string start end) )
              pos ) )
          fdef* ))

      (when need-capture.f-p
        (push
         '(set-capture.f (nth idx pos)
            (let ((start (svref cstack idx))
                  (end   pos) )
              (setf (svref start-vec nth) start)
              (setf (svref end-vec   nth) end)
              (REGEX-DEBUG ";~4T capture[~D]=~S~%"
                  nth (subseq string start end) )
              pos ) )
          fdef* ))

      (push
       `(%DEBUG-TRACE (fname pos expr)
          (format t "; <~A> <~A>~30T pos=~D ~S ~S~%"
              (subseq string (max (- pos 10) string-start) pos)
              (subseq string pos (min (+ pos 10) string-end))
              pos
              fname
              expr ) )
        fdef* )

      (when need-boundary-p
        (push
          `(boundary-p (pos)
            (cond
              ((eql pos string-start)
                (and (not (eql pos string-end))
                     (regex-word-char-p (schar string pos)) ) )
              ((eql pos string-end)
                (regex-word-char-p (schar string (1- pos))) )
              ((regex-word-char-p (schar string (1- pos)))
                (not (regex-word-char-p (schar string pos))) )
              (t
                (regex-word-char-p (schar string pos)) )) )
          fdef* ))
      (let ((form
              `(macrolet (
                  (DEBUG-TRACE (fname pos expr)
                    `(when *regex-debug* (%DEBUG-TRACE ,fname ,pos ',expr)) )
                  )
                  (labels ,fdef* (match)) ) ))
        (values form max-local-index) ) ) ) ) )
