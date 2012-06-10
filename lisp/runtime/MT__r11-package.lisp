;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 11 Packages
;;; lisp/runtime/r11-package.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r11-package.lisp#3 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     %defpackage                 loader
;;;     %in-package                 11.2.6  macro
;;;     ensure-package              internal
;;;     package-rem
;;;     package-rem-external        internal
;;;     package-rem-internal        internal
;;;     package-put
;;;     package-put-external        internal
;;;     package-put-internal        internal
;;;
;;;     delete-package              11.2.11
;;;     export                      11.2.2
;;;     find-all-symbols            11.2.5
;;;     find-package                11.2.4
;;;     find-symbol                 11.2.3
;;;     import                      11.2.6
;;;     intern                      11.2.21
;;;     list-all-packages           11.2.7
;;;     make-package                11.2.12
;;;     package-name                11.2.22
;;;     package-nicknames           11.2.23
;;;     package-shadowing-symbols   11.2.24
;;;     package-use-list            11.2.25
;;;     package-used-by-list        11.2.26
;;;     packagep                    11.2.27 (boot)
;;;     *package*                   11.2.28 (boot)
;;;     rename-package              11.2.8
;;;     shadow                      11.2.9
;;;     shadowing-import            11.2.10
;;;     unexport                    11.2.14
;;;     unintern                    11.2.15
;;;     unuse-package               11.2.17
;;;     use-package                 11.2.18
;
(in-package :si)

;;;; defpackage loader
;;;
;;; Called by:
;;;     defpackage
;
(defun %defpackage (names
                    doc-string
                    exports import-froms interns shadows shadowing-import-froms
                    size
                    supplied-use-p
                    uses )
    (declare (ignore doc-string))
  (labels (
    ;; do-import
    (do-import (import-fn package import-froms)
      (dolist (from import-froms)
        (let ((from-package (ensure-package (first from))))
          (dolist (name (rest from))
            (let ((symbol (find-symbol name from-package)))
              (if symbol
                  (funcall import-fn symbol package)
                (cerror
                  (format nil "Ignore importing symbol ~S from ~A."
                     name
                     package )
                  (make-condition 'simple-package-error
                     :package package
                     :format-control "There is no such symbol ~S in ~A."
                     :format-arguments (list name from-package) ))) ) ) ) ) )
    )
    ;;
    (let ((package (find-package (first names))))
      (if package
          (progn
            (rename-package package (first names) (rest names))
            (unuse-package (package-use-list package) package) ) 
        (setq package (make-package (first names)
                                    :nicknames (rest names)
                                    :use       nil
                                    :size      size )) )
       (shadow shadows package)

       (do-import #'shadowing-import package shadowing-import-froms)

       (unless supplied-use-p
         (setq uses *package-default-use-list*) )

       (use-package uses package)

       (dolist (name interns)
         (intern name package) )

       (do-import #'import package import-froms)

       (dolist (name exports)
         (export (intern name package) package) )

       package ) ) )


;;;; 11.2.16 in-package
;;;
;
(defun %in-package (package)
  (setq *package* (ensure-package package)) )


;;;;  ensure pakcage
;;;
;;; Argumetns:
;;;     packageable - a package designator.
;;;
;;; Returns:
;;;     package object.
(defun ensure-package (thing)
    (declare (type package-designator thing))
    (declare (values package))
  (etypecase thing
    ((or symbol string character)   ; string designator
      (let ((package (find-package thing)))
        (when (null package)
          (error 'package-not-found :package thing) )
         package ) )
   (package thing) ) )


;;;; package-nickname
;;; For printer
(defun package-nickname (package)
    (declare (type package package))
    (declare (values simple-string))
  (let ((name (car (ref package names package))))
    (when (> (length name) 5)
      (setq name (or (second (ref package names package)) name)) )
    name ) )


;;;; Put Symbol Into Vector function.
;;;
;;; Arguments:
;;;     symbol  - a symbol to put.
;;;     vector  - a vector represents hash-table slots.
;;;     count   - number of symbols in vector.
;;;
;;; Returns:
;;;     vector  - specified vector or newly created vector.
;;;     count   - number of symbols in vector after processing.
;;;
;;; Called by:
;;;     package-external-put
;;;     package-internal-put
;;;
;;; Description:
;;;  Puts symbol into vector using hash-table algorithm. This function
;;;  keeps usage ratio under 60% (rehash-threshold 0.6) and grows
;;;  1.3 (rehash-size = 1.3)
;;;
;; BUGBUG: REVIEW: Use prime number for new-table length?
(macrolet (
  ;; for-slot
  (for-slot ((vec hash-code &optional result) &body decl*-form*)
   `(with-slot (,vec ,hash-code)
      (loop
        (locally ,@decl*-form*)
        (next-slot)
        (when (eql runner start)
          (return ,@(when result (list result))) )) ) )

  ;; next-slot
  (next-slot ()
   '(progn
      (incf runner)
      (when (eql runner btm)
        (setq runner top) )) )

  ;; with-slot
  (with-slot ((vec hash-code) &body decl*-form*)
   `(let* ((vec   ,vec)
           (top   1)
           (btm   (length vec))
           (start (+ (rem ,hash-code (- btm top)) top))
           (runner start)
           (rest  (svref vec 0)) )
      (declare (ignorable rest))
      ,@decl*-form* ) )

  ;; get-index, get-key, set-key
  (get-index ()  'runner)
  (get-key   ()  '(svref vec runner))
  (set-key   (x) `(setf (svref vec runner) ,x))

  ;; get-tomb-stone
  (get-tomb-stone ()
   `(let ((next (+ runner 1)))
      (when (eql next btm) (setq next top))
      (if (eql '#.(package-free-slot-marker) (svref vec next))
          '#.(package-free-slot-marker)
        '#.(package-removed-slot-marker) )) )

  ;; define
  (define (where)
    (let ((table (intern (format nil "~A-TABLE" where))))
     `(progn
        (defun ,(intern (format nil "PACKAGE-PUT-~A" where)) (pkg sym)
          (let ((vec (package-put (ref package ,table pkg) sym)))
            (when vec (setf (ref package ,table pkg) vec)) ) )

        (defun ,(intern (format nil "PACKAGE-REM-~A" where)) (pkg sym)
          (package-rem (ref package ,table pkg) sym) ) ) ) )
  )
  (labels (
    ;; find
    (package-find (vector name hash-code)
      (for-slot (vector hash-code 0)
        (let ((present (get-key)))
          (cond
            ((eql '#.(package-free-slot-marker) present)
              (return 0) )
            ((symbolp present)
              (when (string= (symbol-name present) name)
                (return present) )
              (when (zerop (decf rest))
                (return 0) ) )) ) ) )

    ;; package-put
    (package-put (vector symbol)
      (package-put-aux vector symbol)
      (when (>= (* (incf (svref vector 0)) 100) (* (length vector) 63))
        (rehash vector) ) )

   ;; package-rem
   (package-rem (vector symbol)
     (for-slot (vector (sxhash (symbol-name symbol)) nil)
       (let ((present (get-key)))
         (cond
           ((eql '#.(package-free-slot-marker) present)
             (return nil) )
           ((eq present symbol)
             (set-key (get-tomb-stone))
             (return (decf (svref vector 0))) )) ) ) )

    ;; package-put-aux
    (package-put-aux (vector symbol)
      (let ((home 0))
        (for-slot (vector (sxhash (symbol-name symbol)))
          (let ((present (get-key)))
            (cond
              ((eql '#.(package-free-slot-marker) present)
                (when (zerop home) (setq home (get-index)))
                (return) )
              ((eql '#.(package-removed-slot-marker) present)
                (when (zerop home) (setq home (get-index))) )) ) )
        (assert (>= home 1))
        (setf (svref vector home) symbol) ) )

    ;; rehash
    (rehash (oldvec)
      (let ((newvec
              (make-array (truncate (* (length oldvec) 13) 10)
                          :initial-element '#.(package-free-slot-marker) ) ))
        (setf (svref newvec 0) (svref oldvec 0))
        (for-slot (oldvec 0 (CAN-NOT-HAPPEN))
          (let ((present (get-key)))
            (when (symbolp present)
              (rehash-put newvec present)
              (when (eql (decf rest) 0) (return newvec)) ) ) ) ) )

    ;; rehash-put
    (rehash-put (vector symbol)
      (for-slot (vector (sxhash (symbol-name symbol)) (CAN-NOT-HAPPEN))
        (when (eql '#.(package-free-slot-marker) (get-key))
          (set-key symbol)
          (return) ) ) )
   )
   ;;
   (define "EXTERNAL")
   (define "INTERNAL")

   ;;;; 11.2.3 find-symbol
   (defun cl:find-symbol (name &optional (package *package*))
        (declare (values symbol (member nil :external :inherited :internal)))
        (declare (type string name))
        (declare (type package-designator package))
     (let* ((package   (ensure-package package))
            (hash-code (sxhash name)) )

       (let ((present (package-find (ref package internal-table package)
                            name hash-code ) ))
         (when (symbolp present)
           (return-from cl:find-symbol (values present :internal))) )

       (let ((present (package-find (ref package external-table package)
                            name hash-code ) ))
         (when (symbolp present)
           (return-from cl:find-symbol (values present :external))) )

       (dolist (used (ref package use-list package) (values nil nil))
         (let ((present (package-find (ref package external-table used)
                              name hash-code ) ))
           (when (symbolp present)
             (return-from cl:find-symbol (values present :inherited))) ) ) ) )
    ) ) ; labels macrolet


;;;; simple-package-error
;
(defun simple-package-error (package control &rest args)
  (error 'simple-package-error
        :package            package
        :format-control     control
        :format-arguments   args ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public funcitons
;;;;

;;;; 11.2.11 delete-package
(defun cl:delete-package (package)
  (etypecase package
    (package)
    ((or string character symbol)
      (let ((name (string package)))
        (setq package (find-package name))
        (unless package
          (cerror
            "Ignore deletion action."
            (make-condition 'package-not-found :package name) )
          (return-from delete-package nil) ) ) ))

  (when (null (package-name package))
    (return-from delete-package nil) )

  (dolist (used-package (package-use-list package))
    (unuse-package used-package package) )

  (loop
    (when (null (package-used-by-list package))
      (return) )
    (let ((error
            (make-condition 'simple-package-error
              :package package
              :format-control "Can't delete package ~A, since it is used by package ~{~A~^, ~}."
              :format-arguments
                (list (package-nickname package)
                      (mapcar #'package-nickname
                              (package-used-by-list package) ))) ))
        (cerror (format nil "Make package ~{~A~^, ~} unuse package ~A."
                    (mapcar #'package-nickname (package-used-by-list package))
                    (package-name package) )
                error )
        (loop
          (when (null (package-used-by-list package)) (return))
          (unuse-package package (first (package-used-by-list package))) ) ))

  (with-latch (*all-packages-latch*)
    (setq *all-packages* (delq package *all-packages*)) )

  (setf (ref package names package) nil)

  t )


;;;; 11.2.2 export
(defun cl:export (symbols &optional (package *package*))
  (labels (
    ;; export-1
    (export-1 (symbol package)
        (declare (type symbol symbol))
        (declare (type package package))
        (declare (values unspecified))
      (multiple-value-bind (found status)
          (find-symbol (symbol-name symbol) package)
          (declare (ignore found))
        (ecase status
          ((:external)
            ;; Specified symbol is already exported.
            )

          ((:internal)
            (export-1-aux symbol package) )

          ((:inherited)
            ;; Another symbol is accessible. Try import the symbol.
            (import symbol package)
            (export-1 symbol package) )

          ((nil)
            (cerror
              (format nil "Import symbol ~S into ~A."
                symbol
                (package-nickname package) )
              (make-condition 'simple-package-error
                :package package
                :format-control "Can't export ~S from ~A, since it isn't in ~:*~A."
                :format-arguments (list symbol package) ))

            (import symbol package)
            (export-1 symbol package) )) ) )

    ;; export-1-aux
    (export-1-aux (symbol package &aux (name (symbol-name symbol)))
      (dolist (user (package-used-by-list package))
        (multiple-value-bind (found status)
            (find-symbol name user)
          (when (and status
                     (not (eq found symbol))
                     (not (memq found (package-shadowing-symbols user))) )
              (error 'cannot-export-symbol
                     :package package
                     :symbol  symbol
                     :conflict-to found ))) )

      (package-rem-internal package symbol)
      (package-put-external package symbol) )
    )
    ;;
    (setq package (ensure-package package))
    (if (not (listp symbols))
        (export-1 symbols package)
      (dolist (symbol symbols)
        (export-1 symbol package) ))
    t ) )


;;;; 11.2.5 find-all-symbols
(defun cl:find-all-symbols (string)
  (setq string (string string))
  (let ((symbols '()))
    (with-package-iterator (next-symbol (list-all-packages)
                            :internal
                            :external )
      (loop
        (multiple-value-bind (more? symbol) (next-symbol)
          (unless more? (return))
          (when (string= string (symbol-name symbol))
            (push symbol symbols) ) )) )
    (nreverse symbols) ) )


;;;; 11.2.4 find-package
(defun cl:find-package (thing)
  (if (packagep thing)
      thing
    (let ((name (string thing)))
      (dolist (package (list-all-packages))
        (dolist (package-name (ref package names package))
          (when (string= name package-name)
            (return-from find-package package) ) ) ))) )


;;;; 11.2.6 import
(defun cl:import (symbols &optional (package *package*))
  (labels (
    (import-1 (symbol package)
      (loop
        (multiple-value-bind (found status)
            (find-symbol (symbol-name symbol) package)

          (ecase status
            ((nil)
              ;; Symbol isn't accessible.
              (package-put-internal package symbol)
              (when (null (symbol-package symbol))
                (setf (ref symbol package symbol) package) )
              (return) )
            ((:inherited)
              (when (eq symbol found)
                (package-put-internal package symbol)
                (return) ) )
            ((:internal :external)
              ;; Symbol is alredy in the package.
              (when (eq symbol found)
                (return) ) ))

          (cerror (format nil "Unintern symbol ~S" found)
                  'cannot-import-symbol
                  :package     package
                  :symbol      symbol
                  :conflict-to found )
          (unintern found package) )) )
    )
    ;;
    ;; import
    ;;
    (setq package (ensure-package package))
    (if (not (listp symbols))
        (import-1 symbols package)
      (dolist (symbol symbols)
        (import-1 symbol package) ))
    t ) )


;;;; 11.2.21 intern
(defun cl:intern (string &optional (package *package*))
  (setq package (ensure-package package))
  (multiple-value-bind (symbol status) (find-symbol string package)
    (unless status
      (setq symbol (make-symbol string))
      (if (eq '#.(symbol-package :key) package)
          (package-put-external package symbol)
        (package-put-internal package symbol) )
      (setf (ref symbol package symbol) package) )
    (values symbol status) ) )


;;;; 11.2.7 list-all-packages
(defun cl:list-all-packages ()
  (with-latch (*all-packages-latch* :shared)
    (copy-list *all-packages*) ) )


;;;; 11.2.12 make-package
(defun cl:make-package (name &key nicknames
                                  size
                                  (use *package-default-use-list*) )

  (labels (
    ;; make
    (make (names size use-list)
      (let ((package (.allocate-record '#.(class-description 'package))))
        (setf (ref package names package)       nil)
        (setf (ref package protect              package) nil)
        (setf (ref package use-list             package) '())
        (setf (ref package used-by-list         package) '())
        (setf (ref package shadowing-symbols    package) '())
        (setf (ref package external-table       package)
          (make-array size :initial-element '#.(package-free-slot-marker)) )
        (setf (ref package internal-table       package)
          (make-array 211 :initial-element '#.(package-free-slot-marker)) )

        (use-package use-list package)

        (with-latch (*all-packages-latch*)
          (setq *all-packages* (nconc *all-packages* (list package))) )

        (setf (ref package names package) names)

        package ) )
    )
    ;;
    (loop
      (let ((present (find-package (setq name (string name)))))
        (unless present (return))
        (restart-case
            (error 'package-already-exists
                   :package present )
          (store-value (another)
            :report "Use another name for new package."
            (setq name another) )) ))

    (unless (listp nicknames)
      (setq nicknames (list nicknames)) )

    (unless size
      (setq size 101) )

    (let ((names (list name))
          (use-list
            (if (listp use)
                (mapcar #'ensure-package use)
              (list (ensure-package use)) ) ))

      ;; Check nicknames are already used.
      (dolist (name nicknames)
        (setq name (string name))
        (let ((package (find-package name)))
          (if (null package)
              (push name names)
            (cerror
              (format nil "Don't use ~S as package name." name)
              (make-condition 'package-already-exists
                   :package package ))) ) )
      (setq names (nreverse names))

      (make names size use-list) ) ) )


;;;; 11.2.22 package-name
(defun cl:package-name (package)
  (setq package (ensure-package package))
  (car (ref package names package)) )


;;;; 11.2.23 package-nicknames
(defun cl:package-nicknames (package)
  (setq package (ensure-package package))
  (cdr (ref package names package)) )


;;;; 11.2.24 package-shadowing-symbols
(defun cl:package-shadowing-symbols (package)
  (setq package (ensure-package package))
  (ref package shadowing-symbols package) )


;;;; 11.2.25 package-use-list
(defun cl:package-use-list (package)
  (setq package (ensure-package package))
  (ref package use-list package) )


;;;; 11.2.25 package-used-by-list
(defun cl:package-used-by-list (package)
  (setq package (ensure-package package))
  (ref package used-by-list package) )


;;;; 11.2.8 rename-package
;;; Description:
;;;  Renames specified package to new-name and makes it has new-nicknames.
;;;  If new-name and new-nicknames conflict with existing-package,
;;;  package-already-exists error is signaled.
(defun cl:rename-package (package new-name &optional new-nicknames)
  (setq package (ensure-package package))
  (when (packagep new-name) (setq new-name (package-name new-name)))
  (let ((new-names (mapcar #'string (cons new-name new-nicknames))))
    (dolist (new-name new-names)
      (let ((new-package (find-package new-name)))
          (unless (or (null new-package) (eq new-package package))
            (error 'package-already-exists
                   :package new-package )) ) )
    (setf (ref package names package) new-names) )
  package )


;;;; 11.2.9 shadow
(defun cl:shadow (names &optional (package *package*))
  (labels (
    (shadow-1 (name package)
      (setq name (string name))
      (multiple-value-bind (symbol status) (find-symbol name package)
        (ecase status
          ((:internal :external)
            (pushnew symbol (ref package shadowing-symbols package)) )

          ((:inherited)
            (setq symbol (make-symbol name))
            (setf (ref symbol package symbol) package)
            (push symbol (ref package shadowing-symbols package))
            (package-put-internal package symbol) )

          ((nil)
            (setq symbol (intern name package))
            (push symbol (ref package shadowing-symbols package))
            (package-put-internal package symbol) )) ) )
    )
    ;;
    ;; shadow
    ;;
    (setq package (ensure-package package))
    (if (not (listp names))
        (shadow-1 names package)
      (dolist (name names)
        (shadow-1 name package) ))
    t ) )


;;;; 11.2.10 shadowing-import
(defun cl:shadowing-import (symbols &optional (package *package*))
  (labels (
    (shadowing-import-1 (symbol package)
      (multiple-value-bind (found status)
          (find-symbol (symbol-name symbol) package)
        (ecase status
          ((:internal :external)
            (unintern found package)
            (package-put-internal package symbol) )
          ((:inherited))
          ((nil)
            (package-put-internal package symbol) )) )
      (pushnew symbol (ref package shadowing-symbols package)) )
    )
    ;;
    ;; shadowing-import
    ;;
    (setq package (ensure-package package))
    (if (not (listp symbols))
        (shadowing-import-1 symbols package)
      (dolist (symbol symbols)
        (shadowing-import-1 symbol package) ))
    t ) )


;;;; 11.2.14 unexport
(defun cl:unexport (symbols &optional (package *package*))
  (labels (
    (unexport-1 (symbol package)
      (ecase (nth-value 1 (find-symbol (symbol-name symbol) package))
        ((:external)
         (package-rem-external package symbol)
         (package-put-internal package symbol) )
        ((:internal :inherited))
        ((nil)
          (simple-package-error package
                 "Symbol ~S isn't accessible in ~A."
                 symbol package ) )) )
    )
    (setq package (ensure-package package))
    (if (not (listp symbols))
        (unexport-1 symbols package)
      (dolist (symbol symbols)
        (unexport-1 symbol package) ))
    t ) )


;;;; 11.2.15 unintern
(defun cl:unintern (symbol &optional (package *package*))
    (check-type symbol symbol)

  (setq package (ensure-package package))

  (when (memq symbol (ref package shadowing-symbols package))
    (let ((symbols '())
          (name    (symbol-name symbol)) )
      (dolist (package (ref package use-list package))
        (multiple-value-bind (present status) (find-symbol name package)
          (when status
            (pushnew present symbols) ) ) )
      (when (>= (length symbols) 2)
        (error 'will-conflict-symbols
               :package package
               :symbols symbols )) ))

  ;; The symbol is removed from home package.
  (when (eq package (symbol-package symbol))
    (setf (ref symbol package symbol) nil) )

  ;; Remove the symbol from shadowing-symbols.
  (setf (ref package shadowing-symbols package)
          (delq symbol (ref package shadowing-symbols package)) )

  ;; Remove the symbol from hash-table.
  (cond
    ((package-rem-internal package symbol) :internal)
    ((package-rem-external package symbol) :external) ) )


;;;; 11.2.17 unuse-package
(defun cl:unuse-package (packages-to-unuse &optional (package *package*))
  (labels (
    (unuse-package-1 (package-to-unuse package)
      (setq package-to-unuse (ensure-package package-to-unuse))

      (progn
        (setf (ref package use-list package)
              (delq package-to-unuse (ref package use-list package)) )

        (setf (ref package used-by-list package-to-unuse)
              (delq package (ref package used-by-list package-to-unuse)) ) ) )
    )
    (let ((package (ensure-package package)))
      (if (not (listp packages-to-unuse))
          (unuse-package-1 packages-to-unuse package)
        (dolist (package-to-unuse packages-to-unuse)
          (unuse-package-1 package-to-unuse package) ))
      t ) ) )


;;;; 11.2.18 use-package
;;; Note:
;;;  When package-to-use contatins keyword package, this function signals
;;;  package-error.
(defun cl:use-package (packages-to-use &optional (package *package*))
  (labels (
    ;; use-package-1
    (use-package-1 (package-to-use package)
      (setq package-to-use (ensure-package package-to-use))

      (when (eq package-to-use package)
        (error 'use-package-itself
               :package package ) )

      (when (eq package-to-use '#.(symbol-package :key))
        (error 'use-keyword-package
               :package package ) )

      (when (memq package-to-use (ref package use-list package))
        (return-from use-package-1) )

      (do-external-symbols (symbol package-to-use)
        (multiple-value-bind (existing-symbol status)
            (find-symbol (symbol-name symbol) package)
          (cond
            ((not status))
            ((eq existing-symbol symbol))
            ((member (symbol-name symbol)
                     (ref package shadowing-symbols package)
                     :key  #'symbol-name
                     :test #'string= ))
            (t
               (simple-package-error package
                      "Symbol ~S in ~S is conflict to ~S."
                      symbol package-to-use existing-symbol )) )) )

      (progn
        (push package-to-use (ref package use-list package))
        (push package (ref package used-by-list package-to-use)) ) )
    )
    (setq package (ensure-package package))

    (when (eq package (find-package :keyword))
      (error "~S cannot use other packages." package) )

    (if (not (listp packages-to-use))
        (use-package-1 packages-to-use package)
      (dolist (package-to-use packages-to-use)
        (use-package-1 package-to-use package) ))

    t ) )
