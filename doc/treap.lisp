;;;;    treap.lisp

;;;; treap-node
(defstruct treap-node
  (datum    nil :type t)
  (key      ""  :type simple-string)
  (left     nil :type (or treap-node null))
  (priority 0   :type (integer 0 #.most-positive-fixnum))
  (right    nil :type (or treap-node null)) )


(defmethod print-object ((o treap-node) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S p=~D"
        (treap-node-key o)
        (treap-node-priority o) )
    o ) )


;;;; treap-compare
(defun treap-compare (s1 s2)
    (declare (values fixnum))
    (declare (type simple-string s1 s2))
  (let* ((k1 (length s1))
         (k2 (length s2)) )
    (dotimes (i (min k1 k2) (- k1 k2))
      (let ((d (- (char-code (schar s1 i)) (char-code (schar s2 i)))))
        (unless (zerop d) (return d)) ) ) ) )


;;;; treap-delete
(defun treap-delete (tree key)
    (declare (values (or treap-node null)))
    (declare (type (or treap-node null) tree))
    (declare (type simple-string key))
  (labels (
    ;; delete-node
    (delete-node (tree)
      (let ((left  (treap-node-left  tree))
            (right (treap-node-right tree)) )
        (cond
          ((null left)
            right )
          ((null right)
            left )
          ((< (treap-node-priority left) (treap-node-priority right))
            (let ((node (treap-rotate-right tree)))
              (setf (treap-node-right tree) (delete-node node))
              node ) )
          (t
            (let ((node (treap-rotate-left tree)))
              (setf (treap-node-left tree) (delete-node node))
              node ) )) ) )
    )
    ;;
  (when tree
    (let ((diff (treap-compare key (treap-node-key tree))))
      (cond
        ((eql diff 0)
          (delete-node tree) )
        ((< diff 0)
          (setf (treap-node-left tree)
            (treap-delete (treap-node-left tree) key) )
          tree )
        (t
          (setf (treap-node-right tree)
            (treap-delete (treap-node-right tree) key) )
          tree )) )) ) )


;;;; treap-get
(defun treap-get (tree key)
  (when tree
    (let ((diff (treap-compare key (treap-node-key tree))))
      (cond
        ((eql diff 0)
          tree )
        ((< diff 0)
          (treap-get (treap-node-left tree) key) )
        (t
          (treap-get (treap-node-right tree) key) )) )) )


;;;; treap-insert
(defun treap-insert (tree key datum)
    (declare (values treap-node))
    (declare (type (or treap-node null) tree))
    (declare (type simple-string key))
  (if (null tree)
      (make-treap-node
        :datum      datum
        :key        key
        :priority   (treap-random) )
    (let ((diff (treap-compare key (treap-node-key tree))))
      (cond
        ((eql diff 0)
          (setf (treap-node-datum tree) datum) )
        ((< diff 0)
          (let ((left (treap-insert (treap-node-left tree) key datum)))
            (setf (treap-node-left tree) left)
            (when (> (treap-node-priority left) (treap-node-priority tree))
              (setq tree (treap-rotate-right tree)) ) ) )
        (t
          (let ((right (treap-insert (treap-node-right tree) key datum)))
            (setf (treap-node-right tree) right)
            (when (> (treap-node-priority right) (treap-node-priority tree))
              (setq tree (treap-rotate-left tree)) ) ) ))
      tree )) )


(let ((seed 0))
  (defun treap-random ()
    (setq seed (logand (+ (* seed 1664525) 1013904223) most-positive-fixnum)) )
  ) ; let

;;;; treap-rotate-left
(defun treap-rotate-left (node)
    (declare (values treap-node))
    (declare (type treap-node node))
  (let ((right (treap-node-right node)))
    (setf (treap-node-right node)  (treap-node-left right))
    (setf (treap-node-left  right) node)
    right ) )


;;;; treap-rotate-right
(defun treap-rotate-right (node)
    (declare (values treap-node))
    (declare (type treap-node node))
  (let ((left (treap-node-left node)))
    (setf (treap-node-left node)  (treap-node-right left))
    (setf (treap-node-right left) node)
    left ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debug Staff
;;;

;;;; treap-count
(defun treap-count (tree)
    (declare (values (integer 0 #.most-positive-fixnum)))
    (declare (type (or treap-node null) tree))
  (if (null tree)
      0
    (+ (treap-count (treap-node-left tree))
       (treap-count (treap-node-right tree))
       1 )) )


;;;; treap-height
(defun treap-height (tree)
    (declare (values (integer 0  #.most-positive-fixnum)))
    (declare (type (or treap-node null) tree))
  (if (null tree)
      0
    (1+ (max (treap-height (treap-node-left  tree))
             (treap-height (treap-node-right tree)) ))) )


(defun treap-test1 ()
  (let ((treap nil))
    (do-external-symbols (x :cl treap)
      (setq treap (treap-insert treap (symbol-name x) x)) ) ) )


(defun treap-test2 (&optional (n char-code-limit))
  (let ((treap nil))
    (dotimes (code (min n char-code-limit) treap)
      (setq treap (treap-insert treap
                    (make-string 1 :initial-element (code-char code))
                    code )) ) ) )


(defun treap-dump (tree)
  (labels (
    (dump (tree height)
      (when tree
        (format t "~VT ~D ~S ~:D~%"
            height
            height
            (treap-node-key tree)
            (treap-node-priority tree) )
        (incf height)
        (dump (treap-node-left  tree) height)
        (dump (treap-node-right tree) height) ) )
    )
    (dump tree 0) ) )
