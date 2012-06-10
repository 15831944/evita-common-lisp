;;;; aatree.lisp
;;; Arne Andresson Tree

(defstruct aatree-node
  (datum    nil)
  (key      ""  :type simple-string)
  (left     nil :type (or aatree-node null))
  (level    1   :type fixnum)
  (right    nil :type (or aatree-node null)) )


(defmethod print-object ((o aatree-node) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S level=~D"
        (aatree-node-key o)
        (aatree-node-level o) )
    o ) )


;;;; aatree-compare
(defun aatree-compare (s1 s2)
    (declare (values fixnum))
    (declare (type simple-string s1 s2))
  (let* ((k1 (length s1))
         (k2 (length s2)) )
    (dotimes (i (min k1 k2) (- k1 k2))
      (let ((d (- (char-code (schar s1 i)) (char-code (schar s2 i)))))
        (unless (zerop d) (return d)) ) ) ) )


;;;; aatree-delete
(defun aatree-delete (tree key)
  (when tree
    (let ((diff (aatree-compare key (aatree-node-key tree))))
      (cond
        ((zerop diff)
          (cond
            ((and (aatree-node-left  tree)
                  (aatree-node-right tree) )
              (let ((runner (aatree-node-left tree)))
                (loop while (aatree-node-right runner) do
                  (setq runner (aatree-node-right runner)) )
                (setf (aatree-node-key   tree) (aatree-node-key   runner))
                (setf (aatree-node-datum tree) (aatree-node-datum runner))
                (setf (aatree-node-left tree)
                    (aatree-delete (aatree-node-left tree)
                                   (aatree-node-key  tree) )) ) )
            ((aatree-node-left tree)
              (setq tree (aatree-node-left tree)) )
            (t
              (setq tree (aatree-node-right tree)) )) )
        ((minusp diff)
          (setf (aatree-node-left tree)
            (aatree-delete (aatree-node-left tree) key) ) )
        (t
          (setf (aatree-node-right tree)
            (aatree-delete (aatree-node-right tree) key) ) )) )

    (when tree
      (let ((left  (aatree-node-left  tree))
            (right (aatree-node-right tree))
            (level (aatree-node-level tree)) )
        (when (or (and left  (<= (aatree-node-level left)  level))
                  (and right (<= (aatree-node-level right) level)) )
          (decf level)
          (setf (aatree-node-level tree) level)
          (when (and right (> (aatree-node-level right) level))
            (setf (aatree-node-level right) level) )
          (aatree-split (aatree-skew tree)) ) ))) )


;;;; aatree-get
(defun aatree-get (tree key)
  (when tree
    (let ((diff (aatree-compare key (aatree-node-key tree))))
      (cond
        ((eql diff 0)
          tree )
        ((< diff 0)
          (aatree-get (aatree-node-left tree) key) )
        (t
          (aatree-get (aatree-node-right tree) key) )) )) )


;;;; aatree-insert
(defun aatree-insert (tree key datum)
    (declare (values aatree-node))
    (declare (type simple-string key))
  (if (null tree)
      (make-aatree-node :datum datum :key key)
    (let ((diff (aatree-compare key (aatree-node-key tree))))
      (cond
        ((zerop diff)
          (setf (aatree-node-datum tree) datum)
          tree )
        ((< diff 0)
          (setf (aatree-node-left tree)
            (aatree-insert (aatree-node-left tree) key datum) )
          (aatree-split (aatree-skew tree)) )
        (t
           (setf (aatree-node-right tree)
             (aatree-insert (aatree-node-right tree) key datum) )
          (aatree-split (aatree-skew tree)) )) )) )


;;;; aatree-rotate-left
(defun aatree-rotate-left (node)
  (let ((left (aatree-node-left node)))
    (setf (aatree-node-left  node) (aatree-node-right left))
    (setf (aatree-node-right left) node)
    left ) )


;;;; aatree-rotate-right
(defun aatree-rotate-right (node)
  (let ((right (aatree-node-right node)))
    (setf (aatree-node-right node) (aatree-node-left right))
    (setf (aatree-node-left  right) node)
    right ) )


;;;; aatree-skew
;;; Skew is a right rotation when an insertion or deletion creates a left
;;; red link.
(defun aatree-skew (tree)
  (when tree
    (let ((left (aatree-node-left tree)))
      (when (and left
                 (eql (aatree-node-level left) (aatree-node-level tree)) )
          (let ((save tree))
            (setq tree (aatree-node-left tree))
            (setf (aatree-node-left  save) (aatree-node-right tree))
            (setf (aatree-node-right tree) save) )
          (setf (aatree-node-right tree)
            (aatree-skew (aatree-node-right tree)) ))
      tree )) )


;;;; aatree-set-child
(defun aatree-set-child (new old parent)
  (if (eq (aatree-node-left parent) old)
      (setf (aatree-node-left old) new)
    (setf (aatree-node-right old) new) ) )


;;;; aatree-split
;;; Split is a conditional left rotation when an insertion or deletion
;;; creates two consecutive red links.
(defun aatree-split (tree)
  (let ((right (aatree-node-right tree)))
    (if (and right
             (aatree-node-right right)
             (eql (aatree-node-level (aatree-node-right right))
                  (aatree-node-level tree) ))
        (let ((save tree))
          (setq tree (aatree-node-right tree))
          (setf (aatree-node-right save) (aatree-node-left tree))
          (setf (aatree-node-left  tree) save)
          (incf (aatree-node-level tree))
          (setf (aatree-node-right tree)
            (aatree-split (aatree-node-right tree)) )
          tree )
      tree ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debug Staff
;;;

;;;; aatree-count
(defun aatree-count (tree)
    (declare (values (integer 0 #.most-positive-fixnum)))
    (declare (type (or aatree-node null) tree))
  (if (null tree)
      0
    (+ (aatree-count (aatree-node-left tree))
       (aatree-count (aatree-node-right tree))
       1 )) )


;;;; aatree-height
(defun aatree-height (tree)
    (declare (values (integer 0  #.most-positive-fixnum)))
    (declare (type (or aatree-node null) tree))
  (if (null tree)
      0
    (1+ (max (aatree-height (aatree-node-left  tree))
             (aatree-height (aatree-node-right tree)) ))) )


(defun aatree-test1 ()
  (let ((aatree nil))
    (do-external-symbols (x :cl aatree)
      (setq aatree (aatree-insert aatree (symbol-name x) x)) ) ) )


(defun aatree-test2 (&optional (n char-code-limit))
  (let ((aatree nil))
    (dotimes (code (min n char-code-limit) aatree)
      (setq aatree (aatree-insert aatree
                    (make-string 1 :initial-element (code-char code))
                    code )) ) ) )


(defun aatree-dump (tree)
  (labels (
    (dump (tree height)
      (when tree
        (format t "~VT ~D ~S ~:D~%"
            height
            height
            (aatree-node-key tree)
            (aatree-node-level tree) )
        (incf height)
        (dump (aatree-node-left  tree) height)
        (dump (aatree-node-right tree) height) ) )
    )
    (dump tree 0) ) )
