;;;; aatree.lisp
;;; Arne Andresson Tree

(defstruct aatree-node
  (datum    nil)
  (key      ""  :type simple-string)
  (left     nil :type (or aatree-node null))
  (level    1   :type fixnum)
  (parent   nil :type (or aatree-node null))
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
    (declare (values (or aatree-node null)))
    (declare (type simple-string key))
  (labels (
    ;; delete
    (delete (node)
      (let* ((leaf (leaf node))
             (parent (aatree-node-parent leaf))
             (tmp (if (eq parent node) leaf parent)) )
        (if (eq (aatree-node-left parent) leaf)
            (setf (aatree-node-left parent) nil)
          (setf (aatree-node-right parent) nil) )

        (unless (eq node leaf)
          (let ((parent (aatree-node-parent node)))
            (if (eq (aatree-node-left parent) node)
                (setf (aatree-node-left parent) leaf)
              (setf (aatree-node-right parent) leaf) )

            (setf (aatree-node-parent leaf) (aatree-node-parent node))

            (when (aatree-node-left node)
              (setf (aatree-node-parent (aatree-node-left node)) leaf) )

            (setf (aatree-node-left leaf) (aatree-node-left node))

            (when (aatree-node-right node)
              (setf (aatree-node-parent (aatree-node-right node)) leaf) )

            (setf (aatree-node-right leaf) (aatree-node-right node))
            (setf (aatree-node-level leaf) (aatree-node-level node)) ))

        (loop
          for runner = tmp then (aatree-node-parent runner) do
            (cond
              ((> (aatree-node-level runner)
                  (level (aatree-node-left runner)) )
                (decf (aatree-node-level runner))
                (when (aatree-split runner)
                  (when (aatree-split runner)
                    (aatree-skew
                      (grand-parent runner)) ))
                  (return) )
              ((<= (aatree-node-level runner)
                   (level (aatree-node-right runner)) )
                (return) )
              (t
                (aatree-skew runner)
                (when (> (aatree-node-level runner)
                         (aatree-node-level (aatree-node-parent runner)) )
                  (aatree-skew runner)
                  (aatree-split (grand-parent runner))
                  (return) )
                (setq runner (aatree-node-parent runner)) ))) ) )

    ;; grand-parent
    (grand-parent (node)
      (aatree-node-parent (aatree-node-parent node)) )

    ;; leaf
    (leaf (node)
      (cond
        ((aatree-node-left node)
          (loop
            for leaf = (aatree-node-left node) then (aatree-node-right leaf)
            while (aatree-node-right leaf)
            finally (return leaf) ) )
        ((aatree-node-right node)
          (aatree-node-right node) )
        (t
          node )) )

    ;; level
    (level (node)
      (if node (1+ (aatree-node-level node)) 1) )
    )
    ;;
    (let ((node (aatree-get tree key)))
      (when node (delete node)) ) ) )


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
  (labels (
    ;; balance
    (balance (node)
  (format t "; balance ~S~%" node)
      (loop
        for runner = (aatree-node-parent node) then (aatree-node-parent runner)
        while runner do
  (format t "; balance runner=~S~%" runner)
          (unless (eql (aatree-node-level runner)
                       (level (aatree-node-left runner)) )
            (aatree-skew runner)
            (let ((right (aatree-node-right runner)))
              (unless (and right
                           (eql (aatree-node-level runner)
                                (aatree-node-level right) ))
                (setq runner (aatree-node-parent runner)) ) ))
          (unless (aatree-split (aatree-node-parent runner))
            (return) )) )

    ;; insert
    (insert (tree parent)
      (if (null tree)
          (let ((node (make-aatree-node :datum datum :key key :parent parent)))
            (balance node)
            node )
        (let ((diff (aatree-compare key (aatree-node-key tree))))
          (cond
            ((zerop diff)
              (setf (aatree-node-datum tree) datum)
              tree )
            ((< diff 0)
              (setf (aatree-node-left tree)
                (insert (aatree-node-left tree) tree) )
              tree )
            (t
              (setf (aatree-node-right tree)
                (insert (aatree-node-right tree) tree) )
              tree )) )) )

    ;; level
    (level (node)
      (if node (1+ (aatree-node-level node)) 1) )
    )
    ;;
    (insert tree nil) ) )


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
(defun aatree-skew (old)
    (declare (values unspecified))
    (declare (type aatree-node old))
 (format t "; skew ~s~%" old)
  (let ((new    (aatree-node-left old))
        (parent (aatree-node-parent old)) )
    (aatree-set-child new old parent)

    (shiftf (aatree-node-parent new) (aatree-node-parent old) new)

    (setf (aatree-node-left old) (aatree-node-right new))

    (when (aatree-node-left old)
      (setf (aatree-node-parent (aatree-node-left old)) old) )

    (setf (aatree-node-right new) old)

    (setf (aatree-node-level old)
      (let ((left (aatree-node-left old)))
        (if left (1+ (aatree-node-level left)) 1) )) ) )


;;;; aatree-set-child
(defun aatree-set-child (new old parent)
  (if (eq (aatree-node-left parent) old)
      (setf (aatree-node-left old) new)
    (setf (aatree-node-right old) new) ) )


;;;; aatree-split
;;; Split is a conditional left rotation when an insertion or deletion
;;; creates two consecutive red links.
(defun aatree-split (old)
    (declare (values t))
    (declare (type (or aatree-node null) old))
 (format t "; split ~s~%" old)
  (let ((new (and old (aatree-node-right old))))
    (when (let ((right (and new (aatree-node-right new))))
             (and right
                  (eql (aatree-node-level right)
                       (aatree-node-level old) )) )
      (let ((parent (aatree-node-parent old)))
        (aatree-set-child new old parent)
        (setf (aatree-node-parent new) (aatree-node-parent old))
        (setf (aatree-node-parent old) new)

        (let ((right (aatree-node-left new)))
          (setf (aatree-node-right old) right)
          (when right (setf (aatree-node-parent right) old))
          (setf (aatree-node-left new) old)
          (setf (aatree-node-level new) (1+ (aatree-node-level old))) ) )) ) )

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
