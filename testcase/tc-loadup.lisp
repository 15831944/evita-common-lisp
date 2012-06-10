    (load "../../testcase/tc-defs")
    (load "../../testcase/tc-fns")

    (load "../../testcase/tc-00-fns")

    (load "../../testcase/tc-01-cl-intro")
    (load "../../testcase/tc-02-cl-syntax")
    (load "../../testcase/tc-03-cl-decl")
    (load "../../testcase/tc-03-cl-eval")
    (load "../../testcase/tc-04-cl-coerce")
    (load "../../testcase/tc-05-cl-control")
    (load "../../testcase/tc-06-cl-loop")

    (load "../../testcase/tc-07-clos-mop")
    (load "../../testcase/tc-07-cl-object")

    (load "../../testcase/tc-08-cl-struct")
    (load "../../testcase/tc-09-cl-condition")
    (load "../../testcase/tc-10-cl-symbol")
    (load "../../testcase/tc-11-cl-package")

    (load "../../testcase/tc-12-cl-fixnum")     ; 100
    (load "../../testcase/tc-12-cl-bignum")     ; 200
    (load "../../testcase/tc-12-cl-float32")    ; 300
    (load "../../testcase/tc-12-cl-float64")    ; 400

    (load "../../testcase/tc-13-cl-char")
    (load "../../testcase/tc-14-cl-cons")
    ;(load "../../testcase/tc-15-cl-array")
    (load "../../testcase/tc-15-cl-bitvec")
    (load "../../testcase/tc-16-cl-string")
    (load "../../testcase/tc-17-cl-sequence")
    (load "../../testcase/tc-18-cl-hash-table")
    (load "../../testcase/tc-19-cl-filename")

(in-package :tc-user)

;; ignores
(defvar *ignores* '(
        cl-04-24-906    ; 12.1.5.3 Rule of Canonical Representation for Complex Rationals

        ;clos-07-92-002  ; copy-instance
        ;cl-07-07-001    ; update-instance-for-redefined-class
        ;cl-07-08-001    ; change-class
        cl-07-25-001    ; fmakunbound
        cl-07-26-001    ; fmakunbound
        ;cl-07-38-001    ; (setf class-name)

        cl-08-01-002    ; allocate-structure => make-prototype
        ;cl-08-01-008    ; too few arguments for MAKE-STRUCT-BOA-04.
        cl-08-02-001    ; c6::safe-subtypep

        cl-08-03-001    ; copy-instance
        cl-08-03-002    ; safe-subtypep
        cl-08-03-003    ; safe-subtypep

        cl-09-43-001    ; simple-condition-format-control

        ;cl-10-06-001    ; symbol-plist
        ;cl-10-13-001    ; symbol-plist
        ;cl-14-34-102    ; abs

        ;; (typep vec (vector t 4)
        cl-04-24-907 cl-04-24-909 cl-04-24-911 cl-04-24-912
        cl-04-24-913 cl-04-24-914 cl-04-24-915 cl-04-24-916
        cl-04-24-917

        ;cl-17-05-004    ; (aref (array double-float) ...)


#|
        cl-17-10-001    ; coerce
        cl-17-04-001    ; coerce
        cl-17-05-003    ; bit-xor
        cl-17-05-004    ; row-major-aref
        cl-17-05-006    ; bit-xor
        cl-17-22-001    ; coerce
        cl-17-22-001    ; coerce
        cl-17-23-001    ; coerce
|#

        cl-19-04-09-001 ; (logical-pathname "sys:")
 ))

(dolist (name *ignores*)
  (setf (getf (test-case-plist (find-test name)) 'ignore) t) )
