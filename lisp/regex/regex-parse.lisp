;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - parse
;;; lisp/regex/regx-parse.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-parse.lisp#3 $
;;;
;;; Description:
;;;  This file contains regex parser.
;
(in-package :si)

;;;; +regex-meta-chars+
;;;
;;; Description:
;;;  A string contains mata characters of regular expression.
;;;
;;;     .   match any
;;;     *   (0, *) quantifier
;;;     +   (1, *) quantifier
;;;     ?   (0, 1) quantifier
;;;     (   start grouping
;;;     )   end grouping
;;;     {   (n, m) quanitifer
;;;     [   character class
;;;     \   backslash notation
;;;     ^   match start of string/line
;;;     $   match end of string/line
;;;     |   alternation
;
(defconstant +regex-meta-chars+ ".*+?(){[\\|^$")


;;;; +regex-backslash-token-alist+
;;;
;;; Description:
;;;  Alist contains single character backslash notation of regular expression
;;;  token.
;
(defconstant +regex-backslash-token-alist+
  '((#\A . :bos)            ; Match only at start of string
    (#\b . :boundary)       ; Match a word boundary
    (#\B . (not :boundary)) ; Match a non word boundary
    (#\G . :pos)            ; Match a end of last match.
    (#\Z . :seol)           ; Match only at end of string, or before NL at end
    (#\z . :eos )           ; Match only at end of string
    ; \0[1-7]{1,2}          ; Match a character represented by octal digits.
    ; \[1-7]\d*             ; Match a nth captured string.
    ; \[1-7]{1,3}           ; Match a character represented by octal digits.
    ; \[89]\d*              ; Match nth captured string.
  ) ) ; +regex-backslash-token-alist+


;;;; +regex-backslash-char-class-alist+
;;;
;;; Description:
;;;  Alist contains character class backslash notation for non-unicode
;;;  mode.
;
(defconstant +regex-backslash-char-class-alist+
  (let* ((digit '(:range #\0 #\9))
         (space '(union #\Space #\Tab #\u000A #\u000C #\u000D))
         (word  `(union ,digit (:range #\A #\Z) (:range #\a #\z) #\_)) )
    `((#\d . ,digit)          ; Match a [0-9]
      (#\D . (not ,digit))    ; Match a [^0-9]
      (#\s . ,space)          ; Match a whitespace
      (#\S . (not ,space))    ; Match a non whitespace
      (#\w . ,word)           ; Match a word character
      (#\W . (not ,word))     ; Match a non-word character
     ) ) ) ; +regex-backslash-char-class-alist+


;;;; +regex-backslash-char-class-alist+
;;;
;;; Description:
;;;  Alist contains character class backslash notation for unicode
;;;  mode.
;
(defconstant +regex-backslash-unicode-char-class-alist+
  (let* ((digit '(category #.unicode:category-decimal-digit-number))
         (space '(union #\Space #\Tab
                        #\u000A #\u000C #\u000D
                        #\u0085
                        (category #.unicode:category-separator-min
                                  #.unicode:category-separator-max )))
         (word  `(union
                    (category #.unicode:category-letter-min
                              #.unicode:category-letter-max )
                    ,digit
                    (category #.unicode:category-connector-punctuation) )) )
    `((#\d . ,digit)          ; Match a \p{Nd}
      (#\D . (not ,digit))    ; Match a \P{Nd}
      (#\s . ,space)          ; Match a whitespace
      (#\S . (not ,space))    ; Match a non whitespace
      (#\w . ,word)           ; Match a word character
      (#\W . (not ,word))     ; Match a non-word character
     ) ) ) ; +regex-backslash-unicode-char-class-alist+


;;;; +regex-backslash-literal-alist+
;;;
;;; Description:
;;;  Alist contains single character backslash notation of regular expression
;;;  literal character. These backslash notations are recognized in character
;;;  class token.
;;;
;;; Note:
;;;  Both of this and +regex-backslash-token-alist+ contains mapping for
;;;  character "b". When we use these constants in same function, we must
;;;  know which one has precedence.
;
(defconstant +regex-backslash-literal-alist+ '(
    (#\a . #\u0007)         ; Match a U+0007
    (#\b . #\u0008)         ; Match a U+0008
    (#\e . #\u001B)         ; Match a escape (U+001B)
    (#\f . #\u000C)         ; Match a U+000C
    (#\n . #\Newline)       ; Match a newline (U+000A)
    (#\r . #\Return)        ; Match a caridge return (U+000D)
    (#\t . #\Tab)           ; Match a tab (U+0009)
    (#\v . #\u000B)         ; Match a vertical tab (U+000B)
    (#\\ . #\u005C)         ; Match a backslash (U+005C)

    ; cC            Match a control character, where C is [U+0040..U+005F] and
    ;               [U+0061..U+007A].
    ; xXX           Match a character represented by two hexadecimal digits.
    ; x{X+}         Match a character represented by hexadecimal digits.
    ; uXXXX         Match a character represented by four hexadecimal digits.
    ; [0-7]{1,3}    Match a character represented by octal deigits in
    ;               U+0000...U+00FF.
  ) ) ; +regex-backslash-token-literal-alist+


;;;; +regex-positx-class-alist+
;
(defconstant +regex-posix-class-alist+
  (let* ((alphas  '((:range #\A #\Z) (:range #\a #\z)))
         (ascii   '(:range #\u0000 #\u007F))
         (control '(:range #\u0000 #\u001F))
         (digit   '(:range #\0 #\9))
         (spaces  '(#\Space #\Tab #\u000A #\u000B #\u000C #\u000D)) )
  `(
    ("alpha"  . (union ,@alphas))
    ("alnum"  . (union ,@alphas ,digit))
    ("ascii"  . ,ascii)
    ("blank"  . (union #\Space #\Tab))
    ("cntrl"  . ,control)
    ("digit"  . ,digit)                      ; \d
    ("graph"  . (:range #\u0021 #\u007E))
    ("lower"  . (:range #\a #\z))
    ("print"  . (:range #\u0020 #\u007E))    ; graph and space
    ("punct"  .
        (union (:range #\u0021 #\u002F)
               (:range #\u0039 #\u0040)
               (:range #\u005B #\u0060)
               (:range #\u007B #\u007E) ))
    ("space"  . (union ,@spaces)) ; [\s\v]
    ("upper"  . (:range #\A #\Z))
    ("word"   . (union ,@alphas ,digit #\_)) ; \w
    ("xdigit" . (union ,digit (:range #\A #\F) (:range #\a #\f)))
    ) ) ) ; +regex-posix-class-alist+


;;;; +regex-positx-class-unicode-alist+
;
(defconstant +regex-posix-class-unicode-alist+
  (let* ((control
            '(category #.unicode:category-control-min
                       #.unicode:category-control-max ) )
         (alpha
            '(category #.unicode:category-letter-min
                       #.unicode:category-letter-max ) )
         (ascii '(:range #\u0000 #\u007F))
         (digit '(category #.unicode:category-decimal-digit-number))
         (spaces
            '(#\Space #\Tab #\u000A #\u000B #\u000C #\u000D
              #\u0085
              (category #.unicode:category-separator-min
                        #.unicode:category-separator-max ))) )
  `(
    ("alpha" . ,alpha)
    ("alnum" . (union ,alpha ,digit))
    ("ascii" . ,ascii)
    ("blank" . (union #\Space #\Tab))
    ("cntrl" . (category #.unicode:category-control))
    ("digit" . ,digit)
    ("graph" . (not (union ,@spaces ,control)))
    ("lower" . (category #.unicode:category-lowercase-letter))
    ("print" . (not ,control))
    ("punct" . (category #.unicode:category-punctuation-min
                         #.unicode:category-punctuation-max ) )
    ("space" . (union ,@spaces)) ; [\s\v]
    ("upper" . (category #.unicode:category-uppercase-letter))
    ("word"  .
        (union ,alpha ,digit
               (category #.unicode:category-connector-punctuation) ))
    ("xdigit" .
        (union ,digit (:range #\A #\F) (:range #\a #\f)
               (:range #\uFF21 #\uFF26) ; FULLWIDTH LATIN CAPTIAL LETTER
               (:range #\uFF41 #\uFF46) ) ); FULLWIDTH LATIN SMALL LETTER
    ) ) ) ; +regex-posix-class-unicode-alist+


;;;; parse-regex
;;;
;;; Syntax:
;;;     parse-regex string &key start end ... => parse-tree, capture-count
;;; Arguments and Values:
;;;     parse-tree      A list.
;;;     capture-count   A positive interger.
;;;
;;; Description:
;;;  Parses string as regulare expression and returns parse tree and
;;;  number of anon-refs including matched string.
;;;
;;; Note:
;;;  This big defun contains following components:
;;;     regex parser    -- descendant regex parser (prefix "parse-")
;;;     regex lexer     -- lexical scanner (prefix "get-")
;;;     cc parser       -- character class parser (prefix "cc-")
;;;     cc lexer        -- character clas lexical scanner. (prefix "cc-get-")
;
(defun parse-regex (string
                    &key (start 0)
                         end
                         (capture t)
                         from-end
                         ignore-case
                         extended-syntax
                         multiple-lines
                         (single-line t)
                         unicode
                         ;; Arguments used by compiler
                         debug
                         target )
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values regex-expr simple-vector t fixnum))
    (declare (ignore debug target))
  (let* ((source            "")
         (pos               0)
         (offset            0)
         (unget-token        nil)
         (token/option       0)
         (name-sets         '())    ; list of (capture-name . nth)
         (name-refs         '())    ; list of (capture-name . pos)
         (anon-refs         '())    ; list of (nth . pos)
         (max-capture-index 0)
         (strbuf
           (regex-context-buffer (regex-get-context)) )
         (modifiers
           (logior (if ignore-case     REGEX-MOD-IGNORE-CASE 0)
                   (if extended-syntax REGEX-MOD-IGNORE-SPACE 0)
                   (if multiple-lines  REGEX-MOD-MULTILINE 0)
                   (if single-line     REGEX-MOD-SINGLE-LINE 0)
                   (if unicode         REGEX-MOD-UNICODE 0) )) )
    (declare (type simple-string source))
    (declare (type sequence-index pos))
    (declare (type sequence-index offset))
    (declare (type sequence-index max-capture-index))
    (declare (type list anon-refs))
    (declare (type fixnum modifiers))
  (labels (
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Character class parser
    ;;

    ;; cc-get-token
    ;;  Returns token in character class.
    ;;   cc-token ::= character                 -- char itself.
    ;;             |  (unsigned-byte 16)        -- code-point in ci.
    ;;             |  (range char char)
    ;;             |  (range code code)
    ;;             |  :end
    (cc-get-token (which)
        (declare (type (member max min) which))
      (let ((char (or (get-char-aux) (unclosed-pair #\u005B))))
        (case char
          ((#\u005B)
            (let ((char-2 (or (get-char-aux) (unclosed-pair #\u005B))))
              (case char-2 
                ((#\:) (cc-get-token/posix-class char-2))
                ((#\.) (cc-get-token/posix-class char-2))
                ((#\=) (cc-get-token/posix-class char-2))
                (otherwise (unget-char) char) ) ) )
          ((#\\)
            (let ((token (cc-get-token/backslash)))
              (if (and (eq which 'min) (characterp token))
                  (cc-get-token/range token)
                token ) ) )
          ((#\u005D)
            :end )
          (otherwise
            (if (and (ignore-case-p) (both-case-p char))
                (let ((code (char-code char)))
                  (if (eq which 'min)
                      (cc-get-token/range code)
                    code ) )
              (if (eq which 'min)
                  (cc-get-token/range char)
                char )) )) ) )

    ;; cc-get-token/backslash
    ;;  Returns token for backslash notation in character class. This function
    ;;  returns #\u0008 (BS) for "\b".
    ;;
    ;;  See Also: get-token/backslash
    ;;
    (cc-get-token/backslash ()
      (let ((char (or (get-char-aux) (SYNTAX-ERROR))))
        (cond
          ;; BUGBUG: For PCRE test case
          ;; REVIEW: We should allow close bracket follows backslash since
          ;; it makes balance of \[ ... \].
          ((find char "[-]") char)
          ((get-backslash-common char))
          ((digit-char-p char 8) (unget-char) (get-backslash-o))
          ((and (extended-syntax-p) (regex-space-char-p char))
            char )
          (t (invalid-backslash char)) ) ) )

    ;; cc-get-token/posix-class
    ;;  Gets POSIX character class or literal "[". This function is called
    ;;  when caller reads "[".
    ;;
    ;;  POSIX character class starts with string "[" + <type> and ends with
    ;;  <type> + "]".
    ;;
    (cc-get-token/posix-class (type-char)
        (declare (type character type-char))
      (loop
        with start-pos = pos
        with found-type-p = nil
        for char = (or (get-char-aux) (unclosed-pair #\u005B))
        until (eql char #\u005D) do
          (setq found-type-p (eql char type-char))
        finally
          (cond
            ((not found-type-p)
              (setq pos start-pos)
              (unget-char)
              (return #\u005B) )
            ((eql type-char #\:)
              (let ((not-p
                      (and (eql (schar string start-pos) #\^)
                           (incf start-pos) ) )
                    (expr
                      (let ((name (subseq string start-pos (- pos 2))))
                        (or (cdr (assoc name
                                        (if unicode
                                            +regex-posix-class-unicode-alist+
                                            +regex-posix-class-alist+ )
                                        :test #'string-equal ))
                            (simple-parse-error
                              "Invalid POSIX character class name: ~A"
                              name )) ) ))
                (when not-p (setq expr `(not ,expr)))
                (return expr) ) )
            (t
              (parse-error 'regex-not-supported) ))) )

    ;; cc-get-token/range
    (cc-get-token/range (min)
      (let ((save-pos pos))
        (if (not (eql (or (get-char) (unclosed-pair #\u005B)) #\-))
            (progn
              (unget-char)
              min )
          (let ((max (cc-get-token 'max)))
            (if (or (eql max #\u005D)
                    (and (not (characterp max)) (not (typep max 'fixnum))) )
                (progn
                  ;; "a-\D", "a-\d", "a-]"
                  (setq pos save-pos)
                  min )
              (let ((min-code (if (characterp min) (char-code min) min))
                    (max-code (if (characterp max) (char-code max) max)) )
                (unless (< min-code max-code)
                  (parse-error 'regex-invalid-range
                      :start (code-char min-code)
                      :end   (code-char max-code) ))
                (if (and (characterp min) (characterp max))
                    `(:range ,min ,max)
                  `(:range ,min-code ,max-code) ) )) )) ) )

    ;; cc-parse
    (cc-parse ()
      (loop
        with not-p =
          (or (eql (or (get-char-aux) (unclosed-pair #\u005B)) #\^)
              (progn (unget-char) nil) )
        with first-token =
          (let ((char (or (get-char-aux) (unclosed-pair #\u005B))))
            (if (eql char #\u005D)
                (cc-get-token/range char)
              (progn
                (unget-char)
                (cc-get-token 'min) )) )
        for token = (cc-get-token 'min)
        until (eq token :end)
          if (and (consp token) (eq (first token) 'union))
             append (rest token) into subexprs
          else
             collect token into subexprs
        finally
          (let ((expr
                  (cond
                    ((null subexprs)
                      first-token )
                    ((and (consp first-token) (eq (first first-token) 'union))
                      `(union ,@(rest first-token) ,@subexprs) )
                    (t
                      `(union ,first-token ,@subexprs) )) ))
            (when not-p (setq expr `(not ,expr)))
            (return expr) )) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Regex Lexer
    ;;

    ;; get-backslash-c
    ;;  Notation for control character.
    ;;      \c@         U+0000
    ;;      \cA or \ca  U+0001
    ;;      \cZ or \cz  U+001A
    ;;      \c[         U+001B
    ;;      \c\         U+001C
    ;;      \c]         U+001D
    ;;      \c^         U+001E
    ;;      \c_         U+001F
    (get-backslash-c ()
        (declare (values character))
      (let ((char (or (get-char) (SYNTAX-ERROR))))
        (cond
          ((char<= #\u0040 char #\u005F)
            (code-char (- (char-code char) #x40)) )
          ((char<= #\u0061 char #\u007A)
            (code-char (- (char-code char) #x60)) )
          (t
            (invalid-backslash char) )) ) )

    ;; get-backslash-common
    ;;  Handles common backslash notation supported in both regex and
    ;;  character class.
    (get-backslash-common (char)
      (cond
        ((cdr (assoc char +regex-backslash-literal-alist+)))
        ((if unicode
             (cdr (assoc char +regex-backslash-unicode-char-class-alist+))
          (cdr (assoc char +regex-backslash-char-class-alist+)) ))
        ((char= char #\c) (get-backslash-c))
        ((char= char #\p) (get-backslash-p nil))
        ((char= char #\P) (get-backslash-p 'not))
        ((char= char #\u) (get-backslash-x-aux 4 4))
        ((char= char #\x) (get-backslash-x)) ) )

    ;; get-backslash-digit
    (get-backslash-digit (char)
      (loop
        with nth  = (digit-char-p char)
        with code = (digit-char-p char 8)
        for char = (get-char)
        for count = 1 then (1+ count)
        while char do
          (let ((digit (digit-char-p char)))
            (when (null digit) (unget-char) (loop-finish))
            (setq nth (+ (* nth 10) digit)) )

          (incf count)

          (when (and code (<= count 3))
            (let ((digit (digit-char-p char 8)))
              (if (null digit)
                  (setq code nil)
                (setq code (+ (* code 8) digit)) ) ))
        finally
          #+nil (assert (plusp nth))
          (cond
            ((<= nth max-capture-index)
              (return (get-token/reference nth)) )
            ((<= nth 9)
              (setq anon-refs (acons nth pos anon-refs))
              (return (get-token/reference nth)) )
            ((and code (or (zerop code) (>= count 2)))
              (return (code-char code)) )
            (t (parse-error 'regex-unbound-group :index nth)) )) )

    ;; get-backslash-o
    ;;  Octal digit notation
    ;;    '\' [0-7]{1,3}
    (get-backslash-o ()
        (declare (values character))
      (loop
        with code  of-type sequence-index = 0
        with count of-type sequence-index = 0
        for char = (get-char)
        while char do
          (let ((digit (digit-char-p char 8)))
            (when (null digit) (unget-char) (loop-finish))
            (setq code (logior (ash code 3) digit)) )
          (incf count)
          (when (eql count 3) (loop-finish))
        finally
          (return (code-char code)) ) )

    ;; get-backslash-p
    ;;  Handles \p and \P:
    ;;    o \p <letter>
    ;;    o \p { <name> }
    ;;    o \p {^ <name> }
    ;;
    ;; Note: This is not predicate.
    (get-backslash-p (not)
      (let ((expr
             (let ((char (or (get-char) (SYNTAX-ERROR))))
                (if (char/= char #\u007B)
                    (get-backslash-p-aux (string char))
                  (progn
                    (setq char (or (get-char) (SYNTAX-ERROR)))
                    (if (char/= char #\^)
                        (unget-char)
                      (setq not (not not)) )
                    (get-backslash-p-aux (get-backslash-p/prop)) )) ) ))
      (when not (setq expr `(not ,expr)))
      expr ) )

    ;; get-backslash-p-aux
    (get-backslash-p-aux (name)
      (or (and (string= name "ANY") :any)
          (gethash name *regex-unicode-category-table*)
          (and (>= (length name) 3)
               (char= (schar name 0) #\I)
               (or (char= (schar name 0) #\N) (char= (schar name 1) #\S))
               (gethash (subseq name 2) *regex-unicode-block-table*) )
          (simple-parse-error "Invalid property name: ~S" name) ) )

    ;; get-backslash-p/prop
    ;;  Get Unicode property name for \p{prop}.
    (get-backslash-p/prop ()
      (with-output-to-string (s)
        (loop
          for char = (or (get-char) (unclosed-pair #\u007D))
          until (char= char #\u007D) do
            (unless (or (char= char #\Space)
                        (char= char #\-)
                        (char= char #\_) )
              (write-char (char-upcase char) s)) ) ) )

    ;; get-backslash-Q
    (get-backslash-Q ()
      (loop
        with start = pos
        with state = :normal
        for char = (or (get-char-aux) (unclosed-pair #\Q)) do
          (case state
            ((:normal)
              (when (eql char #\\) (setq state :backslash)) )
            ((:backslash)
              (when (eql char #\E)
                (return (get-backslash-Q-aux start (- pos 2))) )
              (setq state :normal) )) ) )

    ;; get-backslash-Q-aux
    (get-backslash-Q-aux (start end)
      (cond
        ((eql start end) :void)
        ((eql (1+ start) end)
          (let ((char (schar string start)))
            (if (ignore-case-p) (char-code char) char) ) )
        (t
          (let ((string (subseq string start end)))
            (if (and (ignore-case-p) (find-if #'both-case-p string))
               `(string-equal ,(string-upcase string))
              string ) ) )) )

    ;; get-backslash-x
    ;;  Hexadecmal digit notation
    (get-backslash-x ()
        (declare (values character))
      (let ((char (or (get-char-aux) (SYNTAX-ERROR))))
        (cond
          ((digit-char-p char 16)
            (unget-char)
            (get-backslash-x-aux 2 2) )
          ((char= char #\u007B)
            (prog1
              (get-backslash-x-aux 1 4)
              (setq char (get-char))
              (unless (eql char #\u007D)
                (when (and char (digit-char-p char 16))
                  (parse-error 'regex-not-supported) )
                (unexpected-char #\u007D) )) )
          (t
            (simple-parse-error "Invalid \\x") )) ) )

    ;; get-backslash-x-aux
    (get-backslash-x-aux (min max)
        (declare (type fixnum min))
        (declare (type fixnum max))
        (declare (values character))
      (loop
        with code  of-type fixnum = 0
        with count of-type fixnum = 0
        for char = (get-char)
        while char do
          (let ((digit (digit-char-p char 16)))
            (when (null digit) (unget-char) (loop-finish))
            (setq code (logior (ash code 4) digit)) )
          (incf count)
          (when (eql count max) (loop-finish))
        finally
          (when (< count min)
            (simple-parse-error "Expect at least ~D digits." min) )
          (when (> code char-code-limit)
            (simple-parse-error "Unsupported code point U+~X" code) )
          (return (code-char code)) ) )

    ;; get-char
    (get-char ()
      (if (not (extended-syntax-p))
          (get-char-aux)
        (loop
          for char = (get-char-aux)
          while (and char (regex-space-char-p char))
          finally (return char) )) )

    ;; get-char-aux
    (get-char-aux ()
        (declare (values (or character null)))
      (unless (eql pos end)
        (prog1
            (schar source pos)
          (incf pos) )) )

    ;; get-token
    (get-token ()
      (when unget-token
        (return-from get-token (shiftf unget-token nil)) )
      (get-token-aux) )

    ;; get-token-aux
    (get-token-aux ()
      (let ((char (get-char)))
        (case char
          ((#\*) '*)
          ((#\+) '+)
          ((#\?) '?)
          ((#\.) (if (single-line-p) :any '(not #\Newline)))
          ((#\$) (if (multiple-lines-p) :meol :seol) )
          ((#\^) (if (multiple-lines-p) :mbol :bos) )
          ((#\|) 'or)
          ((#\u0029) 'close)
          ((#\u005B) (cc-parse))
          ((#\u005C) (get-token/backslash))
          ((#\#)
            (if (not (extended-syntax-p))
                char
              (progn
                (loop
                  for char = (get-char-aux)
                  while (and char (not (eql char #\Newline))) )
                (get-token) )) )
          ((#\u0028)
            (or (get-token/paren) (get-token)) )
          ((#\u007B)
            char )
          ((nil) nil)
          (otherwise
            (get-token/char char) )) ) )

    ;; get-token/backslash
    ;;  Returns token for backslash notation outside character class. This
    ;;  function returns :boundary for "\b" instead of U+0008 (BS).
    ;;
    ;;  See Also: cc-get-token/backslash.
    (get-token/backslash ()
      (let* ((char (or (get-char) (SYNTAX-ERROR))))
        (cond
          ((find char ".*+?()[]{}\\^$#") char)
          ((cdr (assoc char +regex-backslash-token-alist+)))
          ((get-backslash-common char))
          ((eql (char-category char) unicode:category-space-separator)
            char )
          ((char= char #\k) (get-token/backslash-k))
          ((char= char #\0) (unget-char) (get-backslash-o))
          ((digit-char-p char) (get-backslash-digit char))
          ((char= char #\Q) (get-backslash-Q))
          ((find char "LlUuNXC")
            (parse-error 'regex-not-supported) )
          (t
            (invalid-backslash char) )) ) )

    ;; get-token/backslash-k
    (get-token/backslash-k ()
      (let ((frob (get-token/capname nil nil)))
        (etypecase frob
          (integer
            (get-token/reference frob) )
          (cons
            (if (ignore-case-p) `(equal ,frob) `(= ,frob)) )) ) )

    ;; get-token/capname
    ;;  \k<name>    \k'name'
    ;;  \k<number>  \k'number'
    (get-token/capname (open close)
        (declare (values (or integer cons)))
        (declare (type (or character null) open close))

      (setf (fill-pointer strbuf) 0)
      (when (null close)
        (setq open (get-char-aux))
        (cond
          ((eql open #\<) (setq close #\>))
          ((eql open #\u0027) (setq close open))
          (t 
            (simple-parse-error
                "Capture name must be <name> or 'name'." ))))

      (let ((char (or (get-char-aux) (unclosed-pair open))))
        (unget-char)
        (cond
          ((digit-char-p char)
            (loop
              with nth = 0
              for char = (or (get-char-aux) (unclosed-pair open))
              until (eql char close) do
                (let ((digit (digit-char-p char)))
                  (unless digit (simple-parse-error "Expect digit"))
                  (setq nth (+ (* nth 10) digit)) )
              finally
                (when (zerop nth) (invalid-capture-name))
                (when (> nth max-capture-index)
                  (setq anon-refs (acons nth pos anon-refs)) )
                (return nth) ) )

          ((alpha-char-p char)
            (loop
              for char = (or (get-char-aux) (unclosed-pair open))
              until (eql char close) do
                (setq char (regex-name-char-p char))
                (when (null char) (unclosed-pair open))
                (vector-push-extend char strbuf)
              finally
               (let* ((name (intern strbuf :keyword))
                      (name.nth
                        (or (assoc name name-sets)
                            (first (assoc name name-refs :key #'first)) ) ))
                 (return
                   (cond
                     ((cdr name.nth))
                     (name.nth)
                     (t
                       (setq name.nth (list name))
                       (setq name-refs (acons name.nth pos name-refs))
                       name.nth )) ) )) )

          (t (invalid-capture-name)) ) ) )

    ;; get-token/char
    ;;  Collects characters into string until meta character.
    ;;
    ;;  Note: We need to handle qunaitifer different from other meta
    ;;  characters.
    (get-token/char (char)
        (declare (type character))
        (declare (values regex-expr))
      (loop
        with both-case-p = (both-case-p char)
        initially
          (setf (fill-pointer strbuf) 1)
          (setf (char strbuf 0) char)
        do
          (setq char (get-char-aux))

          (when (null char) (loop-finish))

          ;; Quantifiers
          (when (or (eql char #\*) (eql char #\+) (eql char #\?)
                    (eql char #\u007B) )
            (unget-char)

            (let ((length (fill-pointer strbuf)))
              (unless (eql length 1)
                (unget-char)
                (setf (fill-pointer strbuf) (1- length)) ) )

            (loop-finish) )

          ;; Other meta chars
          (when (find char +regex-meta-chars+)
            (unget-char)
            (loop-finish) )

          ;; Space and comment in extend syntax
          (cond
            ((not (extended-syntax-p))
              (when (both-case-p char) (setq both-case-p t))
              (vector-push-extend char strbuf) )
            ((regex-space-char-p char))
            ((char= char #\#)
              (loop
                for char = (get-char-aux)
                until (or (null char) (eql char #\Newline)) ))
            (t
              (when (both-case-p char) (setq both-case-p t))
              (vector-push-extend char strbuf)) )
        finally
          (if (eql (length strbuf) 1)
              (let ((char (char strbuf 0)))
                (if (and both-case-p (ignore-case-p))
                    (return (char-code char))
                  (return char) ) )
             (let ((string (copy-seq strbuf)))
               (if (and both-case-p (ignore-case-p))
                   (return `(string-equal ,string))
                 (return string) ) ))) )

    ;; get-token/reference
    (get-token/reference (nth)
      (if (ignore-case-p)
          `(equal ,nth)
        `(= ,nth) ) )

    ;; get-token/option
    (get-token/option (minusp)
      (loop
        with plus  = 0
        with minus = 0
        with token = 'option
        for char = (get-char)
        until (eql char #\u0029) do
          (when (null char) (SYNTAX-ERROR))
          (case char
            ((#\i) (setq minus (logior minus REGEX-MOD-IGNORE-CASE)))
            ((#\m) (setq minus (logior minus REGEX-MOD-MULTILINE)))
            ((#\s) (setq minus (logior minus REGEX-MOD-SINGLE-LINE)))
            ((#\u) (setq minus (logior minus REGEX-MOD-UNICODE)))
            ((#\x) (setq minus (logior minus REGEX-MOD-IGNORE-SPACE)))
            ((#\-)
              (when minusp (SYNTAX-ERROR))
              (setq minusp t)
              (setq plus minus)
              (setq minus 0) )
            ((#\:)
              (setq token 'and)
              (loop-finish) )
            (otherwise
              (SYNTAX-ERROR) ))
        finally
          (unless minusp (shiftf plus minus 0))
          (setq token/option (logior (ash minus 4) plus))
          (return token) ) )

    ;; get-token/paren
    (get-token/paren ()
      (if (not (get-token/question))
          'capture
        (case (get-char-aux)
          ((nil) (SYNTAX-ERROR))
          ((#\!) 'negative-lookahead)
          ((#\=) 'positive-lookahead)
          ((#\:) (setq token/option 0) 'and)
          ((#\>) 'atom)
          ((#\<)
            (case (get-char)
              ((#\=) 'positive-lookbehind)
              ((#\!) 'negative-lookbehind)
              (otherwise    ; (?<name>...)
                (unget-char)
                (get-token/paren/capture #\< #\>) )) )
          ((#\#) ; comment
            (loop
              for char = (get-char-aux)
              until (eql char #\u0029) do
                (when (null char) (unclosed-pair #\u0028)) ) )
          ((#\i #\m #\s #\u #\x)
            (unget-char)
            (get-token/option nil) )
          ((#\-)
            (get-token/option t) )
          ((#\u0028)
            'if )
          ((#\u0027)    ; (?'name'...)
            (get-token/paren/capture #\' #\') )
          ((#\u0029)
            nil )
          ((#\?)        ; ??{code}
            (parse-error 'regex-not-supported) )
          ((#\u007B)    ; ?{code}
            (parse-error 'regex-not-supported) )
          (otherwise (SYNTAX-ERROR)) )) )

    ;; get-token/paren/capture
    (get-token/paren/capture (open close)
      `(setq ,(get-token/capname open close)) )

    ;; get-token/question
    ;;  Returns true if next character is question mark. For parenthesis.
    (get-token/question ()
      (let ((char (get-char-aux)))
        (or (eql char #\?) (progn (when char (unget-char)) nil)) ) )

    ;; get-uint
    (get-uint ()
      (let ((ndigits 0)
            (n 0) )
        (loop
          for char = (get-char-aux)
          for digit = (and char (digit-char-p char))
          while digit do
            (setq n (+ (* n 10) digit))
            (incf ndigits)
          finally
            (when char (unget-char))
            (return (when (plusp ndigits) n)) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Regex Parser
    ;;

    ;; parse
    (parse (eof)
      (let ((save-option modifiers))
        (prog1
            (parse-aux eof)
          (setq modifiers save-option) ) ) )

    ;; parse-aux
    (parse-aux (eof)
      (let ((expr (parse-alt eof))
            (token (get-token)) )
        (cond
          ((eq token eof) expr)
          ((null eof) (SYNTAX-ERROR))
          (t (unclosed-pair #\u0029)) ) ) )

    ;; parse-alt -- catexp '|' [altexp]
    ;; Note: We can't transform one character alternatives into union, since
    ;; (?(cond) then | else) expects two have two alternatives.
    (parse-alt (eof)
      (loop
        with expr  = (parse-cat eof)
        with exprs = nil
        for token = (get-token)
        while (eq token 'or) do
          (push (parse-cat eof) exprs)
        finally
          (when token (unget-token token))
          (return (if (null exprs) expr `(or ,expr ,@(nreverse exprs)))) ) )

    ;; parse-cat -- qntexp+
    (parse-cat (eof)
        (declare (type sequence-index))
        (declare (values regex-expr))
      (loop
        with expr = (parse-qnt eof)
        with exprs = nil
        for token = (get-token) do
          (unget-token token)
          (case token
            ((or close nil)
              (when exprs
                (setq exprs (cons expr (nreverse exprs)))
                (parse-cat/stringfy exprs)
                (if (null (rest exprs))
                    (setq expr (first exprs))
                  (setq expr `(and ,@exprs)) ))
              (return expr) )
            (otherwise
              (push (parse-qnt eof) exprs) ))) )

    ;; parse-cat/stringfy
    ;;  Transforms character nodes into string node for escaped characters.
    (parse-cat/stringfy (exprs)
        (declare (type list exprs))
      (loop
        with runner = exprs
        while runner do
          (let ((expr (first runner))
                (next (rest runner)) )
            (cond
              ((characterp expr)
                ;; Collect char node
                (loop
                  initially
                    (setf (fill-pointer strbuf) 0)
                    (setq next runner)
                  finally
                    (when (>= (length strbuf) 2)
                      (setf (first runner) (copy-seq strbuf))
                      (setf (rest runner) next) )
                  do
                    (cond
                      ((characterp expr)
                        (vector-push-extend expr strbuf) )
                      ((stringp expr)
                        (loop for char across expr do
                          (vector-push-extend char strbuf) ) )
                      (t (loop-finish)) )

                    (setq next (rest next))
                    (setq expr (first next)) ) )

              ((char/i-expr-p expr)
                ;; Collect char/i expr
                (loop
                  initially
                    (setf (fill-pointer strbuf) 0)
                    (setq next runner)
                  finally
                    (when (>= (length strbuf) 2)
                      (setf (first runner) `(string-equal ,(copy-seq strbuf)))
                      (setf (rest runner) next) )
                  do
                    (cond
                      ((characterp expr)
                        (when (both-case-p expr) (loop-finish))
                        (vector-push-extend expr strbuf) )
                      ((char/i-expr-p expr)
                        (vector-push-extend (code-char expr) strbuf) )
                      ((string/i-expr-p expr)
                        (loop for char across (second expr) do
                          (vector-push-extend char strbuf) ) )
                      (t (loop-finish)) )
                    (setq next (rest next))
                    (setq expr (first next)) ) )

              ((stringp expr)
                ;; Collect char/i expr
                (loop
                  with len = (length expr)
                  initially
                    (setf (fill-pointer strbuf) 0)
                    (setq next runner)
                  finally
                    (when (> (length strbuf) len)
                      (setf (first runner) (copy-seq strbuf))
                      (setf (rest runner) next) )
                  do
                    (cond
                      ((characterp expr)
                        (vector-push-extend expr strbuf) )
                      ((stringp expr)
                        (loop for char across expr do
                          (vector-push-extend char strbuf) ) )
                      (t (loop-finish)) )
                    (setq next (rest next))
                    (setq expr (first next)) ) )

              ((string/i-expr-p expr)
                ;; Collect char/i expr
                (loop
                  with len = (length (second expr))
                  initially
                    (setf (fill-pointer strbuf) 0)
                    (setq next runner)
                  finally
                    (when (> (length strbuf) len)
                      (setf (first runner) `(string-equal ,(copy-seq strbuf)))
                      (setf (rest runner) next) )
                  do
                    (cond
                      ((characterp expr)
                        (when (both-case-p expr) (loop-finish))
                        (vector-push-extend expr strbuf) )
                      ((string/i-expr-p expr)
                        (loop for char across (second expr) do
                          (vector-push-extend char strbuf) ) )
                      ((and (stringp expr) (notany #'both-case-p expr))
                        (loop for char across expr do
                          (vector-push-extend char strbuf) ) )
                      (t (loop-finish)) )
                    (setq next (rest next))
                    (setq expr (first next)) ) ))
            (setq runner next) )) )

    ;; parse-qnt
    ;;  qnt ::= repetable '*'           greedy
    ;;      |   repetable '+'
    ;;      |   repetable '?'
    ;;      |   repetable '*?'          lazy
    ;;      |   repetable '+?'
    ;;      |   repetable '??'
    ;;      |   repetable '*+'          possessive
    ;;      |   repetable '++'
    ;;      |   repetable '?+'
    ;;      |   repetable '{' digit+ [',' [digit+]] '}' ('?' | '+')?
    ;;
    (parse-qnt (eof)
        (declare (type sequence-index))
        (declare (values regex-expr))
      (loop
        with expr = (parse-factor eof)
        for token = (get-token) do
          (multiple-value-bind (min max)
              (case token
                ((*)  (values 0 regex-infinity))
                ((+)  (values 1 regex-infinity))
                ((?)  (values 0 1))
                ((#\u007B) (parse-qnt/brace))
                (otherwise
                  (unget-token token)
                  (return expr) ))

            ;; Note: It is nice to singal error when subexpr is zero-width
            ;; assertion, such as \b, (?<=expr). But, Perl and .NET allows it.

            (when (> min max)
              (parse-error 'regex-invalid-min-max :min min :max max) )

            (multiple-value-bind (type possessive-p)
                (case (get-char)
                  ((#\?) (values 'min nil))
                  ((#\+) (values 'max t))
                  ((#\*) (SYNTAX-ERROR))
                  ((nil) (values 'max nil))
                  (otherwise (unget-char) (values 'max nil)) )
              (unless (and (eql min 1) (eql max 1))
                (setq expr `(,type ,min ,max ,expr)) )
              (when possessive-p
                (setq expr `(atom ,expr)) ) ) )) )

    ;; parse-qnt/brace -- '{' digit+ [ ',' [ digit+ ]] '}'
    ;;  Parses brace syntax and returns min and max.
    (parse-qnt/brace ()
        (declare (values integer integer))
      (let ((min (get-uint))
            (max regex-infinity) )
        (when (null min) (SYNTAX-ERROR))

        (let ((char (get-char)))
          (cond
            ((null char) (unclosed-pair #\u007B))
            ((eql char #\u007D) (setq max min))
            ((eql char #\,)
              (setq char (or (get-char) (SYNTAX-ERROR)))
              (when (digit-char-p char)
                (unget-char)
                (setq max (get-uint))
                (when (null max) (SYNTAX-ERROR))
                (setq char (get-char)) )
              (when (null char) (unclosed-pair #\u007B))
              (unless (eql char #\u007D) (unexpected-char #\u007D)) )
            (t
              (SYNTAX-ERROR) ))
          (values min max ) ) ) )

    ;; prase-factor
    (parse-factor (eof)
        (declare (type sequence-index))
        (declare (values regex-expr))
      (let ((token (get-token)))
        (case token
          ((and)
            (let ((save-option modifiers))
              (parse-factor/option token/option)
              (prog1
                  (parse 'close)
                (setq modifiers save-option) ) ) )

          ((atom)
            `(atom ,(parse 'close)) )

          ((capture)
            (if (not capture)
                (parse 'close)
              (progn
                (incf max-capture-index)
                `(capture ,max-capture-index ,(parse 'close)) )) )

          ((if)
            (parse-factor/if) )

          ((negative-lookahead)
            (parse-factor/lookahead 'unless) )

          ((negative-lookbehind)
            (parse-factor/lookbehind 'unless) )

          ((positive-lookahead)
            (parse-factor/lookahead 'when) )

          ((positive-lookbehind)
            (parse-factor/lookbehind 'when) )

          ((option)
            (parse-factor/option token/option)
            (parse-factor eof) )

          ((or)
            (unget-token token)
            :void )

          ((* + ?)
            (SYNTAX-ERROR) )

          ((:meol)
            (if (not from-end)
                :meol
              '(or :eos (when (reverse #\Newline))) ) )

          ((:seol)
            (if (not from-end)
                :seol
              '(or :eos (when (reverse (and #\Newline :eos)))) ) )

          (otherwise
            (cond
              ((and (consp token) (eq (first token) 'setq))
                (parse-factor/named-capture (second token)) )
              ((eq token eof)
                (unget-token token)
                :void )
              (t
                token )) )) ) )

    ;; parse-factor/if
    ;;  (?(condition)yes|no)
    ;;      condition ::= backreference | lookaround
    (parse-factor/if ()
      (let ((cond (parse-factor/if-cond))
            (expr (parse 'close)) )
        (cond
          ((or (not (consp expr)) (not (eq (first expr) 'or)))
            `(if ,cond ,expr :void) )
          ((= (length expr) 3)
            `(if ,cond ,(second expr) ,(third expr)) )
          (t
            (simple-parse-error
              "Invalid (?(cond) then | else) construct." ) )) ) )

    ;; parse-factor/if-cond
    (parse-factor/if-cond ()
      (case (prog1 (or (get-char) (SYNTAX-ERROR)) (unget-char))
        ((#\?)
          (unget-char)
          (case (get-token)
            ((positive-lookahead)
              (parse-factor/lookahead 'when) )
            ((negative-lookahead)
              (parse-factor/lookahead 'unless) )
            ((positive-lookbehind)
              (parse-factor/lookbehind 'when) )
            ((negative-lookbehind)
              (parse-factor/lookbehind 'unless) )
            (otherwise
              (parse-error 'regex-invalid-if-condition) )) )
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (loop
            with nth = 0
            for char  = (or (get-char) (SYNTAX-ERROR))
            for digit = (digit-char-p char)
            while digit do
              (setq nth (+ (* nth 10) digit))
            finally
              (unless (eql char #\u0029) (SYNTAX-ERROR))
              (when (zerop nth)
                (parse-error 'regex-invalid-if-condition) )

              ;; Note: Perl doesn't signals error if nth captur doesn't
              ;; exist.
              (when (> nth max-capture-index)
                (setq anon-refs (acons nth pos anon-refs)) )
              (return `(boundp ,nth)) ) )
        (otherwise
          (let ((frob (get-token/capname #\( #\))))
            (etypecase frob
              (integer frob)
              (cons `(boundp ,frob)) ) ) )) )

    ;; parse-factor/lookahead
    (parse-factor/lookahead (op)
      (if (not from-end)
          `(,op ,(parse 'close))
        (let ((save-from-end from-end))
          (prog2
            (setq from-end nil)
              `(,op (reverse ,(nreverse-expr (parse 'close))))
            (setq from-end save-from-end) ) )) )

    ;; parse-factor/lookbehind
    (parse-factor/lookbehind (op)
      (if from-end
          `(,op ,(parse 'close))
        (let ((save-from-end from-end))
          (prog2
            (setq from-end t)
              `(,op (reverse ,(nreverse-expr (parse 'close))))
            (setq from-end save-from-end) ) )) )

    ;; parse-factor/named-capture
    (parse-factor/named-capture (frob)
      (let ((nth
              (etypecase frob
                (integer
                  (let ((nth frob))
                    (when (eql (1+ max-capture-index) nth)
                      (incf max-capture-index) )
                    (when (> nth max-capture-index)
                      (parse-error 'regex-unbound-group :index nth) )
                     nth ) )
                (cons
                  (or (cdr frob)
                      (let ((nth (incf max-capture-index)))
                        (push frob name-sets)
                        (setf (cdr frob) nth) )) )) ))
        `(capture ,nth ,(parse 'close)) ) )

    ;; parse-factor/option
    (parse-factor/option (minus-plus)
      (setq modifiers (logior modifiers (logand minus-plus)))
      (setq modifiers (logandc2 modifiers (ash minus-plus -4))) )

    ;; prase-error
    (parse-error (name &rest args)
      (apply #'error name
             :string    string
             :position  (+ (- pos offset) start)
             args ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Utility Functions
    ;;

    ;; Options
    (ignore-case-p ()
      (not (zerop (logand modifiers REGEX-MOD-IGNORE-CASE))) )

    (extended-syntax-p ()
      (not (zerop (logand modifiers REGEX-MOD-IGNORE-SPACE))) )

    (multiple-lines-p ()
      (not (zerop (logand modifiers REGEX-MOD-MULTILINE))) )

    (single-line-p ()
      (not (zerop (logand modifiers REGEX-MOD-SINGLE-LINE))) )

    ;; char/i-p
    (char/i-expr-p (expr)
      (and (typep expr 'fixnum) (not (minusp expr))) )

    ;; invalid-backslash
    (invalid-backslash (char)
      (parse-error 'regex-invalid-backslash :char char) )

    ;; invalid-capture-name
    (invalid-capture-name ()
      (simple-parse-error "Invalid capture name.") )

    ;; nreverse-expr
    (nreverse-expr (expr)
      (if (not (consp expr))
          expr
        (ecase (first expr)
          ((=)
            expr )
          ((and) ; subexpr+
            (setf (rest expr) (nreverse (rest expr)))
            (loop for runner on (rest expr) do
              (setf (first runner) (nreverse-expr (first runner)))
              finally (return expr) ) )
          ((boundp)
            expr )
          ((atom) ; subexpr
            (setf (second expr) (nreverse-expr (second expr)))
            expr )
          ((capture) ; nth subexpr
            (setf (third expr) (nreverse-expr (third expr)))
            expr )
          ((category)
            expr )
          ((equal)
            expr )
          ((if) ; cond then else
            (setf (second expr) (nreverse-expr (second expr)))
            (setf (third  expr) (nreverse-expr (third  expr)))
            (setf (fourth expr) (nreverse-expr (fourth expr)))
            expr )
          ((max min) ; min max subexpr
            (setf (third expr) (nreverse-expr (third expr)))
            expr )
          ((not) ; subexpr
            (setf (second expr) (nreverse-expr (second expr)))
            expr )
          ((or) ; subexpr
            (loop for runner on (rest expr) do
              (setf (first runner) (nreverse-expr (first runner)))
              finally (return expr) ) )
          ((:range)
            expr )
          ((reverse) ; subexpr
            (if (eq (second expr) :void) :void expr) )
          ((string-equal)
            expr )
          ((union)
            expr )
          ((unless) ; subexpr
            (setf (second expr) (nreverse-expr (second expr)))
            expr )
          ((when) ; subexpr
            (setf (second expr) (nreverse-expr (second expr)))
            expr ))) )

    ;; resolve-name-ref
    ;;  Resolved a named capture reference.
    (resolve-name-ref (expr)
      (let ((nth (second expr)))
        (when (consp nth)
          (setf (second expr) (cdr nth)) ) )
      expr )

    ;; resolve-name-refs
    ;;  Resolved named capture references.
    (resolve-name-refs (expr)
      (cond
        ((not (consp expr)) expr)
        ((eq (first expr) '=) (resolve-name-ref expr))
        ((eq (first expr) 'equal) (resolve-name-ref expr))
        ((eq (first expr) 'boundp) (resolve-name-ref expr))
        (t
          (loop
            for runner on (rest expr) do
              (setf (first runner) (resolve-name-refs (first runner)))
            finally (return expr) ) )) )

    ;; simple-parse-error
    (simple-parse-error (control &rest args)
      (parse-error 'regex-simple-parse-error
        :format-control control
        :format-arguments args ) )

    ;; string/i-p
    (string/i-expr-p (expr)
      (and (consp expr) (eq (first expr) 'string-equal)) )

    ;; syntax-error
    (syntax-error ()
      (parse-error 'regex-syntax-error) )

    ;; unclosed-pair
    (unclosed-pair (open-char)
        (declare (type character open-char))
      (parse-error 'regex-unclosed-pair :char open-char) )

    ;; unget-char
    (unget-char ()
      (assert (not (eql pos offset)))
      (decf pos) )

    ;; unget-token
    (unget-token (token)
      (assert (null unget-token))
      (setq unget-token token) )

    ;; unexpected-char
    (unexpected-char (expected-char)
      (parse-error 'regex-unexpected-char :expected-char expected-char) )
    )
    ;;
    ;; parse-regex
    ;;
    (multiple-value-setq (source offset end) (string-data string start end))
    (setq pos offset)

    (let ((expr (parse nil)))
      (when name-refs
        (loop for ((name . nth) . ref-pos) in name-refs do
          (when (null nth)
            (setq pos ref-pos)
            (simple-parse-error "Capture \"~A\" isn't defined." name) ))
        (resolve-name-refs expr) )

      (when anon-refs
        (setq anon-refs (nreverse anon-refs))
        (loop for (index . ref-pos) in anon-refs do
          (when (> index max-capture-index)
            (setq pos ref-pos)
            (parse-error 'regex-unbound-group :index index)) ))

      (when from-end (setq expr (nreverse-expr expr)))

      (let ((name-vector (make-array max-capture-index
                            :initial-element nil ) ))

        (loop for (name . nth) in name-sets do
          (setf (svref name-vector (1- nth)) name) )

        (values expr name-vector from-end modifiers) ) ) ) ) )
