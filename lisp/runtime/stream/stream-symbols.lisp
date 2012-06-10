(in-package :ext)

(export '(
    stream-advance-to-column
    stream-clear-input
    stream-clear-output
    stream-finish-output
    stream-force-output
    stream-fresh-line
    stream-line-number
    stream-line-column
    stream-listen
    stream-output-width
    stream-peek-char
    stream-read-byte
    stream-read-bytes
    stream-read-char
    stream-read-char-no-hang
    stream-read-line
    stream-start-line-p
    stream-terpri
    stream-unread-char
    stream-write-byte
    stream-write-bytes
    stream-write-char
    stream-write-string
    stream-yes-or-no-p
    stream-y-or-n-p
 ) )

(in-package :si)

(defconstant STREAM-FLAG-PROBE     0)
(defconstant STREAM-FLAG-INPUT     1)
(defconstant STREAM-FLAG-OUTPUT    2)
(defconstant STREAM-FLAG-DIRMASK      3)

(defconstant STREAM-FLAG-CLOSED         4)
(defconstant STREAM-FLAG-INTERACTIVE    8)
