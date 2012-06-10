      (setf (logical-pathname-translations "SYS") '(
        ("ROOT;**;"             "c:/home/src/evcl3-1708/**/")
        ("SITE;*.TRANSLATIONS"  "SYS:ROOT;*.xlts")
        ("SOURCE;**;"           "SYS:ROOT;LISP;**;")
        ("SYSTEM;"              "SYS:ROOT;LISP;")

        ("TEST;*"               "SYS:ROOT;LISP;TEST;*.TEST")

        ("DEBUG;**;"            "SYS:ROOT;DEBUG;LISP;**;")
        ("RELEASE;**;"          "SYS:ROOT;RELEASE;LISP;**;")
        ) )
