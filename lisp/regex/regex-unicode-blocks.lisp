;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;; regex - Unicode Block Name Table
;;; lisp/regex/regex-unicode-blocks.lips
;;;
;;; This is automatically generated file. DO NOT EDIT.
;
(in-package :si)


(defvar *regex-unicode-block-table*
  (let ((htb (make-hash-table :test 'equal)))
    (setf (gethash "BASICLATIN" htb) '(:range #\u0000 #\Rubout))
    (setf (gethash "LATINSUPPLEMENT" htb) '(:range #\u0080 #\u00FF))
    (setf (gethash "LATINEXTENDEDA" htb) '(:range #\u0100 #\u017F))
    (setf (gethash "LATINEXTENDEDB" htb) '(:range #\u0180 #\u024F))
    (setf (gethash "IPAEXTENSIONS" htb) '(:range #\u0250 #\u02AF))
    (setf (gethash "SPACINGMODIFIERLETTERS" htb) '(:range #\u02B0 #\u02FF))
    (setf (gethash "COMBININGDIACRITICALMARKS" htb)
            '(:range #\u0300 #\u036F))
    (setf (gethash "GREEKANDCOPTIC" htb) '(:range #\u0370 #\u03FF))
    (setf (gethash "CYRILLIC" htb) '(:range #\u0400 #\u04FF))
    (setf (gethash "CYRILLICSUPPLEMENT" htb) '(:range #\u0500 #\u052F))
    (setf (gethash "ARMENIAN" htb) '(:range #\u0530 #\u058F))
    (setf (gethash "HEBREW" htb) '(:range #\u0590 #\u05FF))
    (setf (gethash "ARABIC" htb) '(:range #\u0600 #\u06FF))
    (setf (gethash "SYRIAC" htb) '(:range #\u0700 #\u074F))
    (setf (gethash "THAANA" htb) '(:range #\u0780 #\u07BF))
    (setf (gethash "DEVANAGARI" htb) '(:range #\u0900 #\u097F))
    (setf (gethash "BENGALI" htb) '(:range #\u0980 #\u09FF))
    (setf (gethash "GURMUKHI" htb) '(:range #\u0A00 #\u0A7F))
    (setf (gethash "GUJARATI" htb) '(:range #\u0A80 #\u0AFF))
    (setf (gethash "ORIYA" htb) '(:range #\u0B00 #\u0B7F))
    (setf (gethash "TAMIL" htb) '(:range #\u0B80 #\u0BFF))
    (setf (gethash "TELUGU" htb) '(:range #\u0C00 #\u0C7F))
    (setf (gethash "KANNADA" htb) '(:range #\u0C80 #\u0CFF))
    (setf (gethash "MALAYALAM" htb) '(:range #\u0D00 #\u0D7F))
    (setf (gethash "SINHALA" htb) '(:range #\u0D80 #\u0DFF))
    (setf (gethash "THAI" htb) '(:range #\u0E00 #\u0E7F))
    (setf (gethash "LAO" htb) '(:range #\u0E80 #\u0EFF))
    (setf (gethash "TIBETAN" htb) '(:range #\u0F00 #\u0FFF))
    (setf (gethash "MYANMAR" htb) '(:range #\u1000 #\u109F))
    (setf (gethash "GEORGIAN" htb) '(:range #\u10A0 #\u10FF))
    (setf (gethash "HANGULJAMO" htb) '(:range #\u1100 #\u11FF))
    (setf (gethash "ETHIOPIC" htb) '(:range #\u1200 #\u137F))
    (setf (gethash "CHEROKEE" htb) '(:range #\u13A0 #\u13FF))
    (setf (gethash "UNIFIEDCANADIANABORIGINALSYLLABICS" htb)
            '(:range #\u1400 #\u167F))
    (setf (gethash "OGHAM" htb) '(:range #\u1680 #\u169F))
    (setf (gethash "RUNIC" htb) '(:range #\u16A0 #\u16FF))
    (setf (gethash "TAGALOG" htb) '(:range #\u1700 #\u171F))
    (setf (gethash "HANUNOO" htb) '(:range #\u1720 #\u173F))
    (setf (gethash "BUHID" htb) '(:range #\u1740 #\u175F))
    (setf (gethash "TAGBANWA" htb) '(:range #\u1760 #\u177F))
    (setf (gethash "KHMER" htb) '(:range #\u1780 #\u17FF))
    (setf (gethash "MONGOLIAN" htb) '(:range #\u1800 #\u18AF))
    (setf (gethash "LIMBU" htb) '(:range #\u1900 #\u194F))
    (setf (gethash "TAILE" htb) '(:range #\u1950 #\u197F))
    (setf (gethash "KHMERSYMBOLS" htb) '(:range #\u19E0 #\u19FF))
    (setf (gethash "PHONETICEXTENSIONS" htb) '(:range #\u1D00 #\u1D7F))
    (setf (gethash "LATINEXTENDEDADDITIONAL" htb) '(:range #\u1E00 #\u1EFF))
    (setf (gethash "GREEKEXTENDED" htb) '(:range #\u1F00 #\u1FFF))
    (setf (gethash "GENERALPUNCTUATION" htb) '(:range #\u2000 #\u206F))
    (setf (gethash "SUPERSCRIPTSANDSUBSCRIPTS" htb)
            '(:range #\u2070 #\u209F))
    (setf (gethash "CURRENCYSYMBOLS" htb) '(:range #\u20A0 #\u20CF))
    (setf (gethash "COMBININGDIACRITICALMARKSFORSYMBOLS" htb)
            '(:range #\u20D0 #\u20FF))
    (setf (gethash "LETTERLIKESYMBOLS" htb) '(:range #\u2100 #\u214F))
    (setf (gethash "NUMBERFORMS" htb) '(:range #\u2150 #\u218F))
    (setf (gethash "ARROWS" htb) '(:range #\u2190 #\u21FF))
    (setf (gethash "MATHEMATICALOPERATORS" htb) '(:range #\u2200 #\u22FF))
    (setf (gethash "MISCELLANEOUSTECHNICAL" htb) '(:range #\u2300 #\u23FF))
    (setf (gethash "CONTROLPICTURES" htb) '(:range #\u2400 #\u243F))
    (setf (gethash "OPTICALCHARACTERRECOGNITION" htb)
            '(:range #\u2440 #\u245F))
    (setf (gethash "ENCLOSEDALPHANUMERICS" htb) '(:range #\u2460 #\u24FF))
    (setf (gethash "BOXDRAWING" htb) '(:range #\u2500 #\u257F))
    (setf (gethash "BLOCKELEMENTS" htb) '(:range #\u2580 #\u259F))
    (setf (gethash "GEOMETRICSHAPES" htb) '(:range #\u25A0 #\u25FF))
    (setf (gethash "MISCELLANEOUSSYMBOLS" htb) '(:range #\u2600 #\u26FF))
    (setf (gethash "DINGBATS" htb) '(:range #\u2700 #\u27BF))
    (setf (gethash "MISCELLANEOUSMATHEMATICALSYMBOLSA" htb)
            '(:range #\u27C0 #\u27EF))
    (setf (gethash "SUPPLEMENTALARROWSA" htb) '(:range #\u27F0 #\u27FF))
    (setf (gethash "BRAILLEPATTERNS" htb) '(:range #\u2800 #\u28FF))
    (setf (gethash "SUPPLEMENTALARROWSB" htb) '(:range #\u2900 #\u297F))
    (setf (gethash "MISCELLANEOUSMATHEMATICALSYMBOLSB" htb)
            '(:range #\u2980 #\u29FF))
    (setf (gethash "SUPPLEMENTALMATHEMATICALOPERATORS" htb)
            '(:range #\u2A00 #\u2AFF))
    (setf (gethash "MISCELLANEOUSSYMBOLSANDARROWS" htb)
            '(:range #\u2B00 #\u2BFF))
    (setf (gethash "CJKRADICALSSUPPLEMENT" htb) '(:range #\u2E80 #\u2EFF))
    (setf (gethash "KANGXIRADICALS" htb) '(:range #\u2F00 #\u2FDF))
    (setf (gethash "IDEOGRAPHICDESCRIPTIONCHARACTERS" htb)
            '(:range #\u2FF0 #\u2FFF))
    (setf (gethash "CJKSYMBOLSANDPUNCTUATION" htb)
            '(:range #\u3000 #\u303F))
    (setf (gethash "HIRAGANA" htb) '(:range #\u3040 #\u309F))
    (setf (gethash "KATAKANA" htb) '(:range #\u30A0 #\u30FF))
    (setf (gethash "BOPOMOFO" htb) '(:range #\u3100 #\u312F))
    (setf (gethash "HANGULCOMPATIBILITYJAMO" htb) '(:range #\u3130 #\u318F))
    (setf (gethash "KANBUN" htb) '(:range #\u3190 #\u319F))
    (setf (gethash "BOPOMOFOEXTENDED" htb) '(:range #\u31A0 #\u31BF))
    (setf (gethash "KATAKANAPHONETICEXTENSIONS" htb)
            '(:range #\u31F0 #\u31FF))
    (setf (gethash "ENCLOSEDCJKLETTERSANDMONTHS" htb)
            '(:range #\u3200 #\u32FF))
    (setf (gethash "CJKCOMPATIBILITY" htb) '(:range #\u3300 #\u33FF))
    (setf (gethash "CJKUNIFIEDIDEOGRAPHSEXTENSIONA" htb)
            '(:range #\u3400 #\u4DBF))
    (setf (gethash "YIJINGHEXAGRAMSYMBOLS" htb) '(:range #\u4DC0 #\u4DFF))
    (setf (gethash "CJKUNIFIEDIDEOGRAPHS" htb) '(:range #\u4E00 #\u9FFF))
    (setf (gethash "YISYLLABLES" htb) '(:range #\uA000 #\uA48F))
    (setf (gethash "YIRADICALS" htb) '(:range #\uA490 #\uA4CF))
    (setf (gethash "HANGULSYLLABLES" htb) '(:range #\uAC00 #\uD7AF))
    (setf (gethash "HIGHSURROGATES" htb) '(:range #\uD800 #\uDB7F))
    (setf (gethash "HIGHPRIVATEUSESURROGATES" htb)
            '(:range #\uDB80 #\uDBFF))
    (setf (gethash "LOWSURROGATES" htb) '(:range #\uDC00 #\uDFFF))
    (setf (gethash "PRIVATEUSEAREA" htb) '(:range #\uE000 #\uF8FF))
    (setf (gethash "CJKCOMPATIBILITYIDEOGRAPHS" htb)
            '(:range #\uF900 #\uFAFF))
    (setf (gethash "ALPHABETICPRESENTATIONFORMS" htb)
            '(:range #\uFB00 #\uFB4F))
    (setf (gethash "ARABICPRESENTATIONFORMSA" htb)
            '(:range #\uFB50 #\uFDFF))
    (setf (gethash "VARIATIONSELECTORS" htb) '(:range #\uFE00 #\uFE0F))
    (setf (gethash "COMBININGHALFMARKS" htb) '(:range #\uFE20 #\uFE2F))
    (setf (gethash "CJKCOMPATIBILITYFORMS" htb) '(:range #\uFE30 #\uFE4F))
    (setf (gethash "SMALLFORMVARIANTS" htb) '(:range #\uFE50 #\uFE6F))
    (setf (gethash "ARABICPRESENTATIONFORMSB" htb)
            '(:range #\uFE70 #\uFEFF))
    (setf (gethash "HALFWIDTHANDFULLWIDTHFORMS" htb)
            '(:range #\uFF00 #\uFFEF))
    (setf (gethash "SPECIALS" htb) '(:range #\uFFF0 #\uFFFF))
    htb))

; EOF
