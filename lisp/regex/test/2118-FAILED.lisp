(regex-test-all)
; Try "evita.retest"
;  0/32, skip=0
; Try "perl-evita.retest"
;  0/106, skip=0
; Try "perl-504.retest"
; GC Fence 4,091,760 => 1,615,528
;  0/144, skip=0
; Try "perl-584.retest"
; GC Fence 4,305,808 => 1,665,712
; GC Fence 4,286,368 => 1,716,392
;  0/945, skip=20
; Try "onig.retest"
;   FAIL:
(TEST-CASE "onig/180" "[[:upper]]" ":"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST ":"))
;     Result: NIL
;     Expect: (":")
;   FAIL:
(TEST-CASE "onig/189" "[[ab]]" "b"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "b"))
;     Result: NIL
;     Expect: ("b")
;   FAIL:
(TEST-CASE "onig/190" "[[ab]c]" "c"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "c"))
;     Result: NIL
;     Expect: ("c")
;   FAIL:
(TEST-CASE "onig/193" "[[ab]&&bc]" "b"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "b"))
;     Result: NIL
;     Expect: ("b")
;   FAIL:
(TEST-CASE "onig/198" "[[^a&&a]&&a-z]" "b"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "b"))
;     Result: NIL
;     Expect: ("b")
;   FAIL:
(TEST-CASE "onig/200" "[[^a-z&&bcdef]&&[^c-g]]" "h"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "h"))
;     Result: NIL
;     Expect: ("h")
;   FAIL:
(TEST-CASE "onig/202" "[^[^abc]&&[^cde]]" "c"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "c"))
;     Result: NIL
;     Expect: ("c")
;   FAIL:
(TEST-CASE "onig/203" "[^[^abc]&&[^cde]]" "e"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "e"))
;     Result: NIL
;     Expect: ("e")
;   FAIL:
(TEST-CASE "onig/249" "(?i:[A-c])" "D"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "D"))
;     Result: NIL
;     Expect: ("D")
;   FAIL:
(TEST-CASE "onig/254" "(?i:[!-k])" "Z"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "Z"))
;     Result: NIL
;     Expect: ("Z")
;   FAIL:
(TEST-CASE "onig/256" "(?i:[T-}])" "b"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "b"))
;     Result: NIL
;     Expect: ("b")
;   FAIL:
(TEST-CASE "onig/261" "(?m:.)" (BACKSLASH "\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST (BACKSLASH "\\n")))
;     Result: NIL
;     Expect: ("
")
;   FAIL:
(TEST-CASE "onig/262" "(?m:a.)" (BACKSLASH "a\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST (BACKSLASH "a\\n")))
;     Result: NIL
;     Expect: ("a
")
;   FAIL:
(TEST-CASE "onig/263" "(?m:.b)" (BACKSLASH "a\\nb")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST (BACKSLASH "\\nb")))
;     Result: NIL
;     Expect: ("
b")
;   FAIL:
(TEST-CASE "onig/311" "[b-d]|[^e-z]" "a"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "a"))
;     Result: NIL
;     Expect: ("a")
;   FAIL:
(TEST-CASE "onig/317" "(?>a|abd)c" "abdc"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("abdc")
;     Expect: NIL
; GC Fence 4,305,952 => 1,768,336
;   FAIL:
(TEST-CASE "onig/414" "((?m:a.c))" (BACKSLASH "a\\nc")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (QUOTE *) (BACKSLASH "a\\nc")))
;     Result: NIL
;     Expect: (* "a
c")
;   FAIL:
(TEST-CASE "onig/508" "(?:()|())*\\1\\2" ""
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST ""))
;     Result: NIL
;     Expect: ("")
;   FAIL:
(TEST-CASE "onig/512" "(?:()|()|()|()|()|())*\\2\\5" ""
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST ""))
;     Result: NIL
;     Expect: ("")
;   FAIL:
(TEST-CASE "onig/513" "(?:()|()|()|(x)|()|())*\\2b\\5" "b"
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "b"))
;     Result: NIL
;     Expect: ("b")
;   FAIL:
(TEST-CASE "onig/564" "\\b" (BACKSLASH " \\u307B")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST ""))
;     Result: NIL
;     Expect: ("")
;   FAIL:
(TEST-CASE "onig/611" (BACKSLASH "(?m:\\u3088.)")
 (BACKSLASH "\\u3088\\n") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3088\\n")))
;     Result: NIL
;     Expect: ("よ
")
;   FAIL:
(TEST-CASE "onig/612" (BACKSLASH "(?m:.\\u3081)")
 (BACKSLASH "\\u307E\\n\\u3081") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\n\\u3081")))
;     Result: NIL
;     Expect: ("
め")
;   FAIL:
(TEST-CASE "onig/654"
 (BACKSLASH "[\\u3044-\\u3051]|[^\\u304B-\\u3053]")
 (BACKSLASH "\\u3042") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3042")))
;     Result: NIL
;     Expect: ("あ")
;   FAIL:
(TEST-CASE "onig/655"
 (BACKSLASH "[\\u3044-\\u3051]|[^\\u304B-\\u3053]")
 (BACKSLASH "\\u304B") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u304B")))
;     Result: NIL
;     Expect: ("か")
;   FAIL:
(TEST-CASE "onig/663"
 (BACKSLASH "(?>\\u3042|\\u3042\\u3044\\u3048)\\u3046")
 (BACKSLASH "\\u3042\\u3044\\u3048\\u3046")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("あいえう")
;     Expect: NIL
;   FAIL:
(TEST-CASE "onig/755" (BACKSLASH "((?m:\\u3042.\\u3046))")
 (BACKSLASH "\\u3042\\n\\u3046") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (QUOTE *) (BACKSLASH "\\u3042\\n\\u3046")))
;     Result: NIL
;     Expect: (* "あ
う")
;   FAIL:
(TEST-CASE "onig/799" (BACKSLASH "[[\\u3072\\u3075]]")
 (BACKSLASH "\\u3075") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3075")))
;     Result: NIL
;     Expect: ("ふ")
;   FAIL:
(TEST-CASE "onig/800" (BACKSLASH "[[\\u3044\\u304A\\u3046]\\u304B]")
 (BACKSLASH "\\u304B") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u304B")))
;     Result: NIL
;     Expect: ("か")
;   FAIL:
(TEST-CASE "onig/803" (BACKSLASH "[^[^\\u3042]]") (BACKSLASH "\\u3042")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST (BACKSLASH "\\u3042")))
;     Result: NIL
;     Expect: ("あ")
;   FAIL:
(TEST-CASE "onig/804"
 (BACKSLASH "[[\\u304B\\u304D\\u304F]&&\\u304D\\u304F]")
 (BACKSLASH "\\u304F") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u304F")))
;     Result: NIL
;     Expect: ("く")
;   FAIL:
(TEST-CASE "onig/809"
 (BACKSLASH "[[^\\u3042&&\\u3042]&&\\u3042-\\u3093]")
 (BACKSLASH "\\u3044") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3044")))
;     Result: NIL
;     Expect: ("い")
;   FAIL:
(TEST-CASE "onig/811"
 (BACKSLASH
  "[[^\\u3042-\\u3093&&\\u3044\\u3046\\u3048\\u304A]&&[^\\u3046-\\u304B]]")
 (BACKSLASH "\\u304D") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u304D")))
;     Result: NIL
;     Expect: ("き")
;   FAIL:
(TEST-CASE "onig/813"
 (BACKSLASH "[^[^\\u3042\\u3044\\u3046]&&[^\\u3046\\u3048\\u304A]]")
 (BACKSLASH "\\u3046") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3046")))
;     Result: NIL
;     Expect: ("う")
;   FAIL:
(TEST-CASE "onig/814"
 (BACKSLASH "[^[^\\u3042\\u3044\\u3046]&&[^\\u3046\\u3048\\u304A]]")
 (BACKSLASH "\\u3048") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3048")))
;     Result: NIL
;     Expect: ("え")
;   FAIL:
(TEST-CASE "onig/817"
 (BACKSLASH
  "[^[^a-z\\u3042\\u3044\\u3046]&&[^bcdefg\\u3046\\u3048\\u304A]q-w]")
 (BACKSLASH "\\u3048") (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3048")))
;     Result: NIL
;     Expect: ("え")
;   FAIL:
(TEST-CASE "onig/818"
 (BACKSLASH
  "[^[^a-z\\u3042\\u3044\\u3046]&&[^bcdefg\\u3046\\u3048\\u304A]g-w]")
 "f" (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "f"))
;     Result: NIL
;     Expect: ("f")
;   FAIL:
(TEST-CASE "onig/819"
 (BACKSLASH
  "[^[^a-z\\u3042\\u3044\\u3046]&&[^bcdefg\\u3046\\u3048\\u304A]g-w]")
 "g" (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST "g"))
;     Result: NIL
;     Expect: ("g")
; GC Fence 4,260,648 => 1,772,496
;   FAIL:
(TEST-CASE "onig/869" "[\\xc2\\x80-\\xed\\x9f\\xbe]"
 (BACKSLASH "\\u00ED\\u009F\\u00BF")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("í")
;     Expect: NIL
;   FAIL:
(TEST-CASE "onig/872" "[\\xc2\\x80-\\xed\\x9f\\xbe]"
 (BACKSLASH "\\u00ED\\u009F\\u00BF")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("í")
;     Expect: NIL
;   FAIL:
(TEST-CASE "onig/875" "[\\xc2\\x80-\\xed\\x9f\\xbe]"
 (BACKSLASH "\\u00ED\\u009F\\u00BF")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("í")
;     Expect: NIL
;   FAIL:
(TEST-CASE "onig/878" "[\\xc3\\xad\\xed\\x9f\\xbe]"
 (BACKSLASH "\\u00ED\\u009F\\u00BF")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("í")
;     Expect: NIL
;   FAIL:
(TEST-CASE "onig/881" "[\\xc4\\x80-\\xed\\x9f\\xbe]"
 (BACKSLASH "\\u00ED\\u009F\\u00BF")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("í")
;     Expect: NIL
;   FAIL:
(TEST-CASE "onig/893" "[\\xfe\\xff\\xc3\\x80]"
 (BACKSLASH "\\u00C3\\u00BE\\u00C3\\u00BF")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST))
;     Result: ("Ã")
;     Expect: NIL
;   FAIL:
(TEST-CASE "onig/910" (BACKSLASH "\\u3002$")
 (BACKSLASH
  "\\u6226\\u5F8C\\u306E\\u65E5\\u672C\\u306B\\u304A\\u3044\\u3066\\u306F\\u3001\\u65E7\\u8ECD\\u306B\\u3064\\u3044\\u3066\\u306F\\u8ABF\\u67FB\\u306B\\u57FA\\u3065\\u304B\\u306C\\u6279\\u5224\\u3082\\u8A31\\u3055\\u308C\\u308B\\u98A8\\u6F6E\\u3082\\u751F\\u3058\\u3001\\n\\u305F\\u3068\\u3048\\u3070\\u4E09\\u5341\\u516B\\u5E74\\u5F0F\\u6B69\\u5175\\u9283\\u306E\\u5236\\u5B9A\\u5E74\\u304C\\u65E5\\u9732\\u6226\\u4E89\\u306E\\u7D42\\u3063\\u305F\\u5E74\\u3067\\u3042\\u308B\\u3053\\u3068\\u3092\\u3082\\u3063\\u3066\\u8EFD\\u5FFD\\u306B\\n\\u65E7\\u8ECD\\u306E\\u65E7\\u5F0F\\u3076\\u308A\\u3092\\u8A87\\u5F35\\u3059\\u308B\\u8AD6\\u8A55\\u304C\\u307E\\u304B\\u308A\\u3068\\u304A\\u3063\\u3066\\u3044\\u308B\\u3002\\n\\u6709\\u540D\\u306A\\u8AD6\\u8005\\u3068\\u3057\\u3066\\u306F\\u3001\\u6545\\u30FB\\u53F8\\u99AC\\u907C\\u592A\\u90CE\\u3092\\u6319\\u3052\\u308B\\u3053\\u3068\\u304C\\u3067\\u304D\\u308B\\u3060\\u308D\\u3046\\u3002\\n\\n\\u5175\\u85E4\\u4E8C\\u5341\\u516B \\u300C\\u6709\\u5742\\u9283\\u300D \\u56DB\\u8C37\\u30E9\\u30A6\\u30F3\\u30C9 (1998)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST (BACKSLASH "\\u3002")))
;     Result: NIL
;     Expect: ("。")
;   FAIL:
(TEST-CASE "onig/911" (BACKSLASH "(^\\u5175\\u85E4..\\u516B)")
 (BACKSLASH
  "\\u6226\\u5F8C\\u306E\\u65E5\\u672C\\u306B\\u304A\\u3044\\u3066\\u306F\\u3001\\u65E7\\u8ECD\\u306B\\u3064\\u3044\\u3066\\u306F\\u8ABF\\u67FB\\u306B\\u57FA\\u3065\\u304B\\u306C\\u6279\\u5224\\u3082\\u8A31\\u3055\\u308C\\u308B\\u98A8\\u6F6E\\u3082\\u751F\\u3058\\u3001\\n\\u305F\\u3068\\u3048\\u3070\\u4E09\\u5341\\u516B\\u5E74\\u5F0F\\u6B69\\u5175\\u9283\\u306E\\u5236\\u5B9A\\u5E74\\u304C\\u65E5\\u9732\\u6226\\u4E89\\u306E\\u7D42\\u3063\\u305F\\u5E74\\u3067\\u3042\\u308B\\u3053\\u3068\\u3092\\u3082\\u3063\\u3066\\u8EFD\\u5FFD\\u306B\\n\\u65E7\\u8ECD\\u306E\\u65E7\\u5F0F\\u3076\\u308A\\u3092\\u8A87\\u5F35\\u3059\\u308B\\u8AD6\\u8A55\\u304C\\u307E\\u304B\\u308A\\u3068\\u304A\\u3063\\u3066\\u3044\\u308B\\u3002\\n\\u6709\\u540D\\u306A\\u8AD6\\u8005\\u3068\\u3057\\u3066\\u306F\\u3001\\u6545\\u30FB\\u53F8\\u99AC\\u907C\\u592A\\u90CE\\u3092\\u6319\\u3052\\u308B\\u3053\\u3068\\u304C\\u3067\\u304D\\u308B\\u3060\\u308D\\u3046\\u3002\\n\\n\\u5175\\u85E4\\u4E8C\\u5341\\u516B \\u300C\\u6709\\u5742\\u9283\\u300D \\u56DB\\u8C37\\u30E9\\u30A6\\u30F3\\u30C9 (1998)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (QUOTE *) (BACKSLASH "\\u5175\\u85E4\\u4E8C\\u5341\\u516B")))
;     Result: NIL
;     Expect: (* "兵藤二十八")
;   FAIL:
(TEST-CASE "onig/912" "^$"
 (BACKSLASH
  "\\u6226\\u5F8C\\u306E\\u65E5\\u672C\\u306B\\u304A\\u3044\\u3066\\u306F\\u3001\\u65E7\\u8ECD\\u306B\\u3064\\u3044\\u3066\\u306F\\u8ABF\\u67FB\\u306B\\u57FA\\u3065\\u304B\\u306C\\u6279\\u5224\\u3082\\u8A31\\u3055\\u308C\\u308B\\u98A8\\u6F6E\\u3082\\u751F\\u3058\\u3001\\n\\u305F\\u3068\\u3048\\u3070\\u4E09\\u5341\\u516B\\u5E74\\u5F0F\\u6B69\\u5175\\u9283\\u306E\\u5236\\u5B9A\\u5E74\\u304C\\u65E5\\u9732\\u6226\\u4E89\\u306E\\u7D42\\u3063\\u305F\\u5E74\\u3067\\u3042\\u308B\\u3053\\u3068\\u3092\\u3082\\u3063\\u3066\\u8EFD\\u5FFD\\u306B\\n\\u65E7\\u8ECD\\u306E\\u65E7\\u5F0F\\u3076\\u308A\\u3092\\u8A87\\u5F35\\u3059\\u308B\\u8AD6\\u8A55\\u304C\\u307E\\u304B\\u308A\\u3068\\u304A\\u3063\\u3066\\u3044\\u308B\\u3002\\n\\u6709\\u540D\\u306A\\u8AD6\\u8005\\u3068\\u3057\\u3066\\u306F\\u3001\\u6545\\u30FB\\u53F8\\u99AC\\u907C\\u592A\\u90CE\\u3092\\u6319\\u3052\\u308B\\u3053\\u3068\\u304C\\u3067\\u304D\\u308B\\u3060\\u308D\\u3046\\u3002\\n\\n\\u5175\\u85E4\\u4E8C\\u5341\\u516B \\u300C\\u6709\\u5742\\u9283\\u300D \\u56DB\\u8C37\\u30E9\\u30A6\\u30F3\\u30C9 (1998)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST ""))
;     Result: NIL
;     Expect: ("")
;   FAIL:
(TEST-CASE "onig/929" (BACKSLASH "\\u3002$")
 (BACKSLASH
  "\\u30AB\\u30CA\\u3084\\u30ED\\u30FC\\u30DE\\u5B57\\u306F\\u4E00\\u4F53\\u6587\\u5B57\\u3067\\u3042\\u308D\\u3046\\u304B\\u3002\\n\\u3082\\u3057\\u3053\\u3068\\u3070\\u3092\\u3057\\u308B\\u3059\\u3082\\u306E\\u304C\\u6587\\u5B57\\u3067\\u3042\\u308B\\u3068\\u3059\\u308B\\u3068\\u3001\\u305D\\u308C\\u306F\\u3053\\u3068\\u3070\\u3092\\u3057\\u308B\\u3059\\u3082\\u306E\\u3067\\u306F\\u306A\\u3044\\u3002\\n\\u672C\\u3084book\\u306F\\u3053\\u3068\\u3070\\u3067\\u3042\\u308B\\u304C\\u3001\\u30DB\\u30F3\\u3084hon\\u306F\\u97F3\\u3092\\u306A\\u3089\\u3079\\u305F\\u3060\\u3051\\u3067\\u3001\\u5341\\u5206\\u306A\\u5358\\u8A9E\\u6027\\u3092\\n\\u3082\\u3064\\u3082\\u306E\\u3067\\u306F\\u306A\\u3044\\u3002\\n\\u5358\\u8A9E\\u3068\\u3057\\u3066\\u306E\\u7279\\u5B9A\\u306E\\u5F62\\u614B\\u3092\\u3082\\u305F\\u306A\\u3044\\u304B\\u3089\\u3067\\u3042\\u308B\\u3002\\n\\u300C\\u5F62\\u306B\\u3088\\u308B\\u8A9E\\u300D\\u3092\\u30A2\\u30E9\\u30F3\\u306F\\u6F22\\u5B57\\u306B\\u5BFE\\u3059\\u308B\\u8EFD\\u8511\\u7684\\u306A\\u610F\\u5473\\u306B\\u7528\\u3044\\u305F\\u304C\\u3001\\n\\u5F62\\u306E\\u306A\\u3044\\u3082\\u306E\\u306F\\u672C\\u5F53\\u306F\\u8A9E\\u3067\\u306F\\u3042\\u308A\\u3048\\u306A\\u3044\\u306E\\u3067\\u3042\\u308B\\u3002\\n\\n\\u767D\\u5DDD\\u9759 \\u300C\\u6F22\\u5B57\\u767E\\u8A71\\u300D\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST (BACKSLASH "\\u3002")))
;     Result: NIL
;     Expect: ("。")
;   FAIL:
(TEST-CASE "onig/931" "^$"
 (BACKSLASH
  "\\u30AB\\u30CA\\u3084\\u30ED\\u30FC\\u30DE\\u5B57\\u306F\\u4E00\\u4F53\\u6587\\u5B57\\u3067\\u3042\\u308D\\u3046\\u304B\\u3002\\n\\u3082\\u3057\\u3053\\u3068\\u3070\\u3092\\u3057\\u308B\\u3059\\u3082\\u306E\\u304C\\u6587\\u5B57\\u3067\\u3042\\u308B\\u3068\\u3059\\u308B\\u3068\\u3001\\u305D\\u308C\\u306F\\u3053\\u3068\\u3070\\u3092\\u3057\\u308B\\u3059\\u3082\\u306E\\u3067\\u306F\\u306A\\u3044\\u3002\\n\\u672C\\u3084book\\u306F\\u3053\\u3068\\u3070\\u3067\\u3042\\u308B\\u304C\\u3001\\u30DB\\u30F3\\u3084hon\\u306F\\u97F3\\u3092\\u306A\\u3089\\u3079\\u305F\\u3060\\u3051\\u3067\\u3001\\u5341\\u5206\\u306A\\u5358\\u8A9E\\u6027\\u3092\\n\\u3082\\u3064\\u3082\\u306E\\u3067\\u306F\\u306A\\u3044\\u3002\\n\\u5358\\u8A9E\\u3068\\u3057\\u3066\\u306E\\u7279\\u5B9A\\u306E\\u5F62\\u614B\\u3092\\u3082\\u305F\\u306A\\u3044\\u304B\\u3089\\u3067\\u3042\\u308B\\u3002\\n\\u300C\\u5F62\\u306B\\u3088\\u308B\\u8A9E\\u300D\\u3092\\u30A2\\u30E9\\u30F3\\u306F\\u6F22\\u5B57\\u306B\\u5BFE\\u3059\\u308B\\u8EFD\\u8511\\u7684\\u306A\\u610F\\u5473\\u306B\\u7528\\u3044\\u305F\\u304C\\u3001\\n\\u5F62\\u306E\\u306A\\u3044\\u3082\\u306E\\u306F\\u672C\\u5F53\\u306F\\u8A9E\\u3067\\u306F\\u3042\\u308A\\u3048\\u306A\\u3044\\u306E\\u3067\\u3042\\u308B\\u3002\\n\\n\\u767D\\u5DDD\\u9759 \\u300C\\u6F22\\u5B57\\u767E\\u8A71\\u300D\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST ""))
;     Result: NIL
;     Expect: ("")
;   FAIL:
(TEST-CASE "onig/945" (BACKSLASH "^\\uFF0D\\uFF0D ")
 (BACKSLASH
  "\\u91C8\\u8FE6\\u304C\\u53E1\\u5C71\\u306B\\u304F\\u3060\\u3063\\u3066\\u304D\\u305F\\u3068\\u3059\\u308C\\u3070\\u3001\\u305D\\u306E\\u304A\\u3073\\u305F\\u3060\\u3057\\u3044\\u5BC6\\u6559\\u7F8E\\u8853\\u306E\\u91CF\\u3068\\u3001\\n\\u305D\\u306E\\u8CEA\\u306E\\u9AD8\\u3055\\u306B\\u304A\\u3069\\u308D\\u304F\\u306B\\u3061\\u304C\\u3044\\u306A\\u3044\\u3002\\n\\u3053\\u306E\\u899A\\u8005\\u304C\\u3001\\u5727\\u5012\\u7684\\u306A\\u9A5A\\u304D\\u3092\\u3082\\u3064\\u306E\\u306F\\u3001\\u304A\\u4E0D\\u52D5\\u3055\\u3093\\u306E\\u50CF\\u306E\\u524D\\u306B\\u7ACB\\u3063\\u305F\\u3068\\u304D\\u3060\\u308D\\u3046\\u3002\\n\\uFF0D\\uFF0D \\u3053\\u308C\\u306F\\u3001\\u30C9\\u30E9\\u30F4\\u30A3\\u30C0\\u4EBA\\u306E\\u5C11\\u5E74\\u5974\\u96B7\\u3067\\u306F\\u306A\\u3044\\u304B\\u3002\\n\\n\\u53F8\\u99AC\\u907C\\u592A\\u90CE \\u300C\\u53E1\\u5C71\\u7F8E\\u8853\\u306E\\u5C55\\u958B\\uFF0D\\u4E0D\\u52D5\\u660E\\u738B\\u306B\\u3075\\u308C\\u3064\\u3064\\u300D \\u30A2\\u30B5\\u30D2\\u30B0\\u30E9\\u30D5(1986)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\uFF0D\\uFF0D ")))
;     Result: NIL
;     Expect: ("－－ ")
;   FAIL:
(TEST-CASE "onig/963" (BACKSLASH "\\u3002$")
 (BACKSLASH
  "\\u304B\\u3068\\u3044\\u3063\\u3066\\u3001\\u6240\\u8A6E\\u306F\\u3001\\u5BFA\\u5185\\u541B\\u3082\\u3001\\u9ED2\\u5CA9\\u541B\\u3082\\u3001\\u305D\\u3057\\u3066\\u3082\\u3046\\u4E00\\u4EBA\\u306E\\u4EBA\\u7269\\u3082\\u3001\\u53E3\\u820C\\u306E\\u5F92\\u306B\\u3059\\u304E\\u306A\\u3044\\u3053\\u3068\\u3092\\u3001\\u3053\\u306E\\u7B2C\\u4E03\\u53F7\\u306F\\u5982\\u5B9F\\u306B\\u7269\\u8A9E\\u3063\\u3066\\u3044\\u308B\\u3002\\n\\u304B\\u308C\\u3089\\u4E09\\u4EBA\\u306E\\u5C0F\\u8AAC\\u306F\\u4E00\\u884C\\u3082\\u51FA\\u3066\\u3044\\u306A\\u3044\\u306E\\u3060\\u3002\\n\\u66F8\\u304F\\u3072\\u307E\\u304C\\u306A\\u304B\\u3063\\u305F\\u306E\\u3067\\u3042\\u308D\\u3046\\u3002\\n\\u3057\\u304B\\u3057\\u3001\\u96D1\\u8A8C\\u300C\\u8FD1\\u4EE3\\u8AAC\\u8A71\\u300D\\u304C\\u3001\\u306A\\u304A\\u7B2C\\u516B\\u53F7\\u3082\\u7B2C\\u4E5D\\u53F7\\u3082\\u51FA\\u3064\\u3065\\u3051\\u3066\\u3086\\u304F\\u3067\\u3042\\u308D\\u3046\\u3053\\u3068\\u306B\\u3064\\u3044\\u3066\\u306F\\u3001\\u79C1\\u306F\\u3076\\u304D\\u307F\\u306A\\u307B\\u3069\\u306E\\u78BA\\u4FE1\\u3092\\u3082\\u3063\\u3066\\u3044\\u308B\\u3002\\u3053\\u306E\\u96D1\\u8A8C\\u306B\\u306F\\u3001\\u4E8B\\u52D9\\u80FD\\u529B\\u306E\\u9B54\\u7269\\u306E\\u3088\\u3046\\u306A\\u4EBA\\u7269\\u304C\\u3001\\u4E09\\u4EBA\\u3082\\u3044\\u308B\\u3002\\n\\u305D\\u308C\\u3092\\u601D\\u3046\\u3068\\u3001\\u3068\\u304D\\u3069\\u304D\\u305F\\u3081\\u606F\\u306E\\u51FA\\u308B\\u3088\\u3046\\u306A\\u304A\\u3082\\u3044\\u304C\\u3059\\u308B\\u306E\\u3067\\u3042\\u308B\\u3002\\n\\n\\u53F8\\u99AC\\u907C\\u592A\\u90CE \\u300C\\u3053\\u3093\\u306A\\u96D1\\u8A8C\\u3084\\u3081\\u3066\\u3057\\u307E\\u3044\\u305F\\u3044\\u300D \\u8FD1\\u4EE3\\u8AAC\\u8A71 \\u7B2C\\u4E03\\u96C6 (1961)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T)) (LIST (BACKSLASH "\\u3002")))
;     Result: NIL
;     Expect: ("。")
;   FAIL:
(TEST-CASE "onig/990" (BACKSLASH "^\\u305D\\u308C\\u306F")
 (BACKSLASH
  "\\u3053\\u3046\\u3057\\u305F\\u65E5\\u672C\\u4EBA\\u306E\\u6B66\\u5668\\u306B\\u5BFE\\u3059\\u308B\\u5909\\u308F\\u3063\\u305F\\u614B\\u5EA6\\u306E\\u88CF\\u306B\\u306F\\u3001\\u3058\\u3064\\u306F\\u3001\\n\\u4E00\\u8CAB\\u3057\\u305F\\u9078\\u629E\\u57FA\\u6E96\\u304C\\u50CD\\u3044\\u3066\\u3044\\u305F\\u3002\\n\\u305D\\u308C\\u306F\\u3001\\u305D\\u306E\\u6B66\\u5668\\u304C\\u300C\\u4E3B\\u5175\\u3092\\u9AD8\\u7D1A\\u306B\\u898B\\u305B\\u308B\\u304B\\u3069\\u3046\\u304B\\u300D\\u3067\\u3042\\u3063\\u305F\\u3002\\n\\n\\u5175\\u85E4\\u4E8C\\u5341\\u516B \\u300C\\u6709\\u5742\\u9283\\u300D \\u56DB\\u8C37\\u30E9\\u30A6\\u30F3\\u30C9 (1998)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u305D\\u308C\\u306F")))
;     Result: NIL
;     Expect: ("それは")
;   FAIL:
(TEST-CASE "onig/991" "^.*$"
 (BACKSLASH
  "\\u3053\\u3046\\u3057\\u305F\\u65E5\\u672C\\u4EBA\\u306E\\u6B66\\u5668\\u306B\\u5BFE\\u3059\\u308B\\u5909\\u308F\\u3063\\u305F\\u614B\\u5EA6\\u306E\\u88CF\\u306B\\u306F\\u3001\\u3058\\u3064\\u306F\\u3001\\n\\u4E00\\u8CAB\\u3057\\u305F\\u9078\\u629E\\u57FA\\u6E96\\u304C\\u50CD\\u3044\\u3066\\u3044\\u305F\\u3002\\n\\u305D\\u308C\\u306F\\u3001\\u305D\\u306E\\u6B66\\u5668\\u304C\\u300C\\u4E3B\\u5175\\u3092\\u9AD8\\u7D1A\\u306B\\u898B\\u305B\\u308B\\u304B\\u3069\\u3046\\u304B\\u300D\\u3067\\u3042\\u3063\\u305F\\u3002\\n\\n\\u5175\\u85E4\\u4E8C\\u5341\\u516B \\u300C\\u6709\\u5742\\u9283\\u300D \\u56DB\\u8C37\\u30E9\\u30A6\\u30F3\\u30C9 (1998)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST
  (BACKSLASH
   "\\u3053\\u3046\\u3057\\u305F\\u65E5\\u672C\\u4EBA\\u306E\\u6B66\\u5668\\u306B\\u5BFE\\u3059\\u308B\\u5909\\u308F\\u3063\\u305F\\u614B\\u5EA6\\u306E\\u88CF\\u306B\\u306F\\u3001\\u3058\\u3064\\u306F\\u3001")))
;     Result: NIL
;     Expect: ("こうした日本人の武器に対する変わった態度の裏には、じつは、")
;   FAIL:
(TEST-CASE "onig/1001"
 (BACKSLASH "\\u3042\\u308A\\u307E\\u3059\\u3002$")
 (BACKSLASH
  "  \\u7A17\\u306F\\u4EBA\\u3082\\u98DF\\u3044\\u3001\\u99AC\\u306E\\u98FC\\u6599\\u306B\\u3082\\u3057\\u307E\\u3057\\u305F\\u3002\\u99AC\\u306B\\u306F\\u7A17\\u4E00\\u5347\\u306B\\u8C46\\u4E8C\\u5408\\u3092\\u305F\\u3044\\u3066\\u307E\\u305C\\u305F\\u3082\\u306E\\u3092\\u4E00\\u65E5\\u306B\\u4E00\\u56DE\\u306F\\u305F\\u3079\\u3055\\u305B\\u305F\\u3002\\u4EBA\\u9593\\u3088\\u308A\\u306F\\u4E0A\\u7B49\\u306E\\u3082\\u306E\\u3092\\u305F\\u3079\\u3055\\u305B\\u305F\\u3082\\u3093\\u3067\\u3042\\u308A\\u307E\\u3059\\u3002\\n  \\u4EBA\\u9593\\u306F\\u65E5\\u9803\\u306F\\u30D8\\u30BA\\u30EA\\u98EF\\u3092\\u305F\\u3079\\u305F\\u3002\\u4E7E\\u83DC\\u3092\\u3086\\u3067\\u3066\\u3001\\u3086\\u3067\\u3058\\u308B\\u3092\\u99AC\\u306B\\u3084\\u308A\\u3001\\u83DC\\u3092\\u3053\\u307E\\u304B\\u306B\\u5207\\u308A\\u3001\\u83DC\\u3068\\u7A17\\u3068\\u7C73\\u3092\\u307E\\u305C\\u3066\\u305F\\u3044\\u3066\\u305F\\u3079\\u305F\\u3002\\u305A\\u3063\\u3068\\u6614\\u306F\\u7C73\\u3068\\u7A17\\u304C\\u534A\\u3005\\u3050\\u3089\\u3044\\u3067\\u3042\\u3063\\u305F\\u304C\\u3001\\u660E\\u6CBB\\u3082\\u4E8C\\u5341\\u5E74\\u4EE3\\u306B\\u306A\\u308B\\u3068\\u3001\\u7A17\\u3092\\u3064\\u304F\\u308B\\u306E\\u304C\\u3078\\u3063\\u3066\\u6765\\u3066\\u3001\\u7A17\\u306F\\u7C73\\u306E\\u4E09\\u5206\\u306E\\u4E00\\u304F\\u3089\\u3044\\u306B\\u306A\\u3063\\u305F\\u3002\\u30D8\\u30BA\\u30EA\\u98EF\\u306B\\u306F\\u5869\\u3092\\u5C11\\u3057\\u3044\\u308C\\u305F\\u3082\\u3093\\u3067\\u3059\\u3002\\n\\n\\u5BAE\\u672C\\u5E38\\u4E00 \\u300C\\u5FD8\\u308C\\u3089\\u308C\\u305F\\u65E5\\u672C\\u4EBA\\u300D (1960)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (BACKSLASH "\\u3042\\u308A\\u307E\\u3059\\u3002")))
;     Result: NIL
;     Expect: ("あります。")
;   FAIL:
(TEST-CASE "onig/1013" (BACKSLASH "^(\\u5409\\u7530\\u677E\\u852D)")
 (BACKSLASH
  "\\u8EAB\\u306F\\u305F\\u3068\\u3072 \\u6B66\\u8535\\u306E\\u91CE\\u8FBA\\u306B\\u673D\\u306C\\u3068\\u3082 \\u7559\\u7F6E\\u307E\\u3057\\u5927\\u548C\\u9B42\\n\\n\\u5409\\u7530\\u677E\\u852D \\u300C\\u7559\\u9B42\\u9332\\u300D (1859)\\n")
 (QUOTE (:SINGLE-LINE NIL :UNICODE T))
 (LIST (QUOTE *) (BACKSLASH "\\u5409\\u7530\\u677E\\u852D")))
;     Result: NIL
;     Expect: (* "吉田松蔭")
;  55/772, skip=47
; Try "pcre-5.retest"
;  0/61, skip=7
; Try "pcre-4.retest"
;   FAIL:
(TEST-CASE "pcre-4/437" "(?<=[\\x{100}\\x{200}]{3})X"
 (BACKSLASH "abc\\u0100\\u0200\\u0100X")
 (QUOTE (:UNICODE T :SINGLE-LINE NIL)) (LIST "X"))
;     Result: NIL
;     Expect: ("X")
;   FAIL:
(TEST-CASE "pcre-4/482" "[z-\\x{100}]" (BACKSLASH "\\u0101")
 (QUOTE (:UNICODE T :IGNORE-CASE T :SINGLE-LINE NIL)) NIL)
;     Result: ("ā")
;     Expect: NIL
;  2/298, skip=20
; Try "pcre-3.retest"
; GC Fence 4,191,176 => 1,774,608
;   FAIL:
(TEST-CASE "pcre-3/37" "(.+)\\b(.+)" (BACKSLASH "\\u00C9cole")
 (QUOTE (:SINGLE-LINE NIL))
 (LIST (BACKSLASH "\\u00C9cole") (BACKSLASH "\\u00C9") "cole"))
;     Result: NIL
;     Expect: ("École" "É" "cole")
;   FAIL:
(TEST-CASE "pcre-3/41" "(.+)\\b(.+)" (BACKSLASH "\\u00C9cole")
 (QUOTE (:SINGLE-LINE NIL))
 (LIST (BACKSLASH "\\u00C9cole") (BACKSLASH "\\u00C9") "cole"))
;     Result: NIL
;     Expect: ("École" "É" "cole")
;   FAIL:
(TEST-CASE "pcre-3/46" (BACKSLASH "\\u00C9cole")
 (BACKSLASH "\\u00E9cole") (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL))
 NIL)
;     Result: ("école")
;     Expect: NIL
;   FAIL:
(TEST-CASE "pcre-3/50" (BACKSLASH "\\u00C9cole")
 (BACKSLASH "\\u00E9cole") (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL))
 NIL)
;     Result: ("école")
;     Expect: NIL
;  4/30, skip=0
; Try "pcre-2.retest"
;   FAIL:
(TEST-CASE "pcre-2/699" "[[:upper:]]" "a"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL)) (LIST "a"))
;     Result: NIL
;     Expect: ("a")
;   FAIL:
(TEST-CASE "pcre-2/702" "[[:lower:]]" "A"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL)) (LIST "A"))
;     Result: NIL
;     Expect: ("A")
;   FAIL:
(TEST-CASE "pcre-2/707" "((?-i)[[:lower:]])[[:lower:]]" "aB"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL)) (LIST "aB" "a"))
;     Result: NIL
;     Expect: ("aB" "a")
;   FAIL:
(TEST-CASE "pcre-2/803" "\\Q\\E" "" (QUOTE (:SINGLE-LINE NIL)) NIL)
;     Result: ("")
;     Expect: NIL
;  4/366, skip=2
; Try "pcre-1.retest"
; GC Fence 4,227,392 => 1,777,352
;   FAIL:
(TEST-CASE "pcre-1/154" (BACKSLASH "^\\\\u0081") (BACKSLASH "\\u0081")
 (QUOTE (:SINGLE-LINE NIL)) (LIST (BACKSLASH "\\u0081")))
;     Result: NIL
;     Expect: ("")
;   FAIL:
(TEST-CASE "pcre-1/302" "^   a\\ b[c ]d       $" "a bcd"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "a bcd"))
;     Result: NIL
;     Expect: ("a bcd")
;   FAIL:
(TEST-CASE "pcre-1/303" "^   a\\ b[c ]d       $" "a b d"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "a b d"))
;     Result: NIL
;     Expect: ("a b d")
; GC Fence 3,935,856 => 1,903,264
;   FAIL:
(TEST-CASE "pcre-1/1301" (BACKSLASH "^a\\tb\\n  \\u000D  \\u000C  c")
 "abc" (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "abc"))
;     Result: NIL
;     Expect: ("abc")
;   FAIL:
(TEST-CASE "pcre-1/1368" "^[\\x3f-\\x5F]+$" "WXY_^abc"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL)) (LIST "WXY_^abc"))
;     Result: NIL
;     Expect: ("WXY_^abc")
;   FAIL:
(TEST-CASE "pcre-1/1369" "^[\\x3f-\\x5F]+$" "wxy_^ABC"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL)) (LIST "wxy_^ABC"))
;     Result: NIL
;     Expect: ("wxy_^ABC")
;   FAIL:
(TEST-CASE "pcre-1/1468" "(abc)\\123" "abcS" (QUOTE (:SINGLE-LINE NIL))
 (LIST "abcS" "abc"))
;     Result: NIL
;     Expect: ("abcS" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1471" "(abc)\\223" (BACKSLASH "abc\\u0093")
 (QUOTE (:SINGLE-LINE NIL)) (LIST (BACKSLASH "abc\\u0093") "abc"))
;     Result: NIL
;     Expect: ("abc" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1474" "(abc)\\323" (BACKSLASH "abc\\u00D3")
 (QUOTE (:SINGLE-LINE NIL)) (LIST (BACKSLASH "abc\\u00D3") "abc"))
;     Result: NIL
;     Expect: ("abcÓ" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1477" "(abc)\\500" "abc@" (QUOTE (:SINGLE-LINE NIL))
 (LIST "abc@" "abc"))
;     Result: NIL
;     Expect: ("abc@" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1478" "(abc)\\500" "abc@" (QUOTE (:SINGLE-LINE NIL))
 (LIST "abc@" "abc"))
;     Result: NIL
;     Expect: ("abc@" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1481" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1482" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1483" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1484" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1485" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1486" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-1/1497"
 "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\12\\123" "abcdefghijkllS"
 (QUOTE (:SINGLE-LINE NIL))
 (LIST "abcdefghijkllS" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
  "l"))
;     Result: NIL
;     Expect: ("abcdefghijkllS" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l")
;   FAIL:
(TEST-CASE "pcre-1/1500" "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\\12\\123"
 (BACKSLASH "abcdefghijk\\nS") (QUOTE (:SINGLE-LINE NIL))
 (LIST (BACKSLASH "abcdefghijk\\nS") "a" "b" "c" "d" "e" "f" "g" "h"
  "i" "j" "k"))
;     Result: NIL
;     Expect: ("abcdefghijk
S" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k")
;   FAIL:
(TEST-CASE "pcre-1/1592"
 "\\000\\001\\002\\003\\004\\005\\006\\007\\010\\011\\012\\013\\014\\015\\016\\017\\020\\021\\022\\023\\024\\025\\026\\027\\030\\031\\032\\033\\034\\035\\036\\037\\040\\041\\042\\043\\044\\045\\046\\047\\050\\051\\052\\053\\054\\055\\056\\057\\060\\061\\062\\063\\064\\065\\066\\067\\070\\071\\072\\073\\074\\075\\076\\077\\100\\101\\102\\103\\104\\105\\106\\107\\110\\111\\112\\113\\114\\115\\116\\117\\120\\121\\122\\123\\124\\125\\126\\127\\130\\131\\132\\133\\134\\135\\136\\137\\140\\141\\142\\143\\144\\145\\146\\147\\150\\151\\152\\153\\154\\155\\156\\157\\160\\161\\162\\163\\164\\165\\166\\167\\170\\171\\172\\173\\174\\175\\176\\177\\200\\201\\202\\203\\204\\205\\206\\207\\210\\211\\212\\213\\214\\215\\216\\217\\220\\221\\222\\223\\224\\225\\226\\227\\230\\231\\232\\233\\234\\235\\236\\237\\240\\241\\242\\243\\244\\245\\246\\247\\250\\251\\252\\253\\254\\255\\256\\257\\260\\261\\262\\263\\264\\265\\266\\267\\270\\271\\272\\273\\274\\275\\276\\277\\300\\301\\302\\303\\304\\305\\306\\307\\310\\311\\312\\313\\314\\315\\316\\317\\320\\321\\322\\323\\324\\325\\326\\327\\330\\331\\332\\333\\334\\335\\336\\337\\340\\341\\342\\343\\344\\345\\346\\347\\350\\351\\352\\353\\354\\355\\356\\357\\360\\361\\362\\363\\364\\365\\366\\367\\370\\371\\372\\373\\374\\375\\376\\377"
 (BACKSLASH
  "\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\u0008\\t\\n\\u000B\\u000C\\u000D\\u000E\\u000F\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001A\\u001B\\u001C\\u001D\\u001E\\u001F !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\\u007F\\u0080\\u0081\\u0082\\u0083\\u0084\\u0085\\u0086\\u0087\\u0088\\u0089\\u008A\\u008B\\u008C\\u008D\\u008E\\u008F\\u0090\\u0091\\u0092\\u0093\\u0094\\u0095\\u0096\\u0097\\u0098\\u0099\\u009A\\u009B\\u009C\\u009D\\u009E\\u009F\\u00A0\\u00A1\\u00A2\\u00A3\\u00A4\\u00A5\\u00A6\\u00A7\\u00A8\\u00A9\\u00AA\\u00AB\\u00AC\\u00AD\\u00AE\\u00AF\\u00B0\\u00B1\\u00B2\\u00B3\\u00B4\\u00B5\\u00B6\\u00B7\\u00B8\\u00B9\\u00BA\\u00BB\\u00BC\\u00BD\\u00BE\\u00BF\\u00C0\\u00C1\\u00C2\\u00C3\\u00C4\\u00C5\\u00C6\\u00C7\\u00C8\\u00C9\\u00CA\\u00CB\\u00CC\\u00CD\\u00CE\\u00CF\\u00D0\\u00D1\\u00D2\\u00D3\\u00D4\\u00D5\\u00D6\\u00D7\\u00D8\\u00D9\\u00DA\\u00DB\\u00DC\\u00DD\\u00DE\\u00DF\\u00E0\\u00E1\\u00E2\\u00E3\\u00E4\\u00E5\\u00E6\\u00E7\\u00E8\\u00E9\\u00EA\\u00EB\\u00EC\\u00ED\\u00EE\\u00EF\\u00F0\\u00F1\\u00F2\\u00F3\\u00F4\\u00F5\\u00F6\\u00F7\\u00F8\\u00F9\\u00FA\\u00FB\\u00FC\\u00FD\\u00FE\\u00FF")
 (QUOTE (:SINGLE-LINE NIL))
 (LIST
  (BACKSLASH
   "\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\u0008\\t\\n\\u000B\\u000C\\u000D\\u000E\\u000F\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001A\\u001B\\u001C\\u001D\\u001E\\u001F !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\\u007F\\u0080\\u0081\\u0082\\u0083\\u0084\\u0085\\u0086\\u0087\\u0088\\u0089\\u008A\\u008B\\u008C\\u008D\\u008E\\u008F\\u0090\\u0091\\u0092\\u0093\\u0094\\u0095\\u0096\\u0097\\u0098\\u0099\\u009A\\u009B\\u009C\\u009D\\u009E\\u009F\\u00A0\\u00A1\\u00A2\\u00A3\\u00A4\\u00A5\\u00A6\\u00A7\\u00A8\\u00A9\\u00AA\\u00AB\\u00AC\\u00AD\\u00AE\\u00AF\\u00B0\\u00B1\\u00B2\\u00B3\\u00B4\\u00B5\\u00B6\\u00B7\\u00B8\\u00B9\\u00BA\\u00BB\\u00BC\\u00BD\\u00BE\\u00BF\\u00C0\\u00C1\\u00C2\\u00C3\\u00C4\\u00C5\\u00C6\\u00C7\\u00C8\\u00C9\\u00CA\\u00CB\\u00CC\\u00CD\\u00CE\\u00CF\\u00D0\\u00D1\\u00D2\\u00D3\\u00D4\\u00D5\\u00D6\\u00D7\\u00D8\\u00D9\\u00DA\\u00DB\\u00DC\\u00DD\\u00DE\\u00DF\\u00E0\\u00E1\\u00E2\\u00E3\\u00E4\\u00E5\\u00E6\\u00E7\\u00E8\\u00E9\\u00EA\\u00EB\\u00EC\\u00ED\\u00EE\\u00EF\\u00F0\\u00F1\\u00F2\\u00F3\\u00F4\\u00F5\\u00F6\\u00F7\\u00F8\\u00F9\\u00FA\\u00FB\\u00FC\\u00FD\\u00FE\\u00FF")))
;     Result: NIL
;     Expect: (" 	
 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
;   FAIL:
(TEST-CASE "pcre-1/1668" "^[W-\\]46]" "Zebra"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "Z"))
;     Result: NIL
;     Expect: ("Z")
;   FAIL:
(TEST-CASE "pcre-1/1669" "^[W-\\]46]" "Xylophone"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "X"))
;     Result: NIL
;     Expect: ("X")
;   FAIL:
(TEST-CASE "pcre-1/1671" "^[W-\\]46]" "[abcd]"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "["))
;     Result: NIL
;     Expect: ("[")
;   FAIL:
(TEST-CASE "pcre-1/1673" "^[W-\\]46]" "\\backslash"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "\\"))
;     Result: NIL
;     Expect: ("\\")
;   FAIL:
(TEST-CASE "pcre-1/1679" "\\d\\d/\\d\\d/\\d\\d\\d\\d" "01/01/2000"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "01/01/2000"))
;     Result: NIL
;     Expect: ("01/01/2000")
; GC Fence 4,297,104 => 1,917,680
;   FAIL:
(TEST-CASE "pcre-1/1898"
 "<tr([\\w\\W\\s\\d][^<>]{0,})><TD([\\w\\W\\s\\d][^<>]{0,})>([\\d]{0,}\\.)(.*)((<BR>([\\w\\W\\s\\d][^<>]{0,})|[\\s]{0,}))</a></TD><TD([\\w\\W\\s\\d][^<>]{0,})>([\\w\\W\\s\\d][^<>]{0,})</TD><TD([\\w\\W\\s\\d][^<>]{0,})>([\\w\\W\\s\\d][^<>]{0,})</TD></TR>"
 "<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE T))
 (LIST
  "<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>"
  " BGCOLOR='#DBE9E9'" " align=left valign=top" "43."
  "<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)"
  "" "" NIL " align=left valign=top" "Lega lstaff.com"
  " align=left valign=top" "CA - Statewide"))
;     Result: NIL
;     Expect: ("<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>" " BGCOLOR='#DBE9E9'" " align=left valign=top" "43." "<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)" "" "" NIL " align=left valign=top" "Lega lstaff.com" " align=left valign=top" "CA - Statewide")
; GC Fence 4,222,640 => 1,932,592
;   FAIL:
(TEST-CASE "pcre-1/2932" "$b" "*** Failers"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL))
 (LIST "*** Failers" "*" "*" " Failers"))
;     Result: NIL
;     Expect: ("*** Failers" "*" "*" " Failers")
;   FAIL:
(TEST-CASE "pcre-1/3591" "(?<=\\d{3}(?!999))foo" "999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-1/3592" "(?<=\\d{3}(?!999))foo" "123999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-1/3597" "(?<=(?!...999)\\d{3})foo" "999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-1/3598" "(?<=(?!...999)\\d{3})foo" "123999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-1/3603" "(?<=\\d{3}(?!999)...)foo" "123abcfoo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-1/3606" "(?<=\\d{3}(?!999)...)foo" "123999foo"
 (QUOTE (:SINGLE-LINE NIL)) NIL)
;     Result: ("foo")
;     Expect: NIL
;   FAIL:
(TEST-CASE "pcre-1/3609" "(?<=\\d{3}...)(?<!999)foo" "123abcfoo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-1/3610" "(?<=\\d{3}...)(?<!999)foo" "123456foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
; GC Fence 4,252,896 => 1,946,872
;  35/1638, skip=24
; Try "pcre-cl.retest"
;   FAIL:
(TEST-CASE "pcre-cl/144" (BACKSLASH "^\\\\u0081") (BACKSLASH "\\u0081")
 (QUOTE (:SINGLE-LINE NIL)) (LIST (BACKSLASH "\\u0081")))
;     Result: NIL
;     Expect: ("")
;   FAIL:
(TEST-CASE "pcre-cl/278" "^   a\\ b[c ]d       $" "a bcd"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "a bcd"))
;     Result: NIL
;     Expect: ("a bcd")
;   FAIL:
(TEST-CASE "pcre-cl/279" "^   a\\ b[c ]d       $" "a b d"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "a b d"))
;     Result: NIL
;     Expect: ("a b d")
; GC Fence 4,115,424 => 2,022,368
;   FAIL:
(TEST-CASE "pcre-cl/1264" (BACKSLASH "^a\\tb\\n  \\u000D  \\u000C  c")
 "abc" (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "abc"))
;     Result: NIL
;     Expect: ("abc")
;   FAIL:
(TEST-CASE "pcre-cl/1324" "^[\\x3f-\\x5F]+$" "WXY_^abc"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL)) (LIST "WXY_^abc"))
;     Result: NIL
;     Expect: ("WXY_^abc")
;   FAIL:
(TEST-CASE "pcre-cl/1325" "^[\\x3f-\\x5F]+$" "wxy_^ABC"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE NIL)) (LIST "wxy_^ABC"))
;     Result: NIL
;     Expect: ("wxy_^ABC")
;   FAIL:
(TEST-CASE "pcre-cl/1413" "(abc)\\123" "abcS"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abcS" "abc"))
;     Result: NIL
;     Expect: ("abcS" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1416" "(abc)\\223" (BACKSLASH "abc\\u0093")
 (QUOTE (:SINGLE-LINE NIL)) (LIST (BACKSLASH "abc\\u0093") "abc"))
;     Result: NIL
;     Expect: ("abc" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1419" "(abc)\\323" (BACKSLASH "abc\\u00D3")
 (QUOTE (:SINGLE-LINE NIL)) (LIST (BACKSLASH "abc\\u00D3") "abc"))
;     Result: NIL
;     Expect: ("abcÓ" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1422" "(abc)\\500" "abc@"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@" "abc"))
;     Result: NIL
;     Expect: ("abc@" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1423" "(abc)\\500" "abc@"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@" "abc"))
;     Result: NIL
;     Expect: ("abc@" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1426" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1427" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1428" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1429" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1430" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1431" "(abc)\\5000" "abc@0"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abc@0" "abc"))
;     Result: NIL
;     Expect: ("abc@0" "abc")
;   FAIL:
(TEST-CASE "pcre-cl/1442"
 "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\12\\123" "abcdefghijkllS"
 (QUOTE (:SINGLE-LINE NIL))
 (LIST "abcdefghijkllS" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
  "l"))
;     Result: NIL
;     Expect: ("abcdefghijkllS" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l")
;   FAIL:
(TEST-CASE "pcre-cl/1445" "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\\12\\123"
 (BACKSLASH "abcdefghijk\\nS") (QUOTE (:SINGLE-LINE NIL))
 (LIST (BACKSLASH "abcdefghijk\\nS") "a" "b" "c" "d" "e" "f" "g" "h"
  "i" "j" "k"))
;     Result: NIL
;     Expect: ("abcdefghijk
S" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k")
;   FAIL:
(TEST-CASE "pcre-cl/1532"
 "\\000\\001\\002\\003\\004\\005\\006\\007\\010\\011\\012\\013\\014\\015\\016\\017\\020\\021\\022\\023\\024\\025\\026\\027\\030\\031\\032\\033\\034\\035\\036\\037\\040\\041\\042\\043\\044\\045\\046\\047\\050\\051\\052\\053\\054\\055\\056\\057\\060\\061\\062\\063\\064\\065\\066\\067\\070\\071\\072\\073\\074\\075\\076\\077\\100\\101\\102\\103\\104\\105\\106\\107\\110\\111\\112\\113\\114\\115\\116\\117\\120\\121\\122\\123\\124\\125\\126\\127\\130\\131\\132\\133\\134\\135\\136\\137\\140\\141\\142\\143\\144\\145\\146\\147\\150\\151\\152\\153\\154\\155\\156\\157\\160\\161\\162\\163\\164\\165\\166\\167\\170\\171\\172\\173\\174\\175\\176\\177\\200\\201\\202\\203\\204\\205\\206\\207\\210\\211\\212\\213\\214\\215\\216\\217\\220\\221\\222\\223\\224\\225\\226\\227\\230\\231\\232\\233\\234\\235\\236\\237\\240\\241\\242\\243\\244\\245\\246\\247\\250\\251\\252\\253\\254\\255\\256\\257\\260\\261\\262\\263\\264\\265\\266\\267\\270\\271\\272\\273\\274\\275\\276\\277\\300\\301\\302\\303\\304\\305\\306\\307\\310\\311\\312\\313\\314\\315\\316\\317\\320\\321\\322\\323\\324\\325\\326\\327\\330\\331\\332\\333\\334\\335\\336\\337\\340\\341\\342\\343\\344\\345\\346\\347\\350\\351\\352\\353\\354\\355\\356\\357\\360\\361\\362\\363\\364\\365\\366\\367\\370\\371\\372\\373\\374\\375\\376\\377"
 (BACKSLASH
  "\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\u0008\\t\\n\\u000B\\u000C\\u000D\\u000E\\u000F\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001A\\u001B\\u001C\\u001D\\u001E\\u001F !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\\u007F\\u0080\\u0081\\u0082\\u0083\\u0084\\u0085\\u0086\\u0087\\u0088\\u0089\\u008A\\u008B\\u008C\\u008D\\u008E\\u008F\\u0090\\u0091\\u0092\\u0093\\u0094\\u0095\\u0096\\u0097\\u0098\\u0099\\u009A\\u009B\\u009C\\u009D\\u009E\\u009F\\u00A0\\u00A1\\u00A2\\u00A3\\u00A4\\u00A5\\u00A6\\u00A7\\u00A8\\u00A9\\u00AA\\u00AB\\u00AC\\u00AD\\u00AE\\u00AF\\u00B0\\u00B1\\u00B2\\u00B3\\u00B4\\u00B5\\u00B6\\u00B7\\u00B8\\u00B9\\u00BA\\u00BB\\u00BC\\u00BD\\u00BE\\u00BF\\u00C0\\u00C1\\u00C2\\u00C3\\u00C4\\u00C5\\u00C6\\u00C7\\u00C8\\u00C9\\u00CA\\u00CB\\u00CC\\u00CD\\u00CE\\u00CF\\u00D0\\u00D1\\u00D2\\u00D3\\u00D4\\u00D5\\u00D6\\u00D7\\u00D8\\u00D9\\u00DA\\u00DB\\u00DC\\u00DD\\u00DE\\u00DF\\u00E0\\u00E1\\u00E2\\u00E3\\u00E4\\u00E5\\u00E6\\u00E7\\u00E8\\u00E9\\u00EA\\u00EB\\u00EC\\u00ED\\u00EE\\u00EF\\u00F0\\u00F1\\u00F2\\u00F3\\u00F4\\u00F5\\u00F6\\u00F7\\u00F8\\u00F9\\u00FA\\u00FB\\u00FC\\u00FD\\u00FE\\u00FF")
 (QUOTE (:SINGLE-LINE NIL))
 (LIST
  (BACKSLASH
   "\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\u0008\\t\\n\\u000B\\u000C\\u000D\\u000E\\u000F\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001A\\u001B\\u001C\\u001D\\u001E\\u001F !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\\u007F\\u0080\\u0081\\u0082\\u0083\\u0084\\u0085\\u0086\\u0087\\u0088\\u0089\\u008A\\u008B\\u008C\\u008D\\u008E\\u008F\\u0090\\u0091\\u0092\\u0093\\u0094\\u0095\\u0096\\u0097\\u0098\\u0099\\u009A\\u009B\\u009C\\u009D\\u009E\\u009F\\u00A0\\u00A1\\u00A2\\u00A3\\u00A4\\u00A5\\u00A6\\u00A7\\u00A8\\u00A9\\u00AA\\u00AB\\u00AC\\u00AD\\u00AE\\u00AF\\u00B0\\u00B1\\u00B2\\u00B3\\u00B4\\u00B5\\u00B6\\u00B7\\u00B8\\u00B9\\u00BA\\u00BB\\u00BC\\u00BD\\u00BE\\u00BF\\u00C0\\u00C1\\u00C2\\u00C3\\u00C4\\u00C5\\u00C6\\u00C7\\u00C8\\u00C9\\u00CA\\u00CB\\u00CC\\u00CD\\u00CE\\u00CF\\u00D0\\u00D1\\u00D2\\u00D3\\u00D4\\u00D5\\u00D6\\u00D7\\u00D8\\u00D9\\u00DA\\u00DB\\u00DC\\u00DD\\u00DE\\u00DF\\u00E0\\u00E1\\u00E2\\u00E3\\u00E4\\u00E5\\u00E6\\u00E7\\u00E8\\u00E9\\u00EA\\u00EB\\u00EC\\u00ED\\u00EE\\u00EF\\u00F0\\u00F1\\u00F2\\u00F3\\u00F4\\u00F5\\u00F6\\u00F7\\u00F8\\u00F9\\u00FA\\u00FB\\u00FC\\u00FD\\u00FE\\u00FF")))
;     Result: NIL
;     Expect: (" 	
 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
;   FAIL:
(TEST-CASE "pcre-cl/1605" "^[W-\\]46]" "Zebra"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "Z"))
;     Result: NIL
;     Expect: ("Z")
; GC Fence 4,042,208 => 2,031,688
;   FAIL:
(TEST-CASE "pcre-cl/1606" "^[W-\\]46]" "Xylophone"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "X"))
;     Result: NIL
;     Expect: ("X")
;   FAIL:
(TEST-CASE "pcre-cl/1608" "^[W-\\]46]" "[abcd]"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "["))
;     Result: NIL
;     Expect: ("[")
;   FAIL:
(TEST-CASE "pcre-cl/1610" "^[W-\\]46]" "\\backslash"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "\\"))
;     Result: NIL
;     Expect: ("\\")
;   FAIL:
(TEST-CASE "pcre-cl/1611" "^[W-\\]46]" "-46]789"
 (QUOTE (:SINGLE-LINE NIL)) NIL)
;     Result: ("-")
;     Expect: NIL
;   FAIL:
(TEST-CASE "pcre-cl/1615" "\\d\\d/\\d\\d/\\d\\d\\d\\d" "01/01/2000"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "01/01/2000"))
;     Result: NIL
;     Expect: ("01/01/2000")
;   FAIL:
(TEST-CASE "pcre-cl/1823"
 "<tr([\\w\\W\\s\\d][^<>]{0,})><TD([\\w\\W\\s\\d][^<>]{0,})>([\\d]{0,}\\.)(.*)((<BR>([\\w\\W\\s\\d][^<>]{0,})|[\\s]{0,}))</a></TD><TD([\\w\\W\\s\\d][^<>]{0,})>([\\w\\W\\s\\d][^<>]{0,})</TD><TD([\\w\\W\\s\\d][^<>]{0,})>([\\w\\W\\s\\d][^<>]{0,})</TD></TR>"
 "<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>"
 (QUOTE (:IGNORE-CASE T :SINGLE-LINE T))
 (LIST
  "<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>"
  " BGCOLOR='#DBE9E9'" " align=left valign=top" "43."
  "<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)"
  "" "" NIL " align=left valign=top" "Lega lstaff.com"
  " align=left valign=top" "CA - Statewide"))
;     Result: NIL
;     Expect: ("<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>" " BGCOLOR='#DBE9E9'" " align=left valign=top" "43." "<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)" "" "" NIL " align=left valign=top" "Lega lstaff.com" " align=left valign=top" "CA - Statewide")
;   FAIL:
(TEST-CASE "pcre-cl/2054" "a{3, 3}" "a{3,3}"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "a{3,3}"))
;     Result: NIL
;     Expect: ("a{3,3}")
;   FAIL:
(TEST-CASE "pcre-cl/2056" "a{3, 3}" "aaa"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) NIL)
;     Result: ("aaa")
;     Expect: NIL
;   FAIL:
(TEST-CASE "pcre-cl/2064" "a{3, }" "a{3,}"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) (LIST "a{3,}"))
;     Result: NIL
;     Expect: ("a{3,}")
;   FAIL:
(TEST-CASE "pcre-cl/2066" "a{3, }" "aaa"
 (QUOTE (:EXTENDED-SYNTAX T :SINGLE-LINE NIL)) NIL)
;     Result: ("aaa")
;     Expect: NIL
; GC Fence 4,270,080 => 2,040,432
; GC Fence 4,310,128 => 2,050,272
;   FAIL:
(TEST-CASE "pcre-cl/3762" "(?<=\\d{3}(?!999))foo" "999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-cl/3763" "(?<=\\d{3}(?!999))foo" "123999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-cl/3767" "(?<=(?!...999)\\d{3})foo" "999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-cl/3768" "(?<=(?!...999)\\d{3})foo" "123999foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-cl/3772" "(?<=\\d{3}(?!999)...)foo" "123abcfoo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-cl/3774" "(?<=\\d{3}(?!999)...)foo" "123999foo"
 (QUOTE (:SINGLE-LINE NIL)) NIL)
;     Result: ("foo")
;     Expect: NIL
;   FAIL:
(TEST-CASE "pcre-cl/3777" "(?<=\\d{3}...)(?<!999)foo" "123abcfoo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-cl/3778" "(?<=\\d{3}...)(?<!999)foo" "123456foo"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "foo"))
;     Result: NIL
;     Expect: ("foo")
;   FAIL:
(TEST-CASE "pcre-cl/3920" "\\Q\\Qa*x\\E" "a\\*x"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "a\\*x"))
;     Result: NIL
;     Expect: ("a\\*x")
;   FAIL:
(TEST-CASE "pcre-cl/3939" (BACKSLASH "(?x)a#\\Q\\n.") "aa"
 (QUOTE (:SINGLE-LINE NIL)) NIL)
;     Result: ("aa")
;     Expect: NIL
;   FAIL:
(TEST-CASE "pcre-cl/3942" "ab(?=.*q)cd" "abcdxklqj"
 (QUOTE (:SINGLE-LINE NIL)) (LIST "abcd"))
;     Result: NIL
;     Expect: ("abcd")
;   FAIL:
(TEST-CASE "pcre-cl/3945" "a(?!.*$)b" "ab" (QUOTE (:SINGLE-LINE NIL))
 NIL)
;     Result: ("ab")
;     Expect: NIL
;  43/1624, skip=59
; [0] FIXNUM: 143
; [1] FIXNUM: 179
; [2] FIXNUM: 6016
CL-USER[11]> a