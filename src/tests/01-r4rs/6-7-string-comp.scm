(display (string=? "" ""))
(newline)

(display (string<? "" ""))
(newline)

(display (string>? "" ""))
(newline)

(display (string<=? "" ""))
(newline)

(display (string>=? "" ""))
(newline)

(display (string-ci=? "" ""))
(newline)

(display (string-ci<? "" ""))
(newline)

(display (string-ci>? "" ""))
(newline)

(display (string-ci<=? "" ""))
(newline)

(display (string-ci>=? "" ""))
(newline)

(display (string=? "A" "B"))
(newline)

(display (string=? "a" "b"))
(newline)

(display (string=? "9" "0"))
(newline)

(display (string=? "A" "A"))
(newline)

(display (string<? "A" "B"))
(newline)

(display (string<? "a" "b"))
(newline)

(display (string<? "9" "0"))
(newline)

(display (string<? "A" "A"))
(newline)

(display (string>? "A" "B"))
(newline)

(display (string>? "a" "b"))
(newline)

(display (string>? "9" "0"))
(newline)

(display (string>? "A" "A"))
(newline)

(display (string<=? "A" "B"))
(newline)

(display (string<=? "a" "b"))
(newline)

(display (string<=? "9" "0"))
(newline)

(display (string<=? "A" "A"))
(newline)

(display (string>=? "A" "B"))
(newline)

(display (string>=? "a" "b"))
(newline)

(display (string>=? "9" "0"))
(newline)

(display (string>=? "A" "A"))
(newline)

(display (string-ci=? "A" "B"))
(newline)

(display (string-ci=? "a" "B"))
(newline)

(display (string-ci=? "A" "b"))
(newline)

(display (string-ci=? "a" "b"))
(newline)

(display (string-ci=? "9" "0"))
(newline)

(display (string-ci=? "A" "A"))
(newline)

(display (string-ci=? "A" "a"))
(newline)

(display (string-ci<? "A" "B"))
(newline)

(display (string-ci<? "a" "B"))
(newline)

(display (string-ci<? "A" "b"))
(newline)

(display (string-ci<? "a" "b"))
(newline)

(display (string-ci<? "9" "0"))
(newline)

(display (string-ci<? "A" "A"))
(newline)

(display (string-ci<? "A" "a"))
(newline)

(display (string-ci>? "A" "B"))
(newline)

(display (string-ci>? "a" "B"))
(newline)

(display (string-ci>? "A" "b"))
(newline)

(display (string-ci>? "a" "b"))
(newline)

(display (string-ci>? "9" "0"))
(newline)

(display (string-ci>? "A" "A"))
(newline)

(display (string-ci>? "A" "a"))
(newline)

(display (string-ci<=? "A" "B"))
(newline)

(display (string-ci<=? "a" "B"))
(newline)

(display (string-ci<=? "A" "b"))
(newline)

(display (string-ci<=? "a" "b"))
(newline)

(display (string-ci<=? "9" "0"))
(newline)

(display (string-ci<=? "A" "A"))
(newline)

(display (string-ci<=? "A" "a"))
(newline)

(display (string-ci>=? "A" "B"))
(newline)

(display (string-ci>=? "a" "B"))
(newline)

(display (string-ci>=? "A" "b"))
(newline)

(display (string-ci>=? "a" "b"))
(newline)

(display (string-ci>=? "9" "0"))
(newline)

(display (string-ci>=? "A" "A"))
(newline)

(display (string-ci>=? "A" "a"))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;#t
;;;#f
;;;#f
;;;#t
;;;#t
;;;#t
;;;#f
;;;#f
;;;#t
;;;#t
;;;#f
;;;#f
;;;#f
;;;#t
;;;#t
;;;#t
;;;#f
;;;#f
;;;#f
;;;#f
;;;#t
;;;#f
;;;#t
;;;#t
;;;#f
;;;#t
;;;#f
;;;#f
;;;#t
;;;#t
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#t
;;;#f
;;;#f
;;;#t
;;;#t
;;;#t
;;;#t
;;;#f
;;;#t
;;;#t
;;;#f
;;;#f
;;;#f
;;;#f
;;;#t
;;;#t
;;;#t
