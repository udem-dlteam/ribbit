(display (cond ((> 3 2) 'greater)
               ((< 3 2) 'less)))
(newline)

(display (cond ((> 3 3) 'greater)
               ((< 3 3) 'less)
               (else 'equal)))
(newline)

;; (display (cond ((assv 'b '((a 1) (b 2))) => cadr)
;;                (else #f)))
;; (newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;greater
;;;equal
