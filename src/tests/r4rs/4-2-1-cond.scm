(display (cond ((> 3 2) 'greater)
               ((< 3 2) 'less)))

(display (cond ((> 3 3) 'greater)
               ((< 3 3) 'less)
               (else 'equal)))

;;;options: -l r4rs
;;;expected:
;;;greater
;;;equal
