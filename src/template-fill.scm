;; Source code of template filler in rsc shell script.

(define sample ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y")
(define template "src/host/scm/rvm.scm")

(define (split str sep)
  (let ((lstr (string-length str))
        (lsep (string-length sep)))
    (let loop1 ((i 0) (j 0) (rparts (list)))
      (if (< (+ j lsep) lstr)
          (let loop2 ((k 0))
            (if (< k lsep)
                (if (char=? (string-ref str (+ j k)) (string-ref sep k))
                    (loop2 (+ k 1))
                    (loop1 i (+ j 1) rparts))
                (loop1 (+ j lsep)
                       (+ j lsep)
                       (cons (substring str i j) rparts))))
          (reverse (cons (substring str i lstr) rparts))))))

(define (concat strs sep)
  (if (pair? strs)
      (let ((rstrs (reverse strs))
            (sep (string->list sep)))
        (let loop ((lst (cdr rstrs))
                   (result (string->list (car rstrs))))
          (if (pair? lst)
              (loop (cdr lst)
                    (append (string->list (car lst))
                            (append sep
                                    result)))
              (list->string result))))
      (string)))

(define escapes (quote ((9 116) (10 110) (13 114) (34 34) (39 39) (92 92))))

(define (escape str)
  (let loop ((lst (reverse (string->list str))) (res (list)))
    (if (pair? lst)
        (loop (cdr lst)
              (let* ((c (char->integer (car lst)))
                     (x (assoc c escapes)))
                (if x
                    (cons 92 (cons (cadr x) res))
                    (cons c res))))
        (list->string (map integer->char res)))))

(define (read-to-eof port)
  (let loop ((res (list)))
    (let ((c (read-char port)))
      (if (char? c)
          (loop (cons c res))
          (list->string (reverse res))))))

(define (read-file path)
  (call-with-input-file path read-to-eof))

(define input (read-to-eof (current-input-port)))

(define parts (split (read-file template) sample))

(display
 (if (null? (cdr parts))
    (string-append (car parts) input)
    (concat parts (escape input))))
