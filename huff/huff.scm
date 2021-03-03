;; Compute stats & possible encoding for the interpreter's code
(import (_match))
(load "./interp.scm")

(define (make-bins code bins)
  (let loop ((todo interp_code))
    (if (pair? todo)
        (let* ((line (car todo))
               (count (table-ref bins line 0)))
          (table-set! bins line (+ 1 count))
          (loop (cdr todo))))))

(define (pair-sort comp accessor)
  (lambda (a b) (comp (accessor a) (accessor b))))

;; Kind of like enumerate in python
(define (enumerate lst)
  (define l (length lst))
  (define reducer
    (lambda (index element acc)
      (cons (cons index element) acc)))
  (fold-right reducer '() (iota l) lst))

(define (cost-in-bits idx)
  (* 4 (ceiling (/ (+ 1 idx) 15))))

(define (compute-size binned-code)

  (define compute
    (lambda (idx-&-bin)
      (let ((idx (car idx-&-bin))
            (bin (cdr idx-&-bin)))
        (let ((cost (cost-in-bits idx))
              (instruction (car bin))
              (count (cdr bin)))
          (list instruction count cost)))))

  (map compute (enumerate binned-code)))

(define (compute-cost line-stats)

  (define (madd line-stat acc)
      (match line-stat
             ((,instr ,count ,cost)
              (+ acc (* count cost)))))

  (fold madd 0 line-stats))

(define (huff-encode with-size)

 (define (encode idx-&-line)
  (let* ((idx (car idx-&-line))
         (line (cdr idx-&-line)))
    (match line
           ((,instr ,count ,cost)
            ;; TODO: compute the encoding
            (list instr count cost )))))

 (map encode (enumerate with-size)))

(define (main)
  (define bins (make-table))

  (make-bins interp-code bins)

  (let* ((counted (table->list bins))
         (sorted (list-sort (pair-sort > cdr) counted))
         (with-size (compute-size sorted))
         (encoded (huff-encode with-size)))
    (pretty-print encoded)
    (compute-cost with-size)))

