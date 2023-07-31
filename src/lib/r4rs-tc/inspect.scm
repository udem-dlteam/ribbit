(##include-once "ribbit:r4rs-tc.scm")
(##include-once "ribbit:prim-wrap-macros.scm")


;;; ---------- SYMBOL TABLE ---------- ;;;
(define symbol-table (%field1 ##rib)) 


;;; ---------- TYPES & MACROS ---------- ;;;
(define-type meta-rib-data)
(define-type meta-rib-op)
(define-type inspected-rib)

(define-macro 
  (define-inspector inspector-name . body)
  `(define (,inspector-name rib type-name config)
     ,@body))

(define-macro 
  (define-type-namer namer-name . body)
  `(define (,namer-name rib type-name config)
     ,@body))


(define-macro 
  (define-inspector-writer writer-name . body)
  `(define (,writer-name data rib-type config port)
     ,@body))

;;; ---------- CONFIG ---------- ;;;


(define-inspector noop-inspector 
  rib)

(define-type-namer noop-type-namer 
  type-name)


(define-inspector simple-inspector 
  (%rib (inspect-data! (%field0 rib) config) (inspect-data! (%field1 rib) config) (inspect-data! (%field2 rib) config)))

(define-inspector default-port-inspector 
  (%rib 'fd (inspect-data! (%field1 rib) config) (%field2 rib)))

(define-inspector default-proc-inspector 
  (if (%rib? (%field0 rib)) ;; procedure is not a primitive
    (%field2-set! (%field0 rib) (##inspect-op (%field2 (%field0 rib)))))
  rib)

(define-type-namer default-proc-type-namer 
  (if (%rib? (%field0 rib)) ;; procedure is not a primitive
    'procedure
    'primitive-procedure))


(define-type-namer default-singleton-inspector 
  (cond 
    ((%eqv? rib #f) 'false)
    ((%eqv? rib #t) 'true)
    ((%eqv? rib '()) 'nil)
    ((eof-object? rib) 'eof)
    (else 'singleton)))


(define (get-config-opt type-name opt-name config) 
  (let* ((type-opts (assq type-name config))
         (opt (and type-opts (assq opt-name (cadr type-opts)))))
    (if opt
      (cadr opt)
      (cadr (assq opt-name (cadr (assq 'default config)))))))


(define default-inspect-config
  `((default 
      ((inspector ,simple-inspector)
       (type-namer ,noop-type-namer)))

    (input-port 
      ((inspector ,default-port-inspector)))

    (output-port 
      ((inspector ,default-port-inspector)))

    (procedure
      ((inspector ,default-proc-inspector)
       (type-namer ,default-proc-type-namer)))

    (singleton
      ((inspector ,default-singleton-inspector)))))

;;; ---------- INSPECTION ---------- ;;;

(define (inspect-data rib (config '()))
  (inspect-data! (rib-deep-copy rib) (append config default-inspect-config)))

(define (inspect-data! rib config)
  (if (number? rib)
    rib
    (let* ((type-name (cadr (assv (%field2 rib) data-types)))
           (inspector (get-config-opt type-name 'inspector config))
           (type-namer (get-config-opt type-name 'type-namer config)))
      (%rib (inspector rib type-name config) (type-namer rib type-name config) meta-rib-data-type))))


(define (inspected-op rib)
  (##inspect-op (rib-deep-copy rib)))

(define (##inspect-op inspected-rib)
  (let ((op-type (assv (%field0 inspected-rib) op-types)))
    ))


(define (inspect-proc rib))


;;; ---------- WRITING ---------- ;;;

(define-inspector-writer 
  simple-data-writer
  (display "(" port)
  (display rib-type port)
  (display " " port)
  (write-inspected-data (%field0 data) config port)
  (display " " port)
  (write-inspected-data (%field1 data) config port)
  (display ")" port))

(define-inspector-writer 
  string-data-writer
  (display "(" port)
  (display rib-type port)
  (display " " port)
  (write-inspected-data (%field1 data) config port)
  (display " " port)
  (if (meta-rib-data? (%field0 data))
    (write-inspected-data (%field0 data) config port)
    (write-inspected-data data config port))
  (display ")" port))

(define-inspector-writer 
  primitive-procedure-writer
  (display "(primitive-procedure " port)
  (display (%field0 data) port)
  (display " " port)
  (display (list-ref symbol-table (%field0 data)) port)
  (display ")\n" port))

(define-inspector-writer
  procedure-writer
  (display "lambda ()" port))

(define-inspector-writer
  singleton-writer
  (display "'" port)
  (write data port))

(define default-writer-config
  `((default
      ((writer ,simple-data-writer)))

    (primitive-procedure
      ((writer ,primitive-procedure-writer)))

    (procedure
      ((writer ,primitive-writer)))

    (singleton
      ((writer ,singleton-writer)))))

(define (write-inspected inspected-rib (config '()) (port (current-output-port)))
  (set! config (append config default-writer-config))
  (if (meta-rib-data? inspected-rib)
    (write-inspected-data inspected-rib config port)
    (write-inspected-op inspected-rib config port)))


(define (write-inspected-data inspected-rib config (port (current-output-port)))
  (cond 
    ((number? inspected-rib)
     (display inspected-rib))
    ((meta-rib-data? inspected-rib)
     (let* ((data (%field0 inspected-rib))
            (rib-type (%field1 inspected-rib))
            (writer (get-config-opt rib-type 'writer config)))
       (writer data rib-type config port)))
    (else 
      (write inspected-rib))))

(define (write-inspected-op inspected-rib))








;;; ---------- UTILS ---------- ;;;

(define (rib-deep-copy rib)
  (if (or (not (%rib? rib)) (singleton? rib))
    rib
    (%rib (rib-deep-copy (%field0 rib)) (rib-deep-copy (%field1 rib)) (rib-deep-copy (%field2 rib)))))

