;;; Frédéric Lahaie-Bertrand 

;;; Implementation of the Ribbit VM in Common Lisp.

;; @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
;; RVM code that prints HELLO!
(defvar *rvm-code* ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y")
;; )@@

(defvar *pair-type*      0)
(defvar *procedure-type* 1)
(defvar *symbol-type*    2)
(defvar *string-type*    3)
(defvar *vector-type*    4)
(defvar *singleton-type* 5)

(defun _rib? (x) (vectorp x))
(defun _rib (x y z) (vector x y z))
(defun _field0 (vec) (aref vec 0))
(defun _field1 (vec) (aref vec 1))
(defun _field2 (vec) (aref vec 2))
(defun _field0-set! (vec val) (setf (aref vec 0) val))
(defun _field1-set! (vec val) (setf (aref vec 1) val))
(defun _field2-set! (vec val) (setf (aref vec 2) val))

(defun _pair? (vec) (and (_rib? vec) (eql (_field2 vec) *pair-type*)))
(defun _cons (car cdr) (_rib car cdr *pair-type*))
(defun _car (pair) (_field0 pair))
(defun _cdr (pair) (_field1 pair))
(defun _set-car! (pair val) (_field0-set! pair val))

(defvar _false (_rib 0 0 *singleton-type*))
(defvar _true  (_rib 0 0 *singleton-type*))
(defvar _nil   (_rib 0 0 *singleton-type*))

(defun _length (list)
  (if (_pair? list)
      (+ 1 (_length (_cdr list)))
    0))

(defun _list-tail (list i)
  (if (< 0 i)
      (_list-tail (_cdr list) (- i 1))
    list))

(defun _list->string (list) (_rib list (_length list) *string-type*))

(defun _string->uninterned-symbol (str) (_rib _false str *symbol-type*))

;; named let for common lisp (stolen from Let Over Lambda)
(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
	   (,n ,@(mapcar #'cadr letargs))))

;;------------------------------------------------------------------------------
;; DEBUG

(defvar *debug*       nil)
(defvar *tracing*     nil)
(defvar *step-count*    0)
(defvar *start-tracing* 0)
(defvar *next-stamp*    0)

(defun show (obj)
  (if (not (_rib? obj))
      (princ obj)
    
    (let ((type (_field2 obj)))
      
      (if (= type 4)
	  (progn (write-string "#") (show (_field0 obj)))
	
	(case type
	      
	      ((0) ;; pair
	       (write-string "(")
	       (show (_field0 obj))
	       (let ((obj
		      (nlet loop5 ((n 1) (obj (_field1 obj)))
			    (if (and (_rib? obj) (= (_field2 obj) 0))
				(if (> n 4)
				    (progn
				      (write-string " ...")
				      _nil)
				  (progn
				    (write-string " ")
				    (show (_field0 obj))
				    (loop5 (+ n 1) (_field1 obj))))
			      obj))))
		 (if (not (eql obj _nil))
		     (progn
		       (write-string " .")
		       (show obj)))
		 (write-string ")")))

	      ((1) ;; procedure
	       (if (_rib? (_field0 obj))
		   (format t "#<procedure nparams=~a>" (_field0 (_field0 obj)))
		 (format t "#<primitive ~a>" (_field0 obj))))

	      ((2) ;; symbol
	       (let ((obj (_field1 obj)))
		 (if (and (_rib? obj)
			  (= (_field2 obj) 3)
			  (> (_field1 obj) 0))
		     (nlet loop6 ((obj (_field0 obj)))
			   (if (and (_rib? obj)
				    (= (_field2 obj) 0))
			       (progn
				 (write-char (code-char (_field0 obj)))
				 (loop6 (_field1 obj)))))
		   
		   (format t "#<symbol ~a>" (show obj)))))

	      ((3) ;; string
	       (write-string "\"")
	       (nlet loop7 ((obj (_field0 obj)))
		     (if (and (_rib? obj)
			      (= (_field2 obj) 0))
			 (let ((c (_field0 obj)))
			   (case c
				 ((10) (write-string "\\n"))
				 ((13) (write-string "\\r"))
				 ((9)  (write-string "\\t"))
				 ((92) (write-string "\\\\"))
				 ((34) (write-string "\\\""))
				 (otherwise (princ (code-char c))))
			   (loop7 (_field1 obj)))
		       (write-string "\""))))

	      ((5) ;; special
	       (cond
		((eql obj _false) (write-string "#f"))
		((eql obj _true)  (write-string "#t"))
		((eql obj _nil)   (write-string "()"))
		(t
		 (format t "[~a,~a,~a]"
		       (show (_field0 obj))
		       (show (_field1 obj))
		       (show (_field2 obj))))))

	      (otherwise 
	       (format t "[~a,~a,~a]"
		       (show (_field0 obj))
		       (show (_field1 obj))
		       (show (_field2 obj)))))))))
        

(defun start-step (stack)
  (setf *step-count* (+ *step-count* 1))
  (if (>= *step-count* *start-tracing*)
      (setf *tracing* t))
  (if (not *tracing*)
      (if (>= *step-count* *next-stamp*)
	  (progn
	    (setf *next-stamp* (values (floor (+ (* *next-stamp* 1.01) 1) 1)))
	    (write-string "@")
	    (princ *step-count*)
	    (terpri)))
    (progn
      (write-string "@")
      (princ *step-count*)
      (write-string " STACK = (")
      (nlet loop9 ((s stack) (sep ""))
	    (if (eql (_field2 s) 0)
		(progn
		  (write-string sep)
		  (show (_field0 s))
		  (loop9 (_field1 s) " "))
	      (progn
		(write-string ")")
		(terpri)))))))

(defun trace-instruction (name opnd)
  ;; (if opnd
  ;;     (format t "~a ~a~%" name opnd)
  ;;   (format t "~a~%" name)))
  (write-string name)
  (if opnd
      (progn
	(write-string " ")
	(show opnd)))
  (terpri))
      
;;------------------------------------------------------------------------------
;; DECODE

(defvar eb/2 46) ;; half of encoding base (92)
(defvar pos 0)

(defun get-byte ()
  (let ((x (char-code (char *rvm-code* pos))))
    (incf pos)
    x))

(defun get-code ()
  (let ((x (- (get-byte) 35)))
    (if (< x 0) 57 x)))

(defun get-int (n)
  (let ((x (get-code))
	(y (* n eb/2)))
    (if (< x eb/2)
	(+ y x)
      (get-int (+ y (- x eb/2))))))

;; Build symbol table

(defun build-symtable (&optional (n (get-int 0)) (symtbl _nil))
  (flet
   ((add-symbol (chars symtable)
		(_cons (_string->uninterned-symbol (_list->string chars))
		       symtable)))
   (if (< 0 n)
       (build-symtable (- n 1) (add-symbol _nil symtbl))    
     (labels
      ((loop2 (&optional (st symtbl))
	      (labels
	       ((loop3 (&optional (chars _nil))
		       (let ((x (get-byte)))
			 (if (= x 44)
			     (loop2 (add-symbol chars st))
			   (if (= x 59)
			       (add-symbol chars st)
			     (loop3 (_cons x chars)))))))
	       (loop3))))
      (loop2)))))
	
;; Decode VM instructions

(defun decode ()
  (let ((symtbl (build-symtable)))
    (labels
     ((decode-loop (stack) 
        (let ((x (get-code)))
          (labels
            ((sym (n)
	  	  (_car (_list-tail symtbl n)))
	     (add-instruction (op opnd stack)
			      (_set-car! stack (_rib op opnd (_car stack)))
			      (decode-loop stack))
	     (loop1 (&optional (op 0) (n x))
		    (let ((d (aref '#(20 30 0 10 11 4) op)))
		      (if (< (+ 2 d) n)
			  (loop1 (+ op 1) (- n (+ d 3)))
			(if (< 90 x)
			    (add-instruction 4 ;; if
					     (_car stack)
					     (_cdr stack))
			  (let ((stack stack)
				(opnd n))
			    (if (= op 0) (setf stack (_cons 0 stack)))
			    (if (< n d)
				(if (< op 3) (setf opnd (sym n)))
			      (if (= n d)
				  (setf opnd (get-int 0))
				(setf opnd (sym (get-int (- (- n d) 1))))))
			    (if (< 4 op)
				(let ((proc (_rib (_rib opnd 0 (_car stack))
						  _nil
						  *procedure-type*))
				      (stack (_cdr stack)))
				  (if (_rib? stack)
				      (add-instruction 3 ;; const-proc
						       proc
						       stack)
				    proc))
			      (if (< 0 op)
				  (add-instruction (- op 1) opnd stack)
				(add-instruction 0 opnd stack)))))))))
	    (loop1)))))
    
     (let ((main-proc (decode-loop 0)))
       (flet
	((set-global (val)
		     (_field0-set! (_car symtbl) val)
		     (setf symtbl (_cdr symtbl))))
	
	;; set predefined globals (always 4 first in the symbol table)
	
	(set-global (_rib 0 symtbl *procedure-type*)) ;; rib  = primitive 0
	(set-global _false) ;; false  = #f
	(set-global _true)  ;; true   = #t
	(set-global _nil)   ;; nil    = ()

	main-proc)))))

;;------------------------------------------------------------------------------
;; PRIMITIVES

(defun prim0 (f)
  (lambda (stack)
    (_cons (funcall f) stack)))

(defun prim1 (f)
  (lambda (stack)
    (let* ((x (_car stack)) (stack (_cdr stack)))
      (_cons (funcall f x) stack))))

(defun prim2 (f)
  (lambda (stack)
    (let* ((y (_car stack)) (stack (_cdr stack))
	   (x (_car stack)) (stack (_cdr stack)))
      (_cons (funcall f x y) stack))))

(defun prim3 (f)
  (lambda (stack)
    (let* ((z (_car stack)) (stack (_cdr stack))
           (y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (funcall f x y z) stack))))

(defun bool (x)
  (if x _true _false))

(defvar *primitives*
  (vector
   ;; @@(primitives (gen body)
   (prim3 #'_rib)                                ;; @@(primitive (rib a b c))@@
   (prim1 #'identity)                            ;; @@(primitive (id x))@@
   #'_cdr                                        ;; @@(primitive (arg1 a b))@@
   (prim2 (lambda (y x) (declare (ignore y)) x)) ;; @@(primitive (arg2 a b))@@
   ;; @@(primitive (close rib)
   (lambda (stack)
     (let* ((x (_car stack)) (stack (_cdr stack)))
       (_cons (_rib (_field0 x) stack *procedure-type*)
	      stack)))
   ;; )@@
   (prim1 (lambda (x) (bool (_rib? x))))         ;; @@(primitive (rib? rib))@@
   (prim1 #'_field0)                             ;; @@(primitive (field0 rib))@@
   (prim1 #'_field1)                             ;; @@(primitive (field1 rib))@@
   (prim1 #'_field2)                             ;; @@(primitive (field2 rib))@@
   (prim2 (lambda (x y) (_field0-set! x y) y))   ;; @@(primitive (field0-set! rib v))@@
   (prim2 (lambda (x y) (_field1-set! x y) y))   ;; @@(primitive (field1-set! rib v))@@
   (prim2 (lambda (x y) (_field2-set! x y) y))   ;; @@(primitive (field2-set! rib v))@@
   (prim2 (lambda (x y) (bool (eql x y))))       ;; @@(primitive (eqv? x y))@@
   (prim2 (lambda (x y) (bool (< x y))))         ;; @@(primitive (< x y))@@
   (prim2 #'+)                                   ;; @@(primitive (+ a b))@@
   (prim2 #'-)                                   ;; @@(primitive (- a b))@@
   (prim2 #'*)                                   ;; @@(primitive (* a b))@@
   ;; @@(primitive (quotient a b)
   (prim2 (lambda (x y)
	    (if (and (>= x 0) (>= y 0))
		(values (floor (/ x y)))
	      (+ 1 (values (floor (/ x y)))))))
   ;; )@@
   ;; @@(primitive (getchar)
   (prim0 (lambda ()
	    (if (< pos (length *rvm-code*))
		(get-byte)
	      (let ((c (read-char)))
		(if (characterp c)
		    (char-code c)
		  -1)))))
   ;; )@@
   ;; @@(primitive (putchar x)
   (prim1 (lambda (x)
	    (write-char (code-char x))
	    (finish-output)
	    x))
   ;; )@@
   (prim1 (lambda (x)
	    (declare (ignore x))
	    (sb-ext:exit :code 0)))
   ;; )@@
   ))

;;------------------------------------------------------------------------------
;; RUN PROGRAM

(defun get-cont (stack)
  (if (_rib? (_field2 stack))
      stack
    (get-cont (_cdr stack))))

(defun get-var (stack opnd)
  (if (_rib? opnd)
      (_field0 opnd)
    (_field0 (_list-tail stack opnd))))

(defun set-var (stack opnd val)
  (if (_rib? opnd)
      (_field0-set! opnd val)
    (_field0-set! (_list-tail stack opnd) val)))

(defun run (pc stack)
  (if *debug* (start-step stack)) 
  (let ((instr (_field0 pc))
	(opnd  (_field1 pc))
	(next  (_field2 pc)))    
    (case instr
	  
	  ((0) ;; jump/call
	   (if *tracing*
	       (if (eql 0 next)
		   (trace-instruction "jump" opnd)
		 (trace-instruction "call" opnd)))
	   
	   (let* ((proc (get-var stack opnd))
		  (code (_field0 proc))
		  ;; @@(feature arity-check (use rest-param)
		  (ncall (_car stack))  
		  (stack (_cdr stack))
		  ;; )@@
		  )
	     (if (_rib? code)
		 
		 ;; calling a lambda
		 (let* ((new-cont (_rib 0 proc 0))
			(nargs (ash (_field0 code) -1))
			(vari (logand (_field0 code) 1))
			(new-stack new-cont))
		   ;; @@(feature arity-check
		   (if (or (and (eql vari 0) (not (eql nargs ncall)))
			   (and (eql vari 1) (> nargs ncall)))
		       (error "*** Arrity check failed"))
		   ;; )@@
		   ;; @@(feature rest-param (use arity-check)
		   (if (eql vari 1)
		       (let ((rest _nil) (i (- ncall nargs)) (_stack stack))
			 (dotimes (_n i)
			   (setf rest (_cons (_car _stack) rest))
			   (_stack (_cdr _stack)))
			 (setf stack (_cons rest _stack))
			 (setf nargs (+ 1 nargs))))
		   ;; )@@
		   (dotimes (_n nargs)
		     (setf new-stack (_cons (_car stack) new-stack))
		     (setf stack (_cdr stack)))
		   (if (_rib? next) ;; non-tail call?
		       (progn
			 (_field0-set! new-cont stack)
			 (_field2-set! new-cont next))
		     (let ((k (get-cont stack)))
		       (_field0-set! new-cont (_field0 k))
		       (_field2-set! new-cont (_field2 k))))
		   (run (_field2 code) new-stack))
	       
	       ;; calling a primitive
	       (let ((stack (funcall (aref *primitives* code) stack)))
		 (if (_rib? next)  ;; non-tail call?
		     (run next stack)
		   (let ((cont (get-cont stack)))
		     (_field1-set! stack (_field0 cont))
		     (run (_field2 cont) stack)))))))
	  
	  ((1) ;; set
	   (if *tracing* (trace-instruction "set" opnd))
	   (set-var stack opnd (_car stack))
	   (run next (_cdr stack)))
	  
	  ((2) ;; get
	   (if *tracing* (trace-instruction "get" opnd))
	   (run next (_cons (get-var stack opnd) stack)))
	  
	  ((3) ;; const
	   (if *tracing* (trace-instruction "const" opnd))
	   (run next (_cons opnd stack)))

	  ((4) ;; if
	   (if *tracing* (trace-instruction "if" nil))
	   (if (eql (_car stack) _false)
	       (run next (_cdr stack))
	     (run opnd (_cdr stack))))

	  (otherwise ;; halt
	   (if *tracing* (trace-instruction "halt" nil))
	   nil))))

(let ((x (decode)))
  (run (_field2 (_field0 x))     ;; instruction stream of main procedure
      (_rib 0 0 (_rib 5 0 0))))  ;; primordial continuation = halt
