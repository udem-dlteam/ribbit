(define-primitive (get-interrupt)
  "{
  if (irqs.i == 0) {
    push2(FALSE,PAIR_TAG);
  } else {
    int __number = irqs.data[irqs.i-1];
    irqs.i--;
    push2(TAG_NUM(__number), PAIR_TAG);
  }
  break;
  }")

(define-primitive (outb port number)
  "{
  PRIM2();
  outb((unsigned short)NUM(x),(unsigned char)NUM(y));
  push2(FALSE,PAIR_TAG);
  break;
  }")

(define-primitive (inb port)
  "{
  PRIM1();
  push2(TAG_NUM(inb((unsigned short)NUM(x))),PAIR_TAG); 
  break;
  }")

(define-primitive (lor a b)
  "{
  PRIM2();
  push2(TAG_NUM(NUM(x) | NUM(y)),PAIR_TAG); 
  break;
  }")

(define-primitive (land a b)
  "{
  PRIM2();
  push2(TAG_NUM(NUM(x) & NUM(y)),PAIR_TAG); 
  break;
  }")

(define-primitive (lxor a b)
  "{
  PRIM2();
  push2(TAG_NUM(NUM(x) ^ NUM(y)),PAIR_TAG); 
  break;
  }")

(define-primitive (lshift a b)
  "{
  PRIM2();
  push2(TAG_NUM(NUM(x) << NUM(y)),PAIR_TAG); 
  break;
  }")
(define-primitive (set-interrupts)
  "{
  __asm__(\"sti\");
  push2(FALSE,PAIR_TAG);
  }")
(define-primitive (disable-interrupts)
  "{
  __asm__(\"cli\");
  push2(FALSE,PAIR_TAG);
  }")


(define com (lambda (k) (+ 1016 k)))

(define setup-serial (lambda (com)
  (begin
  (outb (com 1) 0)
  (outb (com 3) (lshift 1 7))
  (outb (com 0) 3)
  (outb (com 1) 0)
  (outb (com 3) 7)
  (outb (com 2) 199)
  (outb (com 4) 11)
  (outb (com 4) 30)
  (outb (com 0) 23)
  (let ((result (= (inb (com 0)) 23)))
    (begin
      (outb (com 4) 15)
      (outb (com 1) 1) 
      result)))))


(define is-clear (lambda () (not (= (land (inb (com 5)) 32) 0))))
(define is-recv-clear (lambda () (not (= (land (inb (com 5)) 1) 0))))

(define send-serial
  (lambda (x) 
    (if (is-clear) (outb (com 0) x) (send-serial x))))

(define recv-serial
  (lambda () 
    (if (is-recv-clear) (inb (com 0)) (recv-serial))))

(define send-str-serial-len
  (lambda (str len)
    (if (= len 0)
      #f
      (begin
        (send-serial (car str))
        (send-str-serial-len (cdr str) (- len 1))))))

(define send-str-serial
  (lambda (str) (send-str-serial-len (car str) (cdr str))))

(define %%%write-char-fd %%write-char-fd)
(define keyboard-read %%read-char-fd)

(define get-input recv-serial)

(define is-serial-ready #f)

(define set-keyboard-input
  (lambda () (set! get-input (lambda () (keyboard-read #f)))))

(define set-serial-input
  (lambda () (set! get-input recv-serial)))

(define enable-echo 
  (lambda () (set! %%read-char-fd echo-read)))

(define disable-echo 
  (lambda () (set! %%read-char-fd noecho-read)))

(define (%%write-char-fd ch fd)
  (begin
  (if (not is-serial-ready)
    (begin
    (setup-serial com)
    (set! is-serial-ready #t))
    #f)
  (%%%write-char-fd ch fd)
  (send-serial ch)
  ch))

(define (noecho-read fd) (get-input))
(define (echo-read fd) 
  (%%write-char-fd (get-input) fd))
(define %%read-char-fd echo-read)

