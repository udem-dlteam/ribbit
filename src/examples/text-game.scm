;; This example showcases how to create a simple text-based game in Scheme using Ribbit.
;; Notably, it adds the ability to clear the screen in the console using the `clear` primitive.
;;
;; You can either compile this examples to JavaScript or Python.
;;
;; [JavaScript]
;; $ ./rsc.exe -t js -l r4rs/tc -f+ js/web -o game.js examples/text-game.scm
;; $ cat host/js/console.html | sed "s/INPUT_JAVASCRIPT_FILE_HERE.js/game.js/" > game.html
;; $ open game.html
;;
;; [Python]
;; $ ./rsc.exe -t py -l r4rs/tc -o game.py examples/text-game.scm
;; $ python3 game.py


;; Define the clear primitive for JavaScript and Python hosts
(cond-expand
  ((host js)

   ;; In JavaScript, we clear the screen by removing the content of the textarea
   ;; used as a console.
   (define-primitive (clear)
  "() => {
    document.getElementById('repl').value=''; // Clear the repl
    return push(0); // Primitives always push something to the stack
  }, // Always need a comma after a primitive
  "))

  ((host py)

   ;; In python, we clear the screen by calling the 'clear' program using the os.system function.
   (define-feature
     clear
     ((import "import os")))

   (define-primitive (clear)
  "prim(0, lambda: os.system('clear')),")))


;; Utility function to ask questions to the user
(define (ask-question question)
  (display question)
  (newline)
  (read-line))

;; Utility function to ask a question with multiple choices
(define (choice-question question choices)
  (display question)
  (display " [")
  (let loop ((choices choices))
    (display (car choices))
    (if (null? (cdr choices))
        (display "] ")
        (begin
          (display ", ")
          (loop (cdr choices)))))
  (newline)
  (let ((answer (read-line)))
    (if (member answer choices)
        answer
        (begin
          (display "(Invalid answer) ")
          (choice-question question choices)))))


;; Game logic
(define (main)

  (clear)

  (display
    "Welcome, you are a detective investigating the death of Watson. He was murdered in his apartment at night.")

  (let ((choice
          (choice-question
            "What do you want to investigate first?"
            '("apartment" "local bar" "workplace"))))

    (let ((response (ask-question (string-append "What do you want to search first at " choice " ?"))))
      (display "You choose search about '")
      (display response)
      (display "' with some success")
      (newline)
      (ask-question "The game is finished. Press ENTER to restart the game.")
      (main))))

(main)
