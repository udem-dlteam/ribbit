(##include-once "./macros.scm")

(##define-resource-reader
 http
 (lambda (resource-path)
   (shell-cmd (string-append "curl --silent http://" resource-path))))
