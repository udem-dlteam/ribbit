(##include-once "./macros.scm")

(##define-resource-reader
 ipfs
 (lambda (resource-path)
   (shell-cmd (string-append "ipfs cat " resource-path))))
