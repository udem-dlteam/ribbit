(%%include-once "./macros.scm")

(%%define-resource-reader
 ipfs
 (lambda (resource-path)
   (open-input-string (pipe-through (string-append "ipfs cat " resource-path) ""))))
