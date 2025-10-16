(%%include-once "./macros.scm")

(%%define-resource-reader
 http
 (lambda (resource-path)
   (let ((resource-path-real 
           (if (string-prefix? "http:/" resource-path) 
             (string-append "http://" (substring resource-path (string-length "http:/") (string-length resource-path)))
             (string-append "http://" resource-path))))
     (open-input-string (pipe-through (string-append "curl --silent " resource-path-real) "")))))

(%%define-resource-reader
 https
 (lambda (resource-path)
   (let ((resource-path-real 
           (if (string-prefix? "https:/" resource-path) 
             (string-append "https://" (substring resource-path (string-length "https:/") (string-length resource-path)))
             (string-append "https://" resource-path))))
     (open-input-string (pipe-through (string-append "curl --silent " resource-path-real) "")))))
