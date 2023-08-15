(define (##style-to-code style)
  (case style
    ((bold) "1")
    ((dim) "2")
    ((underline) "4")
    ((blink) "5")
    ((reverse) "7")
    ((hidden) "8")
    ((black) "30")
    ((red) "31")
    ((green) "32")
    ((yellow) "33")
    ((blue) "34")
    ((magenta) "35")
    ((cyan) "36")
    ((white) "37")
    ((bg-black) "40")
    ((bg-red) "41")
    ((bg-green) "42")
    ((bg-yellow) "43")
    ((bg-blue) "44")
    ((bg-magenta) "45")
    ((bg-cyan) "46")
    ((bg-white) "47")
    (else (error "Unknown style" style))))


(define (display-with-style o . styles)
  (apply set-display-style! styles)
  (display o)
  (reset-display-style!))


(define (set-display-style! . styles)
  (reset-display-style!)
  (display (string-append 
             "\\033["
             (string-concatenate (map ##style-to-code styles) ";")
             "m")))

(define (reset-display-style!)
  (display "\\033[0m"))
