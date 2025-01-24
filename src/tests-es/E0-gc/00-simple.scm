(viz_heap "simple_0.dot")

(define a (##rib 0 0 0))
(define b (##rib 0 0 0))

(gc_check)
(##field1-set! a b)
(viz_heap "simple_1.dot")

;;;run: -l es-tests
;;;expected:
;;;***REMANING_RIBS=0
;;;***ALIVE_BUT_COLLECTED=0
;;;***REMANING_RIBS=0
;;;***ALIVE_BUT_COLLECTED=0
