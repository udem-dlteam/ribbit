(define a (##rib (##rib 999 999 999) 0 0))

(viz_heap "dealloc_0.dot")
(gc_check)
(reset_d_count)
(##field0-set! a 0) ;; should deallocate a rib
(print_d_count)
(viz_heap "dealloc_1.dot")
(viz_heap "graph.dot")
(##field1-set! a 0)
(viz_heap "dealloc_2.dot")


;;;run: -l es-tests
;;;expected:
;;;***REMANING_RIBS=0
;;;***ALIVE_BUT_COLLECTED_RIBS=0
;;;***REMANING_RIBS=0
;;;***ALIVE_BUT_COLLECTED_RIBS=0
