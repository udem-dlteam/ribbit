(define-feature 
  hs/io-handle 
  ((hs/foreign-type "    | RibHandle !Handle")))

(define-primitive
  (%%stdin-fd)
  (use hs/io-handle)
  " , (push . RibForeign $ RibHandle stdin) >> return ribNil")

(define-primitive
  (%%stdout-fd)
  (use hs/io-handle)
  " , (push . RibForeign $ RibHandle stdout) >> return ribNil")

(define-primitive 
  (%%get-fd-input-file filename)
  (use hs/io-handle scm2str)
  " , prim1 $ \\filename -> scm2str filename >>= (\\x -> openFile x ReadMode) >>= (pure . RibForeign . RibHandle)")

(define-primitive
  (%%get-fd-output-file filename)
  (use hs/io-handle scm2str)
  " , prim1 $ \\filename -> scm2str filename >>= (\\x -> openFile x WriteMode) >>= (pure . RibForeign . RibHandle)")

(define-primitive
  (%%read-char-fd fd)
  (use hs/io-handle)
  " , prim1 $ \\(RibForeign (RibHandle handle)) -> hIsEOF handle >>= \\eof -> if eof then return ribNil else hGetChar handle >>= (pure . RibInt . ord)")

(define-primitive
  (%%write-char-fd ch fd)
  (use hs/io-handle)
  " , prim2 $ \\(RibInt ch) (RibForeign (RibHandle handle)) -> hPutChar handle (chr ch) >> pure ribTrue")

(define-primitive
  (%%close-input-fd fd)
  (use hs/io-handle)
  " , prim1 $ \\(RibForeign (RibHandle handle)) -> hClose handle >> pure ribTrue")

(define (%%close-output-fd port) (%%close-input-fd port))
