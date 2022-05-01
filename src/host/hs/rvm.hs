-- | tortue
-- | A Ribbit Scheme VM implementation in Haskell
-- | Copyright 2022 - Laurent Huberdeau & Marc-André Bélanger
{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, Strict #-}

module Main where
import Control.Monad
import Data.Char
import Data.Foldable
import Data.IORef
import GHC.IO
import System.Environment
import System.IO


-- Utils

-- newRef :: a -> IO (IORef a) -- Debug
newRef = newIORef

-- readRef :: IORef a -> IO a -- Debug
readRef = readIORef

-- writeRef :: IORef a -> a -> IO () -- Debug
writeRef = writeIORef

-- Instead of using the TupleSection extension, we define a synonym that can be partially applied
-- pair :: a -> b -> (a, b) -- Debug
pair a b = (a,b)


-- Rib

-- Rib Objects

data Rib = RibInt !Int | RibRef !(IORef RibObj) deriving (Eq)

data RibObj = RibObj { field0 :: !Rib, field1 :: !Rib, field2 :: !Rib} deriving (Eq)

-- Typeclass permettant l'utilisation de mkObj sans avoir à toujours créer des
-- IORef manuellement.
-- toRib permet la conversion d'une valeur de type a en IORef Rib par une action IO.
class ToRib a where toRib :: a -> IO Rib

-- Rien à faire, on a déjà un IORef Rib
instance ToRib Rib where toRib = pure

-- On crée une référence pour le Rib
instance ToRib RibObj where toRib = fmap RibRef . newRef

-- On wrap le Int dans RibInt et on crée une référence
instance ToRib Int where toRib = toRib . RibInt

-- On convertit le char en RibInt et on crée une référence
-- instance ToRib Char where toRib = toRib . RibInt . ord

-- mkObj :: (ToRib a, ToRib b, ToRib c) => a -> b -> c -> IO Rib -- Debug
mkObj tag r1 r2 = toRib =<< RibObj <$> toRib r1 <*> toRib r2 <*> toRib tag

-- mkInstr :: (ToRib a, ToRib b, ToRib c) => a -> b -> c -> IO Rib -- Debug
mkInstr tag r1 r2 = toRib =<< RibObj <$> toRib tag <*> toRib r1 <*> toRib r2

-- readN :: (RibObj -> Rib) -> Rib -> IO Rib -- Debug
readN f (RibRef r) = f <$> readRef r
-- readN f (RibInt n) = error $ "readN: RibInt " <> show n <> " is not a pointer" -- Debug

-- writeN :: (RibObj -> RibObj) -> Rib -> IO () -- Debug
writeN f (RibRef r) = readRef r >>= writeRef r . f
-- writeN f (RibInt n) = error $ "writeN: RibInt " <> show n <> " is not a pointer" -- Debug

-- read0, read1, read2 :: Rib -> IO Rib -- Debug
read0 = readN field0
read1 = readN field1
read2 = readN field2

-- write0, write1, write2 :: Rib -> Rib -> IO () -- Debug
write0 r v = writeN (\obj -> obj {field0 = v}) r
write1 r v = writeN (\obj -> obj {field1 = v}) r
write2 r v = writeN (\obj -> obj {field2 = v}) r

-- cons, mkProc, mkSymb, mkStr, mkVect :: (ToRib a, ToRib b) => a -> b -> IO Rib -- Debug
cons = mkObj (RibInt 0)
mkProc = mkObj (RibInt 1)
mkSymb = mkObj (RibInt 2)
mkStr = mkObj (RibInt 3)
mkVect = mkObj (RibInt 4)

-- mkSVal :: IO Rib -- Debug
mkSVal = mkObj (RibInt 5) (RibInt 0) (RibInt 0) -- Don't care about zeroes

{-# NOINLINE ribFalse #-}
-- ribFalse, ribTrue, ribNil :: Rib -- Debug
ribFalse = unsafePerformIO (toRib =<< mkSVal)

{-# NOINLINE ribTrue #-}
ribTrue = unsafePerformIO (toRib =<< mkSVal)

{-# NOINLINE ribNil #-}
ribNil = unsafePerformIO (toRib =<< mkSVal)

-- Fonctions de conversion Haskell -> Rib

-- toRibList :: (ToRib a) => [a] -> IO Rib -- Debug
toRibList = foldrM cons ribNil

-- toRibString :: String -> IO Rib -- Debug
toRibString chars = toRibList (ord <$> chars) >>= flip mkStr (length chars)

-- toRibSymbol :: String -> IO Rib -- Debug
toRibSymbol = (=<<) (mkSymb (RibInt 0)) . toRibString


-- VM

-- Reading input

-- Positive numbers outside [0,45] are encoded using a variable length encoding.
-- If the number is encoded in k characters (codes), the first k-1 are in
-- [46, 91] and the k^th code is in [0,45] to mark the end of the number.
-- The codes encode the number in base-46, and are interpreted modulo 46.
-- readInt :: String -> Int -> (String, Int) -- Debug
readInt [] n = ([], n)
readInt (x:xs) n = let c=code x; n' = n * 46 in if c<46 then (xs,n'+c) else readInt xs (n'+c-46)

-- code :: Char -> Int -- Debug
code x = let v=ord x-35 in if v<0 then 57 else v

-- VM environment

type Prim = IO ()

-- push :: ToRib a => a -> IO () -- Debug
push v = getStack >>= cons v >>= setStack

-- pop :: IO Rib -- Debug
pop = getStack >>= \case RibRef r -> readRef r >>= \(RibObj top rest _) -> setStack rest >> pure top
 -- RibInt _ -> error "Empty stack" -- Debug

-- Primitives

-- prim1 :: (Rib -> IO Rib) -> Prim -- Debug
prim1 f = pop >>= f >>= push

-- prim2 :: (Rib -> Rib -> IO Rib) -> Prim -- Debug
prim2 f = flip (,) <$> pop <*> pop >>= uncurry f >>= push

-- prim3 :: ((Rib,Rib,Rib) -> IO Rib) -> Prim -- Debug
prim3 f = (,,) <$> pop <*> pop <*> pop >>= f >>= push

-- safeGetChar :: IO Int -- Debug
safeGetChar = fmap ord getChar `catchAny` const (pure (-1))

-- close :: Prim -- Debug
close = do
 v1 <- pop >>= read0
 getStack >>= mkProc v1 >>= push

-- primitives :: [Prim] -- Debug
primitives =
 [ prim3 (\(c,b,a) -> toRib $ RibObj a b c)                     -- rib object constructor
 , prim1 pure                                    -- id
 , void pop                                     -- take 2 TOS, keep first
 , prim2 (const pure)                                -- take 2 TOS, keep second
 , close                                       -- close
 , prim1 (pure . (\case RibInt _ -> ribFalse; _ -> ribTrue))             -- rib?
 , prim1 read0                                    -- field0 -- 6
 , prim1 read1                                    -- field1 -- 7
 , prim1 read2                                    -- field2 -- 8
 , prim2 $ writePrim write0                             -- field0-set! -- 9
 , prim2 $ writePrim write1                             -- field1-set! -- 10
 , prim2 $ writePrim write2                             -- field2-set! -- 11
 , prim2 $ \r1 r2 -> toBool (r1 == r2)                        -- eqv?
 , prim2 $ \(RibInt r1) (RibInt r2) -> toBool (r1 < r2)               -- <
 , prim2 $ onInt (+)                                 -- add
 , prim2 $ onInt (-)                                 -- sub
 , prim2 $ onInt (*)                                 -- mult
 , prim2 $ onInt quot                                -- quotient
 , safeGetChar >>= push . RibInt                       -- getChar
 , prim1 (\r@(RibInt v) -> putChar (chr v) >> pure r)            -- putChar
 ]

-- writePrim :: Monad m => (t -> b -> m a) -> t -> b -> m b -- Debug
writePrim f r v = f r v >> pure v

-- onInt :: Applicative f => (Int -> Int -> Int) -> Rib -> Rib -> f Rib -- Debug
onInt f (RibInt a) (RibInt b) = pure $ RibInt (f a b)

-- toBool :: Applicative f => Bool -> f Rib -- Debug
toBool b = pure $ if b then ribTrue else ribFalse

-- Initializing symbol table

-- Définition alternative de initialSymbolTable plus idiomatique.
-- initialSymbolTable :: String -> Int -> IO Rib -- Debug
initialSymbolTable symTblStr emptySymCount = toRibList . reverse =<< mapM toRibSymbol (replicate emptySymCount "" <> splitOnCommas symTblStr)

-- splitOnCommas :: String -> [String] -- Debug
splitOnCommas xs = case span (/= ',') xs of
 (sym, "") -> [reverse sym]
 (sym, rest) -> reverse sym : splitOnCommas (drop 1 rest)

-- Decoding RVM instructions

-- symbolRef :: Rib -> Int -> IO Rib -- Debug
symbolRef symbolTable n = read0 =<< listTail n symbolTable

-- listTail :: Int -> Rib -> IO Rib -- Debug
listTail = \case 0 -> pure; n -> read1 >=> listTail (n-1)

-- decodeInstructions :: Rib -> String -> IO Rib -- Debug
decodeInstructions symbolTable [] = pop
decodeInstructions symbolTable (x:rest) = do
 -- First code tells us the operand
 let c=code x; (n,op,d) = findOp c 0
 if c>90
 then do
  tos <- pop
  stack <- getStack
  read0 stack >>= mkInstr (op-1) tos >>= write0 stack
  decodeInstructions symbolTable rest
 else do
  op <- if op==0 then push (RibInt 0) >> pure (op+1) else pure op
  (rest', n) <- if n==d then pure (RibInt <$> readInt rest 0) -- get_int(0)
  else if n>=d then let (rest', i) = readInt rest (n-d-1) in pair rest' <$> symbolRef symbolTable i -- symbol_ref(get_int(n - d - 1))
  else if op<3 then pair rest <$> symbolRef symbolTable n -- symbol_ref(n)
  else pure (rest, RibInt n)
  n <- if 4<op then do
   b <- pop >>= mkInstr n (RibInt 0)
   mkInstr b (RibInt 0) (RibInt 1)
  else pure n
  stack <- getStack
  case stack of
   RibInt i -> read0 n >>= read2 -- End: pc = n[0][2]
   RibRef r -> read0 stack >>= mkInstr (min 4 op-1) n >>= write0 stack >> decodeInstructions symbolTable rest'

-- Finds the op code from the encoded instruction
findOp n op = let d = [20,30,0,10,11,4]!!op in if 2+d<n then findOp (n-(d+3)) (op+1) else (n,op,d)

-- setGlobal :: Rib -> Rib -> IO Rib -- Debug
setGlobal symbolTable val = read0 symbolTable >>= flip write0 val >> read1 symbolTable

-- eval :: Rib -> IO () -- Debug
eval pc = do
 o <- read1 pc
 read0 pc >>= \case
  -- jump/call
  RibInt 0 -> do
   -- traceShowM "jump/call"
   o <- getOpnd o >>= read0
   c <- read0 o
   case c of
    RibRef r -> do
     c2 <- cons (RibInt 0) o
     RibInt arity <- read0 c
     s2 <- foldrM (\_ args -> pop >>= flip cons args) c2 [1..arity] -- while nargs:s2=[pop(),s2,0];nargs-=1
     read2 pc >>= \case
      -- call
      o@RibRef {} -> do
       stack <- getStack
       write0 c2 stack
       write2 c2 o
      -- jump
      RibInt n -> do
       k <- getCont
       write0 c2 =<< read0 k
       write2 c2 =<< read2 k
     setStack s2
     read2 c >>= eval

    RibInt n -> do
     primitives!!n
     read2 pc >>= \case
      -- call
      RibRef _ -> read2 pc >>= eval
      -- jump
      RibInt _ -> do
       k <- getCont
       stack <- getStack
       read0 k >>= write1 stack
       read2 k >>= eval

  -- set
  RibInt 1 -> do
   -- traceShowM "set"
   join (write0 <$> getOpnd o <*> pop)
   read2 pc >>= eval

  -- get
  RibInt 2 -> do
   -- traceShowM "get"
   getOpnd o >>= read0 >>= push
   read2 pc >>= eval

  -- push
  RibInt 3 -> push o >> read2 pc >>= eval
   -- traceShowM "push"

  -- if
  RibInt 4 -> do
   -- traceShowM "if"
   -- IORef Eq's instance is pointer equality.
   tos <- pop
   (if tos==ribFalse then read2 else read1) pc >>= eval

  -- halt
  _ -> pure ()
   -- traceShowM "halt"

-- getOpnd :: Rib -> IO Rib -- Debug
getOpnd (RibInt n) = getStack >>= listTail n
getOpnd o = pure o

-- Look at stack until it finds the continuation rib. The continuation rib is
-- the first rib of the stack that doesn't have an Int as its tag.
-- getCont :: IO Rib -- Debug
getCont = getStack >>= go
 where
 go = \case
  -- RibInt _ -> error "getCont: Stack is not a Rib." -- Debug
  r ->
   read2 r >>= \case
    RibInt _ -> read1 r >>= go
    _ -> pure r

-- initialize :: String -> IO Rib -- Debug
initialize programStr = do
 let (start, end) = span (/= ';') programStr; ((symbolTableStr, emptySymbolsCount), instructionsStr) = (readInt start 0, drop 1 end)
 -- Creating a partial state to decode the instructions.
 -- We just need a stack and the symbol table.
 -- The global object references will be patched later.
 symbolTable <- initialSymbolTable symbolTableStr emptySymbolsCount

 -- Decode instructions.
 -- It would be nice if decoding wouldn't execute in IO.
 instr <- decodeInstructions symbolTable instructionsStr

 -- Set globals
 symbolTable' <- setGlobal symbolTable =<< mkProc (RibInt 0) symbolTable -- primitive 0
 symbolTable'' <- setGlobal symbolTable' ribFalse
 symbolTable''' <- setGlobal symbolTable'' ribTrue
 setGlobal symbolTable''' ribNil

 -- Replace stack with [0,0,[5,0,0]]:
 -- primordial continuation which executes halt instruction.
 halt1 <- mkObj (RibInt 0) (RibInt 5) (RibInt 0)
 halt2 <- mkObj halt1 (RibInt 0) (RibInt 0)
 setStack halt2
 pure instr


-- Env

{-# NOINLINE stack #-}
stack = unsafePerformIO . newRef $ RibInt 0

-- getStack :: IO Rib -- Debug
getStack = readRef stack

-- setStack :: Rib -> IO () -- Debug
setStack = writeRef stack


-- Main

-- inputStr :: String -- Debug
inputStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

-- main :: IO () -- Debug
-- main = do
--  (st, instructions) <- initialize inputStr
--  void $ runReaderIO (eval instructions) st

-- Main that takes arguments.
-- main :: IO () -- Debug
main = do
 hSetBuffering stdout NoBuffering
 args <- getArgs
 programStr <- case args of
  [file] -> readFile file
  _ -> pure inputStr
 instructions <- initialize programStr
 eval instructions
