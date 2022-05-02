import Data.IORef
import Control.Monad.State
import Data.String

import System

input : String
input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!


----------------------
-- Helper functions --
----------------------
error : String -> HasIO io => io (a)
error str = do putStrLn ("*** error : " ++ str)
               exitFailure

getAndIncrement : IORef Int -> HasIO io => io (Int)
getAndIncrement ref = do v <- readIORef ref
                         modifyIORef ref (+ 1)
                         pure v

--------------------
-- Rib Definition --
--------------------
mutual
 -- DELICIOUS RIBS
 data Rib = RInt Int | RRef (IORef RibContainer)
 record RibContainer where
     constructor MakeRibContainer
     id : Int
     -- We called the third field c(g)r because a(1) --+3--> d(4) --+3--> g(7)
     car, cdr, cgr : Rib

(==) : Rib -> Rib -> HasIO io => io Bool
(==) (RInt n) (RInt m) = do pure (n == m)
(==) (RRef ref1)  (RRef ref2) = do rib1 <- readIORef ref1
                                   rib2 <- readIORef ref2
                                   pure (rib1.id == rib2.id)
(==) _ _  = do pure False


MakeRib : IORef Int ->  Rib -> Rib -> Rib -> HasIO io => io (Rib)
MakeRib ribCounter e1 e2 e3 = do v <- getAndIncrement ribCounter
                                 ref <- newIORef (MakeRibContainer v e1 e2 e3)
                                 pure (RRef ref)

eb2 : Int
eb2 = 46

getByte : IORef Int -> HasIO io => io (Int)
getByte posRef = do pos <- getAndIncrement posRef
                    pure (cast {to=Int} (assert_total (strIndex input pos)))

getCode : Int -> Int
getCode b =
  let x = b - 35 in
  if x < 0 then 57 else x

getInt : IORef Int -> HasIO io => Int -> io (Int)
getInt posRef n = do
  x <- (getByte posRef)
  let x = getCode x
  let y = n * eb2   --  <- pure (n * eb2)
  if x < eb2 then
    pure (y + x)
   else
    getInt posRef (y + (x - eb2))




rPairType : Int
rPairType = 0

rProcedureType : Int
rProcedureType = 1

rSymbolType : Int
rSymbolType = 2

rStringType : Int
rStringType = 3

rVectorType : Int
rVectorType = 4

rSingletonType : Int
rSingletonType = 5

rToInt : Rib -> HasIO io => io (Int)
rToInt (RInt n) = do pure n
rToInt _ = do error "Cannot cast rib to int"

rIsInstance : Int -> Rib -> HasIO io => io (Bool)
rIsInstance type (RRef rib) =
  do rib <- readIORef rib
     val <- rToInt rib.cgr
     pure (val == type)
rIsInstance _ _ = do pure False

rIsPair : Rib -> HasIO io => io Bool
rIsPair = rIsInstance rPairType

rId : Rib -> HasIO io => io Int
rId (RRef rib) = do v <- readIORef rib
                    pure (RibContainer.id v)
rId _ = do error "Cannot id on number"

rCar : Rib -> HasIO io => io Rib
rCar (RRef rib) = do v <- readIORef rib
                     pure (RibContainer.car v)
rCar _ = do error "Cannot car on number"

rCdr :  Rib -> HasIO io => io (Rib)
rCdr (RRef rib) = do v <- readIORef rib
                     pure (RibContainer.cdr v)
rCdr _ = do error "Cannot cdr on number"

-- extension to acces the third field
rCgr : Rib -> HasIO io => io Rib
rCgr (RRef rib) = do v <- readIORef rib
                     pure (RibContainer.cgr v)
rCgr _ = do error "Cannot cgr on number"

rListTail : Rib -> Int -> HasIO io => io (Rib)
rListTail rib i =
  if 0 < i then -- we took 2h to find that its i < 0 instead of 0 < i...
        do
           cdr <- rCdr rib
           rListTail cdr (i - 1)
   else
     do pure rib

rLength : Rib -> HasIO io => io (Int)
rLength list = do
  isPair <- rIsPair list
  if isPair
    then do
       cdr <- rCdr list
       l <- rLength cdr
       pure (1 + l)
    else do pure 0

-----------
-- State --
-----------

record State where
  constructor MkState
  true : Rib
  false : Rib
  nil : Rib
  pos : IORef Int
  globalCounter : IORef Int
  symtbl : Rib

-- really not optimal...
toString : State -> Rib -> IORef (List Int) -> HasIO io => io (String)
toString state (RInt n) _ = do pure (cast n)
toString state rib seen = do isNil <- rib == state.nil
                             isTrue <- rib == state.true
                             isFalse <- rib == state.false

                             if isNil
                                then do pure "RNIL"
                                else if isTrue
                                  then do pure "RTRUE"
                                  else if isFalse
                                    then do pure "RFALSE"
                                    else do car <- rCar rib
                                            cdr <- rCdr rib
                                            cgr <- rCgr rib
                                            id <- rId rib
                                            seenLst <- readIORef seen
                                            if elem id seenLst
                                              then if id > 99 then do pure ("#:" ++ (cast id)) else if id > 9 then do pure ("#: " ++ (cast id)) else do pure ("#:  " ++ (cast id))
                                              else do modifyIORef seen (\x => (id :: x))
                                                      scar <- toString state car seen
                                                      scdr <- toString state cdr seen
                                                      scgr <- toString state cgr seen
                                                      pure (
                                                       --"#:" ++ (cast id) ++
                                                       "[" ++ scar ++ ", " ++ scdr ++ ", " ++ scgr ++ "]")


print : State -> Rib -> HasIO io => io ()
print state rib = do x <- newIORef ([0, 0, 0])
                     str <- toString state rib x
                     putStrLn ("" ++ str ++ "\n")



rStringToUninternedSymbol : State -> Rib -> HasIO io => io (Rib)
rStringToUninternedSymbol state string = MakeRib state.globalCounter state.false string (RInt rSymbolType)

rListToString : State -> Rib -> HasIO io => io (Rib)
rListToString state list = do l <- rLength list
                              MakeRib state.globalCounter list (RInt l) (RInt rStringType)

rCons : State -> Rib -> Rib -> HasIO io => io (Rib)
rCons state car cdr = MakeRib state.globalCounter car cdr (RInt rPairType)


rIsRib : Rib -> Bool
rIsRib (RRef _) = True
rIsRib (RInt n) = False

setCar : Rib -> Rib -> HasIO io => io ()
setCar (RRef stack) newval = do ref <- readIORef stack
                                let v = (MakeRibContainer ref.id newval ref.cdr ref.cgr )
                                writeIORef stack v
                                --modifyIORef stack (\ref => { car := newval } ref)
setCar _ _ = do error "First argument of setcar is not a rib"

setCdr : Rib -> Rib -> HasIO io => io ()
setCdr (RRef stack) newval = do ref <- readIORef stack
                                let v = (MakeRibContainer ref.id ref.car newval ref.cgr )
                                writeIORef stack v
setCdr _ _ = do error "Second argument of setcar is not a rib"

setCgr : Rib -> Rib -> HasIO io => io ()
setCgr (RRef stack) newval = do ref <- readIORef stack
                                let v = (MakeRibContainer ref.id ref.car ref.cdr newval)
                                writeIORef stack v
setCgr _ _ = do error "Third argument of setcar is not a rib"

rSetRef : Rib -> Rib -> HasIO io => io()
rSetRef (RRef elem1) (RRef elem2) = do v <- readIORef elem2
                                       writeIORef elem1 v
                                       putStrLn "Ref has been modified"
                                       --modifyIORef elem1 (\ref => v)
rSetRef _ _ = do error "Cannot setref of non ref elements"

rToRef : Rib -> HasIO io => io (IORef RibContainer)
rToRef (RRef rib) = do pure rib
rToRef _ = do error "cannot cast int to IORef"

------------------
-- Symbol table --
------------------

addSymbol : State -> Rib -> Rib -> HasIO io => io (Rib)
addSymbol state chars symtbl = do lst <- rListToString state chars
                                  sym <- rStringToUninternedSymbol state lst
                                  rCons state sym symtbl


buildChar : State -> Rib -> Rib -> HasIO io => io (Rib)
buildChar state chars symtbl = do x <- getByte state.pos
                                  if x == 44
                                    then do sym <- addSymbol state chars symtbl
                                            buildChar state state.nil sym
                                    else if x == 59
                                      then addSymbol state chars symtbl
                                      else do r <- rCons state (RInt x) chars
                                              buildChar state r symtbl

buildSymtbl : State -> Int -> Rib -> HasIO io => io (Rib)
buildSymtbl state n symtbl = if 0 < n
                                then do sym <- addSymbol state state.nil symtbl
                                        buildSymtbl state (n - 1) sym
                                else do buildChar state state.nil symtbl


---------------------------
-- Inscturction decoding --
---------------------------


sym : State -> Int -> HasIO io => io Rib
sym state n = do tail <- rListTail state.symtbl n
                 r <- rCar tail
                 --print state r
                 rCar tail

ins : List Int
ins = [20, 30, 0, 10, 11, 4]

unsafeIndex : List Int -> Int -> Int
unsafeIndex lst n = case lst of
    (head :: tail) => if n == 0 then head else unsafeIndex tail (n-1)
    _ => -1

mutual
  addInstruction : State -> Rib -> Rib -> Rib -> HasIO io => io (Rib)
  addInstruction state op opnd stack = do oscar <- rCar stack
                                          newRib <- MakeRib state.globalCounter op opnd oscar
                                          setCar stack newRib
                                          decodeStack state stack

  decodeStackAux : State -> Int -> Int -> Int -> Rib -> HasIO io => io Rib
  decodeStackAux state op n x oriStack =
    let d = (unsafeIndex ins op) in
        if d + 2 < n
             then decodeStackAux state (op+1) (n-(d+3)) x oriStack
             else
               if 90 < x then
                 do cdrStack <- rCdr oriStack
                    carStack <- rCar oriStack
                    addInstruction state (RInt 4) carStack cdrStack
               else
                 do stack <- if op == 0 then rCons state (RInt 0) oriStack else do pure oriStack
                    opnd <- if (n < d)
                               then if (op < 3)
                                    then sym state n
                                    else do pure (RInt n)
                               else if n == d
                                    then do v <- getInt state.pos 0
                                            pure (RInt v)
                                    else do v <- getInt state.pos ((n - d) - 1)
                                            sym state v
                    if 4 < op
                      then do scar <- rCar stack
                              inter <- MakeRib state.globalCounter opnd (RInt 0) scar
                              proc <- MakeRib state.globalCounter inter state.nil (RInt rProcedureType)
                              stack <- rCdr stack
                              if rIsRib stack
                                then do (addInstruction state (RInt 3) proc stack)
                                else do pure proc
                      else do addInstruction state
                                             (if (0 < op) then (RInt (op - 1)) else (RInt 0))
                                             opnd
                                             stack

  decodeStack : State -> Rib -> HasIO io => io Rib
  decodeStack state stack = do x <- (getByte state.pos)
                               let x = getCode x

                               decodeStackAux state 0 x x stack


----------------
-- Primitives --
----------------

prim3 : State -> (Rib -> Rib -> Rib -> HasIO io => io Rib) -> Rib -> HasIO io => io Rib
prim3 state foo stack = do z <- rCar stack
                           stack <- rCdr stack
                           y <- rCar stack
                           stack <- rCdr stack
                           x <- rCar stack
                           stack <- rCdr stack
                           newVal <- foo x y z
                           rCons state newVal stack

prim2 : State -> (Rib -> Rib -> HasIO io => io Rib) -> Rib -> HasIO io => io Rib
prim2 state foo stack = do y <- rCar stack
                           stack <- rCdr stack
                           x <- rCar stack
                           stack <- rCdr stack
                           newVal <- foo x y
                           rCons state newVal stack


prim1 : State -> (Rib -> HasIO io => io Rib) -> Rib -> HasIO io => io Rib
prim1 state foo stack = do x <- rCar stack
                           stack <- rCdr stack
                           newVal <- foo x
                           rCons state newVal stack

prim1Pure : State -> (Rib -> Rib) -> Rib -> HasIO io => io Rib
prim1Pure state foo stack = do x <- rCar stack
                               stack <- rCdr stack
                               let newVal = foo x
                               rCons state newVal stack

prim2Pure : State -> (Rib -> Rib ->  Rib) -> Rib -> HasIO io => io Rib
prim2Pure state foo stack = do y <- rCar stack
                               stack <- rCdr stack
                               x <- rCar stack
                               stack <- rCdr stack
                               let newVal = foo x y
                               rCons state newVal stack

prim0 : State -> Rib -> Rib -> HasIO io => io Rib
prim0 state return stack = do rCons state return stack

opRInt : (Int -> Int -> Int) -> Rib -> Rib -> Rib
opRInt op (RInt n) (RInt m) = (RInt (op n m))
opRInt _ _ _ = (RInt (0 - 999)) --lol

rEqv : State -> Rib -> Rib -> HasIO io => io Rib
rEqv state x y = do isTrue <- (x == y)
                    if isTrue then do pure state.true else do pure state.false

lessthan : State -> Rib -> Rib -> HasIO io => io Rib
lessthan state (RInt n) (RInt m) = if n < m then do pure state.true else do pure state.false
lessthan _ _ _ = do error "Less than on ribs (non-numbers)"

arg1 : Rib -> Rib -> HasIO io => io Rib
arg1 x y = do pure x

arg2 : Rib -> Rib -> HasIO io => io Rib
arg2 x y = do pure y

close : State -> Rib -> HasIO io => io Rib
close state stack = do x <- rCar stack
                       stack <- rCdr stack
                       xCar <- rCar x
                       newRib <- MakeRib state.globalCounter xCar stack (RInt rProcedureType)
                       rCons state newRib stack

setAndReturn : (Rib -> Rib -> HasIO io => io ()) -> Rib -> Rib -> HasIO io => io Rib
setAndReturn foo x y = do foo x y
                          pure y

writeAndReturn : Rib -> HasIO io => io Rib
writeAndReturn rib = case rib of
                      RInt n => do let x = (cast {to=Char} n)
                                   putChar x
                                   pure rib
                      _ => do error "this is bad :/"

primitive : State -> Int -> Rib -> HasIO io => io Rib
primitive state 0 stack = do let globalCounter = state.globalCounter
                             ribCreator <- pure (MakeRib globalCounter)
                             prim3 state ribCreator stack
primitive state 1 stack = do pure stack
primitive state 2 stack = do rCdr stack
primitive state 3 stack = do prim2 state arg2 stack
primitive state 4 stack = do close state stack

primitive state 5 stack = do prim1Pure state (\x => if rIsRib x then state.true else state.false) stack
primitive state 6 stack = do prim1 state rCar stack
primitive state 7 stack = do prim1 state rCdr stack
primitive state 8 stack = do prim1 state rCgr stack
primitive state 9 stack = do foo <- pure (setAndReturn setCar)
                             prim2 state foo stack
primitive state 10 stack = do foo <- pure (setAndReturn setCdr)
                              prim2 state foo stack
primitive state 11 stack = do foo <- pure (setAndReturn setCgr)
                              prim2 state foo stack
primitive state 12 stack = do foo <- pure (rEqv state)
                              prim2 state foo stack
primitive state 13 stack = do foo <- pure (lessthan state)
                              prim2 state foo stack
primitive state 14 stack = do prim2Pure state (opRInt (+)) stack
primitive state 15 stack = do prim2Pure state (opRInt (-)) stack
primitive state 16 stack = do prim2Pure state (opRInt (*)) stack
primitive state 17 stack = do prim2Pure state (opRInt div) stack
primitive state 18 stack = do pos <- readIORef state.pos
                              if pos < strLength input
                                then do v <- getByte state.pos
                                        if v == 255
                                          then do prim0 state (RInt (-1)) stack
                                          else do prim0 state (RInt v) stack
                                else do x <- getChar
                                        let v = (cast {to=Int} x)
                                        if v == 255
                                          then do prim0 state (RInt (-1)) stack
                                          else do prim0 state (RInt v) stack
primitive state 19 stack = do prim1 state writeAndReturn stack



primitive _ n _ = do error ("Primitive #" ++ (cast n) ++ " is not yet implemented ")

-------------------
-- Globals setup --
-------------------

setGlobal : Rib -> Rib -> HasIO io => io (Rib)
setGlobal symtbl val = do symCar <- rCar symtbl
                          setCar symCar val
                          rCdr symtbl

setupGlobal : State -> HasIO io => io (Rib)
setupGlobal state =
  do primitive0 <- MakeRib state.globalCounter (RInt 0) state.symtbl (RInt rProcedureType)
     let symtbl = state.symtbl
     symtbl <- setGlobal symtbl primitive0
     symtbl <- setGlobal symtbl state.false
     symtbl <- setGlobal symtbl state.true
     setGlobal symtbl state.nil

-----------------------
-- Runtime execution --
-----------------------

getCont : Rib -> HasIO io => io Rib
getCont stack = do stackCgr <- rCgr stack
                   stackCdr <- rCdr stack
                   if rIsRib stackCgr
                    then do pure stack
                    else do getCont stackCdr

getVar : Rib -> Rib -> HasIO io => io Rib
getVar stack (RInt n) = do stackTail <- rListTail stack n
                           stackTailCar <- rCar stackTail
                           pure stackTailCar
getVar stack opnd = do rCar opnd

setVar : Rib -> Rib -> Rib -> HasIO io => io ()
setVar stack (RInt n) val = do stackTail <- rListTail stack n
                               stackTailCar <- rCar stackTail
                               setCar stackTail val
setVar stack opnd val = do setCar opnd val


mutual
  lmdaCall : State -> Rib -> Rib -> Int -> Rib -> Rib -> Rib -> Int -> HasIO io => io ()
  lmdaCall state code next nargs newStack newCont stack id =
    do let isNextRib = rIsRib next
       if 0 < nargs
          then do let nextNargs = nargs - 1
                  stackCar <- rCar stack
                  stackCdr <- rCdr stack
                  superNewStack <- rCons state stackCar newStack
                  lmdaCall state code next nextNargs superNewStack newCont stackCdr id
          else if isNextRib
                  then do setCar newCont stack
                          setCgr newCont next
                          codeCgr <- rCgr code
                          run state codeCgr newStack (id + 1)

                  else do k <- getCont stack
                          k0 <- rCar k
                          k2 <- rCgr k
                          setCar newCont k0
                          setCgr newCont k2
                          codeCgr <- rCgr code
                          run state codeCgr newStack (id + 1)


  run : State -> Rib -> Rib -> Int -> HasIO io => io ()
  run state pc stack id =
    do instr <- rCar pc
       opnd <- rCdr pc
       next <- rCgr pc
       v <- (rToInt instr)

       case instr of
        RInt 0 => do proc <- getVar stack opnd --Jump/Call
                     code <- rCar proc
                     codeIsRib <- pure (rIsRib code)
                     if codeIsRib
                       then do
                               ribCreator <- pure (MakeRib state.globalCounter) --lambda
                               newCont <- ribCreator (RInt 0) proc (RInt 0)
                               codeCar <- rCar code
                               codeCarInt <- rToInt codeCar
                               lmdaCall state code next codeCarInt newCont newCont stack id
                       else do codeInt <- rToInt code
                               stack <- primitive state codeInt stack --primitive
                               nextIsRib <- pure (rIsRib next)
                               output <- if nextIsRib
                                 then do pure next
                                 else do cont <- getCont stack
                                         contCar <- rCar cont
                                         setCdr stack contCar
                                         rCgr cont
                               run state output stack (id + 1)
        RInt 1 => do stackCar <- rCar stack --Set
                     stackCdr <- rCdr stack
                     setVar stack opnd stackCar
                     run state next stackCdr (id + 1)
        RInt 2 => do opndVar <- getVar stack opnd --Get
                     newStack <- rCons state opndVar stack
                     run state next newStack (id + 1)
        RInt 3 => do newStack <- rCons state opnd stack --Const
                     run state next newStack (id + 1)
        RInt 4 => do stackCar <- rCar stack --If
                     stackCdr <- rCdr stack
                     isFalse <- stackCar == state.false
                     if isFalse
                       then do run state next stackCdr (id + 1)
                       else do run state opnd stackCdr (id + 1)
        RInt 5 => do putStr ""
        _ => do error "Unknown instruction"


--------------------------------------
-- Decode instructions and run them --
--------------------------------------

decodeAndRun : IO ()
decodeAndRun = do
  globalCounter <- newIORef 0 -- only used for the ribCreator
  ribCreator <- pure (MakeRib globalCounter)

  true <- ribCreator (RInt 0) (RInt 0) (RInt rSingletonType)
  false <- ribCreator (RInt 0) (RInt 0) (RInt rSingletonType)
  nil <- ribCreator (RInt 0) (RInt 0) (RInt rSingletonType)

  pos <- newIORef 0
  let symtbl = nil

  -- Create state
  let state = MkState true false nil pos globalCounter nil

  v <- (getInt pos 0)
  symtbl <- buildSymtbl state v state.symtbl

  state <- pure (MkState state.true state.false state.nil state.pos state.globalCounter symtbl)
  stack <- decodeStack state (RInt 0)
  symtbl <- setupGlobal state

  x <- rCar stack
  pc <- rCgr x

  y <- ribCreator (RInt 5) (RInt 0) (RInt 0)
  stack <- ribCreator (RInt 0) (RInt 0) y

  run state pc stack 0

-- :)
main : IO ()
main = do
  decodeAndRun
