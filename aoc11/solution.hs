import Prelude hiding (drop)
import Data.Maybe (fromJust)
import Control.Monad.State
import qualified Data.Map as M

-- A monkeys operation, to get 'new' from 'old'.
data Operation = Old                     -- Old value
               | Num Integer             -- Literal Integer value
               | Mul Operation Operation -- Multiplicaiton
               | Add Operation Operation -- Addition

-- Evaluate a monkeys operation, given old value to substitute.
eval :: Integer -> Operation -> Integer
eval old Old   = old
eval _ (Num x) = x
eval old (Mul op1 op2) = (eval old op1) * (eval old op2)
eval old (Add op1 op2) = (eval old op1) + (eval old op2)

-- A monkeys properties.
data Monkey = Monkey { items     :: [Integer] -- List of Items, represented by worry value
                     , operation :: Operation -- Operation of this monkey.
                     , testVal   :: Integer   -- Number for divisibility test
                     , ifTrue    :: Integer   -- Throw to this monkey if the test succeeds.
                     , ifFalse   :: Integer   -- Throw to this monkey if the test fails.
                     }

-- All the monkeys.
data Monkeys = Monkeys { nextMonkey :: Integer              -- The next monkey in line.
                       , numMonkeys :: Integer              -- Number of monkeys.
                       , monkeys    :: M.Map Integer Monkey -- All the monkeys.
                       }

-- The State of the Monkey Business.
type St = State Monkeys


{- ======== Basic State functions ======= -}

-- Update a monkey.
update :: Integer -> Monkey -> St ()
update index monkey = do
  group <- gets monkeys
  state <- get
  put state { monkeys = M.insert index monkey group }

-- Get the monkey with an index.
getMonkey :: Integer -> St Monkey
getMonkey index = do
  group <- gets monkeys
  return $ fromJust $ M.lookup index group


{- ====== Basic things monkeys do ====== -}

-- Monkey receives an item.
receive :: Integer -> Integer -> St ()
receive index item = do
  monkey <- getMonkey index
  update index monkey { items = items monkey ++ [item] }

-- Check if monkey is done.
done :: Integer -> St Bool
done index = do
  monkey <- getMonkey index
  return $ null . items $ monkey

-- Monkey drops an item (throws it to another monkey).
drop :: Integer -> St ()
drop index = do
  monkey <- getMonkey index
  update index monkey { items = tail $ items monkey }

-- Monkey inspects his next item.
inspect :: Integer -> St Integer
inspect index = do
  monkey <- getMonkey index
  return $ eval (head . items $ monkey) (operation monkey) `div` 3

-- Monkey runs his test on an item.
test :: Integer -> Integer -> St Integer
test index item = do
  monkey <- getMonkey index
  if mod item (testVal monkey) == 0 then 
    return $ ifTrue monkey
  else 
    return $ ifFalse monkey


{- ======= The monkey business ======= -}

-- Next monkey in line.
-- Assumes that there are more than 0 items in rotation.
next :: St Integer
next = do
  index <- gets nextMonkey
  b     <- done index
  if not b then do
    return index
  else do
    num   <- gets numMonkeys
    state <- get
    put state { nextMonkey = mod (index + 1) num }
    next

-- Next monkey makes move.
move :: St ()
move = do
  from <- next
  item <- inspect from
  to   <- test item from
  drop from
  receive to item

-- Make n moves
business :: Integer -> St ()
business n | n <= 0    = return ()
           | otherwise = move >> business (n - 1)