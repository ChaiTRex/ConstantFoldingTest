{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE BangPatterns, Strict, TemplateHaskell #-}

-- To use all CPU threads, compile with `ghc-stage2 -threaded -with-rtsopts=-N`.

module Main (main) where

import Control.Concurrent      (forkFinally, forkIO, getNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan
                               , writeList2Chan
                               )
import Control.DeepSeq         (force)
import Control.Exception       (bracket)
import Control.Monad           (forM, forM_, replicateM)
import Data.List               (foldl')
import Data.Int                (Int8)
import Data.Typeable           (typeOf)
import System.Directory        (getTemporaryDirectory, removeFile)
import System.IO               ( Handle, hClose, hFlush, hPutStr, hSeek
                                       , hSetFileSize
                               , SeekMode(AbsoluteSeek)
                               , openTempFile
                               )
import ThisGHC                 (runHaskellFile)

-- ++======================================================================++ --
-- || Configuration section                                                || --
-- |+----------------------------------------------------------------------+| --
-- || WARNING: Be aware that, except for iterationTests, increasing these  || --
-- || parameters linearly leads to exponential runtime increases. Keep     || --
-- || them as low as is sensible.                                          || --
-- ++======================================================================++ --

-- Maximum nesting level of operators.
-- For example, maxNestingDepth of 1 can test (a + b) but not ((a + b)*(c - d)).
-- It is STRONGLY recommended not to exceed 2 unless you have an exceedingly
-- fast core in your CPU.
maxNestingDepth :: Int
{-# INLINE maxNestingDepth #-}
maxNestingDepth = 2

-- This tester creates temporary .hs files to run the tests from. Putting all
-- tests in at once can lead to a GHC heap overflow failure. This many tests are
-- put in the temporary .hs file per testing iteration.
iterationTests :: Int
{-# INLINE iterationTests #-}
iterationTests = 200

-- The type of values we're dealing with.
type Value =  Int

-- Literal and variable values to test.
-- They will be negated by the tests, so it may be unnecessary to negate them
-- here.
values :: [Value]
{-# NOINLINE values #-}
values = [0,1,3,7]

-- The functions we're dealing with. Must be a -> a -> a -> ... -> a with all
-- types the same.
neg :: Fn
neg = (fn1 "negate" negate)
        { fArgS = (\ [s] -> showChar '(' . showChar '-' . s . showChar ')') }

mul :: Fn
mul = binOp "*" (*)

add :: Fn
add = binOp "+" (+)

sub :: Fn
sub = binOp "-" (-)

-- At each nesting depth, stages of functions are applied one after the other.
-- Zero or one of the provided functions for a stage will be applied during it.
nestingStages :: [[Fn]]
{-# NOINLINE nestingStages #-}
nestingStages = [ [mul, add, sub]
                , [neg]
                ]

-- ExprTs to start with before nesting (must be the same length as
-- nestingStages).
initialSlidingWindow :: [[ExprT]]
initialSlidingWindow = [ [ValT, FnT 1 neg [ValT]]
                       , []
                       ]

-- Hash function to use in concisely testing large lists of outputs.
hash :: [Value] -> Value
hash    =  foldl' ((+) . (7 *)) 0
hashStr :: String
hashStr = "foldl' ((+) . (7 *)) 0"

-- ++======================================================================++ --
-- || End of configuration section                                         || --
-- ++======================================================================++ --


-- == Values (literal and variable) == --

data ValueT = L {-# UNPACK #-} !Value
            | V {-# UNPACK #-} !Int
  deriving (Eq, Ord)

instance Show ValueT where
  showsPrec _ (L k) = shows k
  showsPrec _ (V i) = showChar 'x' . shows i

isVar :: ValueT -> Bool
{-# INLINE isVar #-}
isVar (V _) = True
isVar _     = False

-- Gives all combinations of n Values such that the set of variables has a
-- minimized maximum index.
valueLists :: Int -> [[ValueT]]
{-# INLINE valueLists #-}
valueLists 0   = []
valueLists len = go len 0
  where
    go :: Int -> Int -> [[ValueT]]
    {-# INLINABLE go #-}
    go 0 _ = [[]]
    go n m = let ys = go (n - 1)  m
                 zs = go (n - 1) (m + 1)
             in     ((:) <$> map L values       <*> ys)
                 ++ ((:) <$> map V [0 .. m - 1] <*> ys)
                 ++ ((:) <$> pure (V m)         <*> zs)


-- == Functions/operators == --

data Fn = Fn { arity :: {-# UNPACK #-} !Int
             , fName ::                !String
             , fEval ::                !([Value] -> Value)
             , fArgS ::                !([ShowS] -> ShowS)
             }

instance Eq Fn where
  f1 == f2 = compare f1 f2 == EQ

instance Ord Fn where
  compare (Fn a1 n1 _ _) (Fn a2 n2 _ _) = compare (a1, n1) (a2, n2)

instance Show Fn where
  showsPrec _ (Fn _ n _ _) = showString n

binOp :: String -> (Value -> Value -> Value) -> Fn
binOp nm op =
  Fn 2 ('(' : nm ++ ")")
     (\ [x1, x2] -> op x1 x2)
     (\ [s1, s2] -> showChar '(' . s1
                  . showChar ' ' . showString nm . showChar ' '
                  . s2 . showChar ')'
     )

fn1 :: String -> (Value -> Value) -> Fn
fn1 nm f =
  Fn 1 nm
     (\ [x] -> f x)
     (\ [s] -> showChar '(' . showString nm
             . showChar ' ' . s
             . showChar ')'
     )

fn2 :: String -> (Value -> Value -> Value) -> Fn
fn2 nm f =
  Fn 2 nm
     (\ [x1, x2]  -> f x1 x2)
     (\ [s1, s2]  -> showChar '(' . showString nm
                   . showChar ' ' . s1
                   . showChar ' ' . s2
                   . showChar ')'
     )


-- == Expression templates == --

data ExprT = ValT
           | FnT {-# UNPACK #-} !Int !Fn ![ExprT]
  deriving (Eq, Ord)

valCount :: ExprT -> Int
{-# INLINE valCount #-}
valCount ValT        = 1
valCount (FnT n _ _) = n

instance Show ExprT where
  showsPrec _ ValT          = showChar '_'
  showsPrec _ (FnT _ fn ts) = fArgS fn (map shows ts)

-- All ExprTs up to the given nesting depth.
exprTs :: Int -> [ExprT]
{-# INLINE exprTs #-} -- Make lazy
exprTs = concat . exprTs'
  where
    exprTs' :: Int -> [[ExprT]]
    {-# INLINABLE exprTs' #-}
    exprTs' 0 = []:initialSlidingWindow
    exprTs' n = foldl (flip applyAll) (exprTs' (n - 1)) nestingStages

    -- Avoiding duplicates is important because the output list can have a HUGE
    -- number of elements, and running `nub` or `map head . group . sort` on it
    -- brings the whole list into memory, where it might not fit.
    --
    -- The following scheme keeps the potentially-huge output list at
    -- near-constant space when accessing it only by iterating over it.
    --
    -- Completed stages are, at any point in time, partitioned into "ancient"
    -- and "modern" stages. Ancient stages are those stages that were completed
    -- earlier than this same stage in the previous nesting level. Modern stages
    -- are all other completed stages.
    --
    -- The head sublist of the [[ExprT]] argument contains all ExprTs generated
    -- by all ancient stages. The tail sublists contain all ExprTs generated by
    -- all modern stages, one sublist for each modern stage.
    --
    -- To avoid duplicates, we take note that if we use only ancient subExprTs
    -- when generating a new ExprT, we'll get a result that was already
    -- produced by this stage in a prior nesting level because prior nesting
    -- levels had all ancient subExprTs available to them and would have used
    -- them already.
    --
    -- Therefore at least one subExprT must be modern. We can achieve this by
    -- applying `replicateM` to the [[ExprT]] argument and by then throwing away
    -- the head of the result, which contains all those subExprT combinations
    -- that are universally ancient. This leaves us with all combinations of
    -- subExprTs that have at least one modern subExprT, allowing us to form
    -- ExprTs that have not been produced before.
    --
    -- Note that this isn't guaranteed to eliminate all duplicates, as
    -- nestingStages can be engineered to produce duplicates. For example,
    -- [[neg], [neg]] will produce duplicate outputs that have been negated
    -- once: some by the first neg and some by the second neg.
    applyAll :: [Fn] -> [[ExprT]] -> [[ExprT]]
    applyAll fns ts@(us:(vs:ws)) =
      let xs :: [ExprT]
          xs = concat $ apply <$> fns <*> pure ts
      in  (us ++ vs):(ws ++ [xs])

    apply :: Fn -> [[ExprT]] -> [ExprT]
    apply fn = map (fnT fn) . concatMap sequence . tail . replicateM (arity fn)

    fnT :: Fn -> [ExprT] -> ExprT
    fnT fn ts = FnT (foldl' (+) 0 (map valCount ts)) fn ts


-- == Expressions == --

data Expr = Val {-# UNPACK #-} !Int !ValueT
          | FnE {-# UNPACK #-} !Int !Fn ![Expr]
  deriving (Eq, Ord)

instance Show Expr where
  showsPrec _ (Val _ v    ) = shows v
  showsPrec _ (FnE _ fn es) = fArgS fn (map shows es)

varCount :: Expr -> Int
{-# INLINE varCount #-}
varCount (Val n _  ) = n
varCount (FnE n _ _) = n

{-# INLINABLE eval #-}
eval (Val _ v    ) vs = case v of
                           (L k) -> k
                           (V i) -> vs !! i
eval (FnE _ fn es) vs = fEval fn $ map (flip eval vs) es

exprs :: Int -> [Expr]
{-# INLINE exprs #-}
exprs = concatMap go . exprTs
  where
    go :: ExprT -> [Expr]
    go t = map (apply t) $ valueLists (valCount t)

    apply :: ExprT -> [ValueT] -> Expr
    apply ValT          [v] = Val (if isVar v then 1 else 0) v
    apply (FnT n fn es) vs  =
      let vars = (+ 1) . foldl' max (-1) . map (\ (V i) -> i) . filter isVar
               $ vs
          args = map fst . tail . scanl (flip splitAt . snd) ([], vs)
               $ map valCount es
      in  FnE vars fn $ zipWith apply es args


-- == Tests == --

valueStr :: String
{-# NOINLINE valueStr #-}
valueStr   = show (typeOf (undefined :: Value))

tests :: Int -> [String]
{-# INLINE tests #-}
tests = map go
      . takeWhile (not . null)
      . map (take iterationTests)
      . iterate (drop iterationTests)
      . exprs
  where
    go :: [Expr] -> String
    {-# INLINE go #-}
    go = testProgram . zipWith (testParts . ("test" ++) . show) [1..]

    moduleHeader :: String
    {-# NOINLINE moduleHeader #-}
    moduleHeader = unlines [ "module Main where"
                           , ""
                           , "import Control.Monad (replicateM)"
                           , "import Data.Int"
                           , "import Data.Word"
                           , ""
                           ]

    pattern :: Int -> String
    {-# NOINLINE pattern #-}
    pattern 0 = ""
    pattern 1 = "x0"
    pattern n = let n' = n - 1
                in pattern n' ++ ", x" ++ show n'

    testParts :: String -> Expr -> (String, String)
    {-# INLINE testParts #-}
    testParts fn e =
      let expr = show e
          vars = varCount e
-- ZOMG
          hashResult = hash (eval e <$> replicateM vars values)
      in ( unlines [ fn ++ " :: [" ++ valueStr ++ "] -> " ++ valueStr
                   , "{-# NOINLINE " ++ fn ++ " #-}"
                   , fn ++ " [" ++ pattern vars ++ "] = " ++ expr
                   , ""
                   ]
         , unlines [ "  if hash (" ++ fn ++ " <$> replicateM " ++ show vars
                       ++ " xs) == " ++ show hashResult
                   , "     then return ()"
                   , "     else putStrLn \"ERROR! " ++ expr
                       ++ " is optimized incorrectly!\""
                   , ""
                   ]
         )

    testProgram :: [(String, String)] -> String
    testProgram xs = unlines [ "{-# OPTIONS_GHC -O2 #-}"
                             , ""
                             , "module Main where"
                             , ""
                             , "import Control.Monad (replicateM)"
                             , "import Data.List     (foldl')"
                             , "import Data.Int"
                             , "import Data.Word"
                             , ""
                             , concatMap fst xs ++ "hash :: [" ++ valueStr
                                 ++ "] -> " ++ valueStr
                             , "{-# INLINE hash #-}"
                             , "hash = " ++ hashStr
                             , ""
                             , "main :: IO ()"
                             , "{-# INLINE main #-}"
                             , "main = do"
                             , "  let xs :: [" ++ valueStr ++ "]"
                             , "      {-# NOINLINE xs #-}"
                             , "      xs = " ++ show values
                             , concatMap snd xs
                             ]

onAllCapabilities_ :: [a] -> (IO b) -> (b -> a -> IO c) -> (b -> IO d) -> IO ()
onAllCapabilities_ jobInputs setup workOneJob teardown = do
  threadCount <- getNumCapabilities
  workNeeded  <- newChan
  workQueue   <- newChan
  let (initInputs, jobInputs') = splitAt threadCount $ map Just jobInputs
  waiters     <- forM initInputs $ \ workItem -> do
    writeChan workQueue workItem
    waiter <- newEmptyMVar
    forkIO $
       bracket setup
               (\ setupItem -> do
                  teardown setupItem
                  putMVar waiter ()
               )
               (worker workNeeded workQueue workOneJob)
    return waiter
  forM_ jobInputs' $ \ workItem -> do
    readChan workNeeded
    writeChan workQueue workItem
  writeChan workQueue Nothing
  mapM_ takeMVar waiters
  where
    worker :: Chan () -> Chan (Maybe a) -> (b -> a -> IO c) -> b -> IO ()
    worker workNeeded workQueue workOneJob setupItem = do
      workOrStop <- readChan workQueue
      case workOrStop of
           Nothing       -> writeChan workQueue workOrStop
           Just workItem -> do
                writeChan workNeeded ()
                workOneJob setupItem workItem
                worker workNeeded workQueue workOneJob setupItem

overwriteHandle :: Handle -> String -> IO ()
{-# INLINE overwriteHandle #-}
overwriteHandle hnd str = do
  hSeek        hnd AbsoluteSeek 0
  hSetFileSize hnd 0
  hPutStr      hnd str
  hFlush       hnd

main :: IO ()
main = do
  onAllCapabilities_ (map force (tests maxNestingDepth))
                     (do
                        tmpDir <- getTemporaryDirectory
                        openTempFile tmpDir "GHCConstantFoldingTest.hs"
                     )
                     (\ (path, hnd) src -> do
                        overwriteHandle hnd src
                        $( runHaskellFile ) path
                     )
                     (\ (path, hnd) -> do
                        hClose hnd
                        removeFile path
                     )


-- == Debugging == --

maxVars :: Int -> Int
{-# NOINLINE maxVars #-}
maxVars n =
  let start = foldl' max 0 . map valCount $ exprTs 0
  in  (foldl' (*) start . map (foldl' max 1 . map arity) $ nestingStages) ^ n

debug :: IO ()
debug = do
  let nestingLevel = 1
  putStrLn $ "== valueLists (maxVars " ++ show nestingLevel ++ ") ============="
  mapM_ print    $ valueLists (maxVars nestingLevel)
  putStrLn $ "== exprTs " ++ show nestingLevel ++ " ==========================="
  mapM_ print    $ exprTs nestingLevel
  putStrLn $ "== exprs " ++ show nestingLevel ++ " ============================"
  mapM_ print    $ exprs  nestingLevel
  putStrLn $ "== tests " ++ show nestingLevel ++ " ============================"
  mapM_ putStrLn $ tests  nestingLevel

