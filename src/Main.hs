{-# LANGUAGE RankNTypes #-}
module NFA2DFA.Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State

import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set, insert, member)
import           Data.Word

main :: IO ()
main = putStrLn "Hello, World!"

-------------------------------------------------------------------------------

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

view :: Lens' s a -> s -> a
view lens = getConst . lens Const
{-# INLINE view #-}

set :: Lens' s a -> a -> s -> s
set lens = over lens . const
{-# INLINE set #-}

over :: Lens' s a -> (a -> a) -> s -> s
over lens f = runIdentity . lens (Identity . f)
{-# INLINE over #-}

-------------------------------------------------------------------------------

type Stack = []

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> Stack a
pop = tail

peek :: Stack a -> a
peek = head

-------------------------------------------------------------------------------

data NFA = NFA {
    nfaAStates       :: Set AState
  , nfaInputSymbols  :: Set Char
  , nfaTransitionFun :: Map (AState, Transition) (Set AState)
  , nfaInitialAState :: AState
  , nfaFinalAStates  :: Set AState
} deriving (Eq, Ord, Read, Show)

type NFATran = Map (AState, Transition) (Set AState)

data AState = AState {
    aStateNum    :: Word
  , aStateMarked :: Bool
} deriving (Bounded, Eq, Ord, Read, Show)

instance Num AState where
    abs         = error "AState arithmetic not permitted"
    signum      = error "AState arithmetic not permitted"
    (+)         = error "AState arithmetic not permitted"
    (-)         = error "AState arithmetic not permitted"
    (*)         = error "AState arithmetic not permitted"
    fromInteger = flip AState False . fromInteger

_aStateNum :: Lens' AState Word
_aStateNum inj (AState num marked) = flip AState marked <$> inj num
{-# INLINE _aStateNum #-}

type Transition = Maybe InputSymbol

epsilon :: Transition
epsilon = Nothing

type InputSymbol = Char

type DFATran = Map (Set AState, InputSymbol) (Set AState)

eClosure :: NFATran -> Set AState -> Set AState
eClosure nfaTran tSet = evalState (eClosure' nfaTran) (S.toList tSet, tSet)

eClosure' :: NFATran -> State (Stack AState, Set AState) (Set AState)
eClosure' nfaTran = do
    (stack, closure) <- get
    unless (null stack) $ do
        let t = peek stack
        put (pop stack, closure)
        let uSet = S.toList . fromJust $ M.lookup (t, epsilon) nfaTran
        forM_ uSet $ \u -> do
            (stack', closure') <- get
            unless (u `member` closure') $ put (push u stack', u `insert` closure')
        void $ eClosure' nfaTran
    return closure