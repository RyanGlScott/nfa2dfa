{-# LANGUAGE RankNTypes #-}
module NFA2DFA.Data where

-- import           Control.Applicative
-- import           Control.Monad
-- import           Control.Monad.Trans.State

-- import           Data.Functor.Identity
-- import qualified Data.Map as M
import           Data.Map (Map)
-- import           Data.Maybe
-- import qualified Data.Set as S
import           Data.Set (Set, {- insert, member -})
import           Data.Word

-------------------------------------------------------------------------------

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- 
-- type Lens' s a = Lens s s a a
-- 
-- view :: Lens' s a -> s -> a
-- view lens = getConst . lens Const
-- {-# INLINE view #-}
-- 
-- set :: Lens' s a -> a -> s -> s
-- set lens = over lens . const
-- {-# INLINE set #-}
-- 
-- over :: Lens' s a -> (a -> a) -> s -> s
-- over lens f = runIdentity . lens (Identity . f)
-- {-# INLINE over #-}

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

newtype AState = AState Word deriving (Bounded, Eq, Ord, Read, Show)

instance Num AState where
    abs         = error "AState arithmetic not permitted"
    signum      = error "AState arithmetic not permitted"
    (+)         = error "AState arithmetic not permitted"
    (-)         = error "AState arithmetic not permitted"
    (*)         = error "AState arithmetic not permitted"
    fromInteger = AState . fromInteger

data MarkedAStateSet = MarkedAStateSet {
    marked    :: Bool
  , markedSet :: Set AState
} deriving (Eq, Ord, Read, Show)

-- _aStateNum :: Lens' AState Word
-- _aStateNum inj (AState num marked) = flip AState marked <$> inj num
-- {-# INLINE _aStateNum #-}

type Transition = Maybe InputSymbol

epsilon :: Transition
epsilon = Nothing

type InputSymbol = Char

type DFATran = Map (Set AState, InputSymbol) (Set AState)