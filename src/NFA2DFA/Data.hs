{-
The data structures used for NFA-DFA conversion.
This was created as Project 1 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}
{-# LANGUAGE RankNTypes #-}
module NFA2DFA.Data where

import           Control.Applicative
import           Control.Monad

import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Word

-------------------------------------------------------------------------------

-- | Magical data accessor/setter type.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | The simpler Lens type used for most operations.
type Lens' s a = Lens s s a a

-- | The trivial functor.
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f = Identity . f . runIdentity

-- | View a data structure element via a 'Lens'.
viewL :: Lens' s a -> s -> a
viewL lens = getConst . lens Const
{-# INLINE viewL #-}

-- | Replace a data structure element via a 'Lens'.
setL :: Lens' s a -> a -> s -> s
setL lens = overL lens . const
{-# INLINE setL #-}

-- | Modify a data structure element via a 'Lens'.
overL :: Lens' s a -> (a -> a) -> s -> s
overL lens f = runIdentity . lens (Identity . f)
{-# INLINE overL #-}

-------------------------------------------------------------------------------

-- | A stack is simply a singly linked list.
type Stack = []

-- Pushing is cons
push :: a -> Stack a -> Stack a
push = (:)

-- Popping is cdr
pop :: Stack a -> Stack a
pop = tail

-- Peeking is car
peek :: Stack a -> a
peek = head

-------------------------------------------------------------------------------

-- | A nondeterminstic finite automaton data structure.
data NFA = NFA {
    nfaAStates       :: Set AState
  , nfaInputSymbols  :: Set InputSymbol
  , nfaTransitionFun :: NFATran
  , nfaInitialAState :: AState
  , nfaFinalAStates  :: Set AState
} deriving (Eq, Ord, Read, Show)

-- | An NFA transition table.
type NFATran = Map (AState, Transition) (Set AState)

-- | A state data structure.
newtype AState = AState { runAState :: Word } deriving (Bounded, Eq, Ord, Read)

instance Num AState where
    abs         = error "AState arithmetic not permitted"
    signum      = error "AState arithmetic not permitted"
    (+)         = error "AState arithmetic not permitted"
    (-)         = error "AState arithmetic not permitted"
    (*)         = error "AState arithmetic not permitted"
    fromInteger = AState . fromInteger

instance Show AState where
    showsPrec d (AState w) = showsPrec d w

-- After an NFA is converted to a DFA, the DFA's states will actually consist
-- of marked sets of NFA states. Nifty!
data MarkedAStateSet = MarkedAStateSet {
    markedNum :: Word
  , marked    :: Bool
  , markedSet :: Set AState
} deriving (Eq, Read)

-- Only compare 'MarkedAStateSet's by their indexes.
instance Ord MarkedAStateSet where
    compare ms1 ms2 = compare (markedNum ms1) (markedNum ms2)

instance Show MarkedAStateSet where
    showsPrec d (MarkedAStateSet n _ _) = showsPrec d n

-- A 'Lens' that views a 'MarkedAStateSet`'s marked status.
_marked :: Lens' MarkedAStateSet Bool
_marked inj (MarkedAStateSet mn m ms) = (\m' -> MarkedAStateSet mn m' ms) <$> inj m
{-# INLINE _marked #-}

type InputSymbol = Char

-- An input symbol, or epsilon
type Transition = Maybe InputSymbol

epsilon :: Transition
epsilon = Nothing

-- A deterministic finite automaton data structure (as converted from an NFA).
data DFA = DFA {
    dfaAStates       :: Set MarkedAStateSet
  , dfaInputSymbols  :: Set InputSymbol
  , dfaTransitionFun :: DFATran
  , dfaInitialAState :: MarkedAStateSet
  , dfaFinalStates   :: Set MarkedAStateSet
}

-- | A DFA transition table.
type DFATran = Map (MarkedAStateSet, InputSymbol) MarkedAStateSet

-- Prettily prints a DFA to the screen.
printDFA :: DFA -> IO ()
printDFA (DFA states inputs trans initial finals) = do
    putStr "Initial State: "
    printSet $ S.singleton initial
    putStrLn ""
    
    putStr "Final States: "
    printSet finals
    putStrLn ""
    
    putStr "State\t"
    let inputsList = S.toList inputs
    forM_ inputsList $ \input -> do
        putChar input
        putStr "\t"
    
    putStrLn ""
    forM_ (S.toList states) $ \st -> do
        putStr $ show st
        putStr "\t"
        forM_ inputsList $ \input -> do
            if (M.member (st, input) trans)
               then do
                   putChar '{'
                   putStr . show . fromJust $ M.lookup (st, input) trans
                   putStr "}\t"
               else putStr "{}\t"
        putStrLn ""

-- Prettily prints a set's contents surrounded by braces.
printSet :: Show a => Set a -> IO ()
printSet set = do
    putChar '{'
    unless (S.null set) $ do
        let setList     = S.toList set
        putStr . show $ head setList
        forM_ (tail setList) $ \e -> do
            putChar ','
            putStr $ show e
    putChar '}'