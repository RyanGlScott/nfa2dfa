{-
Some of the algorithms used for NFA-DFA conversion.
This was created as Project 1 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}
module NFA2DFA.Algorithm where

import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)

import           NFA2DFA.Data

-- | Calculates the set of states that can be reached from set of states by
--   consuming exactly one input symbol.
move :: NFATran -> Set AState -> Transition -> Set AState
move nfaTran aStates trans = S.foldl' folder S.empty aStates
  where
    folder :: Set AState -> AState -> Set AState
    folder moveStates aState =
        -- Take all the states that can be reached from one state...
        let transStates = fromJust $ M.lookup (aState, trans) nfaTran
        -- ...and add them to the total.
        in S.union transStates moveStates

-- | Calculates the set of states that can be reached from a set of states by
--   taking zero or more empty transitions.
epsilonClosure :: NFATran -> Set AState -> Set AState
epsilonClosure nfaTran set = epsilonClosure' (S.toList set) nfaTran set

-- | Does the "iterative" part (actually, via recursion) of epsilonClosure.
epsilonClosure' :: Stack AState -> NFATran -> Set AState -> Set AState
epsilonClosure' stack nfaTran set = if null stack then set -- If the stack is empty, do nothing
    else let t      = peek stack 
             stack' = pop stack
             -- uMap is every state with with an incoming empty transition
             uMap   = M.filterWithKey (\(st,symb) _ -> st == t && symb == epsilon) nfaTran
             -- If u is not in the closure, then add it and push it onto the stack.
             setFolder (ecSet, qStack) u = if S.member u ecSet
                                              then (ecSet, qStack)
                                              else (S.insert u ecSet, push u qStack)
             (set', stack'') = M.foldl' (S.foldl' setFolder) (set, stack') uMap
          in epsilonClosure' stack'' nfaTran  set'

-- | Checks if a marked state's sets are in a marked state set.
inDStates :: MarkedAStateSet -> Set MarkedAStateSet -> Bool
inDStates (MarkedAStateSet _ _ set) dstates = any ((==set) . markedSet) $ S.toList dstates

-- | Retrieves a marked state in a set of marked states that has the same
--   set contents as another marked state.
lookupDStates :: MarkedAStateSet -> Set MarkedAStateSet -> MarkedAStateSet
lookupDStates (MarkedAStateSet _ _ set) dstates = fromJust . find ((==set) . markedSet) $ S.toList dstates