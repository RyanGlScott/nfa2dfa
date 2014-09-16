module NFA2DFA.Algorithm where

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)

import           NFA2DFA.Data

subsetConstr :: NFA -> DFATran
subsetConstr nfa = subsetConstr' (MarkedAStateSet False . epsilonClosure (nfaTransitionFun nfa) . S.singleton $ nfaInitialAState nfa) nfa

subsetConstr' :: MarkedAStateSet -> NFA -> DFATran
subsetConstr' = undefined

move :: NFATran -> Set AState -> Transition -> Set AState
move nfaTran aStates trans = S.foldl' folder S.empty aStates
  where
    folder :: Set AState -> AState -> Set AState
    folder moveStates aState =
        let transStates = fromJust $ M.lookup (aState, trans) nfaTran
         in S.union transStates moveStates

epsilonClosure :: NFATran -> Set AState -> Set AState
epsilonClosure nfaTran set = epsilonClosure' (S.toList set) nfaTran set

epsilonClosure' :: Stack AState -> NFATran -> Set AState -> Set AState
epsilonClosure' stack nfaTran set = if null stack then set
    else let t      = peek stack
             stack' = pop stack
             uMap   = M.filterWithKey (\(st,symb) _ -> st == t && symb == epsilon) nfaTran
             setFolder (ecSet, qStack) u = if S.member u ecSet
                                              then (ecSet, qStack)
                                              else (S.insert u ecSet, push u qStack)
             (set', stack'') = M.foldl' (S.foldl' setFolder) (set, stack') uMap
         in epsilonClosure' stack'' nfaTran  set'