{-
Converts a nondeterministic finite automaton (NFA) into a deterministic one (DFA).
This was created as Project 1 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}
module Main where

import           Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)

import           NFA2DFA.Algorithm
import           NFA2DFA.Data
import           NFA2DFA.Parse

import           Text.Parsec (parse)

-- | Where the program begins
main :: IO ()
main = do
    -- Parse standard input
    stuff <- getContents
    case parse nfa2dfaInput "" stuff of
         Left _    -> error "Improper input"
         Right nfa -> printDFA =<< subsetConstr nfa

-- | Create the minimal DFA by subset construction
subsetConstr :: NFA -> IO DFA
subsetConstr nfa = do
    -- e-closure(s_0) is unmarked initially
    let ecS0 = MarkedAStateSet 1 False . epsilonClosure (nfaTransitionFun nfa) . S.singleton $ nfaInitialAState nfa
    
    putStr "E-closure(IO) = "
    printSet $ markedSet ecS0
    putStr " = "
    putStrLn "1\n"
    
    -- Determine the DFA transition table
    dtran <- subsetConstr' (S.singleton ecS0) M.empty nfa
    
        -- Use the transition table to determine the states
    let dfaStates = S.fromList . map fst $ M.keys dtran
        -- Filter the final states from dfaStates
        dfaFinals = S.filter (\ms -> any (\u -> S.member u $ markedSet ms) . S.toList $ nfaFinalAStates nfa) dfaStates
    return $ DFA dfaStates (nfaInputSymbols nfa) dtran ecS0 dfaFinals

-- | Construct the DFA transition table
subsetConstr' :: Set MarkedAStateSet -> DFATran -> NFA -> IO DFATran
subsetConstr' dStates dtran nfa =
    let umdStates = S.filter ((==False) . marked) dStates -- Find unmarked states
    in if (S.null umdStates) -- If there are no unmarked states...
          then return dtran  -- ...then do nothing...
          else do            -- ...otherwise, call subsetConstrBody on an unmarked state, then check again.
              (dtran', dStates') <- subsetConstrBody (S.findMin umdStates) dStates dtran nfa
              subsetConstr' dStates' dtran' nfa

-- | Marks a state T and computes e-close(move(T,a)) for all input symbols a, modifying Dstates and Dtran accordingly.
subsetConstrBody :: MarkedAStateSet -> Set MarkedAStateSet -> DFATran -> NFA -> IO (DFATran, Set MarkedAStateSet)
subsetConstrBody t' dstates''' dtran nfa = do
    let lastNum = markedNum $ S.findMax dstates'''    -- The index of the most recently marked state
        t       = setL _marked True t'                -- Mark the state
        dstates = S.insert t (S.delete t' dstates''') -- Put the marked state in Dstates
        
    putStrLn $ "Mark " ++ show t
    
    -- A big, ugly fold that goes through every input symbol a
    (dtran', dstates', _) <- (flip . flip foldM) (dtran, dstates, lastNum) (S.toList $ nfaInputSymbols nfa) $ \(dtran', dstates', lastNum') a -> do
        let m = move (nfaTransitionFun nfa) (markedSet t) (Just a) -- Compute move(T, a)
        if S.null m -- If move(T,a) is the empty set, do nothing
           then return (dtran', dstates', lastNum')
           else do -- Otherwise, compute the e-closure
               printSet $ markedSet t
               putStr $ " --" ++ [a] ++ "--> "
               printSet m
               putStrLn ""
               
               let u = MarkedAStateSet (lastNum'+1) False (epsilonClosure (nfaTransitionFun nfa) m) -- e-closure(move(T, a))
                   
               putStr "E-closure"
               printSet m
               putStr " = "
               printSet $ markedSet u
               putStr " = "
               
               -- Check if u is in Dstates; if not, add it and increment lastNum
               if inDStates u dstates'
                  then do
                      let u' = lookupDStates u dstates'
                      putStrLn $ show u'
                      return (M.insert (t, a) u' dtran', dstates', lastNum')
                  else do
                      let dstates'' = S.insert u dstates'
                      putStrLn $ show u
                      return (M.insert (t, a) u dtran', dstates'', lastNum' + 1)
    putStrLn ""
    return (dtran', dstates')