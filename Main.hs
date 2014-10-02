{-
Converts a nondeterministic finite automaton (NFA) into a deterministic one (DFA).
This was created as Project 1 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Applicative
import           Control.Monad

import           Data.List
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Word

import           Text.Parsec (parse)
import           Text.Parsec.Char
import           Text.Parsec.Language
import qualified Text.Parsec.Prim as PT
import qualified Text.Parsec.Token as PT
import           Text.Parsec.Token (TokenParser)
import           Text.Parsec.String

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

-------------------------------------------------------------------------------
-- Some of the algorithms used for NFA-DFA conversion.
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- The data structures used for NFA-DFA conversion.
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

-------------------------------------------------------------------------------
-- The parser combinators used when reading in NFA input.
-------------------------------------------------------------------------------

lexer :: TokenParser st
lexer = PT.makeTokenParser emptyDef

-- | Parses something between curly braces.
braces :: Parser a -> Parser a
braces = PT.braces lexer

-- | Parses a natural number.
natural :: Parser Integer
natural = PT.natural lexer

-- | Parses a string, ignoring surrounding whitespace.
wsString :: String -> Parser String
wsString str = spaces *> string str <* spaces

-- | Parses any number of natural numbers, separated by commas.
csNaturals :: Parser (Set Integer)
csNaturals = (S.fromList . map fromInteger <$> oneOrMore)
         <|> (spaces *> return S.empty)
  where
    oneOrMore :: Parser [Integer]
    oneOrMore = do
        n  <- natural
        spaces
        ns <- PT.many (char ',' *> spaces *> natural)
        return $ n:ns

-- | Parses "Initial State: {x}"
initialState :: Parser AState
initialState = wsString "Initial" *> wsString "State:" *> (fromInteger <$> braces natural)

-- | Parses "Final States: {x}"
finalStates :: Parser (Set AState)
finalStates = wsString "Final" *> wsString "States:" *> (S.map fromInteger <$> braces csNaturals)

-- | Parses "Total States: x"
totalStates :: Parser Word
totalStates = wsString "Total" *> wsString "States:" *> (fromInteger <$> natural)

-- | Parses the input symbols (e.g., "State a b c E")
transitions :: Parser [Char]
transitions = wsString "State" *> PT.many (PT.try $ spaces *> letter) <* spaces

-- | Parses a list of transitions (e.g., "1 {} {2,3}")
transitionFun :: [Transition] -> Parser NFATran
transitionFun trans = do
    spaces
    aStateKey <- fromInteger <$> natural
    tFun <- (flip . flip foldM) M.empty trans $ \q t -> do
        aStatesValues <- S.map fromInteger <$> braces csNaturals
        return $ M.insert (aStateKey, t) aStatesValues q
    spaces
    return tFun

-- | Combines all of the above parsers to parse the entire input.
nfa2dfaInput :: Parser NFA
nfa2dfaInput = do
    iAState    <- initialState
    fAStates   <- finalStates
    numAStates <- totalStates
    symbsList  <- transitions
    let transList   = flip map symbsList $ \c -> case c of 'E' -> Nothing
                                                           c'  -> Just c'
        aStatesList = [1..numAStates]
        aStatesSet  = S.fromList . map fromIntegral $ aStatesList
        transSet    = S.fromList . map fromJust $ filter isJust transList
    tFun <- foldM (\q _ -> flip M.union q <$> transitionFun transList) M.empty aStatesList
    return $ NFA aStatesSet transSet tFun iAState fAStates