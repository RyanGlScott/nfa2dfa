{-
The parser combinators used when reading in NFA input.
This was created as Project 1 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}
module NFA2DFA.Parse where

import           Control.Applicative
import           Control.Monad

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Word

import           NFA2DFA.Data

import           Text.Parsec.Char
import           Text.Parsec.Language
import qualified Text.Parsec.Prim as PT
import qualified Text.Parsec.Token as PT
import           Text.Parsec.Token (TokenParser)
import           Text.Parsec.String

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