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

braces :: Parser a -> Parser a
braces = PT.braces lexer

natural :: Parser Integer
natural = PT.natural lexer

wsString :: String -> Parser String
wsString str = spaces *> string str <* spaces

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

initialState :: Parser AState
initialState = wsString "Initial" *> wsString "State:" *> (fromInteger <$> braces natural)

finalStates :: Parser (Set AState)
finalStates = wsString "Final" *> wsString "States:" *> (S.map fromInteger <$> braces csNaturals)

totalStates :: Parser Word
totalStates = wsString "Total" *> wsString "States:" *> (fromInteger <$> natural)

transitions :: Parser [Char]
transitions = wsString "State" *> PT.many (PT.try $ spaces *> letter) <* spaces

transitionFun :: [Transition] -> Parser NFATran
transitionFun trans = do
    spaces
    aStateKey <- fromInteger <$> natural
    tFun <- foldM' M.empty trans $ \q t -> do
        aStatesValues <- S.map fromInteger <$> braces csNaturals
        return $ M.insert (aStateKey, t) aStatesValues q
    spaces
    return tFun
  where
    foldM' :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
    foldM' = flip . flip foldM

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