module Main where

import NFA2DFA.Algorithm
import NFA2DFA.Parse

main :: IO ()
main = putStrLn "Hello, World!"

-- eClosure :: NFATran -> Set AState -> Set AState
-- eClosure nfaTran tSet = evalState (eClosure' nfaTran) (S.toList tSet, tSet)
-- 
-- eClosure' :: NFATran -> State (Stack AState, Set AState) (Set AState)
-- eClosure' nfaTran = do
--     (stack, closure) <- get
--     unless (null stack) $ do
--         let t = peek stack
--         put (pop stack, closure)
--         let uSet = S.toList . fromJust $ M.lookup (t, epsilon) nfaTran
--         forM_ uSet $ \u -> do
--             (stack', closure') <- get
--             unless (u `member` closure') $ put (push u stack', u `insert` closure')
--         void $ eClosure' nfaTran
--     return closure