import Data.SBV

-- | Solve puzzle 222 : bc * c == abc
--
-- >>> puzzle
-- Solution #1:
--   a = 3 :: Integer
--   b = 7 :: Integer
--   c = 5 :: Integer
-- Solution #2:
--   a = 1 :: Integer
--   b = 2 :: Integer
--   c = 5 :: Integer
-- Found 2 different solutions.
--   75 * 3 == 375, 25 * 1 == 125

puzzle :: IO AllSatResult
puzzle = allSat $ do
        ds@[a,b,c] <- mapM sInteger ["a", "b", "c"]
        let isDigit x = x .>= 0 &&& x .<= 9
            val xs    = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)
            abc       = val [a,b,c]
            bc        = val [b,c]
        constrain $ bAll isDigit ds
        constrain $ a .> 0
        solve [bc * c .== abc]
