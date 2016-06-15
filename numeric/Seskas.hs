import Data.SBV

-- | Solve puzzle 217 : kas * kas = xexkas
--
-- >>> puzzle
-- Solution #1:
--   x = 1 :: Integer
--   e = 4 :: Integer
--   k = 3 :: Integer
--   a = 7 :: Integer
--   s = 6 :: Integer
-- This is the only solution.
--   376 ^ 2 == 141376

puzzle :: IO AllSatResult
puzzle = allSat $ do
        ds@[x,e,k,a,s] <- mapM sInteger ["x", "e", "k", "a", "s"]
        let isDigit x = x .>= 0 &&& x .<= 9
            val xs    = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)
            xexkas    = val [x,e,x,k,a,s]
            kas       = val [k,a,s]
        constrain $ bAll isDigit ds
        constrain $ k .> 0
        solve [kas * kas .== xexkas]
