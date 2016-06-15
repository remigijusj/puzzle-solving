import Data.SBV

-- | Solve puzzle 215
--
-- >>> puzzle
-- Solution #1:
--   p = 9 :: Integer
--   o = 8 :: Integer
--   r = 6 :: Integer
--   k = 7 :: Integer
--   c = 3 :: Integer
--   h = 2 :: Integer
-- This is the only solution.
--   9867 / 3289 = 3

puzzle :: IO AllSatResult
puzzle = allSat $ do
        ds@[p,o,r,k,c,h] <- mapM sInteger ["p", "o", "r", "k", "c", "h"]
        let isDigit x = x .>= 0 &&& x .<= 9
            val xs    = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)
            pork      = val [p,o,r,k]
            chop      = val [c,h,o,p]
        constrain $ bAll isDigit ds
        constrain $ allDifferent ds
        constrain $ p ./= 0 &&& c .> 2
        solve [chop * c .== pork]
