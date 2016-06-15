import Data.SBV

-- | Solve puzzle two ^ 2 == three
--
-- >>> puzzle
-- Solution #1:
--   t = 1 :: Integer
--   w = 3 :: Integer
--   o = 8 :: Integer
--   h = 9 :: Integer
--   r = 0 :: Integer
--   e = 4 :: Integer
-- This is the only solution.
--   138 ^ 2 == 19044

puzzle :: IO AllSatResult
puzzle = allSat $ do
        ds@[t,w,o,h,r,e] <- mapM sInteger ["t", "w", "o", "h", "r", "e"]
        let isDigit x = x .>= 0 &&& x .<= 9
            val xs    = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)
            two       = val [t,w,o]
            three     = val [t,h,r,e,e]
        constrain $ bAll isDigit ds
        constrain $ allDifferent ds
        constrain $ t ./= 0
        solve [two * two .== three]
