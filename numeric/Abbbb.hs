import Data.SBV

-- | Solve puzzle 214
--
-- >>> puzzle
-- Solution #1:
--   a = 4 :: Integer
--   b = 9 :: Integer
-- This is the only solution.
--   4 + 5 * 9999 = 49999

puzzle :: IO AllSatResult
puzzle = allSat $ do
        ds@[a,b] <- mapM sInteger ["a", "b"]
        let isDigit x = x .>= 1 &&& x .<= 9
            val xs    = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)
            bbbb      = val [b,b,b,b]
            abbbb     = val [a,b,b,b,b]
        constrain $ bAll isDigit ds
        solve [a + 5 * bbbb .== abbbb]
