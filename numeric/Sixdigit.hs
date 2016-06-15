import Data.SBV

-- | Solve puzzle 224. abcde4 * 4 == 4abcde
--
-- >>> puzzle
-- Solution #1:
--   a = 1 :: Integer
--   b = 0 :: Integer
--   c = 2 :: Integer
--   d = 5 :: Integer
--   e = 6 :: Integer
-- This is the only solution.
--   102564 * 4 == 410256

puzzle :: IO AllSatResult
puzzle = allSat $ do
        ds@[a,b,c,d,e] <- mapM sInteger ["a", "b", "c", "d", "e"]
        let isDigit x = x .>= 0 &&& x .<= 9
            val xs    = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)
            _abcde4   = val [a,b,c,d,e,4]
            _4abcde   = val [4,a,b,c,d,e]
        constrain $ bAll isDigit ds
        constrain $ a ./= 0
        solve [_abcde4 * 4 .== _4abcde]
