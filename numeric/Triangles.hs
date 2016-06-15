import Data.SBV

-- | Solve puzzle 227

puzzle :: IO SatResult
puzzle = sat $ do
        ds@[a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,f1,f2,g1,g2,h1,h2] <- mapM sInteger ["a1", "a2", "b1", "b2", "c1", "c2", "d1", "d2", "e1", "e2", "f1", "f2", "g1", "g2", "h1", "h2"]
        let isValid x = x .>= 1 &&& x .<= 16
        constrain $ bAll isValid ds
        constrain $ allDifferent ds
        solve [a1+a2+b1+e1.==34, b1+b2+c1+f1.==34, c1+c2+d1+g1.==34, e2+f1+f2+b2.==34, f2+g1+g2+c2.==34, g2+h1+h2+d2.==34]

-- Satisfiable. Model:
--   a1 = 15 :: Integer
--   a2 =  3 :: Integer
--   b1 =  2 :: Integer
--   b2 =  7 :: Integer
--   c1 = 16 :: Integer
--   c2 =  1 :: Integer
--   d1 =  6 :: Integer
--   d2 =  4 :: Integer
--   e1 = 14 :: Integer
--   e2 =  8 :: Integer
--   f1 =  9 :: Integer
--   f2 = 10 :: Integer
--   g1 = 11 :: Integer
--   g2 = 12 :: Integer
--   h1 = 13 :: Integer
--   h2 =  5 :: Integer

-- 15    2     16     6     
--    3     7      1      4 
-- 14    9     11     13    
--    8    10     12      5 
