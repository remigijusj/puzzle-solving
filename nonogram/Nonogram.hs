module Main where

import Data.SBV
import Data.List
import Data.Map (Map, (!))
import Data.Traversable (forM, mapM)
import Debug.Trace (trace)
import System.Environment (getArgs)

import ParseN -- .n format parser

-- cols, rows as lists of clues
type Input = ([[Word8]], [[Word8]])


nonogram :: Input -> Symbolic SBool
nonogram (colClues, rowClues) = do
  let w = length colClues
  let h = length rowClues
  --
  colAnchors <- mapM mkAnchors colClues
  rowAnchors <- mapM mkAnchors rowClues
  --
  cells <- mkCells w h
  let colCells = transpose cells
  let rowCells = cells
  --
  let colGaps = concatMap (checkGaps h) (zip colClues colAnchors)
  let rowGaps = concatMap (checkGaps w) (zip rowClues rowAnchors)
  --
  let colDefs = concatMap checkDefs (zip3 colCells colClues colAnchors)
  let rowDefs = concatMap checkDefs (zip3 rowCells rowClues rowAnchors)
  --
  let conditions = colGaps ++ rowGaps ++ colDefs ++ rowDefs
  return $ bAll id conditions


mkCells :: Int -> Int -> Symbolic [[SBool]]
mkCells w h =
  forM [0 .. h-1] $ \r ->
    forM [0 .. w-1] $ \c -> do
      exists (show (r, c)) -- symbolic


mkAnchors :: [Word8] -> Symbolic [SWord8]
mkAnchors clues = mkExistVars (length clues)


-- NOTE: &&& (a .< symStop) - otherwise z3 gives values like xFF
checkGaps :: Int -> ([Word8], [SWord8]) -> [SBool]
checkGaps limit (clues, anchors) =
    fmap (\(a, b, c) -> (a + literal b .< c) &&& (a .< symStop)) triples
  where
    symStop = toSym (limit + 1)
    triples = zip3 anchors clues (tail anchors ++ [symStop])


checkDefs :: ([SBool], [Word8], [SWord8]) -> [SBool]
checkDefs (vector, clues, anchors) =
    fmap (\(x, i) -> x .== filled i) $ zip vector [0 ..]
  where
    filled i = bAny id (blocks i)
    blocks i = fmap (contains (toSym i)) (zip anchors clues)
    contains i (a, c) = a .<= i &&& i .< (a + literal c) 


toSym :: Int -> SWord8
toSym = literal . fromIntegral


solveOne :: Input -> IO String
solveOne input = do
  model <- satWith z3{verbose=False} (nonogram input)
  let dict = getModelDictionary model
  return $ showSolution input dict


solveAll :: Input -> IO String
solveAll input = do
  models <- allSatWith z3{verbose=False} (nonogram input)
  let dicts = getModelDictionaries models
  return $ unlines $ fmap (showSolution input) dicts


showSolution :: Input -> Map String CW -> String
showSolution input m =
  if null m
    then "no solution!"
    else showSolutionMap input m


showSolutionMap :: Input -> Map String CW -> String
showSolutionMap (cols, rows) m =
    concatMap (\r ->
      (concatMap (\c ->
        showCell $ fromCW $ m ! show (r, c)
      ) [0 .. w-1]) ++ "\n"
    ) [0 .. h-1] 
  where
    w = length cols
    h = length rows
    showCell filled = if filled then "O" else "."


-- helper for ghci
nono :: String -> IO ()
nono fname = do
  inp <- inputFromFile (fname ++ ".n")
  case inp of
    Nothing    -> putStrLn "ERROR: invalid format?"
    Just input -> solveOne input >>= putStrLn


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: $0 example (.n)"
    (name:_) -> nono name


-- TODO: modern Prelude, String->Text, flags (SMTConfig, all)

