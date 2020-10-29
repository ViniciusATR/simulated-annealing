module Main where

import System.Random
import Control.Lens
import Control.Monad.State.Lazy

----------------
--Knapsack -----
----------------

type Solution = [Integer]
type SearchSpace = [(Integer, Integer)]
type Cost = SearchSpace -> Solution -> Integer

-- (peso,valor)
test_data = [(23, 92),
             (31, 57),
             (29, 49),
             (44, 68),
             (53, 60),
             (38, 43),
             (63, 67),
             (85, 84),
             (89, 87),
             (82, 72)]

max_capacity = 165

solution = [1,1,1,1,0,1,0,0,0,0]

packSum :: SearchSpace -> Solution -> (Integer, Integer)
packSum elems sol = foldr (\(i ,(p,v)) (ap, av) -> if i == 1 then (p + ap, v + av) else (ap, av)) (0,0) zipped
  where
    zipped = zip sol elems

isValid :: SearchSpace -> Solution -> Bool
isValid elems sol = p <= max_capacity
  where
    (p, _) = packSum elems sol

cost :: SearchSpace -> Solution -> Integer
cost elems sol = if p > max_capacity then v - (p - max_capacity) else v
  where
    (p, v) = packSum elems sol

randomRT inf sup = state $ randomR (inf , sup)

flipBit :: Solution -> Int -> Solution
flipBit sol pos = if sol!!pos == 1 then sol & element pos.~0 else sol & element pos.~1

neighbor :: Solution -> State StdGen Solution
neighbor s = do
  id <- randomRT (0::Int) (((length s) - 1)::Int)
  return $ flipBit s id

-----------------------
--Simulated Annealing--
-----------------------

type Config      = ( Int , Temperature, Delta )
type Temperature = Double
type Delta  = Double
type SearchState = ( Temperature, Delta , StdGen, Solution )

type Chooser = Temperature -> Solution -> Solution -> State StdGen Solution
type TempChanger = Temperature -> Temperature
type NeighborGen = Solution -> State StdGen Solution

updateTemp :: Delta -> Temperature -> Temperature
updateTemp d t = t * d

createChooser :: Cost -> SearchSpace -> Chooser
createChooser cost ss = chooseM
  where
    cost' = cost ss
    chooseM :: Chooser
    chooseM t so1 so2 | (cNew) > (cOld) = return so2
                      | otherwise = do
                                      rand <- randomRT 0.0 1.0
                                      let acceptProba = exp $ (cOld - cNew) / t
                                      return $ if acceptProba > rand then so2 else so1
                        where
                            cNew = fromIntegral $ cost' so2
                            cOld = fromIntegral $ cost' so1

sa :: TempChanger -> Chooser -> NeighborGen -> Temperature -> Solution -> Int -> State StdGen Solution
sa tc ch ng t0 s0 steps = sa' t0 s0 steps
  where
  sa' :: Temperature -> Solution -> Int -> State StdGen Solution
  sa' t s 0     = return s
  sa' t s steps = do n <- ng s
                     s'<- ch t s n
                     sa' ( tc t ) s' (steps - 1)

-- EXEMPLO DE USO
--
-- updateTempSample :: TempChanger
-- updateTempSample = updateTemp 0.9
-- chooser = createChooser cost test_data
--
-- sample = sa updateTempSample chooser neighbor
-- initsol = [1,0,1,0,1,0,1,0,1,0]
-- tmp  = 80
-- stps = 40
-- gen  = mkStdGen 30

-- final = evalState (sample tmp initsol stps) gen


main :: IO ()
main = do let updateTempSample = updateTemp 0.9
          let chooser = createChooser cost test_data
          let sample = sa updateTempSample chooser neighbor
          let initsol = [1,0,1,0,1,0,1,0,1,0]
          let tmp = 80
          let stps = 40
          let gen = mkStdGen 30
          print $ evalState (sample tmp initsol stps) gen
