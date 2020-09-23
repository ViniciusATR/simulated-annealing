module Main where

import System.Random
import Control.Lens

-----------------------
-- State Transformer --
-----------------------

data StateT state value = StateT (state -> (value, state))

sTransform :: StateT s v -> s -> (v , s)
sTransform (StateT tf) s = tf s


instance Functor (StateT s) where

  fmap g st = StateT newTFunction
    where
      newTFunction state = (g value , state')
        where (value, state') = sTransform st state

instance Applicative (StateT s) where
  pure v = StateT (\s -> (v, s))

  stf <*> stb = StateT concatTransform
    where
      concatTransform s = ( f x , s'' )
          where
            (f , s' ) = sTransform stf s
            (x , s'') = sTransform stb s'


instance Monad (StateT s) where
  return = pure

  st >>= gt = StateT newTFunction
    where newTFunction s = sTransform (gt x) s'
            where (x , s') = sTransform st s
----------------
--Knapsack -----
----------------

type Solution = [Integer]
type SearchSpace = [(Integer, Integer)]

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

-- Como injetar a funça de custo e outras sem poluir e aumentar o estado ?
cost' = cost test_data

createNeighbor :: Solution -> StdGen -> (Solution , StdGen)
createNeighbor sol gen = (neighbor, gen')
  where
    (pos, gen') = randomR (0::Int, ((length sol) - 1)::Int) gen
    neighbor = if sol!!pos == 1 then sol & element pos.~0 else sol & element pos.~1

-----------------------
--Simulated Annealing--
-----------------------

type Config      = ( Int, Int , Temperature, TempChange )
type Temperature = Double
type TempChange  = Double
type SearchState = ( Temperature, TempChange , StdGen, Solution )

choose :: Solution -> Solution -> StdGen -> Temperature -> (Solution , StdGen)
choose csol candidate gen t =
  if (cNew) > (cOld)
  then (candidate , gen)
  else ( csol', gen' )
    where
      cNew = fromIntegral $ cost' candidate
      cOld = fromIntegral $ cost' csol
      (rand , gen') = randomR (0.0::Double , 1.0::Double) gen
      csol' = if ((exp (cOld - cNew) / t)) > rand then candidate else csol

searchStep :: SearchState -> (Solution , SearchState)
searchStep ( t , dt , gen , csol ) = ( csol' , ( t' , dt , gen'' , csol' ) )
  where
    t'= t * dt
    (candidate , gen') = createNeighbor csol gen
    (csol', gen'') = choose csol candidate gen' t

searchStepT :: StateT SearchState Solution
searchStepT = StateT searchStep

search :: Config -> Solution -> Solution
search (step , seed, iTemp, tChange) initSol = last sol
  where
    initState = (iTemp , tChange , mkStdGen seed , initSol)
    listT  = mapM (const searchStepT) [1..step]
    (sol, _) = sTransform listT initState


main :: IO ()
main = do putStrLn "hello world"
