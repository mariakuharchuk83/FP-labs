module Task3 where

import System.Random
import Data.List (permutations, sortBy)
import Data.Function (on)

type City = Int
type Tour = [City]
type Distance = Int
type Population = [Tour]

populationSize :: Int
populationSize = 100

generations :: Int
generations = 1000

mutationRate :: Float
mutationRate = 0.01

generatePopulation :: Int -> [City] -> Population
generatePopulation size cities = take size $ permutations cities

calculateDistances :: [(Tour, Distance)] -> [(Tour, Distance)]
calculateDistances = map (\(tour, _) -> (tour, tourDistance tour))

tourDistance :: Tour -> Distance
tourDistance [] = 0
tourDistance [_] = 0
tourDistance (x:y:xs) = distanceMatrix !! x !! y + tourDistance (y:xs)

distanceMatrix :: [[Distance]]
distanceMatrix = [
  [0, 10, 15, 20],
  [10, 0, 35, 25],
  [15, 35, 0, 30],
  [20, 25, 30, 0]
  ]

selectBest :: Population -> [(Tour, Distance)]
selectBest population = take populationSize $ calculateDistances $ sortPopulation population

sortPopulation :: Population -> Population
sortPopulation = sortBy (comparing tourDistance)

geneticAlgorithm :: StdGen -> Population -> Int -> [(Tour, Distance)]
geneticAlgorithm _ population 0 = selectBest population
geneticAlgorithm gen population n = do
  let selected = selectBest population
  let crossed = crossover gen selected
  let mutated = mutate gen crossed
  geneticAlgorithm (snd $ next gen) mutated (n-1)

crossover :: StdGen -> [(Tour, Distance)] -> Population
crossover gen selected = concatMap (\(x, y) -> [x, y]) $ zip (take halfSize selected) (drop halfSize selected)
  where halfSize = populationSize `div` 2

mutate :: StdGen -> Population -> Population
mutate gen = map (\tour -> if shouldMutate then mutateTour gen tour else tour)
  where shouldMutate = randomR (0.0, 1.0) gen' < mutationRate
        (gen', _) = next gen

mutateTour :: StdGen -> Tour -> Tour
mutateTour gen tour = take x tour ++ [tour !! y] ++ drop (x + 1) (take y tour) ++ [tour !! x] ++ drop (y + 1) tour
  where (x, gen') = randomR (0, length tour - 1) gen
        (y, _) = randomR (0, length tour - 1) gen'
