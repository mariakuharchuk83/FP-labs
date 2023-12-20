module Main (main) where

import System.Environment  
import Task1 (solveMoves)
import Task4 (findPaths)
import Task3
import Task2

main :: IO ()
main = do
    args <- getArgs
    let a = map read args
    print $ solveMoves (head a) (a !! 1) (a !! 2)
    
    let graph = [[1, 3, 4], [0, 2, 3], [1, 3], [0, 1, 2, 4], [0, 3]]
    print $ findPaths graph 0 1
    
    gen <- getStdGen
    let cities = [0, 1, 2, 3]
    let initialPopulation = generatePopulation populationSize cities
    let result = geneticAlgorithm gen initialPopulation generations
    putStrLn $ "Best tour: " ++ show (fst $ head result)
    putStrLn $ "Distance: " ++ show (snd $ head result)
      
    let inputSize = 2
    let hiddenLayerSize = 3
    let outputSize = 1
    let learningRate = 0.1
    let numEpochs = 10000
    
    nn <- createNeuralNetwork [inputSize, hiddenLayerSize, outputSize]
  
    let trainingData = [ (fromList 2 1 [0, 0], fromList 1 1 [0])
                       , (fromList 2 1 [0, 1], fromList 1 1 [1])
                       , (fromList 2 1 [1, 0], fromList 1 1 [1])
                       , (fromList 2 1 [1, 1], fromList 1 1 [0]) ]
    
    let trainedNN = trainNeuralNetwork nn trainingData numEpochs learningRate
    
    let testInputs = [fromList 2 1 [0, 0], fromList 2 1 [0, 1], fromList 2 1 [1, 0], fromList 2 1 [1, 1]]
    let predictions = map (feedForward trainedNN) testInputs
    
    putStrLn "Predictions:"
    mapM_ (print . toList) predictions
    
    trainNeuralNetwork :: NeuralNetwork -> [(Matrix Double, Matrix Double)] -> Int -> Double -> NeuralNetwork
    trainNeuralNetwork nn trainingData numEpochs learningRate =
      foldl (\network _ ->
        foldl (\network' (input, target) -> train network' input target learningRate) network trainingData
      ) nn [1..numEpochs]