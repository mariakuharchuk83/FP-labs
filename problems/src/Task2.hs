module Task2 where

import System.Random
import Data.Matrix

data NeuralNetwork = NeuralNetwork
  { weights :: [Matrix Double]  
  , biases :: [Matrix Double]
  }

createNeuralNetwork :: [Int] -> IO NeuralNetwork
createNeuralNetwork layerSizes = do
  let numLayers = length layerSizes
  let numWeights = numLayers - 1
  w <- sequence $ replicate numWeights (randomWeights <$> getRandomSeed)
  b <- sequence $ replicate numLayers (randomBiases <$> getRandomSeed)
  return $ NeuralNetwork w b

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoidDerivative :: Double -> Double
sigmoidDerivative x = sigmoid x * (1 - sigmoid x)

feedForward :: NeuralNetwork -> Matrix Double -> Matrix Double
feedForward (NeuralNetwork ws bs) input =
  foldl (\a (w, b) -> sigmoid (multStd w a + b)) input (zip ws bs)

train :: NeuralNetwork -> Matrix Double -> Matrix Double -> Double -> NeuralNetwork
train nn@(NeuralNetwork ws bs) input target learningRate =
  let
    (activations, zs) = feedForwardWithActivations nn input
    outputLayer = last activations
    outputError = target - outputLayer
    outputDelta = outputError * sigmoidDerivative (last zs)
    weightUpdates = reverse $ backpropagate activations zs outputDelta
    newWeights = zipWith (\w delta -> w + scaleMatrix learningRate delta) ws weightUpdates
  in
    NeuralNetwork newWeights bs


feedForwardWithActivations :: NeuralNetwork -> Matrix Double -> ([Matrix Double], [Matrix Double])
feedForwardWithActivations (NeuralNetwork ws bs) input =
  let
    (activations, zs) = foldl (\(a, z) (w, b) ->
      let a' = sigmoid (multStd w a + b)
      in (a' : a, multStd w a + b : z)
    ) ([input], []) (zip ws bs)
  in
    (reverse activations, reverse zs)

backpropagate :: [Matrix Double] -> [Matrix Double] -> Matrix Double -> [Matrix Double]
backpropagate activations zs outputDelta =
  let
    deltas = outputDelta : zipWith3 (\a z w ->
      hadamardProduct (transpose w * a) (sigmoidDerivative z)
    ) (tail activations) (tail zs) (reverse $ init activations)
  in
    reverse deltas

randomWeights :: StdGen -> Matrix Double
randomWeights gen = let (v, gen') = randomVector gen (uniform 0 1) (cols 1) in colVector v

randomBiases :: StdGen -> Matrix Double
randomBiases gen = let (v, gen') = randomVector gen (uniform 0 1) (cols 1) in colVector v

getRandomSeed :: IO StdGen
getRandomSeed = getStdGen

hadamardProduct :: Matrix Double -> Matrix Double -> Matrix Double
hadamardProduct = elementwise (*) 
