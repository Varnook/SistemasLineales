module Triangulacion (triangular) where

type Matriz = [[Double]]
type Vector = [Double] 

divPorEscalar :: Double -> Vector -> Vector
divPorEscalar escalar vector
	| escalar /= 0.0 = map (/escalar) vector
	| otherwise = vector

multPorEscalar :: Double -> Vector -> Vector
multPorEscalar escalar vector = map (*escalar) vector

restarVectores :: Vector -> Vector -> Vector
restarVectores [] [] = []
restarVectores _ [] = []
restarVectores [] _ = []
restarVectores (v1:v1s) (v2:v2s) = [v1-v2] ++ restarVectores v1s v2s

escalarEnPosN :: Vector -> Int -> Double
escalarEnPosN (v:vs) 1 = v
escalarEnPosN (v:vs) n = escalarEnPosN vs (n-1)

primerNoNulo :: Vector -> Double
primerNoNulo [] = 0.0
primerNoNulo (v:vs) 
	| v /= 0.0 = v
	| v == 0.0 = primerNoNulo vs

auxPosPrimerNoNulo :: Vector -> Int
auxPosPrimerNoNulo [] = -1
auxPosPrimerNoNulo vec@(v:vs)
	| v /= 0.0 = length(vs)
	| v == 0.0 = auxPosPrimerNoNulo vs

posPrimerNoNulo :: Vector -> Int
posPrimerNoNulo vector = (length vector) - (auxPosPrimerNoNulo vector)

primerVecConNoNuloEnN :: Matriz -> Int -> Vector
primerVecConNoNuloEnN [] _ = []
primerVecConNoNuloEnN matriz@(m:ms) n
	| posPrimerNoNulo m == n = m
	| posPrimerNoNulo m /= n = primerVecConNoNuloEnN ms n

normalizarPrimerPosicionNoNula :: Vector -> Vector
normalizarPrimerPosicionNoNula vector = divPorEscalar (primerNoNulo vector) vector

normalizarPrmPosMat :: Matriz -> Matriz
normalizarPrmPosMat matriz = map (normalizarPrimerPosicionNoNula) matriz

restarVectorAMatriz :: Vector -> Matriz -> Matriz

restarVectorAMatriz vector matriz@(m:ms) 
	| ms == [] && posPrimerNoNulo m == posPrimerNoNulo vector = 
		[restarVectores m vector]
	| ms == [] && posPrimerNoNulo m < posPrimerNoNulo vector = 	
		[restarVectores m (multPorEscalar (escalarEnPosN m (posPrimerNoNulo vector)) vector)]
	| ms == [] && posPrimerNoNulo m > posPrimerNoNulo vector = 
		[m]
	| posPrimerNoNulo m == posPrimerNoNulo vector = 
		(restarVectores m vector) : (restarVectorAMatriz vector ms)
	| posPrimerNoNulo m < posPrimerNoNulo vector = 
		(restarVectores m (multPorEscalar (escalarEnPosN m (posPrimerNoNulo vector)) vector)) : (restarVectorAMatriz vector ms)
	| posPrimerNoNulo m > posPrimerNoNulo vector = 
		m : (restarVectorAMatriz vector ms)

esOrigen :: Vector -> Bool
esOrigen [] = True
esOrigen vector@(v:vs)
	| (v == 0.0) && (length vector > 1) = esOrigen vs
	| (v == 0.0) && (length vector == 1) = True
	| otherwise = False

sumarVecMatPrimerFlaNula :: Vector -> Matriz -> Matriz
sumarVecMatPrimerFlaNula vector matriz@(m:ms)
	| esOrigen m = vector:ms
	| otherwise = m : sumarVecMatPrimerFlaNula vector ms

restarFilaAMatriz :: Vector -> Matriz -> Matriz
restarFilaAMatriz vector matriz = sumarVecMatPrimerFlaNula vector (restarVectorAMatriz vector matriz)

operarFilaConNuloEnPos :: Matriz -> Int -> Matriz
operarFilaConNuloEnPos matriz n = restarFilaAMatriz vector matriz
	where vector = primerVecConNoNuloEnN matriz n

esColumnaNula :: Int -> Matriz -> Bool
esColumnaNula _ [] = True
esColumnaNula n (m:ms) = escalarEnPosN m n == 0.0 && esColumnaNula n ms 

triangularLoop :: Int -> Int -> Matriz -> Matriz

triangularLoop 0 cantidadColumnas matriz 
	| primerVecConNoNuloEnN matriz (cantidadColumnas -2) == [] = 
		normalizarPrmPosMat matriz
	| otherwise = 
		operarFilaConNuloEnPos (normalizarPrmPosMat matriz) (cantidadColumnas - 2)

triangularLoop i cantidadColumnas matriz 
	| primerVecConNoNuloEnN matriz (cantidadColumnas - i) == [] = 
		triangularLoop (i - 2) cantidadColumnas (operarFilaConNuloEnPos (normalizarPrmPosMat matriz) (cantidadColumnas - (i - 1)))
	| otherwise = 
		triangularLoop (i - 1) cantidadColumnas (operarFilaConNuloEnPos (normalizarPrmPosMat matriz) (cantidadColumnas - i))

triangular :: Matriz -> Matriz
triangular matriz = triangularLoop (cantCol - 1) cantCol matriz
	where cantCol = length(head matriz)
