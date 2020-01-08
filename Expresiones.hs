module Expresiones (Expresion (..)) where

--declaracion de nueva clase "Expresión"
data Expresion a = Constante a
	| Variable String
	| Suma (Expresion a) (Expresion a)
	| Producto (Expresion a) (Expresion a)
	| Negacion (Expresion a)
	| Igual (Expresion a) (Expresion a)
	deriving(Eq)

--dependencias de las clases de operacion
instance (Num a) => Num (Expresion a) where
	(+) = Suma
	a - b = Suma a (Negacion b)
	(*) = Producto
	negate = Negacion 
	abs = undefined
	signum = undefined
	fromInteger = undefined

--dependencias de todas las clases para poder aparecer en pantalla
instance (Show a) => Show (Expresion a) where
	show (Constante a) = show a
	show (Variable a) = a
	show (Suma a b) = show a ++ " + " ++ show b
	show (Negacion a) = "(" ++ "-" ++  show a ++ ")" 
	show (Producto a b) = show a ++ "*" ++ show b
	show (Igual a b) = show a ++ " = " ++ show b

agregarVariables :: (Num a) => [a] -> Expresion a
agregarVariables [] = Constante 0
agregarVariables incognitas@(x:xs) = (Constante x * Variable (show (length incognitas))) + (agregarVariables xs)

agregarVariablesConNombre :: (Num a) => [String] -> [a] -> Expresion a
agregarVariablesConNombre [] _ = Constante 0
agregarVariablesConNombre (x:xs) (c:cs) = (Constante c) * (Variable x) + (agregarVariablesConNombre xs cs)
