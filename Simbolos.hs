module Simbolos	(Expresion (..),simplificar) where

--declaracion de nueva clase "Expresión"
data Expresion a = Constante a
	| Variable String
	| Suma (Expresion a) (Expresion a)
	| Producto (Expresion a) (Expresion a)
	| Negacion (Expresion a)
	| Igualdad (Expresion a) (Expresion a)
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
	show (Suma a b) = show a ++ "+" ++ show b
	show (Negacion a) = "(" ++ "-" ++  show a ++ ")" 
	show (Producto a b) = show a ++ "*" ++ show b
	show (Igualdad a b) = show a ++ "=" ++ show b

agregarVariables :: (Num a) => [a] -> Expresion a
agregarVariables [] = Constante 0
agregarVariables incognitas@(x:xs) = (Constante x * Variable (show (length incognitas))) + (agregarVariables xs)

agregarVariablesConNombre :: (Num a) => [String] -> [a] -> Expresion a
agregarVariablesConNombre [] _ = Constante 0
agregarVariablesConNombre (x:xs) (c:cs) = (Constante c) * (Variable x) + (agregarVariablesConNombre xs cs)

--descripcion de los axiomas para poder usar en la simplificacion de una expresion
--no estan completos, ya que se escribieron los necesarios para resolver sistemas lineales.
axiomas :: (Num a, Eq a) => Expresion a -> Expresion a

axiomas (Negacion (Constante a)) = Constante (-a)
axiomas (Negacion (Variable a)) = (Constante (-1)) * (Variable a)
axiomas (Negacion (Suma a b)) = (Negacion (axiomas a)) + (Negacion (axiomas b))
axiomas (Negacion (Producto a b)) = (Negacion (axiomas a)) * axiomas b

axiomas (Suma (Constante a) (Constante b)) = Constante (a + b)
axiomas (Suma (Constante 0) a) = axiomas a
axiomas (Suma a (Constante 0)) = axiomas a

axiomas (Producto (Constante a) (Constante b)) = Constante (a * b)
axiomas (Producto (Constante 0) a) = Constante 0
axiomas (Producto a (Constante 0)) = Constante 0
axiomas (Producto (Constante 1) a) = axiomas a
axiomas (Producto a (Constante 1)) = axiomas a

axiomas (Producto (Constante a) (Producto (Constante b) expr)) = Constante (a * b) * axiomas expr 
axiomas (Producto (Constante a) (Producto expr (Constante b))) = Constante (a * b) * axiomas expr 
axiomas (Producto (Producto (Constante b) expr) (Constante a)) = Constante (a * b) * axiomas expr
axiomas (Producto (Producto expr (Constante b)) (Constante a)) = Constante (a * b) * axiomas expr

axiomas (Producto expr1 (Producto expr2 (Constante a))) = axiomas (expr1 * expr2) * Constante a
axiomas (Producto expr1 (Producto (Constante a) expr2)) = axiomas (expr1 * expr2) * Constante a
axiomas (Producto (Producto (Constante a) expr1) expr2) = axiomas (expr1 * expr2) * Constante a
axiomas (Producto (Producto expr1 (Constante a)) expr2) = axiomas (expr1 * expr2) * Constante a

axiomas (Suma (Suma (Constante a) expr) (Constante b)) = axiomas expr + Constante (a + b)
axiomas (Suma (Suma expr (Constante a)) (Constante b)) = axiomas expr + Constante (a + b)
axiomas (Suma (Constante a) (Suma (Constante b) expr)) = axiomas expr + Constante (a + b)
axiomas (Suma (Constante a) (Suma expr (Constante b))) = axiomas expr + Constante (a + b)

axiomas (Suma (Suma (Constante a) expr1) expr2) = axiomas (expr1 + expr2) + Constante a
axiomas (Suma (Suma expr1 (Constante a)) expr2) = axiomas (expr1 + expr2) + Constante a
axiomas (Suma expr1 (Suma (Constante a) expr2)) = axiomas (expr1 + expr2) + Constante a
axiomas (Suma expr1 (Suma expr2 (Constante a))) = axiomas (expr1 + expr2) + Constante a

axiomas (Suma (Producto (Constante a) x) (Producto (Constante b) y)) 
	| x == y = Constante (a + b) * axiomas x
axiomas (Suma (Producto x (Constante a)) (Producto (Constante b) y)) 
	| x == y = Constante (a + b) * axiomas x
axiomas (Suma (Producto (Constante a) x) (Producto y (Constante b))) 
	| x == y = Constante (a + b) * axiomas x
axiomas (Suma (Producto x (Constante a)) (Producto y (Constante b))) 
	| x == y = Constante (a + b) * axiomas x

axiomas (Suma x (Producto (Constante a) y)) 
	| x == y = Constante (a + 1) * axiomas x
axiomas (Suma x (Producto y (Constante a))) 
	| x == y = Constante (a + 1) * axiomas x
axiomas (Suma (Producto (Constante a) y) x) 
	| x == y = Constante (a + 1) * axiomas x
axiomas (Suma (Producto y (Constante a)) x) 
	| x == y = Constante (a + 1) * axiomas x

axiomas (Producto a b) = axiomas a * axiomas b

axiomas (Suma a b) | a == b = (Constante 2) * axiomas a
	| a == (Negacion b) = Constante 0
	| otherwise = axiomas a + axiomas b

axiomas a = a
--una funcion que reduce las expresiones todo lo posible usando los axiomas
simplificar a = a_simplificada a
	where 
		paso = axiomas a
		a_simplificada a
			| paso == a = a
			| otherwise = simplificar paso
{-
igualar :: (Num a, Eq a) => Expresion a -> Expresion a -> Expresion a

igualar (Variable x) (Constante b) = Igualdad (Variable x) (Constante b)
igualar (Suma (Variable x) (Variable y)) (Constante b) = Igualdad (Variable x) (Constante b - Variable y)
igualar (Suma (Variable x) (Producto (Constante k) (Variable y))) (Constante b) = Igualdad (Variable x) (Constante b -  Constante k * Variable y)

resolver :: (Num a, Eq a) => [(Expresion a, Expresion a)] -> [(Expresion a, Expresion a)]
resolver [] = []
resolver (expr:exs) = igualar (fst (simplificar expr)) (snd expr)
-}
