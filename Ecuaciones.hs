import Expresiones (Expresion(..))
import Simplificador (simplificar)

--en una expresion cualquiera, reemplazo la variable (exprABuscar) dada 
--(que el segundo parametro sea una variable no está explícito en la función)
--por la expresion de entrada (exprIn)
rplzVar :: (Num a, Fractional a, Eq a) => Expresion a -> Expresion a -> Expresion a -> Expresion a

rplzVar (Suma expr1 expr2) exprABuscar exprIn = (rplzVar expr1 exprABuscar exprIn) + (rplzVar expr2 exprABuscar exprIn)

rplzVar  (Producto expr1 expr2) exprABuscar exprIn = (rplzVar expr1 exprABuscar exprIn) * (rplzVar expr2 exprABuscar exprIn)

rplzVar exprInicial exprABuscar exprIn | exprInicial == exprABuscar = exprIn
	| simplificar (Producto (Constante (-1)) exprInicial) == exprABuscar = - exprIn -- -eIni == eAB = -eI
	| otherwise = exprInicial

--como se obtiene informacion del tipo de dato "Igual"
igualdadEnTupla :: (Num a, Eq a) => Expresion a -> (Expresion a, Expresion a)
igualdadEnTupla (Igual x b) = (x,b)

--como pasar de un vector de Num a a una igualdad sin que se indique el nombre de las variables...
agregarVariables :: (Num a) => [a] -> Expresion a
agregarVariables (x:[]) = Constante 0
agregarVariables incognitas@(x:xs) = (Constante x * Variable (show (length incognitas))) + (agregarVariables xs)

-- ...y dando el nombre explícito de las variables
agregarVariablesConNombre :: (Num a) => [String] -> [a] -> Expresion a
agregarVariablesConNombre [] _ = Constante 0
agregarVariablesConNombre (x:xs) (c:cs) = (Constante c) * (Variable x) + (agregarVariablesConNombre xs cs)

--funciones para formar la ecuacion
igualar :: (Num a) => Expresion a -> a -> Expresion a
igualar expr numero = Igual expr (Constante numero)

formarEcuacion :: (Num a) => [a] -> [String] -> Expresion a
formarEcuacion constantes [] = igualar (agregarVariables constantes) (last constantes)
formarEcuacion constantes incognitas = igualar (agregarVariablesConNombre incognitas constantes) (last constantes)

--dada una variable de una ecuacion, se busca dejarla del lado izquierdo del igual
--despeja hasta dos variables
aislarVariable :: (Num a, Fractional a, Eq a) => Expresion a -> Expresion a-> Expresion a

aislarVariable var (Igual (Suma (Variable x) (Constante a)) (Constante b))
	| var == Variable x = Igual var (Constante (b - a))

aislarVariable var (Igual (Producto (Constante a) (Variable x)) (Constante b))
	| var == Variable x = Igual var (Constante (b/a))

aislarVariable var (Igual (Suma (Producto (Constante a) (Variable x)) (Constante k)) (Constante b))
	| var == Variable x = Igual var (Constante ((b-k)/a))

aislarVariable var (Igual (Suma (Variable x) (Variable y)) (Constante b)) 
	| var == Variable x = Igual var (Constante b - Variable y)
	| var == Variable y = Igual var (Constante b - Variable x)

aislarVariable var (Igual (Suma (Variable x) (Producto (Constante a) (Variable y))) (Constante b))
	| var == Variable x = Igual var (Constante b - (Producto (Constante a) (Variable y)))
	| var == Variable y = Igual var (Constante (b*(1/a)) - (Producto (Constante (1/a)) (Variable x)))

aislarVariable var (Igual (Suma (Producto (Constante a) (Variable y)) (Variable x)) (Constante b))
	| var == Variable x = Igual var (Constante b - (Producto (Constante a) (Variable y)))
	| var == Variable y = Igual var (Constante (b*(1/a)) - (Producto (Constante (1/a)) (Variable x)))

aislarVariable var (Igual (Suma (Producto (Constante r) (Variable x)) (Producto (Constante s) (Variable y))) (Constante b)) -- r*x + s*y = b
	| var == Variable x = Igual var (Constante (b*(1/r)) - (Producto (Constante (s*(1/r))) (Variable y)))
	| var == Variable y = Igual var (Constante (b*(1/s)) - (Producto (Constante (r*(1/s))) (Variable x)))

aislarVariable var expr = expr

--Encontrar una variable con 1 o 2 incógnitas
es1Incognita :: (Num a, Eq a) => Expresion a -> Bool
es1Incognita (Igual (Variable x) (Constante b)) = True
es1Incognita (Igual (Producto (Constante r) (Variable x)) (Constante b)) = True
es1Incognita (Igual (Suma (Variable x) (Constante k)) (Constante b)) = True
es1Incognita (Igual (Suma (Producto (Constante r) (Variable x)) (Constante k)) (Constante b)) = True
es1Incognita _ = False

es2Incognitas :: (Num a, Eq a) => Expresion a -> Bool
es2Incognitas (Igual (Suma (Variable x) (Variable y)) (Constante b)) = True
es2Incognitas (Igual (Suma (Producto (Constante r) (Variable x)) (Variable y)) (Constante b)) = True
es2Incognitas (Igual (Suma (Variable x) (Producto (Constante r) (Variable y))) (Constante b)) = True
es2Incognitas (Igual (Suma (Producto (Constante r) (Variable x)) (Producto (Constante s) (Variable y))) (Constante b)) = True 
es2Incognitas _ = False

buscarEcuacionMinIncognita :: (Num a, Eq a) => [Expresion a] -> Expresion a
buscarEcuacionMinIncognita (x:xs) | es1Incognita x = x
	| es2Incognitas x = x
	| otherwise = buscarEcuacionMinIncognita xs 

--devolver una variable presente en la ecuacion
variableEnEcuacion :: (Num a, Eq a) => Expresion a -> Expresion a
variableEnEcuacion (Igual (Variable x) (Constante b)) = Variable x
variableEnEcuacion (Igual (Producto (Constante r) (Variable x)) (Constante b)) = Variable x
variableEnEcuacion (Igual (Suma (Variable x) (Constante k)) (Constante b)) = Variable x
variableEnEcuacion (Igual (Suma (Producto (Constante r) (Variable x)) (Constante k)) (Constante b)) = Variable x
variableEnEcuacion (Igual (Suma (Variable x) (Variable y)) (Constante b)) = Variable x
variableEnEcuacion (Igual (Suma (Producto (Constante r) (Variable x)) (Variable y)) (Constante b)) = Variable x
variableEnEcuacion (Igual (Suma (Variable x) (Producto (Constante r) (Variable y))) (Constante b)) = Variable x
variableEnEcuacion (Igual (Suma (Producto (Constante r) (Variable x)) (Producto (Constante s) (Variable y))) (Constante b)) = Variable x
--resolverSistema :: (Num a, Eq a, Fractional a) => [Expresion a] -> [Expresion a]
