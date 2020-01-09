import Expresiones (Expresion(..))
import Simplificador (simplificar)

--en una expresion cualquiera, reemplazo la variable (exprABuscar) dada 
--(que el segundo parametro sea una variable no está explícito en la función)
--por la expresion de entrada (exprIn)
rplzVar :: (Num a, Eq a) => Expresion a -> Expresion a -> Expresion a -> Expresion a

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
aislarVariable :: (Num a, Eq a) => Expresion a -> Expresion a-> Expresion a
aislarVariable _ (Igual (Variable x) (Constante b)) = Igual (Variable x) (Constante b)
aislarVariable var (Igual (Suma x (Variable y)) (Constante b)) | var == x = Igual var (Constante b - Variable y)
