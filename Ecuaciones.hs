import Expresiones (Expresion(..))
import Simplificador (simplificar)
reemplazarVariable :: (Num a, Eq a) => Expresion a -> Expresion a -> Expresion a -> Expresion a
reemplazarVariable (Suma expr1 expr2) exprABuscar exprIn = (reemplazarVariable expr1 exprABuscar exprIn) + (reemplazarVariable expr2 exprABuscar exprIn)
reemplazarVariable (Producto expr1 expr2) exprABuscar exprIn = (reemplazarVariable expr1 exprABuscar exprIn) * (reemplazarVariable expr2 exprABuscar exprIn)
reemplazarVariable exprInicial exprABuscar exprIn | exprInicial == exprABuscar = exprIn
	| otherwise = exprInicial
