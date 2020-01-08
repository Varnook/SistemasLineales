module SistLinealesMain (main) where
import Expresiones (Expresion (..))
import Triangulacion (triangular)

main = print(triangular [[1,3,2,2],[5,6,12,7],[3,8,4,6]])
