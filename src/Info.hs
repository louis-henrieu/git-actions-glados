module Info where
    import Data.Map
    import Prelude hiding (lookup)
    import Data.Typeable
    import Data.Data
    import Prelude hiding (lookup)
    import Text.Show.Functions

    data Ast = Define String (Ast)
        | IntegerAst Int
        | FloatAst Float
        | SymbolAst String
        -- | Function [Ast] -> Either String Ast
        | Lambda Ast Ast
        | If Ast Ast Ast
        | Builtin ([Ast] -> Env -> Either String Ast)
        | Call [Ast]
        deriving Show

    type Env = Map String (Ast)
