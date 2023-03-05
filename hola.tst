Comment le langage est pensé :
- Base Haskell
- Impossibilité de redéfinir les fonctions builtins -> correction env (tableau builtins de base)
- point virgule définition de fin de ligne (+ simple en parsing) 

Function :
calc x y > [- [+ x y] 2] ✅

If : Siii cond x y ❎

Case of : 
Prrr symbol of [
    [poss AST]
    [poss AST]
    [poss AST]
    [AST] => error if not found
] ❎

case x of
    1 -> 2
    2 -> 3
    3 -> 4
    _ -> pass

Either : Truc (True : Yupi, False : Oulah) ❎
UnDeux Yupi AST Oulah AST ❎ => Yupi Res

Definition :
a => 5 ✅