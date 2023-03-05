-- Exemple d'assignation de variable
a -> 5
b -> 6
-- Exemple d'appel de fonction
add x y -> [+ x y]
-- ou encore
test a b -> [- [add a b] 1]

-- Exemple de condition if else dans une fonction
test a b -> [Siii [== a b] [add a b] [sub a b]]

-- Exemple de case of dans une fonction
test a b -> Prrr symbol of [
    "a" -> [add a b]
    "b" -> [sub a b]
    _ -> "De fou hein"
]

-- Exemple de fonction récursive
fact n -> [Siii [== n 1] 1 [* n [fact [- n 1]]]]

-- Exemple d'erreur
test a b -> Prrr symbol of [
    1 -> [add a b]
    2 -> "Holala quel horreur"
    _ -> "De fou hein"
]