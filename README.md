<h1 align=center>
GLaDOS
</h1>
Ceci est la documentation pour expliquer le fonctionnement du projet

# Sommaire
- [A propos](#A-PROPOS)
- [Introduction](#Introduction)
- [Types de données](#TYPES-DE-DONNEES)
- [Définition de variables](#DEFINITION-DE-VARIABLES)
- [Définition de fonctions](#DEFINITION-DE-FONCTIONS)
- [Utilisation des fonctions](#UTILISATION-DES-FONCTIONS)
- [Conditions](#Conditions)
- [Messages d'erreur](#MESSAGES-DERREUR)
- [Bibliothèques standard](#BIBLIOTHEQUES-STANDARD)
- [Conclusion](#Conclusion)



# A PROPOS

Bienvenue dans la documentation de Yuyu, un langage de programmation basé sur Haskell.

# INTRODUCTION

Yuyu est un langage de programmation qui a été créé en s'inspirant de Haskell. Il a été conçu pour offrir une syntaxe concise et facile à comprendre, ainsi que des fonctionnalités modernes pour les programmeurs. Yuyu est conçu pour les programmeurs de tous niveaux, qu'ils soient débutants ou avancés.

# TYPES DE DONNEES

Yuyu prend en charge différents types de données tels que les entiers, les flottants, les chaînes de caractères, les booléens, les listes et les tableaux. Il utilise une syntaxe similaire à celle de Python pour définir les types de données et les variables.

# DEFINITION DE VARIABLES

Les variables dans Y sont définies à l'aide du symbole "->". Par exemple, pour définir une variable "x" avec la valeur 5, il suffit d'écrire "x -> 2". Y utilise un typage dynamique, ce qui signifie que vous n'avez pas besoin de déclarer explicitement le type de la variable.
Voici un exemple d'assignation de variable 
```
a -> 5
b -> 6
```

# DEFINITION DE FONCTIONS

Yuyu prend en charge la définition de fonctions en utilisant la syntaxe standard de Haskell. Cependant, il n'est pas possible de redéfinir les fonctions builtins. Par exemple, la fonction "+" est une fonction builtin qui ne peut pas être redéfinie.
Voici un exemple de fonction "add" qui prend deux arguments et renvoie leur somme :
```
add x y -> [+ x y]
```

# UTILISATION DES FONCTIONS

Les fonctions dans Yuyu sont appelées en utilisant une syntaxe similaire à celle de Haskell. Par exemple, pour appeler la fonction "add" avec les arguments 3 et 5, il suffit d'écrire "add 3 5".
Voici un exemple d'appel de fonction :
```
test a b -> [- [add a b] 1]
```
Yuyu prend également en charge les fonctions récursives.
Voici également un exemple de fonction récursive :
```
fact n -> [Siii [== n 1] 1 [* n [fact [- n 1]]]]
```

# CONDITIONS

Yuyu prend en charge l'utilisation de conditions à l'aide de la syntaxe standard de Haskell. Voici un exemple de code qui utilise une condition "if…else" dans une fonction qui vérifie si "a" et "b" sont égaux, si oui, on les rajoute, si non on les soustrait :

```
test a b -> [Siii [== a b] [add a b] [sub a b]]
```
Une autre condition que Yuyu prend en charge est le "case…of ". La condition "case…of " est interprété par "Prrr…of".
Dans cet exemple la condition "Prrr…of" dans une fonction vérifie si le "symbol" correspond à l’argument "a" ou "b" et renvoi soit la somme de "a" et "b" soit leur différence et s’il n’y a ni de "a" ni de "b" ça renvoi un otherwise ("De fou hein").

```
test a b -> Prrr symbol of [
 	"a" -> [add a b]
 	"b" -> [sub a b]
	_ -> "De fou hein"
]
```

# MESSAGES D’ERREUR

Yuyu contient également un système de messages d’erreurs.
Voici un exemple de message d'erreur que l’utilisateur verra lors d’une erreur. Dans ce cas l'erreur est le message "Holala quel horreur".

```
test a b -> Prrr symbol of [
	1 -> [add a b]
	2 -> "Holala quel horreur"
	_ -> "De fou hein"
]
```

# BIBLIOTHEQUES STANDARD

Yuyu dispose d'une bibliothèque standard limitée qui comprend les fonctions et les types de données de base. Il n'est pas possible de redéfinir les fonctions builtins dans la bibliothèque standard.
Les fonctions de base sont les suivantes :

"+", "-", "/", "*" et "%" (addition, soustraction, division, multiplication et modulo).

Les fonctions de comparaison :
"==", "!=", "<", ">", "<=", ">=" (égalité, différence, inférieur, supérieur, inférieur ou égal, supérieur ou égal).

Nous avons également la fonction "display" qui permet d'afficher un caractère à l'écran.
Voici un exemple d'utilisation de la fonction "display" :
```
display "a"
```

# CONCLUSION

Yuyu est un langage de programmation simple et facile à utiliser qui prend en charge les fonctionnalités de base de Haskell et du Python. Il est conçu pour les programmeurs de tout niveau et peut être utilisé pour développer une variété d'applications. Avec sa syntaxe concise et facile à comprendre, Yuyu peut aider les programmeurs à écrire du code de manière efficace et productive.
