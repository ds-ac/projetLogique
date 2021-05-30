# Critères de notation.

L'exécution de make doit fonctionner, et construire tous les binaires
associés aux questions traitées. Pour tous les encodages je mets des
points pour la correction et, quand c'est correct, pour l'efficacité.
Le nombre de points par problème va croissant.

Pour la partie DPLL, j'attribue des points sur la base du résultat de
`make PROVER=... test` et (indépendamment du résultat du test) des
points pour chaque question sur la base de la lecture du code: il faut
donc qu'il soit clair et que l'implémentation corresponde à ce qui
est demandé (ce qui inclus des contraintes d'efficacité e.g. sur les
listes doublement chainées).

## Latin

Je vérifie qu'une amélioration a été apportée, cela doit terminer
en quelques secondes en taille 30. Je vous demande de ne pas éliminer
de solutions, ou en tout cas de ne pas faire plus que l'élimination
de symétrie qui consiste à fixer la première ligne ou colonne.

## Greek

En taille 7, j'attends SAT en environ 2s.

## Pavage

En taille 70 avec le forbidden pattern, solution trouvée en moins de 5s.
En taille 80 avec le forbidden pattern, UNSAT en moins de 8s.

## Pingouins

L'exemple problems/1/flake2v doit passer en moins d'1s.
J'attribuerai des points pour les meilleurs temps sur l'ensemble des tests
non-hard. Pour référence la commande suivante prend moins de 4s chez moi:

```
$ time for i in `seq 0 5` ; do make PENALTY=$i tests_pingouins ; done
```
