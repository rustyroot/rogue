# rogue

## Dépendances

<table>
    <tbody>
        <tr><td>Dépendance</td><td>Comment l'installer</td></tr>
        <tr><td>Ocaml 5.3</td><td>https://github.com/ocaml/ocaml</td></tr>
        <tr><td>Opam</td><td>https://github.com/ocaml/opam</td></tr>
        <tr><td>Dune</td><td><pre><code class="sh">opam install dune</code></pre></td></tr>
        <tr><td>Notty</td><td><pre><code class="sh">opam install notty</code></pre></td></tr>
    </tbody>
</table>

## Compilation

```sh
make build
```

## Paramètres et exemples d'usages

<table>
    <tbody>
        <tr><td>Paramètre</td><td>Autre nom</td><td>Résultat</td></tr>
        <tr><td><pre><code class="sh">--darkness-on</code></pre></td>
        <td><pre><code class="sh">-d</code></pre></td>
        <td>Active le brouillard</td></tr>
        <tr><td><pre><code class="sh">--level-activated</code></pre></td>
        <td><pre><code class="sh">-l</code></pre></td>
        <td>Active le système de jeu avec des niveaux</td></tr>
    </tbody>
</table>

```sh
# launch the program whitout any feature, and one entity of each type
./rogue

# Reduice the visibility of the Camel
./rogue --darkness-on

# Play 10 levels (the 10th will loop) with prepared amount of each entity type
./rogue --level-activated

# Both for more challenges
./rogue --level-activated --darkness-on
```

## Jeu de tests

Les tests sont disponibles dans test/test_rogue.ml.

On peut les lancer avec :

```sh
make test
```
Cela affiche `OK` pour chaque test passé.

## Présentation des fichiers du projets

Voici une présentation succinte des différents fichiers composants le projet

### engine.ml

Contient la boucle de jeu et la logique d'appel des différentes entitées en tour par tour.

### world.ml

Définie la matrice contenant les différents éléments constituants la scène et les fonctions élémentaires pour intéragir avec.

### utils.ml

Contient des fonctions utilitaires pour différents modules.

### flags.ml

Contient les deux paramètres pour l'exécution ainsi que leur récupération via Sys.argv

### ui.ml

Ce fichier rassemble les fonctions d'affichage que ce soit de la scène principal ou des informations affichées au joueur (HUD).

### level.ml

Contient les niveaux et la fonctions pour changer le monde avec le nouveau niveau.

### main.ml

Point d'entrée du programme, initialise les différents éléments avant de lancer la boucle de jeu.

### entity.ml

Définie un objet élémentaire et des fonctions associées utilisées utilisé par les différentes entités du jeu.

### player.ml

Il s'agit de l'entité controlée par le joueur.

### snake.ml

Une entité simple se déplaçant aléatoirement. 

### spider.ml

Implémente la logique des araignée comme décrit dans le sujet ainsi que celle des oeufs créés par celles-ci.

### elephant.ml

Implémente la logique de l'éléphant comme décrit dans le sujet.

### heap.ml

Implémentation d'un tas min pour l'utiliser comme file de priorité min nécessaire pour A*.

### monkey.ml

Implémentation pour l'objectif "entités malines" (c.f. Monkey).

### light.ml

Implémentation pour l'objectif "visibilité"

## Extensions principales

### Objectif entités malines (Monkey)

Le singe est une entité qui utilise l'algorithme A* pour trouver le chameau.

À chaque tour, on calcule le prochain meilleur coup pour aller vers le chameau.

### Objectif système de jeu (keys and levels)

Avec l'option d'exécution -l, on active le système de jeu.

Dans chaque niveau, on retrouve une clé et différents ennemis. Le but est de rejoindre la clé sans se faire toucher par les ennemis.

Le chameau meurt après 3 dégats mais regagne un point de vie par niveau complété (sans dépasser un maximum de 3)

Les niveaux 0 à 10 sont implémentés, à partir du 11ème on répète en boucle le dernier.

Lorsqu'on meurt, le prochain input ferme le jeu et on affiche le nombre de niveaux complétés.

### Objectif visibilité (light)

Avec l'option d'exécution -d, on active la visibilité restreinte.

Des rayons (discrets) de lumières sont envoyés depuis le Camel vers l'ensemble des cases, on éclaire les cases tant que le rayon ne rencontre pas d'obstacle.
