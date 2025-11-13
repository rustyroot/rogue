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

## Exemples d'usages

```sh
./rogue
```

ou

```sh
make run
```

## Présentation des fichiers du projets

Voici une présentation succinte des différents fichiers composants le projet

### engine.ml

Contient la boucle de jeu et la logique d'appel des différentes entitées en tour par tour.

### world.ml

Définie la matrice contenant les différents éléments constituants la scène et les fonctions élémentaires pour intéragir avec.

### utils.ml

Contient un opérateur d'addition de couples.

### ui.ml

Ce fichier rassemble les fonctions d'affichage que ce soit de la scène principal ou des informations affichées au joueur (HUD).

### main.ml

Point d'entrée du programme, initialise les différents éléments avant de lancer la boucle de jeu.

### entity.ml

Définie un objet élémentaire et des fonctions associées utilisées utilisé par les différentes entités du jeu.

### player.ml

Il s'agit de l'entité controlée par le joueur.

### snake.ml

Une entité simple se déplaçant aléatoirement. 

### spider.ml

Implémente la logique des araignée comme décrit dans le sujet ainsi que celle des oeufs créés par celles-ci.

### elephant.ml

Implémente la logique de l'éléphant comme décrit dans le sujet.

### file.ml

Implémentation d'une file de priorité min nécessaire pour A*.

### monkey.ml

Implémentation pour l'objectif "entités malines" (c.f. Monkey).

### light.ml

Implémentation pour l'objectif "visibilité"

## Extensions principales

### Objectif entités malines (Monkey)

### Objectif système de jeu (keys and levels)

### Objectif visibilité (light)