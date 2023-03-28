# Block Knight

## Software Labs I | Laboratórios de Informática I
## Grade: 18/20 :star:

This game was developed during the first semester of the Software Engineering degree @ University of Minho.

The project consisted of recreating the old game BlockDude, with the freedom to add features and completely change the graphical appearance, which was, in this case, heavily inspired by Hollow Knight.

It was fully developed in Haskell and consisted of 6 tasks:

1. Verify if a given map is valid;

2. Construct and destruct maps (from type `[(Peca, Coordenadas)]` to type `Mapa` and vice-versa);

3. Instantiate `Show` class in order to display an output of the type `Jogo` as a String;

4. Update the current game state according to the player movements;

5. Create and implement game graphics, using Gloss.

6. Create a game bot to solve, in the minimum necessary player movements, a map.

## Game gallery

<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/mpJogar.jpg"/>
<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/eBackground1.jpg"/>
<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/mjMapa1.jpg"/>
<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/playingMap.png"/>

## Installing and running the game

**If you use any arch-based distro, head to the next section.**
Firstly, install Haskell Platform (GHC and Cabal).
To do so, follow the instructions for your specific system at: [haskell.org/downloads](https://www.haskell.org/downloads/)

Finally, since the graphical interface of the game was developed using the [Gloss](https://hackage.haskell.org/package/gloss) library, you'll need to install it:

```bash
$ cabal update
$ cabal install --lib gloss
$ cabal install --lib gloss-juicy
```

Since I used some more external libraries, you'll need to install them too:

```bash
$ cabal install --lib strict-io
```

#### Cloning the repository

```bash
$ git clone git@github.com:RuiL1904/Block-Knight.git
```

#### Compiling

```bash
$ cd Block-Knight/src
$ ghc -package strict-io Tarefa5_2021li1g033.hs
```

#### Running

```bash
$ cd ..
$ ./src/Tarefa5_2021li1g033
```
## Arch-based distros
Since arch is not that good working with cabal, I recommend you to use stack (you can install it with pacman).
Finally, since the graphical interface of the game was developed using the Gloss library, you'll need to install it:

```bash
$ stack upgrade
$ stack install gloss
$ stack install gloss-juicy
```

Since I used some more external libraries, you'll need to install them too:

```bash
$ stack install strict-io
```

#### Cloning the repository

```bash
$ git clone git@github.com/RuiL1904/Block-Knight.git
```

#### Compiling

```bash
$ cd Block-Knight/src
$ stack ghc --package gloss --package gloss-juicy --package strict-io Tarefa5_2021li1g033.hs
```

#### Running

```bash
$ cd ..
$ ./src/Tarefa5_2021li1g033
```

## Possible problems

In case you get an mpv error, you'll need to install it (as it was used to play audio inside the game).
Follow the instructions here: [mpv.io/installation](https://mpv.io/installation/)

# Developed by:

- [Rui Lopes](https://github.com/RuiL1904)

- Diogo Abreu
