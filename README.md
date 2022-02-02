# Block Knight

## Laboratórios de Informática I

This game was developed during the first semester of the Software Engineering degree @ University of Minho. The full process of development was evaluated and established the final grade for the "Laboratórios de Informática I" subject.

The project consisted of recreating the old game [BlockDude](http://azich.org/blockdude/), with the freedom to add features and completely change the graphical appearance, which was, in this case, heavily inspired by Hollow Knight.

It was fully developed in Haskell and consisted of 6 tasks:

1. Verify if a given map is valid;

2. Construct and destruct maps (from type "[(Peca, Coordenadas)]" to type "Mapa" and vice-versa);

3. Instantiate Show in order to display an output of the type "Jogo" as a String;

4. Update the current game state according to the player movements;

5. Create and implement game graphics, using Gloss.

6. Create a game bot to solve, in the minimum necessary player movements, a map.

## Game gallery
<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/mpJogar.jpg"/>
<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/eBackground1.jpg"/>
<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/mjMapa1.jpg"/>
<img align = "center" width = 900px src = "https://github.com/RuiL1904/Block-Knight/blob/main/assets/playingMap.png"/>

## Installing and running the game

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
$ git clone https://github.com/RuiL1904/Block-Knight.git
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

## Possible problems

In case you get a mpv error, you'll need to install it (since it was used to play audio inside the game).
Follow the instructions here: [mpv.io/installation](https://mpv.io/installation/)

# Developed by:

- A100643 Rui Lopes;

- A100646 Diogo Abreu.
