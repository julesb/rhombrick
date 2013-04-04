# rhombrick
<img src="http://xanthus.zapto.org/rhomb-screen-tmp.jpg" title="rhombrick work in progress screenshots" />

## Intro
This is a hobby project written in Clojure using the [Quil](https://github.com/quil/quil) library. It was inspired by Paul Harrison's [Ghost Diagrams](http://www.logarithmic.net/pfh/ghost-diagrams). The tile assembler algorithm on which the current algorithm is loosely based is described in detail in Chapter 7 of [Paul Harrison's thesis](http://www.logarithmic.net/pfh/thesis). I had contemplated the idea of extending the Ghost Diagrams concept to three dimensions for a while, and finally decided to try and implement it. I've ended spending more time on it than I thought I would. Ghost Diagrams creates planar tilings using either a square or hexagonal grid. The rhombrick program works in a similar way, but creates three dimensional structures using rhombic dodecahedral "tiles" on a rhombick dodecahedral "grid" (see [Rhombic dodecahedral honeycomb](http://en.wikipedia.org/wiki/Rhombic_dodecahedral_honeycomb)).

## Tiles and tile sets
A tile is specified by a 12 digit alphanumeric "tilecode". Each digit represents the compatibility state of one face of a rhombic dodecahedral tile. The digits '0' to '9' represent like-compatible faces. The digits 'a' to 'f' represent opposite compatible faces, where for example and uppercase 'A' is only compatible with a lowercase 'a'. For like-compatible codes '0' represents the smallest bezier box (described below) radius and '9' represents the largest. For opposite-compatible codes it's the same, with 'A' or 'a' being the smallest radius and 'F' or 'f' the largest. The digit '-' represents an empty face.
Tiles may be rotated to any of up to 24 orientations specified by chiral octahedral symmetry. Some tilecodes will have less than 24 rotations in the sense that some codes will have multiple rotations which produce the exact same form.

Two dimensional tilesets on a square or hex grid can be simulated by creating a tileset which is constrained to only allow certain faces to be non-empty. This almost gives a trivial way to map from Ghost Diagram's 2d codes to rhombrick's codes. It doesn't quite work because in Ghost Diagrams implementation the tiles can only rotate, but when simulating a 2d tileset in 3d, the tiles can be flipped (rotated 180°) as well as rotated.


An example tilecode: 
```
"-010----bB--"
```

A tile set is simply a set of one or more distinct tilecodes.

An example tile set:
```
["-1-C-2------" "5-02B-1-4---" "0c------5-1-" "--4-C04---A-"]
```

## Bezier box primitive
Currently the tiles are visualised using one or more instances of a structure that I called "bezier box". A bezier box is constructed by creating four triangle strips with vertices specified by four bezier curves with anchors creating a square profile centered on respecive face centers. A bezier box connects two faces of a tiles rhombic dodecahedron boundary (there is the case of terminal tilecode, ie. a code that only has a single non-empty face). The bezier box radius may vary along the it's length.

## Tile assembly
In general, the space of possible tilings is too vast to enumerate or exhaustively search, so a stochastic backtracking search is used to attempt to find a valid tiling for a given tileset. Some tilesets may only be tiled in a single configuration and these are generally easy for the tile assembler to solve. Other tilesets will present many options at each tile placement and some of these tilesets can take a very long time to solve, it they are solveable at all.
See the paper linked above for a more complete description of the tile assembler algorithm. 

## Usage
Clone the repo then do: lein deps; lein run.

## Keyboard commands
```
wasd        | Move camera (behaviour depends on camera mode)
khjl        | same as wasd
Arrow keys  | navigate tileset editor
, .         | decrease/increase model scale 
r           | soft init tiler
R           | init tiler with random tile set
- =         | decrease/increase camera field of view
c           | cycle camera mode (1 - mouse/kb, 2 - rubberband to glider, 3 - follow paths)
C           | toggle draw console (WIP)
f           | toggle draw bezier box faces
F           | toggle draw rhombic dodecahedra faces
l           | toggle draw bezier box lines
L           | toggle draw simple lines
M           | toggle bezier box smooth shading
g           | toggle draw gliders
G           | toggle draw graphs (WIP)
_ +         | decrease/increase bezier box resolution
; '         | increase/decrease bezier box control bias (change shape of bezier curves)
* &         | increae/decrease bezier box line weight
t T         | decrease/increase max tiles
[ ]         | decrease/increase tiler adhd
{ }         | decrease/increase tiler autism (adhd and autism are parameters
              which affect the tiler's characteristics regarding backtracking)
p           | pause/unpause tiler
( )         | decrease/increase assemblage max radius
< >         | load prev/next saved tileset from library
S           | save the current tileset to library
n m         | symmetry display index (debugging)
b           | cycle boundary mode (0 - none, 1 - terminal, 2 - all)
#           | save frame to image file
```
