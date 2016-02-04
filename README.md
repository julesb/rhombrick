# rhombrick
<img src="http://173.230.146.130/rhombrick-screens.jpg" title="rhombrick work in progress screenshots" />

## Intro
This is a hobby project written in Clojure using the [Quil](https://github.com/quil/quil) library. It was inspired by Paul Harrison's [Ghost Diagrams](http://www.logarithmic.net/pfh/ghost-diagrams). The tile assembler algorithm is described in detail in Chapter 7 of [Paul Harrison's thesis](http://www.logarithmic.net/pfh/thesis). Ghost Diagrams creates planar tilings using either a square or hexagonal grid. The rhombrick program works in a similar way, but extends Ghost Diagrams square and hexagon tiles to include cube, hex prism, rhombic dodecahedron and truncated octahedron shapes tiles.

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


## Usage
Clone the repo then do: lein deps; lein run.

## Keyboard commands
```
wasd        | Move camera (behaviour depends on camera mode)
Arrow keys  | navigate tileset editor
, .         | decrease/increase model scale 
r           | soft init tiler
R           | init tiler with random tile set
A           | toggle auto-seed
- =         | decrease/increase camera field of view
c           | cycle camera mode (1 - mouse/kb, 2 - rubberband to glider, 3 - follow paths)
C           | toggle draw console (WIP)
f           | toggle draw bezier box faces
F           | toggle draw cell faces
e           | toggle draw empty
l           | toggle draw bezier box lines
L           | toggle draw simple lines
M           | toggle bezier box smooth shading
g           | toggle draw gliders
G           | toggle draw graphs (WIP)
i           | toggle draw info
B           | toggle draw voxelized tiles
Z           | toggle game mode
u           | place tile at cursor (game mode)
j           | backtrack (game mode)
h k         | prev/next candidate (game mode)
y           | voxelize tileset
!           | cycle skybox texture
@           | cycle tile texture
`           | reload post process shader
% ^         | decrease/increase palette offset
_ +         | decrease/increase bezier box resolution
; '         | increase/decrease bezier box control bias (change shape of bezier curves)
* &         | increae/decrease bezier box line weight
t T         | decrease/increase max tiles
[ ]         | decrease/increase tiler adhd
{ }         | decrease/increase tiler autism (adhd and autism are parameters
              which affect the tiler's characteristics regarding backtracking)
P           | pause/unpause tiler
( )         | decrease/increase assemblage max radius
< >         | load prev/next saved tileset from library
S           | save the current tileset to library
n m         | symmetry display index (debugging)
b           | cycle boundary mode (0 - none, 1 - terminal, 2 - all)
#           | save frame to image file
```
