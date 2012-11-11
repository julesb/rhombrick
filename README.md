# rhombrick

Implements a tiling algorithm that tiles 3d space with rhombic dodecahedral blocks.

This is a little project for me to experiment with graphics in Clojure using the [Quil](https://github.com/quil/quil) library.

The tile assembler was inspired by the algorithm described in Chapter 7 of [Paul Harrison's thesis](http://www.logarithmic.net/pfh/thesis). I found this algorithm fascinating and wanted to try and extend it to work in three dimensions. 

Clone the repo then do: lein deps; lein run.


<img src="http://xanthus.zapto.org/rhombrick-screen.png" title="rhombrick" />

```
Keyboard commands
-----------------
wasd        | Move camera (behaviour depends on camera mode)
khjl        | same as wasd
Arrow keys  | navigate tileset editor
, \.        | change model scale 
r           | soft init tiler
R           | init tiler with random tile set
\- =        | camera field of view
c           | cycle camera mode (1 - mouse/kb, 2 - rubberband to glider, 3 - follow paths)
f           | toggle draw faces
g           | toggle draw gliders
t T         | decrease/increase max tiles
\[ \]       | decrease/increase tiler adhd
\{ \}       | decrease/increase tiler autism (adhd and autism are parameters which affect the tiler's characteristics regarding backtracking)
p           | pause/unpause tiler
\( \)       | decrease/increase assemblage max radius
< >         | load prev/next saved tileset from library
S           | save the current tileset to library
n m         | symmetry display index (debugging)
\#          | save frame to image file
```
