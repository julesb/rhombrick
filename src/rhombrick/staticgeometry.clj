(ns rhombrick.staticgeometry
  (:use [rhombrick.vector]
        [clojure.math.combinatorics]))
; _______________________________________________________________________
;
; Rhombic dodecahedron
; Faces     12
; Edges     24
; Vertices  14

(def ^:const rd-verts [
  [ 1 -1 -1]
  [ 1 -1  1]
  [-1 -1  1]
  [-1 -1 -1]
  [ 1  1 -1]
  [ 1  1  1]
  [-1  1  1]
  [-1  1 -1]
  [ 0  0  2]
  [-2  0  0]
  [ 2  0  0]
  [ 0 -2  0]
  [ 0  0 -2]
  [ 0  2  0]
])

; anticlockwise
;(def ^:const rd-faces [
;               [0 10 1 11]
;               [5 8 1 10]
;               [4 13 5 10]
;               [6 8 5 13]
;               [7 13 4 12]
;               [0 12 4 10]
;               [0 11 3 12]
;               [1 8 2 11]
;               [6 9 2 8]
;               [7 9 6 13]
;               [7 12 3 9]
;               [3 11 2 9]
;])

(def ^:const rd-faces
[[11 1 10 0]
 [10 1 8 5]
 [10 5 13 4]
 [13 5 8 6]
 [12 4 13 7]
 [10 4 12 0]
 [12 3 11 0]
 [11 2 8 1]
 [8 2 9 6]
 [13 6 9 7]
 [9 3 12 7]
 [9 2 11 3]])

;(def rd-face-colors [
;                  [255   0   0] ;  0 red
;                  [255 128   0] ;  1 orange
;                  [255 255   0] ;  2 yellow
;                  [  0 255   0] ;  3 green
;                  [  0   0 255] ;  4 blue
;                  [128   0 255] ;  5 purple
;                  [  0 128   0] ;  9 dark green
;                  [  0   0 128] ; 10 dark blue
;                  [ 64   0 128] ; 11 dark purple
;                  [128   0   0] ;  6 dark red
;                  [128  64   0] ;  7 dark orange
;                  [128 128   0] ;  8 dark yellow
;                  ])

;(def ^:const rd-face-colors [
;                  [255   0   0] ;  0 red
;                  [255 128   0] ;  1 orange
;                  [255 255   0] ;  2 yellow
;                  [128 255   0] ;  3 light green
;                  [  0 255   0] ;  4 green
;                  [  0 255 128] ;  5 purple
;                  [  0 255 255] ;  9 cyan
;                  [  0 128 255] ; 10 light-blue
;                  [  0   0 255] ; 11 blue
;                  [128   0 255] ;  6 red-blue
;                  [255   0 255] ;  7 magenta
;                  [255   0 128] ;  8 pink
;                  ])

(def ^:const rd-face-colors
[[96,134,68],
[180,91,202],
[192,70,57],
[87,53,85],
[197,175,172],
[117,213,82],
[91,137,229],
[199,82,132],
[119,204,180],
[207,205,88],
[74,70,51],
[191,131,64]]


)


(def ^:const rd-neighbour-offsets [
                       [ 1 -1  0]
                       [ 1  0  1]
                       [ 1  1  0]
                       [ 0  1  1]
                       [ 0  1 -1]
                       [ 1  0 -1]
                       [-1  1  0]
                       [-1  0 -1]
                       [-1 -1  0]
                       [ 0 -1 -1]
                       [ 0 -1  1]
                       [-1  0  1]
                       ])

(def ^:const rd-connecting-faces [6 7 8 9 10 11 0 1 2 3 4 5])

;
; Cuboctahedron
; Faces     14
; Edges     24
; Vertices  12
;
; Being the dual of the rhombic dodecahedron, this
; gives us the vertices of the centers of the
; faces of said polyhedron and are also used to
; calculate face normals.

(def ^:const co-verts [
                       [ 1 -1  0]
                       [ 1  0  1]
                       [ 1  1  0]
                       [ 0  1  1]
                       [ 0  1 -1]
                       [ 1  0 -1]

                       [-1  1  0]
                       [-1  0 -1]
                       [-1 -1  0]
                       [ 0 -1 -1]
                       [ 0 -1  1]
                       [-1  0  1] ])



(def ^:const boundary-colors {
  \0 [0 0 0]
  \1 [255 0 0]
  \2 [255 128 0]
  \3 [255 255 0]
  \4 [0 255 0]
  \5 [0 0 255]
  \6 [75 0 130]
  \7 [143 0 255]
  \a [0 0 0]
  \A [255 255 255]
  \b [0 0 0]
  \B [255 255 255]
  \c [0 0 0]
  \C [255 255 255]
  \d [0 0 0]
  \D [255 255 255]
})

; Truncated Octahedron
; Faces     14
; Edges     36
; Vertices  24

(def ^:const to-verts [
  [ 1  0  2]
  [-1  0  2]
  [ 0 -1 -2]
  [-1 -0 -2]
  [ 0  1 -2]
  [ 1 -0 -2]
  [ 2  0  1]
  [ 2 -1  0]
  [ 2 -0 -1]
  [ 2  1 -0]
  [ 1  2 -0]
  [ 0  2 -1]
  [-1  2 -0]
  [ 0  2  1]
  [-1 -2  0]
  [ 0 -2 -1]
  [ 1 -2  0]
  [ 0 -2  1]
  [-2  0  1]
  [-2  1 -0]
  [-2 -0 -1]
  [-2 -1  0]
  [ 0  1  2]
  [ 0 -1  2]
])

(def ^:const to-face-centers [
                              [ 0  2  0] ; sqr top
                              [-1 -1  1] ; hex t fr l
                              [-2  0  0] ; sql l
                              [ 0  0 -2] ; sqr front
                              [ 1 -1  1] ; hex t fr r
                              [ 1  1  1] ; hex t ba r
                              [-1  1  1] ; hex t ba l
                              [ 0 -2  0] ; sqr bot
                              [ 1  1 -1] ; hex b ba r
                              [ 2  0  0] ; sqr r
                              [ 0  0  2] ; sqr back
                              [-1  1 -1] ; hex b ba l
                              [-1 -1 -1] ; hex b fr l
                              [ 1 -1 -1] ; hex b fr r
                              ])

(def to-tris-and-quads [
[0 22 1 23]
 [2 3 4 5]
 [6 7 8 9]
 [10 11 12 13]
 [14 15 16 17]
 [18 19 20 21]
 [12 11 4]
 [20 19 3]
 [22 13 12]
 [18 1 19]
 [5 8 7]
 [2 16 15]
 [6 0 7]
 [16 23 17]
 [9 10 13]
 [0 6 22]
 [5 4 11]
 [9 8 10]
 [18 21 14]
 [1 17 23]
 [20 3 2]
 [21 15 14]
 [6 9 22]
 [9 13 22]
 [1 22 12]
 [1 12 19]
 [3 19 4]
 [4 19 12]
 [2 15 21]
 [2 21 20]
 [1 14 17]
 [1 18 14]
 [0 23 7]
 [7 23 16]
 [5 7 16]
 [2 5 16]
 [5 10 8]
 [5 11 10]
])

; anti-clockwise
;(def to-faces [
; [10 11 12 13]
; [1 18 21 14 17 23]
; [18 19 20 21]
; [2 3 4 5]
; [0 23 17 16 7 6]
; [0 6 9 10 13 22]
; [1 22 13 12 19 18]
; [14 15 16 17]
; [11 10 9 8 5 4]
; [6 7 8 9]
; [0 22 1 23]
; [20 19 12 11 4 3]
; [14 15 2 3 20 21]
; [16 15 2 5 8 7]
;])

; clockwise
(def to-faces
  [[13 12 11 10]
 [23 17 14 21 18 1]
 [21 20 19 18]
 [5 4 3 2]
 [6 7 16 17 23 0]
 [22 13 10 9 6 0]
 [18 19 12 13 22 1]
 [17 16 15 14]
 [4 5 8 9 10 11]
 [9 8 7 6]
 [23 1 22 0]
 [3 4 11 12 19 20]
 [21 20 3 2 15 14]
 [7 8 5 2 15 16]])


; edge-centered cubic lattice logic
;
; V        V % 2
; ==============
; 0 0 0 ,  0 0 0
; 0 0 1 ,  0 0 1 *
; 0 0 2 ,  0 0 0
; --------------
; 0 1 0 ,  0 1 0 *
; 0 1 1 ,  0 1 1
; 0 1 2 ,  0 1 0 *
; --------------
; 0 2 0 ,  0 0 0
; 0 2 1 ,  0 0 1 *
; 0 2 2 ,  0 0 0
; ==============
; 1 0 0 ,  1 0 0 *
; 1 0 1 ,  1 0 1
; 1 0 2 ,  1 0 0 *
; --------------
; 1 1 0 ,  1 1 0
; 1 1 1 ,  1 1 1 *
; 1 1 2 ,  1 1 0
; --------------
; 1 2 0 ,  1 0 0 *
; 1 2 1 ,  1 0 1
; 1 2 2 ,  1 0 0 *
; ==============
; 2 0 0 ,  0 0 0
; 2 0 1 ,  0 0 1 *
; 2 0 2 ,  0 0 0
; --------------
; 2 1 0 ,  0 1 0 *
; 2 1 1 ,  0 1 1
; 2 1 2 ,  0 1 0 *
; --------------
; 2 2 0 ,  0 0 0
; 2 2 1 ,  0 0 1 *
; 2 2 2 ,  0 0 0

;
;
; ---------------------------------------------------------------------------
; The rhombick dodecahedron has octahedral symmetry. A regular octahedron
; has 24 rotational (or orientation-preserving) symmetries, and a symmetry
; order of 48 including transformations that combine a reflection and a
; rotation.

; We will be using chiral octahedral symmetry (24 symmetries, no reflections):
;
; • identity
; • 6 × rotation by 90°
; • 8 × rotation by 120°
; • 3 × rotation by 180° about a 4-fold axis
; • 6 × rotation by 180° about a 2-fold axis
;
; The axes for six 180° rotations around the 6 2-fold axes can be found by
; looking at the vectors formed by opposing pairs of cuboctahedron vertices.
; We know that there are no opposing vertices in the first 6 of co-verts,
; so :
; => (map vec3-normalize (take 6 co-verts)))
;
;
;
;(def ^:const symmetries
;  {
;  ;:identity      -
;
;   90             [[ 0  0  1]
;                   [-1  0  0]
;                   [ 1  0  0]
;                   [ 0 -1  0]
;                   [ 0  0 -1]
;                   [ 0  1  0]]
;
;   120            [[ 0.5773502691896258 -0.5773502691896258 -0.5773502691896258]
;                   [ 0.5773502691896258 -0.5773502691896258  0.5773502691896258]
;                   [-0.5773502691896258 -0.5773502691896258  0.5773502691896258]
;                   [-0.5773502691896258 -0.5773502691896258 -0.5773502691896258]
;                   [ 0.5773502691896258  0.5773502691896258 -0.5773502691896258]
;                   [ 0.5773502691896258  0.5773502691896258  0.5773502691896258]
;                   [-0.5773502691896258  0.5773502691896258  0.5773502691896258]
;                   [-0.5773502691896258  0.5773502691896258 -0.5773502691896258]]
;
;   180            [[0 0 1]
;                   [0 1 0]
;                   [1 0 0]
;                   [0.7071067811865475  -0.7071067811865475   0.0]
;                   [0.7071067811865475   0.0                  0.7071067811865475]
;                   [0.7071067811865475   0.7071067811865475   0.0]
;                   [0.0                  0.7071067811865475   0.7071067811865475]
;                   [0.0                  0.7071067811865475  -0.7071067811865475]
;                   [0.7071067811865475   0.0                 -0.7071067811865475]]
;   }
;)

(def ^:const symmetries-flattened
 [[0 [1 0 0]]
  [90 [0 0 1]]
  [90 [-1 0 0]]
  [90 [1 0 0]]
  [90 [0 -1 0]]
  [90 [0 0 -1]]
  [90 [0 1 0]]
  [120 [0.5773502691896258 -0.5773502691896258 -0.5773502691896258]]
  [120 [0.5773502691896258 -0.5773502691896258 0.5773502691896258]]
  [120 [-0.5773502691896258 -0.5773502691896258 0.5773502691896258]]
  [120 [-0.5773502691896258 -0.5773502691896258 -0.5773502691896258]]
  [120 [0.5773502691896258 0.5773502691896258 -0.5773502691896258]]
  [120 [0.5773502691896258 0.5773502691896258 0.5773502691896258]]
  [120 [-0.5773502691896258 0.5773502691896258 0.5773502691896258]]
  [120 [-0.5773502691896258 0.5773502691896258 -0.5773502691896258]]
  [180 [0 0 1]]
  [180 [0 1 0]]
  [180 [1 0 0]]
  [180 [0.7071067811865475 -0.7071067811865475 0.0]]
  [180 [0.7071067811865475 0.0 0.7071067811865475]]
  [180 [0.7071067811865475 0.7071067811865475 0.0]]
  [180 [0.0 0.7071067811865475 0.7071067811865475]]
  [180 [0.0 0.7071067811865475 -0.7071067811865475]]
  [180 [0.7071067811865475 0.0 -0.7071067811865475]]
  ])


(def ^:const rd-symmetry-face-idx-map [
  [ 0  1  2  3  4  5  6  7  8  9 10 11 ] ; 0 identity
  [ 8 10  0  1  5  9  2  4  6  7 11  3 ] ; 1
  [ 5  0  1 10  3  2 11  6  7  4  9  8 ] ; 2
  [ 1  2  5  4  9  0  7  8 11 10  3  6 ] ; 3
  [ 9  5  4  2  6  7  3 11 10  8  0  1 ] ; 4
  [ 2  3  6 11  7  4  8  9  0  5  1 10 ] ; 5
  [10 11  3  6  2  1  4  5  9  0  8  7 ] ; 6
  [ 5  4  7  6  8  9 11 10  1  0  2  3 ] ; 7
  [10  0  9  5  7  8  4  6  3 11  1  2 ] ; 8
  [ 7  9  5  0  2  4  1  3 11  6  8 10 ] ; 9
  [ 4  2  3  1 11  6 10  8  9  7  5  0 ] ; 10
  [ 3  6  4  7  5  2  9  0 10  1 11  8 ] ; 11
  [11  3  1  2  0 10  5  9  7  8  6  4 ] ; 12
  [ 9  8 10 11  1  0  3  2  4  5  7  6 ] ; 13
  [ 1 10 11  8  6  3  7  4  5  2  0  9 ] ; 14
  [ 6 11  8 10  9  7  0  5  2  4  3  1 ] ; 15
  [ 8  7  6  4  3 11  2  1  0 10  9  5 ] ; 16
  [ 2  5  0  9 10  1  8 11  6  3  4  7 ] ; 17
  [ 0  9  8  7 11 10  6  3  2  1  5  4 ] ; 18
  [ 3  1 10  0  8 11  9  7  4  6  2  5 ] ; 19
  [ 6  4  2  5  1  3  0 10  8 11  7  9 ] ; 20
  [ 7  6 11  3 10  8  1  0  5  9  4  2 ] ; 21
  [11  8  7  9  4  6  5  2  1  3 10  0 ] ; 22
  [ 4  7  9  8  0  5 10  1  3  2  6 11 ] ; 23
  ])


(def ^:const to-symmetry-face-idx-map [
  [ 0  1  2  3  4  5  6  7  8  9 10 11 12 13] ; 0 identity
  [ 9  6  0  3  1  4  5  2 13  7 10  8 11 12] ; 1
  [10 12  2  0 13  4  1  3  5  9  7  6 11  8] ; 2
  [ 3  6  2  7  5  8 11 10 13  9  0 12  1  4] ; 3
  [ 0  4 10  2 13  8  5  7 11  3  9  6  1 12] ; 4
  [ 2  4  7  3  5  6  1  9 11  0 10 12 13  8] ; 5
  [ 0 12  3  9  1  6 11  7  5 10  2  8 13  4] ; 6
  [ 2  5 10  7  8 11  6  9 12  3  0  1  4 13] ; 7
  [ 3  5  0  2  4 13  8 10 12  7  9 11  6  1] ; 8
  [ 9  1 10  0 12 13  4  2  8  3  7  5  6 11] ; 9
  [10 13  7  2  8  5  4  3  6  0  9  1 12 11] ; 10
  [ 3  1  7  9  6 11 12 10  8  0  2 13  4  5] ; 11
  [ 9 11  3  7  6  5  8  2  4 10  0 13 12  1] ; 12
  [10 11  0  9 12  1  6  3  4  7  2  5  8 13] ; 13
  [ 2 13  3  0  4  1 12  9  6 10  7 11  8  5] ; 14
  [ 7  5  9  3  6  1  4  0 12  2 10 13  8 11] ; 15
  [ 0 13  9 10 12 11  8  7  6  2  3  5  4  1] ; 16
  [ 7 11  2 10  8 13 12  0  4  9  3  1  6  5] ; 17
  [ 2  8  0 10 13 12 11  9  1  7  3  6  5  4] ; 18
  [ 7  8  3  2  5  4 13  0  1 10  9 12 11  6] ; 19
  [ 9 12  7 10 11  8 13  2  5  0  3  4  1  6] ; 20
  [10  8  9  7 11  6  5  3  1  2  0  4 13 12] ; 21
  [ 3  4  9  0  1 12 13 10 11  2  7  8  5  6] ; 22
  [ 7  6 10  9 11 12  1  0 13  3  2  4  5  8] ; 23
  ])


(def ^:const cube-symmetry-face-idx-map [
  [0 1 2 3 4 5] ; 0 identity
  [0 5 1 3 2 4] ; 1
  [2 1 3 5 4 0] ; 2
  [5 1 0 2 4 3] ; 3
  [4 0 2 1 3 5] ; 4
  [0 2 4 3 5 1] ; 5
  [1 3 2 4 0 5] ; 6
  [5 0 4 2 3 1] ; 7
  [4 5 0 1 2 3] ; 8
  [2 0 1 5 3 4] ; 9
  [4 2 3 1 5 0] ; 10
  [1 2 0 4 5 3] ; 11
  [5 3 1 2 0 4] ; 12
  [1 5 3 4 2 0] ; 13
  [2 3 4 5 0 1] ; 14
  [0 4 5 3 1 2] ; 15
  [3 4 2 0 1 5] ; 16
  [3 1 5 0 4 2] ; 17
  [3 5 4 0 2 1] ; 18
  [4 3 5 1 0 2] ; 19
  [3 2 1 0 5 4] ; 20
  [5 4 3 2 1 0] ; 21
  [2 4 0 5 1 3] ; 22
  [1 0 5 4 3 2] ; 23
])



(def ^:const bezier-anchor-offsets [
  (vec (map vec3-normalize [(co-verts 8) (rd-verts 12) (co-verts 2) (rd-verts 8)])) ; ok
  (vec (map vec3-normalize [(rd-verts 11) (co-verts 5) (rd-verts 13) (co-verts 11)])) ; ok
  (vec (map vec3-normalize [(co-verts 0) (rd-verts 12) (co-verts 6) (rd-verts 8)])) ; ok
  (vec (map vec3-normalize [(co-verts 10) (rd-verts 10) (co-verts 4) (rd-verts 9)])) ; ok
  (vec (map vec3-normalize [(co-verts 9) (rd-verts 9) (co-verts 3) (rd-verts 10)])) ; ok
  (vec (map vec3-normalize [(rd-verts 11) (co-verts 7) (rd-verts 13) (co-verts 1)])) ; ok
  (vec (map vec3-normalize [(co-verts 8) (rd-verts 12) (co-verts 2) (rd-verts 8)])) ; ok
  (vec (map vec3-normalize [(rd-verts 11) (co-verts 5) (rd-verts 13) (co-verts 11)])) ; ok
  (vec (map vec3-normalize [(co-verts 0) (rd-verts 12) (co-verts 6) (rd-verts 8)])) ; ok
  (vec (map vec3-normalize [(co-verts 10) (rd-verts 10) (co-verts 4) (rd-verts 9)])) ; ok
  (vec (map vec3-normalize [(co-verts 9) (rd-verts 9) (co-verts 3) (rd-verts 10)])) ; ok
  (vec (map vec3-normalize [(rd-verts 11) (co-verts 7) (rd-verts 13) (co-verts 1)])) ; ok

  (vec (map vec3-normalize [(co-verts 9) (rd-verts 9) (co-verts 3) (rd-verts 10)])) ; ok
  (vec (map vec3-normalize [(rd-verts 11) (co-verts 7) (rd-verts 13) (co-verts 1)])) ; ok
])


(def jigsaw-tolerance 0.02)

(defn apply-tolerance [verts]
  (vec (map #(vec3-sub % (vec3-scale (vec3-normalize %) (* jigsaw-tolerance 0.5)))
            verts)))

(apply-tolerance [[1.0 1.0 1.0] [1.0 0.0 0.0]])

(def topologies {
  :square {
    :num-faces 4
    :symmetry-face-map [[0 1 2 3]
                        [1 2 3 0]
                        [2 3 0 1]
                        [3 0 1 2]]
    :face-centers [[ 1  0  0]
                [ 0  1  0]
                [-1  0  0]
                [ 0 -1  0]]
    :op-face-idx [2 3 0 1]
    :id :square
    :aabb-radius 1.001
    :verts-2d [
              [-1 -1  0]
              [ 1 -1  0]
              [ 1  1  0]
              [-1  1  0]
               ]
    :verts [[ 1 -1 -0.5]
            [ 1 -1  0.5]
            [-1 -1  0.5]
            [-1 -1 -0.5]
            [ 1  1 -0.5]
            [ 1  1  0.5]
            [-1  1  0.5]
            [-1  1 -0.5]]
    :faces [
            [1 5 6 2]
            [2 6 7 3]
            [4 7 6 5]
            [4 0 3 7]
            [0 4 5 1]
            [0 1 2 3]
            ]


  }

  :hexagon {
    :num-faces 6
    :symmetry-face-map [[0 1 2 3 4 5]
                        [1 2 3 4 5 0]
                        [2 3 4 5 0 1]
                        [3 4 5 0 1 2]
                        [4 5 0 1 2 3]
                        [5 0 1 2 3 4]]
    :face-centers (apply-tolerance
               [ [1.0 0.0 0.0]
                 [0.5  -0.8660254037844 0.0]
                 [-0.5 -0.8660254037844 0.0]
                 [-1.0 0.0 0.0]
                 [-0.5 0.8660254037844 0.0]
                 [0.5  0.8660254037844 0.0]
                ])

    :face-centers-ideal ;(apply-tolerance
               [ [1.0 0.0 0.0]
                 [0.5  -0.8660254037844 0.0]
                 [-0.5 -0.8660254037844 0.0]
                 [-1.0 0.0 0.0]
                 [-0.5 0.8660254037844 0.0]
                 [0.5  0.8660254037844 0.0]
                ];)
    :op-face-idx [3 4 5 0 1 2]
    :id :hexagon
    :aabb-radius 1.0
    :verts-2d-ideal
            [[0.0 1.0 0.0]
             [0.866025 0.5 0.0]
             [0.866025 -0.5 0.0]
             [0.0 -1.0 0.0]
             [-0.866025 -0.5 0.0]
             [-0.866025 0.5 0.0]
            ]

    :verts-2d ;(apply-tolerance
            [[0.0 1.0 0.0]
             [0.866025 0.5 0.0]
             [0.866025 -0.5 0.0]
             [0.0 -1.0 0.0]
             [-0.866025 -0.5 0.0]
             [-0.866025 0.5 0.0]
            ]

    :faces-2d [
               [5 4 3 2 1 0]
               ]
    :verts ;(apply-tolerance
            [[0.0 1.0 -0.01]
             [0.866025 0.5 -0.01]
             [0.866025 -0.5 -0.01]
             [0.0 -1.0 -0.01]
             [-0.866025 -0.5 -0.01]
             [-0.866025 0.5 -0.01]
             [0.0 1.0 -0.125]
             [0.866025 0.5 -0.125]
             [0.866025 -0.5 -0.125]
             [0.0 -1.0 -0.125]
             [-0.866025 -0.5 -0.125]
             [-0.866025 0.5 -0.125]
            ];)
    :faces [
                  [5 4 3 2 1 0]
                  [1 2 8 7]
                  [0 1 7 6]
                  [0 6 11 5]
                  [4 5 11 10]
                  [3 4 10 9]
                  [2 3 9 8]
                  [6 7 8 9 10 11]
            ]
  }

  :cube {
    :num-faces 6
    :symmetry-face-map cube-symmetry-face-idx-map
    :face-centers [
                [ 0  0  1]
                [-1  0  0]
                [ 0 -1  0]
                [ 0  0 -1]
                [ 1  0  0]
                [ 0  1  0]]
    :op-face-idx [3 4 5 0 1 2]
    :id :cube
    :aabb-radius 1.001
    :verts [[ 1 -1 -1]
            [ 1 -1  1]
            [-1 -1  1]
            [-1 -1 -1]
            [ 1  1 -1]
            [ 1  1  1]
            [-1  1  1]
            [-1  1 -1]]
    :faces [
            [1 5 6 2]
            [2 6 7 3]
            [4 7 6 5]
            [4 0 3 7]
            [0 4 5 1]
            [0 1 2 3]
            ]}

  :hexagonal-prism {
    :num-faces 8
    :symmetry-face-map [
                        [0 1 2 3 4 5 6 7]
                        [1 2 3 4 5 0 6 7]
                        [2 3 4 5 0 1 6 7]
                        [3 4 5 0 1 2 6 7]
                        [4 5 0 1 2 3 6 7]
                        [5 0 1 2 3 4 6 7]
                        [0 5 4 3 2 1 7 6]
                        [5 4 3 2 1 0 7 6]
                        [4 3 2 1 0 5 7 6]
                        [3 2 1 0 5 4 7 6]
                        [2 1 0 5 4 3 7 6]
                        [1 0 5 4 3 2 7 6]
                        ]
    :face-centers [
                 [1.0 0.0 0.0]
                 [0.5  -0.8660254037844 0.0]
                 [-0.5 -0.8660254037844 0.0]
                 [-1.0 0.0 0.0]
                 [-0.5 0.8660254037844 0.0]
                 [0.5  0.8660254037844 0.0]
                 [0.0 0.0 0.5] ; top
                 [0.0 0.0 -0.5] ; bot
                   ]
    :op-face-idx [3 4 5 0 1 2 7 6]
    :id :hexagonal-prism
    :aabb-radius 1.001
    :verts [
             [0.0 1.0 0.5]
             [0.866025 0.5 0.5]
             [0.866025 -0.5 0.5]
             [0.0 -1.0 0.5]
             [-0.866025 -0.5 0.5]
             [-0.866025 0.5 0.5]
             [0.0 1.0 -0.5]
             [0.866025 0.5 -0.5]
             [0.866025 -0.5 -0.5]
             [0.0 -1.0 -0.5]
             [-0.866025 -0.5 -0.5]
             [-0.866025 0.5 -0.5]
            ]
    :faces [
                  [5 4 3 2 1 0]
                  [1 2 8 7]
                  [0 1 7 6]
                  [0 6 11 5]
                  [4 5 11 10]
                  [3 4 10 9]
                  [2 3 9 8]
                  [6 7 8 9 10 11]
            ]}

  :rhombic-dodecahedron {
    :num-faces 12
    :symmetry-face-map rd-symmetry-face-idx-map
    :face-centers rd-neighbour-offsets
    :op-face-idx [6 7 8 9 10 11 0 1 2 3 4 5]
    :id :rhombic-dodecahedron
    :aabb-radius 2.0
    :verts rd-verts
    :faces rd-faces}

  :truncated-octahedron {
    :num-faces 14
    :symmetry-face-map to-symmetry-face-idx-map
    :face-centers to-face-centers
    :op-face-idx [7 8 9 10 11 12 13 0 1 2 3 4 5 6]
    :id :truncated-octahedron
    :aabb-radius 2.001
    :verts to-verts
    :faces to-faces}
  })


(def next-topology {:square :hexagon
                    :hexagon :cube
                    :cube :hexagonal-prism
                    :hexagonal-prism :rhombic-dodecahedron
                    :rhombic-dodecahedron :truncated-octahedron
                    :truncated-octahedron :square
                    })

(def current-topology (atom (topologies :cube)))


(def debug-symmetry-idx (atom 0))

(defn debug-next-symmetry []
  (reset! debug-symmetry-idx (mod (inc @debug-symmetry-idx) (count (@current-topology :symmetry-face-map)))))


(defn get-code-symmetry [^String code sym-idx]
  ;(println "code:" code "sym-idx:" sym-idx)
  ;(apply str (map #(.charAt code ((rd-symmetry-face-idx-map sym-idx) %))
  (apply str (map #(.charAt code (((@current-topology :symmetry-face-map) sym-idx) %))
                  ;(range (count (@current-topology :num-faces))))))
                  (range (count code)))))


(defn get-code-symmetries [code]
  (vec (distinct (map #(get-code-symmetry code %)
                      (range (count (@current-topology :symmetry-face-map)))))))



; obsolete
; 2d fourfold chiral symmetry hack - tiles can only rotate mutiples of 90°
; around the Z axis. Symmetry Indices: 0 1 5 15
;(defn get-code-symmetries-2d-fourfold [code]
;  (vec (distinct (map #(get-code-symmetry code %) [0 1 5 15]))))
;
; identity - eg for non-rotatable wang tiles
;(defn get-code-symmetries-identity-only [code]
;  code)

(defn face-idxs-to-verts [face-idxs]
  (vec (map #((@current-topology :verts) %) face-idxs)))

(defn get-connected-idxs [code]
  (filter #(not= nil %)
          (map #(if (and (not= %2 \-) (not= %2 \0) ) %1 nil)
               (range (count code)) code)))


(defn get-non-connected-idxs [code]
  (filter #(not= nil %)
          (map #(if (or (= %2 \-) (= %2 \0) ) %1 nil)
               (range (count code))
               code)))


(defn get-angle-for-face-idxs [[idx1 idx2]]
  (vec3-angle-between (vec3-normalize ((@current-topology :face-centers) idx1))
                      (vec3-normalize ((@current-topology :face-centers) idx2))))

(defn get-tilecode-angles [code]
  (->> (map-indexed #(vec [%1 %2]) code)
       (filter #(not= (%1 1) \-))
       (map first)
       ((fn [v] (vec (combinations v 2))))
       (map get-angle-for-face-idxs)
       (map int)))


(defn get-tilecode-angle-ids [code]
  (vec (sort (get-tilecode-angles code))))


(defn tileset-valid? [tileset topology]
  (if (or (nil? tileset) (= (count tileset) 0))
    false
    (= (count (filter #(= (topology :num-faces) (count %)) tileset))
       (count tileset))))

