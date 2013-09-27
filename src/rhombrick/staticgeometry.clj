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

(def ^:const rd-faces [
               [0 10 1 11]
               [5 8 1 10]
               [4 13 5 10]
               [6 8 5 13]
               [7 13 4 12]
               [0 12 4 10]
               [0 11 3 12]
               [1 8 2 11]
               [6 9 2 8]
               [7 9 6 13]
               [7 12 3 9]
               [3 11 2 9]
])

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


(def ^:const rd-neighbour-offsets-orig [
                       [ 1 -1  0]
                       [ 1  0  1]
                       [ 1  1  0]
                       [ 0  1  1]
                       [ 0  1 -1]
                       [ 1  0 -1]
                       [ 0 -1 -1]
                       [ 0 -1  1]
                       [-1  0  1]
                       [-1  1  0]
                       [-1  0 -1]
                       [-1 -1  0]])

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

(def ^:const co-verts-orig [
               [ 1 -1  0] ;  0 red
               [ 1  0  1] ;  1 orange
               [ 1  1  0] ;  2 yellow
               [ 0  1  1] ;  3 green
               [ 0  1 -1] ;  4 blue
               [ 1  0 -1] ;  5 purple
               [ 0 -1 -1] ;  9 dark green
               [ 0 -1  1] ; 10 dark blue
               [-1  0  1] ; 11 dark purple
               [-1  1  0] ;  6 dark red
               [-1  0 -1] ;  7 dark orange
               [-1 -1  0] ;  8 dark yellow
               ])

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
                       [-1  0  1]

               ])


(def ^:const connecting-faces [6 7 8 9 10 11 0 1 2 3 4 5])


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
(def ^:const symmetries
  {
  ;:identity      -

   90             [[ 0  0  1]
                   [-1  0  0]
                   [ 1  0  0]
                   [ 0 -1  0]
                   [ 0  0 -1]
                   [ 0  1  0]]

   120            [[ 0.5773502691896258 -0.5773502691896258 -0.5773502691896258]
                   [ 0.5773502691896258 -0.5773502691896258  0.5773502691896258]
                   [-0.5773502691896258 -0.5773502691896258  0.5773502691896258]
                   [-0.5773502691896258 -0.5773502691896258 -0.5773502691896258]
                   [ 0.5773502691896258  0.5773502691896258 -0.5773502691896258]
                   [ 0.5773502691896258  0.5773502691896258  0.5773502691896258]
                   [-0.5773502691896258  0.5773502691896258  0.5773502691896258]
                   [-0.5773502691896258  0.5773502691896258 -0.5773502691896258]]

   180            [[0 0 1]
                   [0 1 0]
                   [1 0 0]
                   [0.7071067811865475  -0.7071067811865475   0.0]
                   [0.7071067811865475   0.0                  0.7071067811865475]
                   [0.7071067811865475   0.7071067811865475   0.0]
                   [0.0                  0.7071067811865475   0.7071067811865475]
                   [0.0                  0.7071067811865475  -0.7071067811865475]
                   [0.7071067811865475   0.0                 -0.7071067811865475]]
   }
)

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

(def ^:const symmetry-face-idx-map [
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


(defn get-code-symmetry [^String code sym-idx]
  (apply str (map #(.charAt code ((symmetry-face-idx-map sym-idx) %))
                  (range (count code)))))


(defn get-code-symmetries [code]
  (vec (distinct (map #(get-code-symmetry code %)
                      (range (count symmetry-face-idx-map))))))


(defn get-angle-for-face-idxs [[idx1 idx2]]
  (vec3-angle-between (vec3-normalize (co-verts idx1))
                      (vec3-normalize (co-verts idx2))))


(defn get-connected-idxs [code]
  (filter #(not= nil %)
          (map #(if (and (not= %2 \-) (not= %2 \0) ) %1 nil)
               (range 12) code)))


(defn face-idxs-to-verts [face-idxs]
  (vec (map #(rd-verts %) face-idxs)))


(defn get-tilecode-angles [code]
  (->> (map-indexed #(vec [%1 %2]) code)
       (filter #(not= (%1 1) \-))
       (map first)
       ((fn [v] (vec (combinations v 2))))
       (map get-angle-for-face-idxs)
       (map int)))


(defn get-tilecode-angle-ids [code]
  (vec (sort (get-tilecode-angles code))))


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
])
