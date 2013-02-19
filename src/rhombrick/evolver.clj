(ns rhombrick.evolver
  (:use [rhombrick.tiling]
        [rhombrick.vector]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.facecode]
        [ordered.map]))

(defn new-population [size]
  (map (fn [_] (get-random-tileset)) (range size)))


(defn evaluate-tileset [tileset max-iters max-radius]
  (reset! assemblage-max-radius 4)
  (init-tiler tileset)
  (init-dead-loci)
  (reset! tiler-state :running)
  (run-backtracking-tiling-thread tileset)
)

(defn -main [& args]
  (let [population (new-population 5)
        max-iters 100
        max-radius 4]
    (doseq [tileset population max-iters max-radius]
      (evaluate-tileset tileset)
  )))


; The idea here is to run the tiler offline (without graphics), where it can
; iterate much faster. We can run batch tests and assign scores to things such
; as tilesets, tiler parameters (autism, adhd), and the tiler algorithm
; performance. 
; It will be interesting to consider the tileset as a genome and try and 
; evolve tilesets with desirable properties.
; 
;
; Metrics for evaluating a tileset
; These metrics are dependent on tiler parameters and the specifics of the
; tiling algorithm.
;
; - Difficulty. 0 means a tile was layed on every iteration, with no
;   backtracking, 1 means a tiling was not achieved:
;
;     1 - num-tiles / iters
;
;   For a tileset to be difficult is not always a bad thing. Some interesting
;   tilesets are difficult to tile. Many tilesets have a difficulty of 1, or
;   close enough to it to consider them useless.
;
; - Complexity: the number of tiles which are actually able to be used in a
;   tiling, in proportion to the total number of tiles in the set:
;   
;     num-tiles-used / num-tiles-in-tileset.
;   
;   Some tileset are k-morphic, meaning they have a number of distinct modes,
;   as opposed to others which have a single mode only. This presents a
;   difficulty because each mode of a k-morphic tileset will most likely
;   generate different metrics. Maybe we could do multiple runs with each
;   tileset and put the generated metrics for each run into bins. This may
;   allow programmatically determining the number of modes for a tileset.
;   
;   
