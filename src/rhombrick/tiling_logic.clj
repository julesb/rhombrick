(ns rhombrick.tiling-logic
  (:use [clojure.core.logic]
        [rhombrick.tiling]
        [rhombrick.vector]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.facecode])
  (:refer-clojure :exclude [==]))

; Can we simplify tile selection by using core.logic?
; How does this core.logic thing work anyway?

(run* [q]
  (membero q [1 2 3])
      )
