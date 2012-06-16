(ns rhombrick.camera
  (:use [quil.core]
        [rhombrick.vector]))

(def camera-data {
                  :pos [0 0 -1]
                  :lookat [0 0 0]
                  :fov 60})

