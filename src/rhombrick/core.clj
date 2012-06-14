(ns rhombrick.core
  (:use [quil.core]
        [rhombrick.facecode]
        [rhombrick.staticgeometry]
        [rhombrick.tiling]
        [rhombrick.tiling-render]))

(def mouse-position (atom [0 0]))
(def view-scale (atom 1.0))
(def model-scale (atom 75))
(def frame (atom 0))




; _______________________________________________________________________
; Rendering and events 


(defn setup []
    (smooth)
    (sphere-detail 4)
    ;(display-filter :blur 10)
    (text-font (load-font "FreeMono-16.vlw"))
    (set-state! :mouse-position (atom [0 0]))
    (build-normalised-facecode-set)
    ;(make-cubic-tiling 10 10 10)
    ;(reset! tiles {})
    (init-tiler)
    ;(init-todo)
 
    )
    ;(doseq [code @normalised-facecodes]
    ; (println code))
    ;(println "total" (count @normalised-facecodes) "facecodes"))

; _______________________________________________________________________

(defn draw-info []
  (fill 255 255 255 255)
  (text (str "fps: " (current-frame-rate)) 10 40))

; _______________________________________________________________________


(defn draw-xyplane []
  (quad -1 -1
         1 -1
         1  1
        -1  1))

; _______________________________________________________________________

(defn draw []
  (make-tiling-iteration)
  (auto-seed-todo)
  (if (> (count @tiles) max-tiles)
    (init-tiler))
  (swap! frame + 1)
  (background 0 0 0)
  (lights)    
  ;(light-specular 255 255 255)
  ;(stroke 0 0 0 250)
  ;(stroke-weight 1)
  (let [[mx my] @(state :mouse-position)] 
    (push-matrix)
    (translate [700 400 0])
    (scale @model-scale)
    (rotate-x (* (- my 400) -0.01))
    (rotate-y (* (- mx 700) 0.01))
    (rotate-x (* @frame 0.00351471))
    (rotate-y (* @frame 0.0035236))
    (rotate-z (* @frame 0.0035123))

    (stroke 255 255 255 192)
    (stroke-weight 1)
    (draw-tiling)
    (stroke-weight 1)
    (draw-todo-head)

    (pop-matrix)
    
    ;(no-fill)
    ;(stroke-weight 1)
    ;(stroke 255 255 255 255)
    ;(push-matrix)
    ;(translate [700 400 0])
    ;(scale @model-scale)
    ;(draw-normalized-facecodes @frame )
    ;(pop-matrix)


  )
;  (if (> (count @tiles) max-tiles)
;    (do
;      (delete-fully-connected-tiles)
;      (delete-nonconnected-tiles)
;      ;(dotimes [n 1]
;      ;  (if (= (mod @frame 1) 0)
;      ;    (delete-random-tile)))
;      ))

  (draw-info)
  )



; _______________________________________________________________________

(def key-command-map
  {
   \, #(swap! model-scale + 1.0)
   \. #(swap! model-scale - 1.0)
   ;\r #(make-cubic-tiling 10 10 10)
   \r #(init-tiler)
   \R #(do 
         (init-tiler)
         (random-tileset))
   \- #(swap! auto-delete-max-lonlieness - 1)
   \= #(swap! auto-delete-max-lonlieness + 1)
   })

; _______________________________________________________________________

(defn key-typed []
  (let [keychar (raw-key)]
    (if (contains? key-command-map keychar)
      ((key-command-map keychar)))))

; _______________________________________________________________________

(defn mouse-moved []
  (let [x (mouse-x) y (mouse-y)]
    (reset! (state :mouse-position) [x y])))

; _______________________________________________________________________
 
(defsketch rhombrick 
  :title "rhombrick"
  :setup setup 
  :draw draw
  :size [1400 800]
  :renderer :opengl 
  :key-typed key-typed
  :mouse-moved mouse-moved)


(sketch-start rhombrick)

