(ns rhombrick.camera
  (:use [quil.core]
        [rhombrick.vector]))

(import processing.core.PMatrix3D)
(import processing.core.PVector)


(def camera-pos (atom [0 0 0]))
(def camera-lookat (atom [0 0 0]))
(def camera-fov (atom 60.0))
(def camera-aspect-ratio (atom 1.0))
(def camera-near-clip (atom 10))
(def camera-far-clip (atom 1000))
(def camera-mode (atom 0))
(def camera-num-modes 3)



(def x-rot (atom 0.0))
(def y-rot (atom 0.0))
;(def d (atom 0.0))

(def cam (atom (new PMatrix3D)))


(defn get-camera-distance [p]
  (abs (vec3-length (vec3-sub @camera-pos p))))

(defn get-camera-dir []
  (let [x (.mult @cam (new PVector 1 0 0) (new PVector 0 0 0))
            y (.mult @cam (new PVector 0 1 0) (new PVector 0 0 0))
            d (.cross x y)
            dn (.normalize d)
            ds [(.x d) (.y d) (.z d)]]
    ds)) 


(defn get-camera-x-dir []
  (let [x (.mult @cam (new PVector 1 0 0) (new PVector 0 0 0))
            y (.mult @cam (new PVector 0 1 0) (new PVector 0 0 0))
            d (.cross x y)
            dn (.normalize d)
            dx (.cross d y)]
    (.normalize dx)
    [(.x dx) (.y dx) (.z dx)] ))


(defn do-camera-transform [pos x-rot y-rot]
  (do
    (let [cam-new @cam]
      (.rotateX cam-new x-rot)
      (.rotateY cam-new y-rot)
      (let [x (.mult cam-new (new PVector 1 0 0) (new PVector 0 0 0))
            y (.mult cam-new (new PVector 0 1 0) (new PVector 0 0 0))
            d (.cross x y)
            dn (.normalize d)
            ds [(* (.x d) 100) (* (.y d) 100) (* (.z d) 100) ] 
            lookat (vec3-add ds pos)]
        (camera (@camera-pos 0) (@camera-pos 1) (@camera-pos 2)
                (lookat 0) (lookat 1) (lookat 2)
                (.x y) (.y y) (.z y))
        (reset! cam cam-new)
        ))))


(defn update-camera []
  (reset! camera-aspect-ratio (/ (width) (height)))
  (let [fov-rad (radians @camera-fov)
        camz  (/ (/ (height) 2.0) (tan (/ fov-rad 2.0)))
        near (/ camz 200.0)
        far (* camz 10.0) ]
    (reset! camera-near-clip near)
    (reset! camera-far-clip far)
    (println "fov:" @camera-fov
             "aspect:" @camera-aspect-ratio
             "near: " @camera-near-clip
             "far" @camera-far-clip)))
