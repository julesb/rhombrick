(ns rhombrick.offline
  (:use [rhombrick.tiling]
        [rhombrick.vector]
        [rhombrick.staticgeometry :as geom]
        [rhombrick.facecode]
        [ordered.map]))


(def tilecode-to-number-map
  {\- \0
   \0 \0
   \1 \1
   \2 \2
   \3 \3
   \4 \4
   \5 \5
   \6 \6
   \7 \7
   \a \8
   \A \9
   \b \a
   \B \b
   \c \c
   \C \d
   \d \e
   \D \f})

(def number-to-tilecode-map
  {\0 \-
   \1 \1
   \2 \2
   \3 \3
   \4 \4
   \5 \5
   \6 \6
   \7 \7
   \8 \a
   \9 \A
   \a \b
   \b \B
   \c \c
   \d \C
   \e \d
   \f \D
   })


(defn hex-to-num [#^String s]
   (Long/parseLong (.substring s 2) 16))


(defn number-to-tilecode [n]
  (->> (format "%012x" n)
       (map number-to-tilecode-map)
       (apply str)))

(defn tilecode-to-number [code]
  (->> code
       (map tilecode-to-number-map)
       (apply str "0x")
       hex-to-num))


(defn tilecode-to-hex-string [code]
  (apply str "0x" (map #(if (= \- %) \0 %) code)))



(defn normalize-tilecode [code]
  (->> (expand-tiles-preserving-symmetry [code])
       (map tilecode-to-number)
       sort
       first
       number-to-tilecode))

(defn normalize-tileset [tileset]
  (->> tileset
    (map normalize-tilecode)
    (map tilecode-to-number)
    sort
    (map number-to-tilecode)
    vec))


(defn tileset-to-number [tileset]
  (->> tileset
    normalize-tileset
    (map tilecode-to-number)
    (map #(format "%015d" %))
    (apply str)
    java.math.BigInteger.))

(def test-tiler-params {:tileset ["111111111111"]
                        :iters 1000
                        :radius 2
                        :adhd 1.5
                        :autism 1.5
                        })


(defn iterate-tiler [_tiles tileset-expanded params]
  (if (and (= @tiler-state :running)
           (< (count _tiles) @max-tiles)
           (< @tiler-iterations (params :iters))
           (> (count (get-empty-positions _tiles)) 0))
    (do
      (swap! tiler-iterations inc)
      (recur (ordered-map (make-backtracking-tiling-iteration3 _tiles tileset-expanded))
             tileset-expanded
             params))
    _tiles))


(defn evaluate-tileset [params ]
  (reset! assemblage-max-radius (params :radius))
  (reset! adhd (params :adhd))
  (reset! autism (params :autism))
  (init-tiler (params :tileset))
  (init-dead-loci!)
  (let [tileset-expanded (expand-tiles-preserving-symmetry (params :tileset))
        seed-tile (ordered-map {[0 0 0] (first (params :tileset))})]
    (println "tileset exp:" tileset-expanded)
    (reset! tiler-state :running)
    (let [tiling (iterate-tiler seed-tile tileset-expanded params)
          tileset (params :tileset)
          seed (first (params :tileset))
          tilecount (count tiling)
          iters @tiler-iterations ]
      {:tiling tiling
       :tileset tileset
       :seed seed
       :tilecount tilecount
       :iters iters
       }
      )))





(defn evaluate-tileset-seeds [params]
  ; for each tile in tileset:
  ;   generate tiling using tile as initial seed
  ; write result to database

  )

;; A tiling run is the result of generating a tiling using each tile in the
;; tileset as the initial seed tile 
;
;{:tileset ["111---------" "---1-2------"]
;  ; tilings should contain at least one entry for each tile in the tileset
;  :tilings [{:seed "111---------"
;             :tiles <ordered-set>
;             }
;            {:seed "---1-2------"
;             :tiles <ordered-set>
;             }
;            ]
;  :iterations 1000
;  :max-radius 4
;  :adhd 1.5
;  :autism 1.5
;  :timestamp "20130824"
;  
; }
;
;
; 1 = 1
; 2 = 13
; 3 = 55
; 4 = 135
; 5 = 249
; 6 = 429
; 7 = 683
; 8 = 1055
;
(defn sphere_vol [r]
  ( * (/ 4.0 3.0) Math/PI (Math/pow r 3.0)))

; approximate num of RD volumes in radius r
; (- (/ (sphere_vol r) 2.0) 1)

