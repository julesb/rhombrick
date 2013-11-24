(ns rhombrick.tilecode
  (:use [rhombrick.staticgeometry :as geom]
        [rhombrick.tileset-data])
  (:require [clojure.core.memoize :as m])
)




(def ^:const facecode-compatible #{
  [\- \-]
  [\0 \0]
  [\1 \1]
  [\2 \2]
  [\3 \3]
  [\4 \4]
  [\5 \5]
  [\6 \6]
  [\7 \7]
  [\a \A]
  [\b \B]
  [\c \C]
  [\d \D]
  })

(def ^:const facecode-compatible-map {
  \. \-
  \- \-
  \0 \0
  \1 \1
  \2 \2
  \3 \3
  \4 \4
  \5 \5
  \6 \6
  \7 \7
  \a \A \A \a
  \b \B \B \b
  \c \C \C \c
  \d \D \D \d
  })




; NOTE: the tile renderer currently expects face digits no higher than 6 or D 
; for simplicity, even though the tiler will handle any hex digit.
(def ^:const random-tilecode-distribution [
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \- \- \- \- \- \- \- \- \- \- \- \- \- \- \- \-
  \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1
  \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1
  \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1
  \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1 \1
  \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2
  \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2 \2
  \3 \3 \3 \3 \3 \3 \3 \3 \3 \3 \3 \3 \3 \3 \3 \3
  \4 \4 \4 \4 \4 \4 \4 \4
  ;\5 \5 \5 \5 
  ;\6 \6
  ;\7
  \a \a \a \a \a \a \a \a
  \A \A \A \A \A \A \A \A
  \b \b \b \b
  \B \B \B \B
  \c \c
  \C \C
  \d
  \D])






(defn get-num-connected [code]
  (count (filter #(and  (not= \- %) (not= \0 %) ) code) ))


; compares single digits of two facecodes, using the
; compatibility table 
(defn face-digit-compatible? [inner outer]
  (or (contains? facecode-compatible [inner outer])
      (contains? facecode-compatible [outer inner])
      (= outer \.)))


(defn face-digit-like-compatible? [d]
  (not (contains? #{\a \b \c \d \e \f \A \B \C \D \E \F} d)))


; determine if faces are compatible without rotation
(defn tilecodes-directly-compatible-fn [outercode innercode]
  (= 12 
     (count (filter #(true? %)
                    (map #(face-digit-compatible? %1 %2) 
                         innercode outercode)))))

(def tilecodes-directly-compatible? (memoize tilecodes-directly-compatible-fn))
;(def tilecodes-directly-compatible-m? (m/lru tilecodes-directly-compatible?
;                                             :lru/threshold 32))

(defn make-minimal-tilecode-to-fit [outercode]
  (apply str (map facecode-compatible-map outercode)))


(defn make-random-tilecode-to-fit [outercode]
  (apply str (map #(if (not= % \.)
                     (facecode-compatible-map %) 
                     (rand-nth random-tilecode-distribution))
                  outercode)))


(defn get-neighbour-abutting-face2 [neighbourhood face-idx]
  (let [op-face-idx (geom/connecting-faces face-idx)
        nb-code (neighbourhood face-idx)]
    ;(println "nb-code:"nb-code "op-face-idx:" op-face-idx "neighbourhood:" neighbourhood)
    (if (nil? nb-code) \. (nth nb-code op-face-idx))))


(defn get-outer-facecode2 [neighbourhood]
  (apply str (map #(get-neighbour-abutting-face2 neighbourhood %) (range 12))))

;(def get-outer-facecode2-m (m/lru get-outer-facecode2-fn
;                                :lru/threshold 65536))


; generate all unique rotations of tiles in tileset 
(defn expand-tiles-preserving-symmetry [tiles]
  (set (flatten (map #(get-code-symmetries %) tiles))))
  ;(set (flatten (map #(get-code-symmetries-identity-only %) tiles))))
  ;(set (flatten (map #(get-code-symmetries-2d-fourfold %) tiles))))




(defn find-candidates [neighbourhood tileset dead]
  (let [outercode (get-outer-facecode2 neighbourhood)]
    (if (contains? dead outercode)
      ()
      (filter #(tilecodes-directly-compatible? outercode %)
              tileset))))


(defn choose-tilecode [neighbourhood tileset dead]
  (let [candidates (find-candidates neighbourhood tileset dead)]
    (if (seq candidates)
      (rand-nth candidates)
      nil)))


(defn make-random-tilecode []
  (let [code (apply str (map (fn [_] (rand-nth random-tilecode-distribution))
                             (range 12)))]
    (if (= code "------------")
      (make-random-tilecode) 
      code)))


; Builds a random tileset of specified length. As the set is built, a tile
; must must have least one tilecode digit compatible with existing digits in
; the set. This ensures that each tile added to the set is compatible with at
; least one other tile.
(defn make-random-tileset [num-tiles tileset]
  (cond
    (= (count tileset) 0)
      ;(recur num-tiles [(rand-nth self-compatible-1-2-3)])
      (recur num-tiles [(make-random-tilecode)])
    (>= (count tileset) num-tiles)
      tileset
    :else
      (let [new-tile (make-random-tilecode)
      ;(let [new-tile (rand-nth self-compatible-1-2-3)
            new-tile-sites (into #{} (map facecode-compatible-map
                                          (filter #(not= \- %) new-tile)))
            tileset-sites (into #{} (filter #(not= \- %)
                                            (apply str tileset)))
            compatible? (some new-tile-sites tileset-sites) ]
        (if compatible?
          (recur num-tiles (conj tileset new-tile))
          (recur num-tiles tileset)))))


(defn get-random-tileset []
  [(rand-nth (take 128 self-compatible-1-2-3))
   (rand-nth self-compatible-1-2-3)
   (rand-nth self-compatible-1-2-3)
;   (rand-nth self-compatible-1-2-3)
   ]
  )



(defn get-random-tileset-1 []
  (let [max-tiles 3]
    (make-random-tileset (+ 1 (rand-int max-tiles)) [])))



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


(defn tilecode-to-hex-number [code]
  (->> code
       (map tilecode-to-number-map)
       (apply str)))


(defn tilecode-to-hex-string [code]
  (apply str "0x" (map #(if (= \- %) \0 %) code)))


(defn normalize-tilecode [code]
  (->> (expand-tiles-preserving-symmetry [code])
       (map tilecode-to-number)
       sort
       first
       number-to-tilecode))


(defn tilecode-is-normalized? [code]
  (= (tilecode-to-number code)
     (tilecode-to-number (normalize-tilecode code))))


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


(defn tileset-to-hex-number [tileset]
  (->> tileset
    normalize-tileset
    (map tilecode-to-hex-number)
    (apply str)))

; These functions are to do with being able to quickly ignore tiles and
; tilesets which are not able to create any sort of tiling

(defn is-digit-connectable? [d]
  (contains? #{\1 \2 \3 \4 \5 \6 \7 \a \b \c \d \A \B \C \D} d))


(defn tilecode-to-binary-connection-number [code]
  (let [bits (apply str (map #(if (is-digit-connectable? %) \1 \0 ) code))]
    (Integer/parseInt bits 2)))


(defn get-self-compatible-digits [code]
  (->> code
       (filter is-digit-connectable?)
       (filter #(some #{(facecode-compatible-map %)} code))))


(defn is-tilecode-fully-self-compatible? [code]
  (=
    (count (filter is-digit-connectable? code))
    (count (get-self-compatible-digits code))))


(defn is-tilecode-partly-self-compatible? [code]
  (> (count (get-self-compatible-digits code)) 0))


(defn is-tilecode-fully-self-compatible-and-normalized? [code]
  (and (tilecode-is-normalized? code) 
    (is-tilecode-fully-self-compatible? code)))

; _______________________________________________________________________



(defn spec-to-fn-part [spec]
  (let [[fullname zeros nonzeros] spec]
    (str "C" (count zeros) "Z" nonzeros)
  ))


(defn compress-ts-filename [ucname]
  (let [[fullname tileset-num seed] (re-matches #"^(.*)_S(.*)\.png$" ucname)
        tile-nums (map #(apply str %) (partition 12 tileset-num))
        specs (map #(re-matches #"^(0*)(.*)$" %) tile-nums)
        fn-parts (map spec-to-fn-part specs)
        compressed-ts-num (apply str (map #(apply str %) fn-parts)) ]
      compressed-ts-num))


; map tilecodes between pfh's 2d codes and the current implementation
;
; (I think there is a problem with this. In 3 dimensions the 2 dimensional
; tiles may be flipped as well as rotated, whereas in 2d they can only rotate)
;
; hexagonal:
;   "AaAa--"  ->  "Aa-----A-a--"
;
;   hex index map:
;   [0 1 3 6 7 9]
;
; square:
; ...

(def pfh-tilecode-map-hex [0 1 3 6 7 9 ])

(defn convert-pfh-tilecode-hex [phf-code]
  (apply str (map #(if (some #{%} pfh-tilecode-map-hex)
                    (.charAt phf-code (.indexOf pfh-tilecode-map-hex %))
                    \-)
                  (range 12))))


; pfh code for CA rule 110: 
(def ca-rule-110 [
                  "a-aC-C"
                  "a-bC-D"
                  "b-aD-C"
                  "b-bD-D"
                  "cacAAA" ; 0
                  "dacBBB" ; 1
                  "cbcBBB" ; 1
                  "dbcBBB" ; 1
                  "cadAAA" ; 0
                  "dadBBB" ; 1
                  "cbdBBB" ; 1
                  "dbdAAA" ; 0
                  ])

