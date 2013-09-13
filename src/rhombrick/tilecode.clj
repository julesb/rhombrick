(ns rhombrick.tilecode
)




(def facecode-compatible #{
  [\- \-]
  [\0 \0]
  [\1 \1]
  [\2 \2]
  [\3 \3]
  [\4 \4]
  [\5 \5]
  [\6 \6]
  [\7 \7]
  ;[\8 \8]
  ;[\9 \9]
  [\a \A]
  [\b \B]
  [\c \C]
  [\d \D]
  ;[\e \E]
  ;[\f \F]
  })

(def facecode-compatible-map {\. \-
                              \- \-
                              \0 \0
                              \1 \1
                              \2 \2
                              \3 \3
                              \4 \4
                              \5 \5
                              \6 \6
                              \7 \7
                              ;\8 \8
                              ;\9 \9
                              \a \A \A \a
                              \b \B \B \b
                              \c \C \C \c
                              \d \D \D \d
                              ;\e \E \E \e
                              ;\f \F \F \f
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
  \5 \5 \5 \5 
  \6 \6
  \7
  \a \a \a \a \a \a \a \a
  \A \A \A \A \A \A \A \A
  \b \b \b \b
  \B \B \B \B
  \c \c
  \C \C
  \d
  \D])


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
      (recur num-tiles [(make-random-tilecode)])
    (>= (count tileset) num-tiles)
      tileset
    :else
      (let [new-tile (make-random-tilecode)
            new-tile-sites (into #{} (map facecode-compatible-map
                                          (filter #(not= \- %) new-tile)))
            tileset-sites (into #{} (filter #(not= \- %)
                                            (apply str tileset)))
            compatible? (some new-tile-sites tileset-sites) ]
        (if compatible?
          (recur num-tiles (conj tileset new-tile))
          (recur num-tiles tileset)))))


(defn get-random-tileset []
  (let [max-tiles 4]
    (make-random-tileset (+ 1 (rand-int max-tiles)) [])))



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
(defn facecodes-directly-compatible? [outercode innercode]
  (= 12 
     (count (filter #(true? %)
                    (map #(face-digit-compatible? %1 %2) 
                         innercode outercode)))))


(defn make-minimal-tilecode-to-fit [outercode]
  (apply str (map facecode-compatible-map outercode)))


(defn make-random-tilecode-to-fit [outercode]
  (apply str (map #(if (not= % \.)
                     (facecode-compatible-map %) 
                     (rand-nth random-tilecode-distribution))
                  outercode)))


