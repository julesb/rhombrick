(ns rhombrick.offline
  (:use [rhombrick.tiling]
        [rhombrick.tilebase]
        [clojure.math.combinatorics] 
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


(defn get-tilecode-digit-permutations [conn-digits]
  (let [ndigits (count conn-digits)
        nzeros (- 12 ndigits)
        zeros (repeat nzeros \-)
        digits (concat zeros conn-digits)]
    (permutations digits)))


(defn generate-normalized-tilecode-permutations [ncons]
  (->> (selections #{\1 \2 \3 \4 \5 \6 \7 \a \b \c \d \A \B \C \D} ncons)
       (mapcat get-tilecode-digit-permutations)
       (map #(apply str %))
       (into #{})
       (filter tilecode-is-normalized?)
       ;(filter is-tilecode-fully-self-compatible-and-normalized?)
  ))

(defn generate-normalized-tilecode-permutations2 [start end]
  (->> (range start (+ start end 1))
       (map number-to-tilecode)
       (filter is-tilecode-fully-self-compatible-and-normalized?)))


(defn generate-normalized-tilecode-set [start end]
  (->> (range start (+ end 1))
       (map number-to-tilecode)
       (filter tilecode-is-normalized?)))


(defn generate-normalized-tilecode-set2 [start end]
  (doseq [n (range start (+ end 1))]
    (let [code (number-to-tilecode n)]
      (when (tilecode-is-normalized? code)
        (println code))
      (when (zero? (mod n 0x1000000))
        (println "CP:" code ))
    )))

; batch runs etc below here


(defn make-params-for-seeds [tileset]
  (map #(make-params :tileset tileset :seed %) tileset))


(defn make-tiling [ts]
  (if (and (= (ts :run-status) :runnable)
           (< (ts :iters) ((ts :params) :max-iters )))
    (do
      ;(println "BEFORE ITER:" ts )
      (recur (make-backtracking-tiling-iteration4 ts)))
    ts
  ))



(defn iterate-tiler [_tiles tileset-expanded params]
  (if (and (= @tiler-run-state :running)
           (< (count _tiles) (params :max-tiles))
           (< @tiler-iterations (params :max-iters))
           (> (count (get-empty-positions _tiles (params :max-radius))) 0))
    (do
      (swap! tiler-iterations inc)
      (recur (ordered-map (make-backtracking-tiling-iteration3 _tiles tileset-expanded))
             tileset-expanded
             params))
    _tiles))


(defn evaluate-tileset [params ]
  (reset! assemblage-max-radius (params :max-radius))
  (reset! adhd (params :adhd))
  (reset! autism (params :autism))
  (init-tiler (params :tileset))
  (init-dead-loci!)
  (let [tileset-expanded (expand-tiles-preserving-symmetry (params :tileset))
        seed-tile (ordered-map {[0 0 0] (params :seed)})]
    (reset! tiler-run-state :running)
    (let [tiling (iterate-tiler seed-tile tileset-expanded params)
          tileset (params :tileset)
          seed (params :seed)
          tilecount (count tiling)
          iters-done @tiler-iterations
          tileset-number (tileset-to-number (params :tileset))
          ]
      {:params params
       :result {:tiling tiling
                :tilecount tilecount
                :iters-done iters-done}
       }
       )))


;(defn print-results [results]
;  (pp (map #(% :result) results)))

(defn evaluate-tileset-best-of [params]
  (->> (map (fn [_] (evaluate-tileset params)) (range (params :best-of)))
       (sort-by #((% :result) :tilecount))
       reverse
       first
    ))


(defn evaluate-tileset-all-seeds [params]
  (->> (make-params-for-seeds (params :tileset))
    (map evaluate-tileset)
    ;(map #(evaluate-tileset-best-of %))
  ))



(defn test-dbwrite []
  (let [param (make-params :tileset (get-random-tileset))]
    (print "evaluate-tileset:" (pr-str (param :tileset)) "x" (param :max-iters) "|")
    (let [result (evaluate-tileset param)]
      (println "result: tilecount:" ((result :result) :tilecount)
               "iters-done:" ((result :result) :iters-done))
      (save-tiling-result result)
      ;result
      )))


(defn test-dbwrite2 []
  (let [param (make-params :tileset (get-random-tileset))]
    (println "evaluate-tileset:" (pr-str (param :tileset)) "x" (param :max-iters) "|")
    (let [results (evaluate-tileset-all-seeds param)]
      (doseq [result results]
        (println "result: seed:" ((result :params) :seed) 
                 "tilecount:" ((result :result) :tilecount)
                 "iters-done:" ((result :result) :iters-done))
        (save-tiling-result result))
      ;result
      )))






; (pp (evaluate-tileset-all-seeds testparam))


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




; => (time (count (filter is-tilecode-completely-compatible-and-normalized? 
;                         (map number-to-tilecode  (range 1000000)))))
;"Elapsed time: 259991.235 msecs"
;49071
;rhombrick.offline=> (* 0xffffffffffff (/ 49071 1000000))
;2762451716433710301/200000
;rhombrick.offline=> (long (* 0xffffffffffff (/ 49071 1000000)))
;13812258582168


; (map tilecode-to-binary-connection-number (generate-normalized-tilecode-permutations 2))
; 
