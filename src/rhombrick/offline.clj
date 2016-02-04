(ns rhombrick.offline
  (:use [rhombrick.tiling]
        [rhombrick.tilecode]
        ;[rhombrick.tilebase]
        [rhombrick.staticgeometry]
        [clojure.math.combinatorics] 
        [ordered.map]
        [clojure.java.io]))


(reset! current-topology (topologies :rhombic-dodecahedron))



(defn get-tilecode-digit-permutations [conn-digits]
  (let [ndigits (count conn-digits)
        nzeros (- (@current-topology :num-faces) ndigits)
        zeros (repeat nzeros \-)
        digits (concat zeros conn-digits)]
    (permutations digits)))


(defn generate-normalized-tilecode-permutations [ncons]
  (->> (selections #{\1 \2 \3 \4 \a \b \c \d \A \B \C \D} ncons)
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


; receive a params hashmap and return an array of params with each tile in
; the tileset set as a seed 
(defn make-params-for-seeds [param]
  (map #(assoc param :seed %) (param :tileset)))


(defn make-tiling [ts & best]
  (if (nil? best)
    (recur ts ts)
    (if (tiler-can-iterate? ts)
      (if (> (count (ts :tiles)) (count (best :tiles)))
        (recur (make-backtracking-tiling-iteration4 ts) ts)
        (recur (make-backtracking-tiling-iteration4 ts) best))
      (if (>= (count (ts :tiles)) (count (best :tiles)))
       ts
       best))))


(defn make-tiling-best-of-n [ts n]
  (->> (pmap (fn [_] (make-tiling ts)) (range n))
       (sort-by #(count (% :tiles)))
       (last)
    ))


(defn save-tiler-state [ts filename]
  (let [ts-save (assoc ts :dead #{})]
    (with-open [wrtr (writer filename :append true)]
        (.write wrtr (str (pr-str ts-save) "\n")))))


(defn load-tiler-state [filename]
  (with-open [rdr (reader filename)]
    (if-let [line (first (line-seq rdr))]
      (read-string line))))



(defn parse-state-lines [lines acc]
  (if-let [line (first lines)]
    (recur (rest lines) (conj acc (read-string line)))
    acc))


(defn load-all-tiler-states [filename]
  (with-open [rdr (reader filename)]
    (parse-state-lines (line-seq rdr) [])))



; this belongs with the client rather than here as it's very specific to 
; what we are generating on the day... 
(defn make-initial-states-file [filename n]
  (doseq [i (range n)]
    (let [states (->> (make-params :tileset (get-random-tileset)
                                   :max-iters 5000
                                   :max-radius 512
                                   :max-tiles 10000)
                      (make-params-for-seeds)
                      (map make-state))]
      (doseq [s states]
        (save-tiler-state s filename)))
  ))


(defn make-initial-states-file-with-tilesets [filename tilesets]
  (doseq [i (range (count tilesets))]
    (let [states (->> (make-params :tileset (tilesets i)
                                   :max-iters 5000
                                   :max-radius 8
                                   :max-tiles 1000)
                      (make-params-for-seeds)
                      (map make-state))]
      (doseq [s states]
        (save-tiler-state s filename)))
  ))

(defn make-initial-states-file-with-tilesets-noseeds [filename tilesets]
  (doseq [i (range (count tilesets))]
    (let [state (->> (make-params :tileset (tilesets i)
                                  :seed (first (tilesets i))
                                   :max-iters 5000
                                   :max-radius 8
                                   :max-tiles 1000)
                      ;(make-params-for-seeds)
                      (make-state))]
      ;(doseq [s states]
        (save-tiler-state state filename))
      ;)
  ))

;(defn print-ts [ts]
;  (clojure.pprint/pprint {:params (ts :params)
;                          :solved (ts :solved)
;                          :tiles (count (ts :tiles))
;                          :iters (ts :iters)
;                          :dead (count (ts :dead))
;                          :run-status (ts :run-status)
;                          }
;  ))

;(defn iterate-tiler [_tiles tileset-expanded params]
;  (if (and (= @tiler-run-state :running)
;           (< (count _tiles) (params :max-tiles))
;           (< @tiler-iterations (params :max-iters))
;           (> (count (get-empty-positions _tiles (params :max-radius))) 0))
;    (do
;      (swap! tiler-iterations inc)
;      (recur (ordered-map (make-backtracking-tiling-iteration3 _tiles tileset-expanded))
;             tileset-expanded
;             params))
;    _tiles))
;
;
;(defn evaluate-tileset [params ]
;  (reset! assemblage-max-radius (params :max-radius))
;  (reset! adhd (params :adhd))
;  (reset! autism (params :autism))
;  (init-tiler (params :tileset))
;  (init-dead-loci!)
;  (let [tileset-expanded (expand-tiles-preserving-symmetry (params :tileset))
;        seed-tile (ordered-map {[0 0 0] (params :seed)})]
;    (reset! tiler-run-state :running)
;    (let [tiling (iterate-tiler seed-tile tileset-expanded params)
;          tileset (params :tileset)
;          seed (params :seed)
;          tilecount (count tiling)
;          iters-done @tiler-iterations
;          tileset-number (tileset-to-number (params :tileset))
;          ]
;      {:params params
;       :result {:tiling tiling
;                :tilecount tilecount
;                :iters-done iters-done}
;       }
;       )))
;

;(defn print-results [results]
;  (pp (map #(% :result) results)))

;(defn evaluate-tileset-best-of [params]
;  (->> (map (fn [_] (evaluate-tileset params)) (range (params :best-of)))
;       (sort-by #((% :result) :tilecount))
;       reverse
;       first
;    ))
;
;
;(defn evaluate-tileset-all-seeds [params]
;  (->> (make-params-for-seeds (params :tileset))
;    (map evaluate-tileset)
;    ;(map #(evaluate-tileset-best-of %))
;  ))



;(defn test-dbwrite []
;  (let [param (make-params :tileset (get-random-tileset))]
;    (print "evaluate-tileset:" (pr-str (param :tileset)) "x" (param :max-iters) "|")
;    (let [result (evaluate-tileset param)]
;      (println "result: tilecount:" ((result :result) :tilecount)
;               "iters-done:" ((result :result) :iters-done))
;      (save-tiling-result result)
;      ;result
;      )))
;
;
;(defn test-dbwrite2 []
;  (let [param (make-params :tileset (get-random-tileset))]
;    (println "evaluate-tileset:" (pr-str (param :tileset)) "x" (param :max-iters) "|")
;    (let [results (evaluate-tileset-all-seeds param)]
;      (doseq [result results]
;        (println "result: seed:" ((result :params) :seed) 
;                 "tilecount:" ((result :result) :tilecount)
;                 "iters-done:" ((result :result) :iters-done))
;        (save-tiling-result result))
;      ;result
;      )))
;





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

; For hex topology, single tile tilesets:
; alphabet = 3 b B
; total number of prototiles = 4^6 = 4096
; exclude symmetries - 699 prototiles remaining
; exclude unbalanced tiles - 159 prototiles remaining




(defn enumerate-tilecodes [alphabet numfaces]
  (->> (selections alphabet numfaces)
        (map #(apply str %))
        (map normalize-tilecode)
        (into #{})
        (sort)
       )
  )

(defn make-prototiles [maxtilenum numfaces] 
   (->> (map number-to-tilecode  (range 1 maxtilenum))
      ;(map #(apply str (remove #{\1 \2 \4 \5 \6 \7 \8 \9 \a \A \c \C \d \D} %)))
      (map #(apply str (remove #{\1 \2 \4 \5 \6 \7 \8 \9 \0 \a \A \c \C \d \D \e \E \f \F} %)))
      (filter #(= (count %) numfaces))
      (map normalize-tilecode)
      (into #{})
      (sort)
      ))


(defn count-char [s c]
  (count (filter #(= c %) s)))


; 
(defn is-balanced? [code]
  (and
    (= (count-char code \a) (count-char code \A))
    (= (count-char code \b) (count-char code \B))
    (= (count-char code \c) (count-char code \C))
    (= (count-char code \d) (count-char code \D)))
  )

(defn is-tileset-balanced? [tileset]
  (let [fr (frequencies (apply str tileset)) ]
    (and
      (= (get fr \a 0) (get fr \A 0))
      (= (get fr \b 0) (get fr \B 0))
      (= (get fr \c 0) (get fr \C 0))
      (= (get fr \d 0) (get fr \D 0)))))


;(defn tiling-uses-all-tile-forms? [ts]
;  (let [tilecodes (map #(normalize-tilecode(val (ts :tiles))))
;        tileforms (into #{} tilecodes)
;        tileset (set (get-in ts [:params :tileset]))
;        ]
;    (= tileforms tileset)
;  ))

(defn make-prototiles-hex-13Cc-selfcompatible-balanced []
  (->> (make-prototiles 0xffffff 6)
       (filter is-tilecode-fully-self-compatible-and-normalized?)
       (filter is-balanced?)
       (map #(vec [%]))
       (into [])
       ))

;(def prototiles-hex-13Cc-selfcompatible-balanced (make-prototiles-hex-13Cc-selfcompatible-balanced))
;
;(make-initial-states-file-with-tilesets "hex-13Cc-selfcompatible-balanced.state"
;                                        prototiles-hex-13Cc-selfcompatible-balanced)

(defn make-hex-3Cc-two-tiles-tilesets []
  (let [ prototiles (make-prototiles 0xfffff 6) ]
    (->> (combinations prototiles 2)
         (filter is-tileset-balanced?)
         (map vec)
         vec)
))


(defn make-prototiles-rd-elementary []
  (->> (selections [\- \3] 12)
       (map #(apply str %))
       (filter #(not= % "------------"))
       (map normalize-tilecode)
       (into #{})
       (sort)
       ;(map vec)
       vec
       )
  )

(defn make-rd-two-tiles-tilesets []
  (let [prototiles (make-prototiles-rd-elementary)]
    (->> (combinations prototiles 2)
         (map vec)
         vec)))

;(def randtwotile-tilesets (into [] (map (fn [_] (make-random-twotile-tileset))
;                               (range 32768))))
;(make-initial-states-file-with-tilesets-noseeds "rd-twotile-random-32768.state"
;                                                randtwotile-tilesets)


;(make-initial-states-file-with-tilesets-noseeds "rd-elementary-two-tiles.state"
;                                                (make-rd-two-tiles-tilesets))


; java -jar rhombrick-survey-1.0.0-SNAPSHOT-standalone.jar hex-3Cc-two-tiles.state-2 hex-3Cc-two-tiles.tiling-2-00140000-00150000 150000 161597

;(def prototiles-hex-elementary (make-prototiles 0xffffff 6))

;(def two-tile-tilesets (make-hex-3Cc-two-tiles-tilesets))

;(make-initial-states-file-with-tilesets-noseeds "hex-3Bb-two-tiles-balanced.state"
;                                                (make-hex-3Cc-two-tiles-tilesets))

;prototiles-hex-13Cc-selfcompatible-balanced
