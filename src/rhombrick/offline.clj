(ns rhombrick.offline
  (:use [rhombrick.tiling]
        [rhombrick.tilecode]
        [rhombrick.tilebase]
        [clojure.math.combinatorics] 
        [ordered.map]))





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
  (if (tiler-can-iterate? ts)
    (recur (make-backtracking-tiling-iteration4 ts))
    ts))


(defn make-tiling-best-of-n [ts n]
  (->> (pmap (fn [_] (make-tiling ts)) (range n))
       (sort-by #(count (% :tiles)))
       (last)
    ))


(defn print-ts [ts]
  (clojure.pprint/pprint {:params (ts :params)
                          :solved (ts :solved)
                          :tiles (count (ts :tiles))
                          :iters (ts :iters)
                          :dead (count (ts :dead))
                          :run-status (ts :run-status)
                          }
  ))

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
