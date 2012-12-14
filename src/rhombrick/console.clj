(ns rhombrick.console
  (:use [quil.core]))

(def line-buffer (atom ()))
(def max-lines 50)


(defn init-console []
  (reset! line-buffer ()))


(defn writeline [l]
  (dosync
    (swap! line-buffer conj l)
    (when (> (count @line-buffer) max-lines)
      (swap! line-buffer #(take max-lines %)))))


(defn draw-buffer [x y]
  (let [line-space 15
        lines (vec @line-buffer)]
    (doseq [i (range (count lines))]
      (let [idx (- (dec (count lines)) i)]
        (text (lines idx) x (+ y (* i line-space)))))))
