(ns rhombrick.tiling-test
  (:use [clojure.test]
        [rhombrick.tiling]))

(deftest tiling-test
  (is (= true (facecodes-directly-compatible? "------------" "------------"))))
