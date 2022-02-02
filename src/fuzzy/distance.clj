(ns fuzzy.distance)

(defn levenshtein
  "Implementation of the Wagner-Fischer algorithm to calculate the Levenshtein distance of
  2 finite sequences
  reference: https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm"
  [a b]
  (letfn [(next-row [last-row y]
            (reduce (fn [nr [x deletion]]
                      (let [x            (dec x)
                            sub-cost     (if (= (nth a x) (nth b y)) -1 0)
                            substitution (+ (last-row x) sub-cost)
                            insertion    (peek nr)]
                        (conj nr (inc (min deletion insertion substitution)))))
                    [(inc y)]
                    (next (map-indexed vector last-row))))]
    (peek (reduce next-row
                  (into [] (range (inc (count a))))
                  (range (count b))))))

(defn lcs
  "Find the longest common subsequence of 2 finite sequences
  reference: https://en.wikipedia.org/wiki/Longest_common_subsequence_problem"
  [a b]
  (letfn [(next-row [last-row y]
            (reduce (fn [nr [x deletion]]
                      (if (= (nth a (dec x)) (nth b y))
                        (conj nr (inc (last-row (dec x))))
                        (conj nr (max (peek nr) deletion))))
                    [0]
                    (next (map-indexed vector last-row))))]
    (peek (reduce next-row
                  (into [] (repeat (inc (count a)) 0))
                  (range (count b))))))

(defn hamming
  "Find the hamming distance of 2 finite sequences
  reference: https://en.wikipedia.org/wiki/Hamming_distance"
  [a b]
  (assert (= (count a) (count b)))
  (count (remove identity (map = a b))))
