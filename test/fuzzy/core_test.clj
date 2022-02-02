(ns fuzzy.core-test
  (:require [clojure.test :refer :all]
            [fuzzy.distance :refer :all]))

(deftest test-levenshtein
  (testing "levenshtein"
    (testing "with same strings"
      (is (zero? (levenshtein "abc" "abc")))
      (is (zero? (levenshtein "" ""))))
    (testing "with different length"
      (is (= 1 (levenshtein "ab" "abc")))
      (is (= 1 (levenshtein "abc" "ab")))
      (is (= 1 (levenshtein "bc" "abc")))
      (is (= 1 (levenshtein "abc" "bc")))
      (is (= 2 (levenshtein "abcd" "bc")))
      (is (= 2 (levenshtein "bc" "abcd")))
      (is (= 2 (levenshtein "ab" "abcd")))
      (is (= 2 (levenshtein "abcd" "ab"))))
    (testing "with disjunct strings"
      (is (= 4 (levenshtein "abcd" "")))
      (is (= 4 (levenshtein "" "abcd")))
      (is (= 4 (levenshtein "abcd" "efgh")))
      (is (= 5 (levenshtein "abcde" "fghi")))
      (is (= 5 (levenshtein "abcd" "efghi"))))
    (testing "with wikipedia examples"
      (is (= 3 (levenshtein "kitten" "sitting")))
      (is (= 3 (levenshtein "Saturday" "Sunday"))))
    (testing "with changed chars"
      (is (= 1 (levenshtein "abcde" "abfde")))
      (is (= 2 (levenshtein "abcde" "agfde")))
      (is (= 3 (levenshtein "abcde" "agfhe"))))
    (testing "with non-strings"
      (is (= 3 (levenshtein (list 1 2 3 4 5) (list 1 2 8 9 10))))
      (is (= 2 (levenshtein (range 3 10) (range 2 9))))
      (is (= 3 (levenshtein (vector 1 2 3 4 5) (vector 1 2 8 9 10)))))))

(deftest test-lcs
  (testing "lcs"
    (testing "with same sequences"
      (is (= 3 (lcs "abc" "abc")))
      (is (= 0 (lcs "" ""))))
    (testing "with subsequence"
      (is (= 3 (lcs "def" "abcdefg")))
      (is (= 3 (lcs "abcdefg" "def")))
      (is (= 5 (lcs "acefg" "abcdefg")))
      (is (= 2 (lcs "cg" "abcdefg")))
      (is (= 4 (lcs "AGGTAB" "GXTXAYB"))))
    (testing "without subsequence"
      (is (= 0 (lcs "abc" "def"))))))

(deftest test-hamming
  (testing "hamming"
    (testing "with same sequences"
      (is (= 0 (hamming (range 10) (range 10))))
      (is (= 0 (hamming () ()))))
    (testing "with disjunct sequences"
      (is (= 4 (hamming "0000" "1111"))))
    (testing "with differences"
      (is (= 3 (hamming "karolin" "kathrin")))
      (is (= 3 (hamming "karolin" "kerstin")))
      (is (= 4 (hamming "kathrin" "kerstin")))
      (is (= 3 (hamming "2173896" "2233796"))))))
