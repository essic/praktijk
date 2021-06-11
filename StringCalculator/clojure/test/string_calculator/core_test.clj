(ns string-calculator.core-test
  (:require [clojure.test :refer :all]
            [string-calculator.core :refer :all]))

(deftest step-1.1 
  (testing "when given \"\" should return 0"
    (is (= 0 (add "")))))

(deftest step-1.2
  (testing "when given \"1\" should return 1"
    (is (= 1 (add "1")))))

(deftest step-1.3
  (testing "when given \"1,2\" should return 3"
    (is (= 3 (add "1,2")))))

(deftest step-2
  (testing "when given \"1,2,6\" should return 9"
    (is (= 9 (add "1,2,6")))))

(deftest step-3
  (testing "when given \"1\\n2,3\" should return 6"
    (is (= 6 (add "1\n2,3")))))

(deftest step-4
  (testing "when given \"//;\\n1;2”\" should return 3"
    (is (= 3 (add "//;\n1;2")))))

(deftest step-5
  (testing "when given \"//#\\n1#2#-3#-4\" should throw with (-3,-4)"
    (is (thrown-with-msg? Exception #"negatives not allowed : [-3 -4]" (add "//#\n1#2#-3#-4")))))
