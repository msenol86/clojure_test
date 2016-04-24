(ns my-clojure-proj.core
  (:gen-class))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; tail-call-optimization
(defn is_not_prime [x acc1]
  (cond
    (= x acc1) false
    :else (or
            (= 0 (dbg (mod x acc1)))
            (recur x (+ acc1 1)))))


;; without tail-call-optimization
(defn is_not_prime_bad [x acc1]
  (cond
    (= x acc1) false
    :else (or
            (is_not_prime_bad x (+ acc1 1))
            (= 0 (dbg (mod x acc1))))))


(defn is_prime [x]
  (cond
    (= x 1) true
    (= x 2) true
    :else (not (is_not_prime x 2))))

(assert (is_prime 1))
(assert (is_prime 2))
(assert (is_prime 5))
(assert (not (is_prime 12)))

(defn is_prime_bad [x]
  (cond
    (= x 1) true
    (= x 2) true
    :else (not (is_not_prime_bad x 2))))

(assert (is_prime_bad 1))
(assert (is_prime_bad 2))
(assert (is_prime_bad 5))
(assert (not (is_prime_bad 12)))

; (is_prime 8191)
; stackoverflow:
; (is_prime_bad 8191)
; very big prime number 2147483647



(defn fact [x]
  (if
    (< x 2)
    1
    (* x (fact (- x 1)))))

(assert (= (fact 5) 120))
(assert (= (fact 1) 1))
(assert (= (fact 0) 1))
(assert (= (fact 2) 2))


(defn digits [n]
  (loop [result (list), n n]
    (if (pos? n)
      (recur (conj result (rem n 10))
             (quot n 10))
      result)))

(assert (= (digits 15) '(1 5)))
(assert (= (digits 40585) '(4 0 5 8 5)))

(defn weird_pattern? [a]
  (= a (reduce + (map (fn [x] (fact x)) (digits a)))))

; Weird Patterns:
; 1 = 1!
; 2 = 2!
; 145 = 1! + 4! + 5!
; 40585 = 4! + 0! + 5! + 8! + 5!
; There are no other numbers fit to this pattern
(assert (weird_pattern? 1))
(assert (weird_pattern? 2))
(assert (weird_pattern? 145))
(assert (weird_pattern? 40585))
(assert (not (weird_pattern? 10)))
(assert (not (weird_pattern? 146)))


(defn find_weird_numbers_until [x]
   (reverse
     (reduce
       (fn [a b] (if (weird_pattern? b) (cons b a) a))
       nil
       (rest (range (+ (bigint x) 1)))))) ;applied rest to elimnate 0 at start

(assert (= '(1 2) (find_weird_numbers_until 5)))
(assert (= '(1 2 145 40585) (find_weird_numbers_until 40585)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
