(ns my-clojure-proj.core
  (:gen-class))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; tail-call-optimization
(defn is_not_prime [x acc1] (cond (= x acc1) false :else (or (= 0 (dbg (mod x acc1))) (recur x (+ acc1 1)))))


;; without tail-call-optimization
(defn is_not_prime_bad [x acc1] (cond (= x acc1) false :else (or (is_not_prime_bad x (+ acc1 1)) (= 0 (dbg (mod x acc1))))))


(defn is_prime [x]
  (cond
    (= x 1) true
    (= x 2) true
    :else (not (is_not_prime x 2))))

(defn is_prime_bad [x]
  (cond
    (= x 1) true
    (= x 2) true
    :else (not (is_not_prime_bad x 2))))


; (is_prime 8191)
; stackoverflow:
; (is_prime_bad 8191)
; very big prime number 2147483647

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
