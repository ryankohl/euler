(ns euler.core)
(use 'clojure.contrib.math)

(defn div35? [num]
  "divisible by 3 or 5"
  (or (= (mod num 3) 0) 
      (= (mod num 5) 0)))
(defn euler1 [num]
  "the sum of numbers less than 100 divisible by 3 or 5"
  (reduce + (filter div35? (range num))))

(defn fib [ [oldsum x y]]
  "fibonacci"
  (let [z (+ x y)
        newsum (cond (even? y) (+ oldsum y)
                     (odd? y) oldsum)]
    [newsum y z]))
(defn euler2 [max]
  "sum of even-valued fibonacci terms less than 4x10^6"
  (loop [ans [0 1 1]]
    (if (> (last ans) max)
      (first ans)
      (recur (fib ans)))))
  
(defn fac-out [factor num]
  "factor out the factor"
  (loop [ans num]
    (if (< 0 (mod ans factor))
      ans
      (recur (/ ans factor)))))
(defn euler3 [num]
  "find the largest prime factor of a composite number (600851475143)"
  (loop [factor 1
         ans num]
    (let [f (+ 1 factor)
          x (fac-out f ans)]
      (if (= x 1) f (recur f x)))))

(defn palin? [x]
  "is this number a palindrome?"
  (= (seq (str x)) (reverse (seq (str x)))))
(defn euler4 [x]
  "find the largest palindrome that's the product of 2 x-digit numbers"
  (let [M (- (expt 10 x) 1)
        m (- (expt 10 (- x 1)))]
  (->> (for [x (range M m -1)
             y (range M (dec x) -1)]
         (* x y))
       (filter #(palin? %))
       (take 20)
       (apply max))))

(defn lcd [n t]
  (loop [a t]
    (cond (> (* a a) n) n
          (= (rem n a) 0) a
          (= 2 a) (recur 3)
          true (recur (+ a 2)))))
(defn prime? [x] (= x (lcd x 2)))
(defn next-prime-factor [x]
  (loop [a 2]
    (cond (and (prime? a) (= (rem x a) 0)) a
          true (recur (inc a)))))
(defn get-subvec [s S]
  "ensure that s is a sub-vector of S (adding elements of s when needed)"
  (loop [a s
         b S]
    (cond (= 0 (rem (reduce * b) (reduce * s))) b
          true (recur (pop a) (conj b (last a))))))
(defn get-factors [x]
  "get a list of factors of x"
  (loop [a []
         b x]
    (cond (prime? b) (conj a b)
          true (recur (conj a (next-prime-factor b)) (/ b (next-prime-factor b))))))
(defn euler5 [x]
  "what is the smallest PosInt that is evenly divisible by all the ints from 1 to 20?"
  (loop [a []
         b (vec (map get-factors (range 1 (inc x))))]
    (cond (empty? b) (reduce * a)
          true (recur (get-subvec (last b) a) (pop b)))))
