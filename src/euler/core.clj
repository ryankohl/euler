(ns euler.core)
(use 'clojure.contrib.math)

(defn euler1a [num]
  (or (= (mod num 3) 0) 
      (= (mod num 5) 0)))
(defn euler1 [num]
  (reduce + (filter euler1a (range num))))

(defn euler2a [ [oldsum x y]]
  (let [z (+ x y)
        newsum (cond (even? y) (+ oldsum y)
                     (odd? y) oldsum)]
    [newsum y z]))
(defn euler2 [max]
  (loop [ans [0 1 1]]
    (if (> (last ans) max)
      (first ans)
      (recur (euler2a ans)))))
  
(defn euler3a [factor num]
  "factor out the factor"
  (loop [ans num]
    (if (< 0 (mod ans factor))
      ans
      (recur (/ ans factor)))))
(defn euler3 [num]
  (loop [factor 1
         ans num]
    (let [f (+ 1 factor)
          x (euler3b f ans)]
      (if (= x 1) f (recur f x)))))

(defn euler4a [num]
  "how big is the number?"
  (loop [s 1]
    (if (= num (mod num (expt 10 s)))
      (- s 1)
      (recur (+ s 1)))))
(defn euler4b [i num]
  "calculate 0th position in the number"
     (- num (mod num (expt 10 (- (euler4a num) i)))))
(defn euler4c [i num]
  "get the ith position in the number"
  ; dammit - zeros in #'s like 102 screw things up...
    (cond
     (= 0 i) (euler4b i num)
     (< 0 i) (euler4c (- i 1) (- num (euler4c (- i 1) num)))))
(defn euler4d [num]
  "get a vector representation of the number"
  (vec (map #(/ (euler4c % num) (expt 10 (- (euler4a num) %))) (range (+ 1 (euler4a num))))))
(defn euler4e [num]
  "for i in 0..size/2 (rounded down), see if num[i] == num[size-i]"
  (let [n (euler4d num)
        s (euler4a num)]
    (every? true? (map #(= (nth n %) (nth n (- s %))) (range 0 (Math/ceil (/ s 2)))))))

