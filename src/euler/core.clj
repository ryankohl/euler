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
(defn euler4b [num]
  (loop [e (euler4a num)
         n num
         a []]
    (if (< e 0)
      a
      (recur (- e 1) (rem n (expt 10 e)) (conj a (quot n (expt 10 e)))))))
(defn euler4c [num]
  "for i in 0..size/2 (rounded down), see if num[i] == num[size-i]"
  (let [n (euler4b num)
        s (euler4a num)]
    (every? true? (map #(= (nth n %) (nth n (- s %))) (range 0 (Math/ceil (/ s 2)))))))
(defn euler4 [s]
  "find the largest palindrome that's the product of two numbers with s-many digits"
  (loop [x (expt 10 s)
         a []]
    (if (= x 0)
      (reduce max a)
      (let [ b  (reduce max
                        (conj (filter euler4c
                                (map (partial * (- x 1))
                                     (range (expt 10 (- s 1))
                                            (expt 10 s))))
                              0))]
           (recur (- x 1) (conj a b))))))


