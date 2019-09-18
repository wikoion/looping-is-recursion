(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp] 
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq] (if (empty? (rest a-seq))
                             (first a-seq)
                             (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper 
        (fn [seq1 seq2] 
          (cond
            (and (empty? seq1) (empty? seq2)) true
            (or (empty? seq1) (empty? seq2)) false
            (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
            :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) acc
      :else (recur (inc acc) (rest s)))))

(defn avg [a-seq]
  (loop [acc 0 
         sum 0 
         s a-seq]
    (cond 
      (and (empty? s) (not (zero? acc))) (/ sum acc)
      (and (empty? s) (zero? acc)) nil
      :else (recur (inc acc) (+ sum (first s)) (rest s)))))

(defn parity [a-seq]
  (loop [acc {}
         s a-seq]
    (cond
      (empty? s) (keys (filter #(odd? (val %)) acc)) 
      (contains? acc (first s)) (recur (assoc acc
                                              (first s)
                                              (inc (get acc (first s)))) 
                                       (rest s))
      :else (recur (assoc acc (first s) 1) (rest s)))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [acc 1
           x 1
           y 0]
      (if (== acc n)
        x
        (recur (inc acc) (+ x y) x)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         s a-seq]
    (cond
      (empty? s) acc 
      (some #(= (first s) %) acc) acc
      :else (recur (conj acc (first s)) (rest s)))))

