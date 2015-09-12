(ns looping-is-recursion)

(defn power [base exp]
  (let [hlpr (fn [acc n]
               (if (zero? n)
                 acc
                 (recur (* acc base) (dec n))))]
    (hlpr 1 exp)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (not= (count seq1) (count seq2)) false
   (not= (first seq1) (first seq2)) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) i
     :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         cnt 0
         s a-seq]
    (if (empty? s)
      (/ sum cnt)
      (recur (+ sum (first s)) (inc cnt) (rest s)))))

(defn- contains-first-element? [seq-1 seq-2]
  (some (set (take 1 seq-1)) seq-2))

(defn parity [a-seq]
  (loop [result #{}
         s a-seq]
    (let [toggled-result ((if (contains-first-element? s result)
                            disj
                            conj) result (first s))]
      (if (empty? s)
        result
        (recur toggled-result (rest s))))))

(defn fast-fibo [n]
  (loop [prev-f 0
         curr-f 1
         i n]
    (if (zero? i)
      prev-f
      (recur curr-f (+ prev-f curr-f) (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [result []
         s a-seq]
    (if (or (empty? s)
            (contains-first-element? s result))
      result
      (recur (conj result (first s)) (rest s)))))
