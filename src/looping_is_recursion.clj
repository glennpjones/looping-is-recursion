(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [seq acc]
                 (if (empty? seq)
                   acc
                   (recur (rest seq) (first seq))))]
      (helper a-seq (first a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [iter-seq a-seq
         index 0]
    (cond
     (empty? iter-seq) nil
     (pred (first iter-seq)) index
     :else (recur (rest iter-seq) (inc index)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [sequence a-seq
           acc 0
           iter 0]
      (if (empty? sequence)
        (/ acc iter)
        (recur (rest sequence) (+ acc (first sequence)) (inc iter))))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (if (empty? a-seq)
      nil
      (loop [sequence a-seq
             acc #{}]
        (if (empty? sequence)
          acc
          (recur (rest sequence) (toggle acc (first sequence))))))))

(defn fast-fibo [n]
  (if (= n 0)
    0
    (loop [n-forward 1
           n-back 0
           iter (- n 1)]
      (if (= iter 0)
        n-forward
        (recur (+ n-forward n-back) n-forward (dec iter))))))

(defn cut-at-repetition [a-seq]
  (loop [sequence a-seq
         encountered-seq []
         front (first a-seq)]
    (if (or (some #{front} encountered-seq) (empty? sequence))
      encountered-seq
      (recur (rest sequence) (conj encountered-seq front) (first (rest sequence))))))
