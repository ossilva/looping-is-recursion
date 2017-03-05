(ns looping-is-recursion)

(defn power [base exp]
  (let [loop-helper (fn [p n]
                      (if (== n 0)
                        p
                        (recur (* base p) (dec n))))]
    (loop-helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil 
    (let [loop-helper (fn [b-seq]
                        (if (== 1 (count b-seq))
                          (first b-seq)
                          (recur (rest b-seq))))]
      (loop-helper a-seq))))

(defn seq= [seq1 seq2]
  (let [loop-helper (fn [a-seq b-seq]
                      (if (and (empty? a-seq) (empty? b-seq))
                        true
                        (if (not= (first a-seq) (first b-seq))
                         false
                         (recur (rest a-seq) (rest b-seq)))))]
    (if (not= (count seq2) (count seq1))
      false
      (loop-helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [ind 0
         rec-seq a-seq]
    (if (empty? rec-seq)
      nil
      (if (pred (first rec-seq))
       ind
       (recur (inc ind) (rest rec-seq))))))

(defn avg [a-seq]
  (loop [sum 0
         length 0
         rec-seq a-seq]
    (if (empty? rec-seq) (/ sum length)
(recur (+ sum (first rec-seq)) (inc length) (rest rec-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [rec-seq a-seq
          unpaired-set #{}]
     (if (empty? rec-seq)
       unpaired-set
       (recur (rest rec-seq) (toggle unpaired-set (first rec-seq)))))))

(defn fast-fibo [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else
    (loop [f-2 0
           f-1 1
           ind 2]
      (if (== ind n)
        (+ f-1 f-2)
        (recur f-1 (+ f-1 f-2) (inc ind))))))



(defn cut-at-repetition [a-seq]
  (loop [head-set #{}
         head-seq []
         tail-seq a-seq]
    (if (empty? tail-seq)
head-seq
      (let [first-tail (first tail-seq)]
       (if (contains? head-set first-tail)
         head-seq
         (recur (conj head-set first-tail) (conj head-seq first-tail) (rest tail-seq)))))))

