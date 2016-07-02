(ns looping-is-recursion)

(defn power [base exp]
  (apply * (repeat exp base)))

(defn last-element [a-seq]
  (let [next-seq (rest a-seq)]
    (cond
      (empty? a-seq) nil
      (empty? next-seq) (first a-seq)
      :else (recur next-seq))))

(defn seq= [seq1 seq2]
  (= seq1 seq2))

(defn find-first-index
  ([pred a-seq]
   (find-first-index pred 0 a-seq))
  ([pred n a-seq]
   (cond
     (empty? a-seq) nil
     (pred (first a-seq)) n
     :else (recur pred (inc n) (rest a-seq)))))

(defn avg [a-seq]
  (/ (apply + a-seq) (count a-seq)))

(defn parity [a-seq]
  (->> a-seq
       (frequencies)
       (map #(if (odd? (second %)) (first %)))
       (remove nil?)
       (set)))

(defn fibber [acc n]
  (let [prev1 (second acc)
        prev2 (first acc)
        newacc (drop-last acc)]
    (conj newacc (+ prev1 prev2))))

(defn fast-fibo [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (first (reduce fibber '(1 0) (range 2 (inc n))))))

(defn cut-at-repetition
  ([a-seq]
   (cut-at-repetition a-seq {:prevs #{} :content []}))
  ([a-seq context]
   (let [cur (first a-seq)
         prevs (context :prevs)
         content (context :content)]
     (cond
       (empty? a-seq) content
       (some #{cur} prevs) content
       :else (recur (rest a-seq)
                    {:prevs (conj prevs cur) :content (conj content cur)})))))

