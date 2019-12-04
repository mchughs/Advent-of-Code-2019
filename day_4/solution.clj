(ns day-3.solution)

;;-Part 1-

(def my-input [367479 893698])

(defn int->digits [n]
  (->> n
       str
       (map #(Character/digit % 10))))

(defn digits->int [d]
  (->> d
      (apply str)
      Integer.))

(defn ascending? [n]
  (let [ascending-sort (->> n
                            int->digits
                            (sort-by max)
                            digits->int)]
    (= ascending-sort
       n)))

(defn has-double-digits? [n]
  (let [digits (int->digits n)]
    (not= (dedupe digits)
          digits)))

(defn meets-criteria? [n]
  (and (ascending? n)
       (has-double-digits? n)))

(def meet-criteria
  (let [candidates (range (first my-input) (second my-input))]
    (count (filter meets-criteria? candidates))))

;-Part 2-

(def all-digits [0 1 2 3 4 5 6 7 8 9])

(defn dedupe-digit [digits digit-to-remove]
  (let [shortened (remove #(= % digit-to-remove)
                          digits)]
    (= (count shortened) 4)))

(defn has-double-digits?' [n]
  (let [digits (int->digits n)]
    (->> all-digits
         (map (partial dedupe-digit digits))
         (some identity))))

(defn meets-criteria?' [n]
  (and (ascending? n)
       (has-double-digits?' n)))

(def meet-criteria'
 (let [candidates (range (first my-input) (second my-input))]
   (count (filter meets-criteria?' candidates))))
