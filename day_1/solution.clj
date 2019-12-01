(ns day-1.solution)

;;-Part 1-

(def my-mass-list
  (as-> "day_1/input.txt" $
        (slurp $)
        (clojure.string/split $ #"\n")
        (map read-string $)))

(defn mass->needed-fuel [mass]
  (-> mass
      (quot 3)
      (- 2)))

(defn fuel-counter-upper [mass-list]
  (->> mass-list
       (map mass->needed-fuel)
       (reduce +)))

(fuel-counter-upper my-mass-list)

;;-Part 2-

(defn mass->needed-fuel-mass
  "Any items with a mass less than 9 require 'no fuel' to transport."
  [mass]
  (when (> mass 8)
    (mass->needed-fuel mass)))

(defn module-mass->total-fuel-mass
  "The additional mass is first calculated as the amount of fuel to move the module's mass.
  Then while there is still mass that needs to be carried, the mass->fuel calculation is preformed recursively,
  all the while adding any additional mass to the total."
  [module-mass]
  (loop [additional-mass (mass->needed-fuel-mass module-mass)
         total-fuel-mass 0]
    (if additional-mass
      (recur (mass->needed-fuel-mass additional-mass) (+ total-fuel-mass additional-mass))
      total-fuel-mass)))

(defn fuel-counter-upper' [mass-list]
  (->> mass-list
       (map module-mass->total-fuel-mass)
       (reduce +)))

(fuel-counter-upper' my-mass-list)
