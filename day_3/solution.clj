(ns day-3.solution)

;;-Part 1-

(defn- apply-move [instruction [x y]]
  (let [direction (->> instruction
                       first
                       str)
        step-num  (->> instruction
                       rest
                       (apply str)
                       Integer.)
       steps (range 1 (inc step-num))]
    (map (fn [step] (case direction
                      "U" [x (+ y step)]
                      "D" [x (- y step)]
                      "R" [(+ x step) y]
                      "L" [(- x step) y]
                      "error"))
         steps)))

(defn- get-path [instructions]
  (loop [instructions instructions
         past-path    (list [0 0])
         pivot-point  [0 0]]
      (if instructions
        (let [instruction (first instructions)
              next-leg    (apply-move instruction pivot-point)]
          (recur (next instructions)
                 (concat past-path next-leg)
                 (last next-leg)))
        past-path)))

(defn- find-intersections [& wires]
  (let [paths (map #(-> % get-path set) wires)]
    (->> paths
         (apply clojure.set/intersection)
         (remove #(= [0 0] %)))))

(defn- manhattan-dist [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(defn- find-closest-point [& wires]
  (->> wires
       (apply find-intersections)
       (apply min-key manhattan-dist)))

(defn- find-smallest-distance [& wires]
 (->> wires
      (apply find-intersections)
      (map manhattan-dist)
      (apply min)))

(def my-wires
  (as-> "day_3/input.txt" $
        (slurp $)
        (clojure.string/split $ #"\n")
        (map #(clojure.string/split % #",") $)))

(apply find-smallest-distance my-wires)

;-Part 2-

(defn- points-and-paths [& wires]
  (let [paths     (map get-path wires)
        paths-set (map set paths)
        common-points (->> paths-set
                           (apply clojure.set/intersection)
                           (remove #(= [0 0] %)))]
    [common-points paths]))

(defn- signal-dist [point wire-path]
  (.indexOf wire-path point))

(defn- total-signal-dist [point & wire-paths]
  (->> wire-paths
       (map (partial signal-dist point))
       (reduce +)))

(defn- find-smallest-signal-distance [& wires]
  (let [[possible-points paths] (apply points-and-paths wires)]
    (->> possible-points
      (map #(apply total-signal-dist % paths))
      (apply min))))

(apply find-smallest-signal-distance my-wires)
