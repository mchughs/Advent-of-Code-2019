(ns day-2.solution)

;;-Part 1-

(def my-intcode-program
  (as-> "day_2/input.txt" $
        (slurp $)
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)
        (assoc $ 1 12)
        (assoc $ 2 2)))

(defn get-operation [op-code]
  (case op-code
    1 +
    2 *
    99 nil ;; halts program
    (fn [x y] (println "error!"))))

(defn run-step [code-seq start-pos]
  ;; check that there are still enough items left in the sequence to run the program
  (when (< (+ 3 start-pos)
           (count code-seq))
    ;; check that the op-code didn't halt by code 99 or some errors
    (when-let [op (-> code-seq (nth (+ 0 start-pos)) get-operation)]
      (let [x-pos   (-> code-seq (nth (+ 1 start-pos))) ;;get position of first input
            x       (-> code-seq (nth x-pos)) ;; get first input
            y-pos   (-> code-seq (nth (+ 2 start-pos))) ;;get position of second input
            y       (-> code-seq (nth y-pos)) ;; get second input
            res-pos (-> code-seq (nth (+ 3 start-pos))) ;; get position to place the result
            res     (op x y)] ;; get the result
       (assoc code-seq res-pos res)))))

(defn run-intcode [intcode]
 (loop [code     intcode
        start    0
        new-code (run-step code start)]
   (if new-code
     (let [new-start (+ start 4)]
       (recur new-code
              new-start
              (run-step new-code new-start)))
     code)))

(run-intcode my-intcode-program)

;;-Part 2-

(def my-intcode-program'
  (as-> "day_2/input.txt" $
        (slurp $)
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)))

(defn update-n-v [intcode noun verb]
  (-> intcode
    (assoc 1 noun)
    (assoc 2 verb)))

(def find-input
  (for [noun (range 100)
        verb (range 100)]
    (let [intcode (update-n-v my-intcode-program' noun verb)]
      (when (= 19690720 (nth (run-intcode intcode) 0))
        [noun verb]))))

(defn calc-answer [[noun verb]]
  (+ verb
     (* 100 noun)))

(def answer
  (as-> find-input $
        (remove nil? $)
        (nth $ 0)
        (calc-answer $)))
