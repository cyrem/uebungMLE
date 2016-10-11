(ns uebungmle.core
  (:gen-class))

(def cities 100)

(defn getCities [m cityCol cityRow]
  (if (<= cityCol cityRow)
    (nth m (+ cityCol (* cities cityRow)))
    (nth m (+ cityRow (* cities cityCol)))))

(defn create-map []
  (mapv (fn [x]
          (if (=  (quot x cities) (mod x cities) )
            0
            (rand-int 25)))
        (range (* cities cities))))

(defn getFitnessValue [v mapping]
  (+ (loop [i 0
            erg 0]
       (if (< i 99)
         (recur (inc i) 
                (+ erg 
                   (getCities mapping (nth v i) (nth v (inc i)))))
         erg))
     (getCities mapping (nth v 0) (nth v (dec(count v))))))

(def startVec (vec (range 100)))
(def curMap (create-map))

(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))


;(hillClimbing startVec curMap 100000)
(defn hillClimbing [v mapping nr]
  (let [nrA (rand-int (count v))
        nrB (rand-int (count v))
        currentFitness (getFitnessValue v mapping)
        newVec (swap v nrA nrB)
        newFitness (getFitnessValue newVec mapping)]
    ;(println nr currentFitness " -> " newFitness)
    (if (= nr 0) 
      (do (println currentFitness)
        newVec)
      (if (< newFitness currentFitness)
        (recur newVec mapping (dec nr))
        (recur v mapping (dec nr))))))

(defn weightedFitness [fY fX t]
  (Math/exp (- (*(/ (- fY fX) t)1000))))

(def eps 5)

;(simAnneal startVec 100000)
(defn simAnneal [v t]
  (let [nrA (rand-int (count v))
        nrB (rand-int (count v))
        currentFitness (getFitnessValue v curMap)
        newVec (swap v nrA nrB)
        newFitness (getFitnessValue newVec curMap)]
    (println currentFitness t)
    (if (< t eps)
        newVec
      (if (or (< newFitness currentFitness)  (< (rand) (weightedFitness newFitness currentFitness t)) )
        (recur newVec (- t eps))
        (recur v (- t eps))))
    ))

(defn -main []
  (println "Hello, World!"))



