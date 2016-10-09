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
     (getCities mapping (first v) (last v))))

(def startVec (vec (range 100)))
(def curMap (create-map))

(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))

(def theta 200)

(defn optimize [v mapping]
  (let [nrA (rand-int (count v))
        nrB (rand-int (count v))
        currentFitness (getFitnessValue v mapping)
        newVec (swap v nrA nrB)
        newFitness (getFitnessValue newVec mapping)]
    (println currentFitness " -> " newFitness)
    (if (< newFitness currentFitness)   
      (if (< newFitness theta)
        newVec
        (recur newVec mapping))
      (recur v mapping))))


(defn -main []
  (println "Hello, World!"))



