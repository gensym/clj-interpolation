(ns interpolation.core
  (require [incanter.charts :as charts]
           [incanter.core :as core]
           [datomic.api :as d]))

(defn plot [fun]
  (core/view (charts/function-plot fun 0 5)))

(comment (plot core/sin))

(def points [[0 10]
             [1 20]
             [2 40]
             [3 50]
             [4 55]
             [5 50]
             [6 40]
             [7 20]])



(defn exact [points x]
  (first
   (d/q '[:find ?x ?y
          :in $points ?v
          :where
          [$points ?x ?y]
          [(= ?x ?v)]]
        points, x)))



(defn lneighbor [points x]
  (let [lx
        (ffirst
         (d/q '[:find (max ?x)
                :in $points ?v
                :where
                [$points ?x ?y]
                [(> ?v ?x)]]
              points, x))]
    (exact points lx)))

(defn rneighnor [points x]
  (let [rx (ffirst (d/q '[:find (min ?x)
                          :in $points ?v
                          :where
                          [$points ?x ?y]
                          [(< ?v ?x)]]
                        points, x))]
    (exact points rx)))


(defn linear [points]
  (fn [x]
    (do
      (println (str "X: " x))
      (if-let [[_ y] (exact points x)]
        y
        (let [[x1 y1] (lneighbor points x)
              [x2 y2] (rneighnor points x)
              m (/ (- y2 y1) (- x2 x1))]
          (do
            (+ (* m (- x x1)) y1)))))))



(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
