(ns vec3)

;; vec3 is [x y z]

(defn add [v1 v2] (mapv + v1 v2))
(defn subtract [v1 v2] (mapv - v1 v2))
(defn negative [v] (mapv #(- %) v))

(defn multiply [v s] (mapv #(* % s) v))
(defn divide [v s] (mapv #(/ % s) v))

(defn length-squared [[x y z]] (+ (* x x) (* y y) (* z z)))
(defn length [v] (Math/sqrt (length-squared v)))

(defn dot [v1 v2] (reduce + (mapv * v1 v2)))
(defn cross [[u0 u1 u2] [v0 v1 v2]]
  [(- (* u1 v2) (* u2 v1))
   (- (* u2 v0) (* u0 v2))
   (- (* u0 v1) (* u1 v0))])

(defn unit [v] (divide v (length v)))

(defn random-vec3
  ([] [(rand) (rand) (rand)])
  ([vmin vmax] (let [rand-range #(+ vmin (rand (- vmax vmin)))]
                 [(rand-range) (rand-range) (rand-range)])))

(defn random-unit-vec3 []
  (->> (repeatedly #(random-vec3 -1 1))
       (map (fn [p] (let [lensq (length-squared p)]
                      (when (and (> lensq 1e-160) (<= lensq 1)) (divide p (Math/sqrt lensq))))))
       (remove nil?)
       (first)))

(defn random-on-hemisphere [normal]
  (let [on-unit-sphere (random-unit-vec3)]
    (if (> (dot on-unit-sphere normal) 0)
      on-unit-sphere
      (negative on-unit-sphere))))
