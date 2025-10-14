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