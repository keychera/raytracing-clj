(ns ray
  (:require
   [vec3]))

;; ray is {::origin vec3 ::direction vec3}

(defn at [{::keys [origin direction]} t]
  (vec3/add origin (vec3/multiply direction t)))

(defn color [{::keys [direction]}]
  (let [[_x y _z] (vec3/unit direction)
        a         (* 0.5 (+ y 1.0))]
    (vec3/add (vec3/multiply [1.0 1.0 1.0] (- 1.0 a))
              (vec3/multiply [0.5 0.7 1] a))))
