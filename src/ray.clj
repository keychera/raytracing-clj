(ns ray
  (:require
   [body :as body]
   [vec3]))

;; ray is {::origin vec3 ::direction vec3}

(defn at [{::keys [origin direction]} t]
  (vec3/add origin (vec3/multiply direction t)))

(defn color [{::keys [direction] :as ray} {::body/keys [hit-fn]}]
  (if-let [hit-record (hit-fn ray 0 10)]
    (let [t          (::body/t hit-record)
          [nx ny nz] (vec3/unit (vec3/subtract (at ray t) [0 0 -1]))]
      (vec3/multiply [(inc nx) (inc ny) (inc nz)] 0.5))
    (let [[_x y _z] (vec3/unit direction)
          a         (* 0.5 (+ y 1.0))]
      (vec3/add (vec3/multiply [1.0 1.0 1.0] (- 1.0 a))
                (vec3/multiply [0.5 0.7 1] a)))))
