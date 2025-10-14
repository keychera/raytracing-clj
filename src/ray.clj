(ns ray
  (:require
   [vec3]))

;; ray is {::origin vec3 ::direction vec3}

(defn at [{::keys [origin direction]} t]
  (vec3/add origin (vec3/multiply direction t)))

(defn hit-sphere [{::keys [origin direction]} center radius]
  (let [oc (vec3/subtract center origin)
        a  (vec3/length-squared direction)
        h  (vec3/dot direction oc)
        c  (- (vec3/length-squared direction) (* radius radius))
        discriminant (- (* h h) (* a c))]
    (when (>= discriminant 0)
      (/ (- h (Math/sqrt discriminant)) a))))

(defn color [{::keys [direction] :as ray}]
  (if-let [t (hit-sphere ray [0 0 -1] 0.5)]
    (let [[nx ny nz] (vec3/unit (vec3/subtract (at ray t) [0 0 -1]))]
      (vec3/multiply [(inc nx) (inc ny) (inc nz)] 0.5))
    (let [[_x y _z] (vec3/unit direction)
          a         (* 0.5 (+ y 1.0))]
      (vec3/add (vec3/multiply [1.0 1.0 1.0] (- 1.0 a))
                (vec3/multiply [0.5 0.7 1] a)))))
