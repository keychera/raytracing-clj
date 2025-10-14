(ns ray
  (:require
   [vec3]))

;; ray is {::origin vec3 ::direction vec3}

(defn at [{::keys [origin direction]} t]
  (vec3/add origin (vec3/multiply direction t)))

(defn hit-sphere [{::keys [origin direction]} center radius]
  (let [oc (vec3/subtract center origin)
        a  (vec3/dot direction direction)
        b  (* (vec3/dot direction oc) -2.0)
        c  (- (vec3/dot oc oc) (* radius radius))
        discriminant (- (* b b) (* 4 a c))]
    (when (>= discriminant 0)
      (/ (- 0 b (Math/sqrt discriminant)) (* 2.0 a)))))

(defn color [{::keys [direction] :as ray}]
  (if-let [t (hit-sphere ray [0 0 -1] 0.5)]
    (let [[nx ny nz] (vec3/unit (vec3/subtract (at ray t) [0 0 -1]))]
      (vec3/multiply [(inc nx) (inc ny) (inc nz)] 0.5))
    (let [[_x y _z] (vec3/unit direction)
          a         (* 0.5 (+ y 1.0))]
      (vec3/add (vec3/multiply [1.0 1.0 1.0] (- 1.0 a))
                (vec3/multiply [0.5 0.7 1] a)))))
