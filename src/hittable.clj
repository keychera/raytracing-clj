(ns hittable
  (:require
   [ray :as ray]
   [vec3a :as vec3a]))

(defrecord Hit [^Object what ^doubles point ^boolean front-face? ^doubles normal ^double t])

(defprotocol Hittable
  (hit [self ray t-min t-max]))

(defrecord Sphere [^doubles center ^double radius]
  Hittable
  (hit [self {::ray/keys [origin direction] :as the-ray} t-min t-max]
    (let [oc (vec3a/subtract center origin)
          a  (vec3a/length-squared direction)
          h  (vec3a/dot direction oc)
          c  (- (vec3a/length-squared oc) (* radius radius))
          discriminant (- (* h h) (* a c))]
      (if (< discriminant 0.0)
        nil
        (let [sqrt-d (Math/sqrt discriminant)
              root   (let [root' (/ (- h sqrt-d) a)]
                       (if (or (<= root' t-min) (<= t-max root'))
                         (/ (+ h sqrt-d) a)
                         root'))]
          (if (or (<= root t-min) (<= t-max root))
            nil
            (let [point          (ray/at the-ray root)
                  outward-normal (vec3a/divide! (vec3a/subtract point center) radius)
                  front-face?    (< (vec3a/dot direction outward-normal) 0)
                  normal         (if front-face? outward-normal (vec3a/negative! outward-normal))]
              (->Hit self point front-face? normal root))))))))
