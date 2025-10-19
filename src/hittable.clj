(ns hittable
  (:require
   [hit :as hit]
   [ray :as ray]
   [vec3a :as vec3a]))

(defn sphere [^doubles center ^double radius]
  {::hit-fn
   (fn [self {::ray/keys [origin direction] :as the-ray} t-min t-max]
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
                   front-face?    (hit/front-face? the-ray outward-normal)]
               {::hit/what        self
                ::hit/point       point
                ::hit/front-face? front-face?
                ::hit/normal      (if front-face? outward-normal (vec3a/negative! outward-normal))
                ::hit/t           root}))))))})
