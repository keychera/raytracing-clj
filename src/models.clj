(ns models
  (:require
   [body :as body]
   [ray :as ray]
   [vec3 :as vec3]))

(defn sphere [center radius]
  {::body/hit-fn
   (fn [{::ray/keys [origin direction] :as the-ray} t-min t-max]
     (let [oc (vec3/subtract center origin)
           a  (vec3/length-squared direction)
           h  (vec3/dot direction oc)
           c  (- (vec3/length-squared direction) (* radius radius))
           discriminant (- (* h h) (* a c))]
       (if (< discriminant 0)
         nil
         (let [sqrt-d (Math/sqrt discriminant)
               root   (let [root' (/ (- h sqrt-d) a)]
                        (if (or (<= root' t-min) (<= t-max root'))
                          (/ (+ h sqrt-d) a)
                          root'))]
           (if (or (<= root t-min) (<= t-max root))
             nil
             (let [point          (ray/at the-ray root)
                   outward-normal (vec3/divide (vec3/subtract point center) radius)]
               (body/hit-record the-ray point outward-normal root)))))))})
