(ns material
  (:require
   [clojure.spec.alpha :as s]
   [hit :as hit]
   [ray :as ray]
   [vec3a :as vec3a]))

;; scatter-fn is (fn ^{:scattered ^ray :attenuation ^doubles} [ray hit])
;; these comment type notation is still flux
(s/def ::scatter-fn fn?)
(s/def ::attenuation any?)

(defn lambertian [^doubles albedo]
  {::scatter-fn
   (fn lambertian-scatter [_ray {::hit/keys [point normal]}]
     (let [scatter   (vec3a/add! (vec3a/random-unit-vec3) normal)
           direction (if (vec3a/near-zero? scatter) normal scatter)]
       {::scattered-ray #::ray{:origin point :direction direction}
        ::attenuation   albedo}))})

(defn metal [^doubles albedo ^double fuzz]
  {::scatter-fn
   (fn metal-scatter [{::ray/keys [direction]} {::hit/keys [point normal]}]
     (let [reflected (vec3a/reflect direction normal)
           reflected (vec3a/add! (vec3a/mult-scalar! (vec3a/random-unit-vec3) fuzz) reflected)]
       (when (> (vec3a/dot reflected normal) 0)
         {::scattered-ray #::ray{:origin point :direction reflected}
          ::attenuation   albedo})))})

(defn reflectance [^double cosine ^double refraction-index]
  (let [r0 (Math/pow (/ (- 1.0 refraction-index) (+ 1.0 refraction-index)) 2)]
    (+ r0 (* (- 1.0 r0) (Math/pow (- 1.0 cosine) 5)))))

(defn dielectric [refraction-index]
  {::scatter-fn
   (fn dielectric-scatter [{::ray/keys [direction]} {::hit/keys [front-face? point normal]}]
     (let [ri          (if front-face? (/ 1.0 refraction-index) refraction-index)
           unit-dir    (vec3a/unit direction)
           cos-theta   (min (vec3a/dot (vec3a/negative unit-dir) normal) 1.0)
           sin-theta   (Math/sqrt (- 1.0 (* cos-theta cos-theta)))
           refract?    (<= (* ri sin-theta) 1.0)
           direction   (if (or (not refract?) (> (reflectance cos-theta ri) (rand)))
                         (vec3a/reflect unit-dir normal)
                         (vec3a/refract unit-dir normal ri))]
       {::scattered-ray #::ray{:origin point :direction direction}
        ::attenuation (vec3a/make 1.0 1.0 1.0)}))})