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
     (let [scatter-direction (vec3a/add normal (vec3a/random-unit-vec3))]
       {::scattered-ray #::ray{:origin point :direction scatter-direction}
        ::attenuation   albedo}))})

(defn metal [^doubles albedo ^double fuzz]
  {::scatter-fn
   (fn metal-scatter [{::ray/keys [direction]} {::hit/keys [point normal]}]
     (let [reflected (-> (vec3a/reflect direction normal)
                         (vec3a/add (vec3a/mult-scalar (vec3a/random-unit-vec3) fuzz)))]
       (when (> (vec3a/dot reflected normal) 0)
         {::scattered-ray #::ray{:origin point :direction reflected}
          ::attenuation   albedo})))})
