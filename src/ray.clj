(ns ray
  (:require
   [vec3a]))

;; ray is {::origin vec3a ::direction vec3a}

(defn at [{::keys [^doubles origin ^doubles direction]} t]
  (vec3a/add origin (vec3a/mult-scalar direction t)))
