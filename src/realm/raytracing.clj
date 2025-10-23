(ns realm.raytracing
  (:require
   [clj-async-profiler.core :as prof]
   [clojure.java.io :as io]
   [clojure.math :as math]
   #_[criterium.core :as criterium]
   [realm.vec3])
  (:import [realm.vec3 Realm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; hmm maybe this is a bad idea... but I want to do it....


(defn create-i> [globals offset]
  (let [offset (long offset)]
    (into {} (map-indexed (fn [i v] [v (* 3 (+ offset (long i)))])) globals)))

(def aspect-ratio    16/9)
(def image-width     400)
(def image-height    (int (/ ^double image-width ^double aspect-ratio)))
(def samples-per-px  100)
(def max-depth       50)

(def pixel-scale     (/ 1.0 (double samples-per-px)))

(def focal-length    1.0)
(def viewport-height 2.0)
(def viewport-width  (* ^double viewport-height (/ ^double image-width ^double  image-height)))
(def pixel-count     (int (* ^double image-width ^double image-height)))

;; header, is this c?
(def global-vecs [::camera-center
                  ::viewport-u
                  ::viewport-v
                  ::pixel-du
                  ::pixel-dv
                  ::U-L
                  ::pixel-00])
(def world       [::sphere-1
                  ::sphere-2
                  ::ground-color
                  ::center-color])
(def loop-vecs   [::sample
                  ::ray-origin
                  ::ray-direction
                  ::pixel-color])
(def temporaries [::temp
                  ::hit-point
                  ::hit-normal
                  ::attenuation])

;; this temp var stuff will be a hard thing to keep track of
;; vague rule we use for now
;; Hittable.hit mutate ::temp, ::hit-normal, and ::hit-point, return ^double t and will not modify argument vectors
;; Ray.hitAnything does not mutate realm, return positive t if it hitAnything
;; Ray.raycolor mutate argument target, ray-origin, ray-direction, and ::temp...
;; do we only need one ::temp ? cannot be, let's see

(def all-vector-i (concat global-vecs world loop-vecs temporaries))

(defmacro i> [k]
  (let [im (create-i> all-vector-i pixel-count) v (im k)] v))

;; macro to avoid reflection
(defmacro rayAt [realm target-i origin-i direction-i t]
  `(do (.mult ^realm.vec3.Realm ~realm ~target-i ~direction-i ~t)
       (.add  ^realm.vec3.Realm ~realm ~target-i ~target-i ~origin-i)))

(definterface Hittable
  (^double hit [^realm.vec3.Realm realm ^long ray-origin ^long ray-direction ^double t-min ^double t-max]))

(deftype Sphere [^long center-i ^double radius]
  Hittable
  (hit [_this realm ray-origin ray-direction t-min t-max]
    (.subt realm (i> ::temp) center-i ray-origin)
    (let [a (.lengthSquared realm ray-direction)
          h (.dot realm ray-direction (i> ::temp))
          c (- (.dot realm (i> ::temp) (i> ::temp)) (* radius radius))
          discriminant (- (* h h) (* a c))]
      (if (< discriminant 0.0)
        -1.0
        (let [sqrt-d (Math/sqrt discriminant)
              root   (let [root' (/ (- h sqrt-d) a)]
                       (if (<= root' t-min)
                         (/ (+ h sqrt-d) a)
                         (if (<= t-max root')
                           (/ (+ h sqrt-d) a)
                           root')))]
          (if (<= root t-min)
            -1.0
            (if (<= t-max root)
              -1.0
              (do (rayAt realm (i> ::hit-point)  ray-origin ray-direction root)
                  (.subt realm (i> ::temp)       (i> ::hit-point) center-i)
                  (.divi realm (i> ::hit-normal) (i> ::temp) radius)
                  (let [dot-product (.dot realm ray-direction (i> ::hit-normal))]
                    (when (>= dot-product 0.0) ;; not front-face, written like this to avoid reflection
                      (.mult realm (i> ::hit-normal) (i> ::hit-normal) -1.0)))
                  root))))))
    #_"this mutates ::hit-normal and ::hit-point"))

(definterface Material
  ;; no scattered ray argument because we reuse ::ray-origin and ::ray-direction
  (^boolean scatter [^realm.vec3.Realm realm ^long ray-origin ^long ray-direction ^long hit-point ^long hit-normal ^long attenuation]))

(deftype Lambertian [^long albedo]
  Material
  (scatter [_this realm ray-origin ray-direction hit-point hit-normal attenuation]
    (.randUnitVec3 realm ray-direction)
    (.add  realm ray-direction ray-direction hit-normal)
    (.copy realm ray-origin hit-point)
    (.copy realm attenuation albedo)
    true))

(deftype HitRecord
         [^double t
          ^clojure.lang.IPersistentMap gotHit])

(definterface Ray
  (^realm.raytracing.HitRecord hitAnything
   [^realm.vec3.Realm realm hittables ^long ray-origin ^long ray-direction ^double t-min ^double t-max])
  (rayColor [^realm.vec3.Realm realm hittables ^long target ^long ray-origin ^long ray-direction ^long depth]))

(def a-ray
  (reify Ray
    (hitAnything [_this realm hittables ray-origin ray-direction t-min t-max]
      (let [length (count hittables)]
        (loop [i 0 closest-so-far t-max last-hit nil]
          (if (< i length)
            (let [got-hit   (nth hittables i)
                  ^Hittable hittable (::hittable got-hit)
                  t        (.hit hittable realm ray-origin ray-direction t-min closest-so-far)]
              (if (> t 0.0)
                (recur (inc i) t got-hit)
                (recur (inc i) closest-so-far last-hit)))
            (if (some? last-hit)
              (HitRecord. closest-so-far last-hit)
              (HitRecord. -1.0 nil))))))

    (rayColor [this realm hittables target ray-origin ray-direction depth]
      (.vec3 realm target 1.0 1.0 1.0) ;; this is important, the initial accumulated value
      (loop [depth depth]
        (if (<= depth 0)
          (.vec3 realm target 0.0 0.0 0.0)
          (let [hit-record (.hitAnything this realm hittables
                                         ray-origin ray-direction 1e-3 ##Inf)]
            (if (> (.t hit-record) 0.0)
              #_hittingSomething
              (let [got-hit       (.gotHit hit-record)
                    ^Material mat (::material got-hit)
                    scattered?    (when mat
                                    (.scatter mat realm ray-origin ray-direction
                                              (i> ::hit-point) (i> ::hit-normal) (i> ::attenuation)))]
                ;; .scatter is expected to mutate ray-origin and ray-direction
                ;; when recur, the new loop will follow the scattered ray, getting new attenuation if hitAnything
                ;; or getting the color of the sky
                (if scattered?
                  (do (.multVec3 realm target target (i> ::attenuation))
                      (recur (- depth 1)))
                  (.vec3 realm target 0.0 0.0 0.0)))
              #_theSky
              (do (.unitVec3 realm (i> ::temp) ray-direction)
                  (let [y (.y realm (i> ::temp))
                        a (* 0.5 (+ y 1.0))
                        r (+ (* (- 1.0 a) 1.0) (* a 0.5))
                        g (+ (* (- 1.0 a) 1.0) (* a 0.7))
                        b (+ (* (- 1.0 a) 1.0) (* a 1.0))]
                    (.vec3 realm (i> ::temp) r g b)
                    (.multVec3 realm target target (i> ::temp)))))))))))

(comment
  (require '[clj-java-decompiler.core :refer [decompile]])

  (binding [*compiler-options* {:disable-locals-clearing false}]
    (spit ".zzz/This.java"
          (with-out-str
            (decompile (println "hell0"))))))

(defn clamp ^double [^double x ^double min-v ^double max-v] (min max-v (max x min-v)))

(defn linear->gamma ^double [^double component]
  (if (> component 0.0) (Math/sqrt component) 0.0))

(defn -main []
  (let [realm-size   (* (+ ^long pixel-count (count all-vector-i)) 3)
        ^Realm realm (Realm. (make-array Double/TYPE realm-size))
        center       (let [circle-i (i> ::sphere-1)
                           albedo-i (i> ::center-color)]
                       (.vec3 realm circle-i 0.0 0.0 -1.0)
                       (.vec3 realm albedo-i 0.1 0.2 0.5)
                       {::hittable (Sphere. circle-i 0.5)
                        ::material (Lambertian. albedo-i)})
        ground       (let [circle-i (i> ::sphere-2)
                           albedo-i (i> ::ground-color)]
                       (.vec3 realm circle-i 0.0 -100.5 -1.0)
                       (.vec3 realm albedo-i 0.5 0.5 0.5)
                       {::hittable (Sphere. circle-i 100.0)
                        ::material (Lambertian. albedo-i)})
        hittables    [center ground]]

    ;; (prof/start)

    (time #_criterium/bench
     (do (doto realm
           (.vec3 (i> ::camera-center) 0.0 0.0 0.0)
           (.vec3 (i> ::viewport-u) viewport-width 0.0 0.0)
           (.vec3 (i> ::viewport-v) 0.0 (- ^double viewport-height) 0.0)
           (.divi (i> ::pixel-du) (i> ::viewport-u) ^double image-width)
           (.divi (i> ::pixel-dv) (i> ::viewport-v) ^double image-height)

           ;; multiple operand is complex to represent currently 
           (.vec3 (i> ::temp) 0.0 0.0 focal-length)
           (.subt (i> ::U-L)  (i> ::camera-center) (i> ::temp))
           (.divi (i> ::temp) (i> ::viewport-u) 2.0)
           (.subt (i> ::U-L)  (i> ::U-L) (i> ::temp))
           (.divi (i> ::temp) (i> ::viewport-v) 2.0)
           (.subt (i> ::U-L)  (i> ::U-L) (i> ::temp))

           (.add  (i> ::temp) (i> ::pixel-du) (i> ::pixel-dv))
           (.divi (i> ::temp) (i> ::temp) 2.0)
           (.add  (i> ::pixel-00) (i> ::U-L) (i> ::temp)))


         (let [image-height (double image-height)
               image-width (double image-width)]
           (loop [j 0.0 i 0.0]
             (when (< j image-height)
               (if (< i image-width)
                 (do (.vec3 realm (i> ::pixel-color) 0.0 0.0 0.0)
                     (dotimes [_ samples-per-px]
                       (.copy realm (i> ::sample) (i> ::pixel-00))
                       (.mult realm (i> ::temp)   (i> ::pixel-du) (+ i (- (math/random) 0.5)))
                       (.add  realm (i> ::sample) (i> ::sample)   (i> ::temp))
                       (.mult realm (i> ::temp)   (i> ::pixel-dv) (+ j (- (math/random) 0.5)))
                       (.add  realm (i> ::sample) (i> ::sample)   (i> ::temp))

                       (.copy realm (i> ::ray-origin) (i> ::camera-center))
                       (.subt realm (i> ::ray-direction) (i> ::sample) (i> ::ray-origin))

                       ;; pixel-sample is used as temp, this is some bug-prone way of programming huh
                       (.rayColor ^Ray a-ray realm hittables (i> ::sample) (i> ::ray-origin) (i> ::ray-direction) max-depth)
                       (.add realm (i> ::pixel-color) (i> ::pixel-color) (i> ::sample)))
                     (.mult realm (long (* 3.0 (+ i (* j image-width)))) (i> ::pixel-color) pixel-scale)
                     (recur j (+ i 1.0)))
                 (recur (+ j 1.0) 0.0)))))))

    ;; (prof/stop)

    (let [image-height (int image-height)
          image-width (int image-width)]
      (with-open [out (io/writer "scene-realm.ppm")]
        (.write out (str "P3\n" image-width " " image-height "\n255\n"))
        (dotimes [j image-height]
          (dotimes [i image-width]
            (let [[r g b] (->> (.read realm (long (* 3 (+ i (* j image-width)))))
                               (map #(int (* 256.0 (clamp (linear->gamma %) 0.0 0.999)))))]
              (.write out (str r " " g " " b "\n")))))))))

