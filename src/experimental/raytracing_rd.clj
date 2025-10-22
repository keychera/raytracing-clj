(ns experimental.raytracing-rd
  (:require
   [clj-async-profiler.core :as prof]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [experimental.vec3_realm]
   #_[criterium.core :as criterium])
  (:import [experimental.vec3_realm Realm]))

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
(def pixel-scale     (/ 1.0 (double samples-per-px)))

(def focal-length    1.0)
(def viewport-height 2.0)
(def viewport-width  (* ^double viewport-height (/ ^double image-width ^double  image-height)))
(def pixel-count     (int (* ^double image-width ^double image-height)))

;; header, is this c?
(def global-vecs     [::camera-center
                      ::viewport-u
                      ::viewport-v
                      ::pixel-du
                      ::pixel-dv
                      ::upper-left
                      ::pixel-00])
(def spheres         [::sphere-1
                      ::sphere-2])
(def loop-vecs       [::pixel-sample
                      ::ray-origin
                      ::ray-direction
                      ::pixel-color])
(def temporaries     [::temp
                      ::hit-point
                      ::hit-normal])

(def all-vector-i (concat global-vecs spheres loop-vecs temporaries))

(defmacro i> [k]
  (let [im (create-i> all-vector-i pixel-count) v (im k)] v))

;; macro to avoid reflection
(defmacro ray-at [realm target-i origin-i direction-i t]
  `(do (.multScalar ^experimental.vec3_realm.Realm ~realm ~target-i ~direction-i ~t)
       (.add ^experimental.vec3_realm.Realm ~realm ~target-i ~target-i ~origin-i)))

(definterface Hittable
  (^double hit [^experimental.vec3_realm.Realm realm ^long ray-origin ^long ray-direction ^double t-min ^double t-max]))

(deftype Sphere [^long center-i ^double radius]
  Hittable
  (hit [_this realm ray-origin ray-direction t-min t-max]
    (let [temp-i       (i> ::temp)
          hit-point-i  (i> ::hit-point)
          hit-normal-i (i> ::hit-normal)]
      (.subtract realm temp-i center-i ray-origin)
      (let [a (.lengthSquared realm ray-direction)
            h (.dot realm ray-direction temp-i)
            c (- (.dot realm temp-i temp-i) (* radius radius))
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
                (do (ray-at realm hit-point-i ray-origin ray-direction root)
                    (.subtract realm temp-i hit-point-i center-i)
                    (.divideScalar realm hit-normal-i temp-i radius)
                    (let [dot-product (.dot realm ray-direction hit-normal-i)]
                      (when (>= dot-product 0.0)
                        (.multScalar realm hit-normal-i hit-normal-i -1.0)))
                    root)))))))
    #_"this mutates ::hit-normal and ::hit-point"))

(definterface Ray
  (^double hitAnything
   [^experimental.vec3_realm.Realm realm hittables ^long ray-origin ^long ray-direction ^double t-min ^double t-max])
  (rayColor [^experimental.vec3_realm.Realm realm hittables ^long target ^long ray-origin ^long ray-direction]))

(def a-ray
  (reify Ray
    (hitAnything [_this realm hittables ray-origin ray-direction t-min t-max]
      (let [length (count hittables)]
        (loop [i 0 found? false closest-so-far t-max]
          (if (< i length)
            (let [^Hittable hittable (:hittable (nth hittables i))
                  t        (.hit hittable realm ray-origin ray-direction t-min closest-so-far)]
              (if (> t 0.0)
                (recur (inc i) true t)
                (recur (inc i) found? closest-so-far)))
            (if found? closest-so-far -1.0)))))
    (rayColor [this realm hittables target ray-origin ray-direction]
      (let [t (.hitAnything this realm hittables ray-origin ray-direction 1e-3 ##Inf)]
        (if (> t 0.0)
          (do (.create realm (i> ::temp) 1.0 1.0 1.0)
              (.add realm (i> ::temp) (i> ::hit-normal) (i> ::temp))
              (.multScalar realm (i> ::temp) (i> ::temp) 0.5)
              (.copy realm target (i> ::temp)))
          (do (.unitVec3 realm (i> ::temp) ray-direction)
              (let [y (.y realm (i> ::temp))
                    a (* 0.5 (+ y 1.0))
                    r (+ (* (- 1.0 a) 1.0) (* a 0.5))
                    g (+ (* (- 1.0 a) 1.0) (* a 0.7))
                    b (+ (* (- 1.0 a) 1.0) (* a 1.0))]
                (.create realm target r g b))))))))

(comment
  (require '[clj-java-decompiler.core :refer [decompile]])

  (binding [*compiler-options* {:disable-locals-clearing false}]
    (spit ".zzz/This.java"
          (with-out-str
            (decompile (println "hell0"))))))

(defn -main []
  (let [realm-size   (* (+ ^long pixel-count (count all-vector-i)) 3)
        ^Realm realm (Realm. (make-array Double/TYPE realm-size))
        sphere-1     (let [circle-i (i> ::sphere-1)]
                       (.create realm circle-i 0.0 0.0 -1.0)
                       {:hittable (Sphere. circle-i 0.5)})
        sphere-2     (let [circle-i (i> ::sphere-2)]
                       (.create realm circle-i 0.0 -100.5 -1.0)
                       {:hittable (Sphere. circle-i 100.0)})
        hittables    [sphere-1 sphere-2]]
    (time #_criterium/bench
     (do (doto realm
           (.create (i> ::camera-center) 0.0 0.0 0.0)
           (.create (i> ::viewport-u) viewport-width 0.0 0.0)
           (.create (i> ::viewport-v) 0.0 (- ^double viewport-height) 0.0)
           (.divideScalar (i> ::pixel-du) (i> ::viewport-u) ^double image-width)
           (.divideScalar (i> ::pixel-dv) (i> ::viewport-v) ^double image-height)

           ;; multiple operand is complex to represent currently 
           (.create (i> ::temp) 0.0 0.0 focal-length)
           (.subtract (i> ::upper-left) (i> ::camera-center) (i> ::temp))
           (.divideScalar (i> ::temp) (i> ::viewport-u) 2.0)
           (.subtract (i> ::upper-left) (i> ::upper-left) (i> ::temp))
           (.divideScalar (i> ::temp) (i> ::viewport-v) 2.0)
           (.subtract (i> ::upper-left) (i> ::upper-left) (i> ::temp))

           (.add (i> ::temp) (i> ::pixel-du) (i> ::pixel-dv))
           (.divideScalar (i> ::temp) (i> ::temp) 2.0)
           (.add (i> ::pixel-00) (i> ::upper-left) (i> ::temp)))

         (prof/start)

         (let [image-height (double image-height)
               image-width (double image-width)]
           (loop [j 0.0 i 0.0]
             (when (< j image-height)
               (if (< i image-width)
                 (do (.create realm (i> ::pixel-color) 0.0 0.0 0.0)
                     (dotimes [_ samples-per-px]
                       (.copy realm (i> ::pixel-sample) (i> ::pixel-00))
                       (.multScalar realm (i> ::temp) (i> ::pixel-du) (+ i (- (math/random) 0.5)))
                       (.add realm (i> ::pixel-sample) (i> ::pixel-sample) (i> ::temp))
                       (.multScalar realm (i> ::temp) (i> ::pixel-dv) (+ j (- (math/random) 0.5)))
                       (.add realm (i> ::pixel-sample) (i> ::pixel-sample) (i> ::temp))

                       (.copy realm (i> ::ray-origin) (i> ::camera-center))
                       (.subtract realm (i> ::ray-direction) (i> ::pixel-sample) (i> ::ray-origin))

                       ;; pixel-sample is used as temp, this is some bug-prone way of programming huh
                       (.rayColor ^Ray a-ray realm hittables (i> ::pixel-sample) (i> ::ray-origin) (i> ::ray-direction))
                       (.add realm (i> ::pixel-color) (i> ::pixel-color) (i> ::pixel-sample)))
                     (.multScalar realm (long (* 3.0 (+ i (* j image-width)))) (i> ::pixel-color) pixel-scale)
                     (recur j (+ i 1.0)))
                 (recur (+ j 1.0) 0.0)))))))

    (prof/stop)

    (let [image-height (int image-height)
          image-width (int image-width)]
      (with-open [out (io/writer "scene-i.ppm")]
        (.write out (str "P3\n" image-width " " image-height "\n255\n"))
        (dotimes [j image-height]
          (dotimes [i image-width]
            (let [[r g b] (->> (.read realm (long (* 3 (+ i (* j image-width)))))
                               (map #(int (* 255.999 (double %)))))]
              (.write out (str r " " g " " b "\n")))))))))

