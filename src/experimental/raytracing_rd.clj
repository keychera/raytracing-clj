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

;; macro to avoid reflection
(defmacro ray-at [realm target-i origin-i direction-i t]
  `(do (.multScalar ^experimental.vec3_realm.Realm ~realm ~target-i ~direction-i ~t)
       (.add ^experimental.vec3_realm.Realm ~realm ~target-i ~target-i ~origin-i)))

(definterface Hittable
  (^double hit [^experimental.vec3_realm.Realm realm i> ^long ray-origin ^long ray-direction ^double t-min ^double t-max]))

(deftype Sphere [^long center-i ^double radius]
  Hittable
  (hit [_this realm i> ray-origin ray-direction t-min t-max]
    (let [temp-i       (long (i> ::temp))
          hit-point-i  (long (i> ::hit-point))
          hit-normal-i (long (i> ::hit-normal))]
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
  (^double hitAnything [^experimental.vec3_realm.Realm realm i> hittables ^long ray-origin ^long ray-direction ^double t-min ^double t-max])
  (rayColor [^experimental.vec3_realm.Realm realm i> hittables ^long target ^long ray-origin ^long ray-direction]))

(def a-ray
  (reify Ray
    (hitAnything [_this realm i> hittables ray-origin ray-direction t-min t-max]
      (let [length (count hittables)]
        (loop [i 0 found? false closest-so-far t-max]
          (if (< i length)
            (let [^Hittable hittable (:hittable (nth hittables i))
                  t        (.hit hittable realm i> ray-origin ray-direction t-min closest-so-far)]
              (if (> t 0.0)
                (recur (inc i) true t)
                (recur (inc i) found? closest-so-far)))
            (if found? closest-so-far -1.0)))))
    (rayColor [this realm i> hittables target ray-origin ray-direction]
      (let [t (.hitAnything this realm i> hittables ray-origin ray-direction 1e-3 ##Inf)]
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

(defn create-i> [globals offset]
  (let [offset (long offset)]
    (into {} (map-indexed (fn [i v] [v (* 3 (+ offset (long i)))])) globals)))

(comment
  (require '[clj-java-decompiler.core :refer [decompile]])

  (binding [*compiler-options* {:disable-locals-clearing false}]
    (spit ".zzz/This.java"
          (with-out-str
            (decompile
             (reify Ray
               (hitAnything [_this realm i> hittables ray-origin ray-direction t-min t-max]
                 (let [length (count hittables)]
                   (loop [i 0 found? false closest-so-far t-max]
                     (if (< i length)
                       (let [^Hittable hittable (:hittable (nth hittables i))
                             t        (.hit hittable realm i> ray-origin ray-direction t-min closest-so-far)]
                         (if (> t 0.0)
                           (recur (inc i) true t)
                           (recur (inc i) found? closest-so-far)))
                       (if found? closest-so-far -1.0)))))
               
               (rayColor [this realm i> hittables target ray-origin ray-direction]
                 (let [t (.hitAnything this realm i> hittables ray-origin ray-direction 1e-3 ##Inf)]
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
                           (.create realm target r g b))))))))))))


(defn -main []
  (prof/profile
   {:event :alloc}
   (let [aspect-ratio    (double 16/9)
         image-width     400
         image-height    (int (/ image-width aspect-ratio))
         samples-per-px  100
         pixel-scale     (/ 1 samples-per-px)

         focal-length    1.0
         viewport-height 2.0
         viewport-width  (* viewport-height (double (/ image-width image-height)))

         pixel-count     (* image-width image-height)
         global-vecs     [::camera-center
                          ::viewport-u
                          ::viewport-v
                          ::pixel-du
                          ::pixel-dv
                          ::upper-left
                          ::pixel-00]
         spheres         [::sphere-1
                          ::sphere-2]
         loop-vecs       [::pixel-sample
                          ::ray-origin
                          ::ray-direction
                          ::pixel-color]
         temporaries     [::temp
                          ::hit-point
                          ::hit-normal]
         i>              (create-i>
                          (concat global-vecs
                                  loop-vecs
                                  spheres
                                  temporaries)
                          pixel-count)
         realm-size      (* (+ pixel-count (count i>)) 3)
         ^Realm realm    (Realm. (make-array Double/TYPE realm-size))

         sphere-1        (let [circle-i (i> ::sphere-1)]
                           (.create realm circle-i 0.0 0.0 -1.0)
                           {:hittable (Sphere. circle-i 0.5)})
         sphere-2        (let [circle-i (i> ::sphere-2)]
                           (.create realm circle-i 0.0 -100.5 -1.0)
                           {:hittable (Sphere. circle-i 100.0)})
         hittables       [sphere-1 sphere-2]]

     (time #_criterium/bench
      (do (doto realm
            (.create (i> ::camera-center) 0.0 0.0 0.0)
            (.create (i> ::viewport-u) viewport-width 0.0 0.0)
            (.create (i> ::viewport-v) 0.0 (- viewport-height) 0.0)
            (.divideScalar (i> ::pixel-du) (i> ::viewport-u) image-width)
            (.divideScalar (i> ::pixel-dv) (i> ::viewport-v) image-height)

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

          (dotimes [j image-height]
            (dotimes [i image-width]
              (.create realm (i> ::pixel-color) 0.0 0.0 0.0)
              (dotimes [_ samples-per-px]
                (.copy realm (i> ::pixel-sample) (i> ::pixel-00))
                (.multScalar realm (i> ::temp) (i> ::pixel-du) (+ i (- (math/random) 0.5)))
                (.add realm (i> ::pixel-sample) (i> ::pixel-sample) (i> ::temp))
                (.multScalar realm (i> ::temp) (i> ::pixel-dv) (+ j (- (math/random) 0.5)))
                (.add realm (i> ::pixel-sample) (i> ::pixel-sample) (i> ::temp))

                (.copy realm (i> ::ray-origin) (i> ::camera-center))
                (.subtract realm (i> ::ray-direction) (i> ::pixel-sample) (i> ::ray-origin))

                ;; pixel-sample is used as temp, this is some bug-prone way of programming huh
                (.rayColor ^Ray a-ray realm i> hittables (i> ::pixel-sample) (i> ::ray-origin) (i> ::ray-direction))
                (.add realm (i> ::pixel-color) (i> ::pixel-color) (i> ::pixel-sample)))
              (.multScalar realm (long (* 3 (+ i (* j image-width)))) (i> ::pixel-color) pixel-scale)))))

     (with-open [out (io/writer "scene-i.ppm")]
       (.write out (str "P3\n" image-width " " image-height "\n255\n"))
       (dotimes [j image-height]
         (dotimes [i image-width]
           (let [[r g b] (->> (.read realm (long (* 3 (+ i (* j image-width)))))
                              (map #(int (* 255.999 (double %)))))]
             (.write out (str r " " g " " b "\n")))))))))

