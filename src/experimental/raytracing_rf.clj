(ns experimental.raytracing-rf
  (:require
   #_[clj-async-profiler.core :as prof]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [experimental.vec3_realmf]
   [criterium.core :as criterium])
  (:import [experimental.vec3_realmf Realm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defmacro fv [v] `(float ~v))

;; hmm maybe this is a bad idea... but I want to do it....
(defn ray-at [realm target origin direction t]
  (doto ^Realm realm
    (.multScalar target direction t)
    (.add target target origin)))

(definterface Hittable
  (^float hit [^experimental.vec3_realmf.Realm realm i> ray-origin ray-direction ^float t-min ^float t-max]))

(deftype Sphere [center-i ^float radius]
  Hittable
  (^float hit [_this ^experimental.vec3_realmf.Realm realm i> ray-origin ray-direction ^float t-min ^float t-max]
    (doto realm (.subtract (i> :temp) center-i ray-origin))
    (let [a (.lengthSquared realm ray-direction)
          h (.dot realm ray-direction (i> :temp))
          c (- (.dot realm (i> :temp) (i> :temp)) (* radius radius))
          discriminant (- (* h h) (* a c))]
      (if (< discriminant (fv 0.0))
        (fv -1.0)
        (let [sqrt-d (fv (Math/sqrt discriminant))
              root   (let [root' (/ (- h sqrt-d) a)]
                       (if (or (<= root' t-min) (<= t-max root'))
                         (/ (+ h sqrt-d) a)
                         root'))]
          (if (or (<= root t-min) (<= t-max root))
            (fv -1.0)
            (do (doto realm
                  (ray-at (i> :hit-point) ray-origin ray-direction root)
                  (.subtract (i> :temp) (i> :hit-point) center-i)
                  (.divideScalar (i> :hit-normal) (i> :temp) radius))
                (let [front-face? (< (.dot realm ray-direction (i> :hit-normal)) (fv 0))]
                  (when (not front-face?)
                    (.multScalar realm (i> :hit-normal) (i> :hit-normal) (fv -1))))
                root)))))
    #_"this mutates :hit-normal and :hit-point"))

(defn hit-anything! [realm i> ray-origin ray-direction hittables t-min t-max]
  (let [length (count hittables)]
    (loop [i 0 found? false ^float closest-so-far t-max]
      (if (< i length)
        (let [^Hittable hittable (:hittable (nth hittables i))
              t        (fv (.hit hittable realm i> ray-origin ray-direction t-min (or closest-so-far t-max)))]
          (if (> t (fv 0.0))
            (recur (inc i) true t)
            (recur (inc i) found? closest-so-far)))
        (if found? closest-so-far -1.0)))))

(defn ray-color! [realm i> target ray-origin ray-direction hittables]
  (let [^Realm realm realm
        t     (fv (hit-anything! realm i> ray-origin ray-direction hittables (fv 1e-3) Float/MAX_VALUE))
        one   (fv 1.0)]
    (if (> t (fv 0.0))
      (doto realm
        (.create (i> :temp) one one one)
        (.add (i> :temp) (i> :hit-normal) (i> :temp))
        (.multScalar (i> :temp) (i> :temp) (fv 0.5))
        (.copy target (i> :temp)))
      (do (.unitVec3 realm (i> :temp) ray-direction)
          (let [y (.y realm (i> :temp))
                a (* (fv 0.5) (+ y (fv 1.0)))
                r (+ (* (- (fv 1.0) a) (fv 1.0)) (* a (fv 0.5)))
                g (+ (* (- (fv 1.0) a) (fv 1.0)) (* a (fv 0.7)))
                b (+ (* (- (fv 1.0) a) (fv 1.0)) (* a (fv 1.0)))]
            (.create realm target r g b))))))

(defn create-i> [globals offset]
  (let [offset (long offset)]
    (into {} (map-indexed (fn [i v] [v (* 3 (+ offset (long i)))])) globals)))

(defn -main []
  (do #_#_prof/profile
        {:event :alloc}
   (let [aspect-ratio    (fv 16/9)
         image-width     400
         image-height    (int (/ image-width aspect-ratio))
         samples-per-px  100
         pixel-scale     (/ 1 samples-per-px)

         focal-length    1.0
         viewport-height (fv 2.0)
         viewport-width  (* viewport-height (fv (/ image-width image-height)))

         pixel-count     (* image-width image-height)
         global-vecs     [:camera-center
                          :viewport-u
                          :viewport-v
                          :pixel-du
                          :pixel-dv
                          :upper-left
                          :pixel-00]
         loop-vecs       [::pixel-sample
                          ::ray-origin
                          ::ray-direction
                          ::pixel-color]
         spheres         [:sphere-1
                          :sphere-2]
         temporaries     [:temp
                          :hit-point
                          :hit-normal]
         i>              (create-i>
                          (concat global-vecs
                                  loop-vecs
                                  spheres
                                  temporaries)
                          pixel-count)
         realm-size      (* (+ pixel-count (count i>)) 3)
         ^Realm realm    (Realm. (make-array Float/TYPE realm-size))

         sphere-1        (let [circle-i (i> :sphere-1)]
                           (.create realm circle-i 0.0 0.0 -1.0)
                           {:hittable (Sphere. circle-i 0.5)})
         sphere-2        (let [circle-i (i> :sphere-2)]
                           (.create realm circle-i 0.0 -100.5 -1.0)
                           {:hittable (Sphere. circle-i 100.0)})
         hittables       [sphere-1 sphere-2]]

     (criterium/bench
      (do (doto realm
            (.create (i> :camera-center) 0.0 0.0 0.0)
            (.create (i> :viewport-u) viewport-width 0.0 0.0)
            (.create (i> :viewport-v) 0.0 (- viewport-height) 0.0)
            (.divideScalar (i> :pixel-du) (i> :viewport-u) image-width)
            (.divideScalar (i> :pixel-dv) (i> :viewport-v) image-height)

            ;; multiple operand is complex to represent currently 
            (.create (i> :temp) 0.0 0.0 focal-length)
            (.subtract (i> :upper-left) (i> :camera-center) (i> :temp))
            (.divideScalar (i> :temp) (i> :viewport-u) 2.0)
            (.subtract (i> :upper-left) (i> :upper-left) (i> :temp))
            (.divideScalar (i> :temp) (i> :viewport-v) 2.0)
            (.subtract (i> :upper-left) (i> :upper-left) (i> :temp))

            (.add (i> :temp) (i> :pixel-du) (i> :pixel-dv))
            (.divideScalar (i> :temp) (i> :temp) 2.0)
            (.add (i> :pixel-00) (i> :upper-left) (i> :temp)))

          (dotimes [j image-height]
            (dotimes [i image-width]
              (.create realm (i> ::pixel-color) 0.0 0.0 0.0)
              (dotimes [_ samples-per-px]
                (doto realm
                  (.copy (i> ::pixel-sample) (i> :pixel-00))
                  (.multScalar (i> :temp) (i> :pixel-du) (+ i (- (math/random) 0.5)))
                  (.add (i> ::pixel-sample) (i> ::pixel-sample) (i> :temp))
                  (.multScalar (i> :temp) (i> :pixel-dv) (+ j (- (math/random) 0.5)))
                  (.add (i> ::pixel-sample) (i> ::pixel-sample) (i> :temp))

                  (.copy (i> ::ray-origin) (i> :camera-center))
                  (.subtract (i> ::ray-direction) (i> ::pixel-sample) (i> ::ray-origin))

                  ;; pixel-sample is used as temp, this is some bug-prone way of programming huh
                  (ray-color! i> (i> ::pixel-sample) (i> ::ray-origin) (i> ::ray-direction) hittables)
                  (.add (i> ::pixel-color) (i> ::pixel-color) (i> ::pixel-sample))))
              (.multScalar realm (long (* 3 (+ i (* j image-width)))) (i> ::pixel-color) pixel-scale)))))

     (with-open [out (io/writer "scene-i.ppm")]
       (.write out (str "P3\n" image-width " " image-height "\n255\n"))
       (dotimes [j image-height]
         (dotimes [i image-width]
           (let [[r g b] (->> (.read realm (long (* 3 (+ i (* j image-width)))))
                              (map #(int (* 255.999 (fv %)))))]
             (.write out (str r " " g " " b "\n")))))))))

