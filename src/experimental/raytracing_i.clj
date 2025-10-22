(ns experimental.raytracing-i
  (:require
   #_[clj-async-profiler.core :as prof]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [experimental.vec3i :as vec3i]
   [criterium.core :as criterium]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; hmm maybe this is a bad idea... but I want to do it....
(defn ray-at [realm target origin direction t]
  (doto realm
    (vec3i/mult-scalar! target direction t)
    (vec3i/add! target target origin)))

(definterface Hittable
  (^double hit [^doubles realm i> ray-origin ray-direction ^double t-min ^double t-max]))

(deftype Sphere [center-i ^double radius]
  Hittable
  (^double hit [_this ^doubles realm i> ray-origin ray-direction ^double t-min ^double t-max]
    (doto realm (vec3i/subtract! (i> :temp) center-i ray-origin))
    (let [a (double (vec3i/length-squared realm ray-direction))
          h (vec3i/dot realm ray-direction (i> :temp))
          c (- (vec3i/dot realm (i> :temp) (i> :temp)) (* radius radius))
          discriminant (- (* h h) (* a c))]
      (if (< discriminant 0.0)
        -1.0
        (let [sqrt-d (Math/sqrt discriminant)
              root   (let [root' (/ (- h sqrt-d) a)]
                       (if (or (<= root' t-min) (<= t-max root'))
                         (/ (+ h sqrt-d) a)
                         root'))]
          (if (or (<= root t-min) (<= t-max root))
            -1.0
            (do (doto realm
                  (ray-at (i> :hit-point) ray-origin ray-direction root)
                  (vec3i/subtract! (i> :temp) (i> :hit-point) center-i)
                  (vec3i/divide! (i> :hit-normal) (i> :temp) radius))
                (let [front-face? (< (vec3i/dot realm ray-direction (i> :hit-normal)) 0)]
                  (when (not front-face?)
                    (vec3i/mult-scalar! realm (i> :hit-normal) (i> :hit-normal) -1)))
                root)))))
    #_"this mutates :hit-normal and :hit-point"))

(defn hit-anything! [realm i> ray-origin ray-direction hittables t-min t-max]
  (let [length (count hittables)]
    (loop [i 0 found? false ^double closest-so-far t-max]
      (if (< i length)
        (let [^Hittable hittable (:hittable (nth hittables i))
              t        (.hit hittable realm i> ray-origin ray-direction t-min (or closest-so-far t-max))]
          (if (> t 0.0)
            (recur (inc i) true t)
            (recur (inc i) found? closest-so-far)))
        (if found? closest-so-far -1.0)))))

(defn ray-color! [^doubles realm i> target ray-origin ray-direction hittables]
  (let [^double t (hit-anything! realm i> ray-origin ray-direction hittables 1e-3 ##Inf)]
    (if (> t 0.0)
      (doto realm
        (vec3i/create! (i> :temp) 1.0 1.0 1.0)
        (vec3i/add! (i> :temp) (i> :hit-normal) (i> :temp))
        (vec3i/mult-scalar! (i> :temp) (i> :temp) 0.5)
        (vec3i/copy! target (i> :temp)))
      (do (vec3i/unit-vec3! realm (i> :temp) ray-direction)
          (let [y (vec3i/y realm (i> :temp))
                a (* 0.5 (+ y 1.0))
                r (+ (* (- 1.0 a) 1.0) (* a 0.5))
                g (+ (* (- 1.0 a) 1.0) (* a 0.7))
                b (+ (* (- 1.0 a) 1.0) (* a 1.0))]
            (vec3i/create! realm target r g b))))))

(defn create-i> [globals offset]
  (let [offset (long offset)]
    (into {} (map-indexed (fn [i v] [v (* 3 (+ offset (long i)))])) globals)))

(defn -main []
  (do #_#_prof/profile
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
         ^doubles realm  (make-array Double/TYPE realm-size)

         sphere-1        (let [circle-i (i> :sphere-1)]
                           (vec3i/create! realm circle-i 0.0 0.0 -1.0)
                           {:hittable (Sphere. circle-i 0.5)})
         sphere-2        (let [circle-i (i> :sphere-2)]
                           (vec3i/create! realm circle-i 0.0 -100.5 -1.0)
                           {:hittable (Sphere. circle-i 100.0)})
         hittables       [sphere-1 sphere-2]]

     (criterium/bench
      (do (doto realm
            (vec3i/create! (i> :camera-center) 0.0 0.0 0.0)
            (vec3i/create! (i> :viewport-u) viewport-width 0.0 0.0)
            (vec3i/create! (i> :viewport-v) 0.0 (- viewport-height) 0.0)
            (vec3i/divide! (i> :pixel-du) (i> :viewport-u) image-width)
            (vec3i/divide! (i> :pixel-dv) (i> :viewport-v) image-height)

            ;; multiple operand is complex to represent currently 
            (vec3i/create! (i> :temp) 0.0 0.0 focal-length)
            (vec3i/subtract! (i> :upper-left) (i> :camera-center) (i> :temp))
            (vec3i/divide! (i> :temp) (i> :viewport-u) 2.0)
            (vec3i/subtract! (i> :upper-left) (i> :upper-left) (i> :temp))
            (vec3i/divide! (i> :temp) (i> :viewport-v) 2.0)
            (vec3i/subtract! (i> :upper-left) (i> :upper-left) (i> :temp))

            (vec3i/add! (i> :temp) (i> :pixel-du) (i> :pixel-dv))
            (vec3i/divide! (i> :temp) (i> :temp) 2.0)
            (vec3i/add! (i> :pixel-00) (i> :upper-left) (i> :temp)))

          (dotimes [j image-height]
            (dotimes [i image-width]
              (vec3i/create! realm (i> ::pixel-color) 0.0 0.0 0.0)
              (dotimes [_ samples-per-px]
                (doto realm
                  (vec3i/copy! (i> ::pixel-sample) (i> :pixel-00))
                  (vec3i/mult-scalar! (i> :temp) (i> :pixel-du) (+ i (- (math/random) 0.5)))
                  (vec3i/add! (i> ::pixel-sample) (i> ::pixel-sample) (i> :temp))
                  (vec3i/mult-scalar! (i> :temp) (i> :pixel-dv) (+ j (- (math/random) 0.5)))
                  (vec3i/add! (i> ::pixel-sample) (i> ::pixel-sample) (i> :temp))

                  (vec3i/copy! (i> ::ray-origin) (i> :camera-center))
                  (vec3i/subtract! (i> ::ray-direction) (i> ::pixel-sample) (i> ::ray-origin))

                  ;; pixel-sample is used as temp, this is some bug-prone way of programming huh
                  (ray-color! i> (i> ::pixel-sample) (i> ::ray-origin) (i> ::ray-direction) hittables)
                  (vec3i/add! (i> ::pixel-color) (i> ::pixel-color) (i> ::pixel-sample))))
              (vec3i/mult-scalar! realm (long (* 3 (+ i (* j image-width)))) (i> ::pixel-color) pixel-scale)))))

     (with-open [out (io/writer "scene-i.ppm")]
       (.write out (str "P3\n" image-width " " image-height "\n255\n"))
       (dotimes [j image-height]
         (dotimes [i image-width]
           (let [[r g b] (->> (vec3i/read! realm (long (* 3 (+ i (* j image-width)))))
                              (map #(int (* 255.999 (double %)))))]
             (.write out (str r " " g " " b "\n")))))))))

