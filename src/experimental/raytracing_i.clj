(ns experimental.raytracing-i
  (:require
   [clojure.java.io :as io]
   [experimental.vec3i :as vec3i]))

(set! *warn-on-reflection* true)

;; hmm maybe this is a bad idea... but I want to do it....
(defn ray-at [realm target origin direction t]
  (-> realm
      (vec3i/mult-scalar! target direction t)
      (vec3i/add! target target origin)))

(defn hit-sphere [^doubles realm i> center-i radius ray-origin ray-direction]
  (-> realm (vec3i/subtract! (i> :temp) center-i ray-origin))
  (let [a (vec3i/length-squared realm ray-direction)
        h (vec3i/dot realm ray-direction (i> :temp))
        c (- (vec3i/dot realm (i> :temp) (i> :temp)) (* radius radius))
        discriminant (- (* h h) (* a c))]
    (when (>= discriminant 0)
      (/ (- h (Math/sqrt discriminant)) a))))

(defn ray-color! [^doubles realm i> target ray-origin ray-direction]
  (if-let [t (hit-sphere realm i> (i> :sphere-1) 0.5 ray-origin ray-direction)]
    (-> realm
        (ray-at (i> :temp) ray-origin ray-direction t)
        (vec3i/subtract! (i> :temp) (i> :temp) 0.0 0.0 -1.0)
        (vec3i/unit-vec3! (i> :temp) (i> :temp))
        (vec3i/add! (i> :temp) (i> :temp) 1.0 1.0 1.0)
        (vec3i/mult-scalar! (i> :temp) (i> :temp) 0.5)
        (vec3i/copy! target (i> :temp)))
    (do (vec3i/unit-vec3! realm (i> :temp) ray-direction)
        (let [y (vec3i/y realm (i> :temp))
              a (* 0.5 (+ y 1.0))
              r (+ (* (- 1.0 a) 1.0) (* a 0.5))
              g (+ (* (- 1.0 a) 1.0) (* a 0.7))
              b (+ (* (- 1.0 a) 1.0) (* a 1.0))]
          (vec3i/create! realm target r g b)))))

(defn create-i> [globals offset]
  (into {} (map-indexed (fn [i v] [v (+ offset i)])) globals))

(defn -main []
  (let [aspect-ratio    16/9
        image-width     400
        image-height    (int (/ image-width aspect-ratio))

        focal-length    1.0
        viewport-height 2.0
        viewport-width  (* viewport-height (/ image-width image-height))

        pixel-count     (* image-width image-height)
        i>              (create-i>
                         [;; global vectors
                          :camera-center
                          :viewport-u
                          :viewport-v
                          :pixel-du
                          :pixel-dv
                          :upper-left
                          :pixel-00

                          ;; loop vectors
                          :pixel-center
                          :ray-direction
                          :pixel-color

                          :sphere-1
                          :temp]
                         pixel-count)

        realm-size      (* (+ pixel-count (count i>)) 3)
        ^doubles realm  (make-array Double/TYPE realm-size)]

    (time
     (do (-> realm
             (vec3i/create! (i> :sphere-1) 0.0 0.0 -1.0)

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
             (-> realm
                 (vec3i/copy! (i> :pixel-center) (i> :pixel-00))
                 (vec3i/mult-scalar! (i> :temp) (i> :pixel-du) i)
                 (vec3i/add! (i> :pixel-center) (i> :pixel-center) (i> :temp))
                 (vec3i/mult-scalar! (i> :temp) (i> :pixel-dv) j)
                 (vec3i/add! (i> :pixel-center) (i> :pixel-center) (i> :temp))

                 (vec3i/subtract! (i> :ray-direction) (i> :pixel-center) (i> :camera-center))

                 (ray-color! i> (i> :pixel-color) (i> :camera-center) (i> :ray-direction))

                 (vec3i/copy! (long (+ i (* j image-width))) (i> :pixel-color)))))))

    (with-open [out (io/writer "scene-i.ppm")]
      (.write out (str "P3\n" image-width " " image-height "\n255\n"))
      (dotimes [j image-height]
        (dotimes [i image-width]
          (let [[r g b] (->> (vec3i/read! realm (long (+ i (* j image-width))))
                             (map #(int (* 255.999 %))))]
            (.write out (str r " " g " " b "\n"))))))))

