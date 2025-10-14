(ns raytracing
  (:require
   [clojure.java.io :as io]
   [ray :as ray]
   ;; this is weird, calva repl is fine with just [ray] but `bb -m raytracing` fails and need [ray :as ray]
   [vec3]))

;; following https://raytracing.github.io/books/RayTracingInOneWeekend.html

(defn write-color! [out color]
  (let [[r g b] (mapv #(int (* 255 %)) color)]
    (.write out (str r " " g " " b "\n"))))

(defn -main []
  (time
   (let [;; image
         aspect-ratio    16/9
         image-width     400
         image-height    (int (/ image-width aspect-ratio))

         ;; camera
         focal-length    1.0
         viewport-height 2.0
         ;; not using aspect-ratio is deliberate here
         viewport-width  (* viewport-height (/ image-width image-height))
         camera-center   [0 0 0]
         viewport-u      [viewport-width 0 0]
         viewport-v      [0 (- viewport-height) 0] ;; negative because we want upper-left to be zero and increases at we scan down
         pixel-du        (vec3/divide viewport-u image-width)
         pixel-dv        (vec3/divide viewport-v image-height)
         upper-left      (-> camera-center
                             (vec3/subtract [0 0 focal-length])
                             (vec3/subtract (vec3/divide viewport-u 2))
                             (vec3/subtract (vec3/divide viewport-v 2)))
         pixel-00-loc    (vec3/add upper-left (vec3/multiply (vec3/add pixel-du pixel-dv) 0.5))]
     (with-open [out (io/writer "scene.ppm")]
       (.write out (str "P3\n" image-width " " image-height "\n255\n"))
       (doall
        (for [j (range image-height)
              i (range image-width)]
          (let [pixel-center  (-> pixel-00-loc
                                  (vec3/add (vec3/multiply pixel-du i))
                                  (vec3/add (vec3/multiply pixel-dv j)))
                ray-direction (vec3/subtract pixel-center camera-center)
                a-ray         #::ray{:origin camera-center :direction ray-direction}
                color         (ray/color a-ray)]
            (write-color! out color)))))
     :done)))

