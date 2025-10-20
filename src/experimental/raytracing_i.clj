(ns experimental.raytracing-i
  (:require
   [clojure.java.io :as io]
   [experimental.vec3i :as vec3i]))

(defn -main []
  (let [image-width  256.0
        image-height 256.0
        realm        (make-array Double/TYPE (* image-width image-height 3))]
    (dotimes [j image-height]
      (dotimes [i image-width]
        (vec3i/create! realm (long (+ i (* j image-width))) (/ i image-width) (/ j image-width) 0.0)))
    (with-open [out (io/writer "scene-i.ppm")]
      (.write out (str "P3\n" image-width " " image-height "\n255\n"))
      (dotimes [j image-height]
        (dotimes [i image-width]
          (let [[r g b] (->> (vec3i/read! realm (long (+ i (* j image-width))))
                             (map #(int (* 255.999 %))))]
            (.write out (str r " " g " " b "\n"))))))))

