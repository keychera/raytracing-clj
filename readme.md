# raytracing in not exactly one weekend

https://raytracing.github.io/books/RayTracingInOneWeekend.html

## current progress

![raytraced scene](scene.png)

## run

use clojure

```
clojure -M:main
```

using [babashka](https://book.babashka.org/#getting_started) is possible 
but significantly slower and lower render quality
(using lower sample-per-px and max-depth)

```
bb -m raytracing 5 2
```


## rough measurement

in my machine, the scene for commit c224224477a7e6dea5cec4906dcdf0159281196d took:

```
clojure,  {:samples-per-px 100, :max-depth 50} : 15793.215 msecs
babashka, {:samples-per-px 5,   :max-depth 2}  : 285660.4118 msecs
```

(I wonder why bb is significantly slower?)