# raytracing in not exactly one weekend

https://raytracing.github.io/books/RayTracingInOneWeekend.html

## current progress

![raytraced scene](scene.png)

## run

use clojure

```
clojure -M:main
```

using [babashka](https://book.babashka.org/#getting_started) is possible but significantly slower 
(need to lower sample-per-px and hit depth for faster calculation)

```
bb -m raytracing
```
