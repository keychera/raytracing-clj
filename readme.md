# raytracing in not exactly one weekend

https://raytracing.github.io/books/RayTracingInOneWeekend.html

## current progress

![raytraced scene](scene.png)

## run

use clojure

```
clojure -M:main
```


## note

- still doesn't understand parallelism here, the faster is when I give it size 2 threadpool, why?


## raytracing2 + vec3i

vec3i = vector operation that lives on a single preallocated contiguous primitive array

I dont know if this is a good idea, gonna try and test my understanding

### criterium/bench

raytracing-i
```
Evaluation count : 60 in 60 samples of 1 calls.
             Execution time mean : 3.921437 sec
    Execution time std-deviation : 22.136790 ms
   Execution time lower quantile : 3.887313 sec ( 2.5%)
   Execution time upper quantile : 3.962837 sec (97.5%)
                   Overhead used : 5.663971 ns

Found 1 outliers in 60 samples (1.6667 %)
        low-severe       1 (1.6667 %)
 Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
```

raytracing-rf
```
Evaluation count : 60 in 60 samples of 1 calls.
             Execution time mean : 3.336118 sec
    Execution time std-deviation : 88.058390 ms
   Execution time lower quantile : 3.258130 sec ( 2.5%)
   Execution time upper quantile : 3.586640 sec (97.5%)
                   Overhead used : 7.218311 ns

Found 3 outliers in 60 samples (5.0000 %)
        low-severe       3 (5.0000 %)
 Variance from outliers : 14.1590 % Variance is moderately inflated by outliers
```