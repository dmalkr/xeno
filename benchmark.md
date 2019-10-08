# Introduction

There are two issues with old `xeno` benchmarks:

* They run on threaded Haskell RTS (compiled with `-threaded`). Threading lead to very big variance in benchmarks
  (sometimes R² is less then 0.8). Therefore threaded benchmarking is not accurate.
* They run on relatively small files. Some speed improvements become apparent only on big files (more then 100 Mb) and
  benchmarking with small files does not detect an effect of all improvements.

So here are several benchmarks.

## Hardware
* CPU: AMD Ryzen 9 3900X 12-Core Processor with 4200MHz
* Memory: 32 Gb (4 banks),  2400MHz
* Motherboard: ASRock X570 Phantom Gaming X

# Benchmark with small files

In this section we present results of original `xeno` benchmarks, runned in two modes: with threaded support and without
threaded support.

## Threaded environment

Compiled with `ghc-options: -O2 -threaded -rtsopts "-with-rtsopts=-N"`

### Current benchmarks

Short version:

```
benchmarking 4KB/xeno-sax
time                 2.836 μs   (2.824 μs .. 2.846 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.837 μs   (2.830 μs .. 2.842 μs)
std dev              19.04 ns   (15.24 ns .. 24.36 ns)

benchmarking 4KB/xeno-dom
time                 5.766 μs   (5.757 μs .. 5.775 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.752 μs   (5.742 μs .. 5.759 μs)
std dev              27.27 ns   (17.17 ns .. 48.07 ns)

benchmarking 4KB/xeno-dom-with-recovery
time                 8.216 μs   (8.196 μs .. 8.241 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.222 μs   (8.207 μs .. 8.237 μs)
std dev              50.45 ns   (42.55 ns .. 60.33 ns)

benchmarking 31KB/xeno-sax
time                 1.919 μs   (1.911 μs .. 1.932 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.941 μs   (1.932 μs .. 1.955 μs)
std dev              35.39 ns   (23.10 ns .. 65.33 ns)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 31KB/xeno-dom
time                 3.305 μs   (3.292 μs .. 3.318 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.290 μs   (3.284 μs .. 3.297 μs)
std dev              23.01 ns   (17.78 ns .. 31.51 ns)

benchmarking 31KB/xeno-dom-with-recovery
time                 4.583 μs   (4.563 μs .. 4.606 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.599 μs   (4.588 μs .. 4.610 μs)
std dev              39.26 ns   (33.63 ns .. 47.23 ns)

benchmarking 31KB/xml-dom
time                 6.549 ms   (6.331 ms .. 6.851 ms)
                     0.991 R²   (0.987 R² .. 0.996 R²)
mean                 6.743 ms   (6.618 ms .. 6.860 ms)
std dev              336.6 μs   (286.8 μs .. 395.1 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking 211KB/xeno-sax
time                 139.3 μs   (138.7 μs .. 139.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 138.9 μs   (138.6 μs .. 139.2 μs)
std dev              1.119 μs   (964.9 ns .. 1.333 μs)

benchmarking 211KB/xeno-dom
time                 266.8 μs   (265.7 μs .. 268.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 267.7 μs   (266.6 μs .. 268.5 μs)
std dev              3.359 μs   (2.898 μs .. 3.984 μs)

benchmarking 211KB/xeno-dom-with-recovery
time                 389.8 μs   (388.5 μs .. 391.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 391.3 μs   (390.3 μs .. 392.6 μs)
std dev              4.065 μs   (3.037 μs .. 6.029 μs)

benchmarking 211KB/xml-dom
time                 63.70 ms   (59.51 ms .. 67.15 ms)
                     0.995 R²   (0.992 R² .. 0.999 R²)
mean                 59.64 ms   (58.40 ms .. 61.05 ms)
std dev              2.512 ms   (1.622 ms .. 4.014 ms)

```

Full version:


```
benchmarking 4KB/hexml-dom
time                 3.325 μs   (3.306 μs .. 3.348 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.335 μs   (3.328 μs .. 3.345 μs)
std dev              27.46 ns   (16.57 ns .. 46.75 ns)

benchmarking 4KB/xeno-sax
time                 2.836 μs   (2.824 μs .. 2.846 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.837 μs   (2.830 μs .. 2.842 μs)
std dev              19.04 ns   (15.24 ns .. 24.36 ns)

benchmarking 4KB/xeno-dom
time                 5.766 μs   (5.757 μs .. 5.775 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.752 μs   (5.742 μs .. 5.759 μs)
std dev              27.27 ns   (17.17 ns .. 48.07 ns)

benchmarking 4KB/xeno-dom-with-recovery
time                 8.216 μs   (8.196 μs .. 8.241 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.222 μs   (8.207 μs .. 8.237 μs)
std dev              50.45 ns   (42.55 ns .. 60.33 ns)

benchmarking 4KB/hexpat-sax
time                 47.99 μs   (47.83 μs .. 48.15 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 47.89 μs   (47.78 μs .. 48.04 μs)
std dev              430.4 ns   (326.5 ns .. 549.4 ns)

benchmarking 4KB/hexpat-dom
time                 133.5 μs   (133.1 μs .. 134.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 133.7 μs   (133.4 μs .. 134.1 μs)
std dev              1.104 μs   (870.1 ns .. 1.362 μs)

benchmarking 4KB/xml-dom
time                 980.3 μs   (974.5 μs .. 986.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 990.6 μs   (986.5 μs .. 997.2 μs)
std dev              17.26 μs   (12.75 μs .. 23.09 μs)

benchmarking 31KB/hexml-dom
time                 2.820 μs   (2.813 μs .. 2.826 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.819 μs   (2.810 μs .. 2.826 μs)
std dev              27.95 ns   (22.14 ns .. 35.34 ns)

benchmarking 31KB/xeno-sax
time                 1.919 μs   (1.911 μs .. 1.932 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.941 μs   (1.932 μs .. 1.955 μs)
std dev              35.39 ns   (23.10 ns .. 65.33 ns)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 31KB/xeno-dom
time                 3.305 μs   (3.292 μs .. 3.318 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.290 μs   (3.284 μs .. 3.297 μs)
std dev              23.01 ns   (17.78 ns .. 31.51 ns)

benchmarking 31KB/xeno-dom-with-recovery
time                 4.583 μs   (4.563 μs .. 4.606 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.599 μs   (4.588 μs .. 4.610 μs)
std dev              39.26 ns   (33.63 ns .. 47.23 ns)

benchmarking 31KB/hexpat-sax
time                 167.2 μs   (166.8 μs .. 167.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 167.4 μs   (167.0 μs .. 168.2 μs)
std dev              1.757 μs   (1.013 μs .. 3.195 μs)

benchmarking 31KB/hexpat-dom
time                 172.3 μs   (171.8 μs .. 172.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 173.2 μs   (172.7 μs .. 173.6 μs)
std dev              1.480 μs   (1.258 μs .. 1.752 μs)

benchmarking 31KB/xml-dom
time                 6.549 ms   (6.331 ms .. 6.851 ms)
                     0.991 R²   (0.987 R² .. 0.996 R²)
mean                 6.743 ms   (6.618 ms .. 6.860 ms)
std dev              336.6 μs   (286.8 μs .. 395.1 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking 211KB/hexml-dom
time                 120.5 μs   (120.1 μs .. 121.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 120.7 μs   (120.4 μs .. 121.1 μs)
std dev              1.263 μs   (1.002 μs .. 1.639 μs)

benchmarking 211KB/xeno-sax
time                 139.3 μs   (138.7 μs .. 139.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 138.9 μs   (138.6 μs .. 139.2 μs)
std dev              1.119 μs   (964.9 ns .. 1.333 μs)

benchmarking 211KB/xeno-dom
time                 266.8 μs   (265.7 μs .. 268.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 267.7 μs   (266.6 μs .. 268.5 μs)
std dev              3.359 μs   (2.898 μs .. 3.984 μs)

benchmarking 211KB/xeno-dom-with-recovery
time                 389.8 μs   (388.5 μs .. 391.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 391.3 μs   (390.3 μs .. 392.6 μs)
std dev              4.065 μs   (3.037 μs .. 6.029 μs)

benchmarking 211KB/hexpat-sax
time                 12.02 ms   (11.40 ms .. 12.49 ms)
                     0.990 R²   (0.983 R² .. 0.995 R²)
mean                 11.87 ms   (11.65 ms .. 12.10 ms)
std dev              598.5 μs   (500.5 μs .. 747.9 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking 211KB/hexpat-dom
time                 11.50 ms   (11.12 ms .. 12.04 ms)
                     0.991 R²   (0.985 R² .. 0.996 R²)
mean                 12.34 ms   (12.11 ms .. 12.64 ms)
std dev              690.0 μs   (556.8 μs .. 840.7 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking 211KB/xml-dom
time                 63.70 ms   (59.51 ms .. 67.15 ms)
                     0.995 R²   (0.992 R² .. 0.999 R²)
mean                 59.64 ms   (58.40 ms .. 61.05 ms)
std dev              2.512 ms   (1.622 ms .. 4.014 ms)

```

### Before speed improvements

