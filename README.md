# interval

Interval arithmetic for SWI-Prolog

Installation
```
pack_install(interval).
```

Usage
```
use_module(library(interval)).
interval((2...3 * 1...2) / -1...1, Res).
```

Include R-functions
```
pack_install(rologp).
use_module(library(rint)).
interval(pbinom(3, 10, 0.1...0.2, Res)).
```
