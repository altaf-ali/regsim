
<!-- README.md is generated from README.Rmd. Please DO NOT edit README.md directly -->
regsim
======

[![Travis-CI Build Status](https://travis-ci.org/altaf-ali/regsim.svg?branch=master)](https://travis-ci.org/altaf-ali/regsim)

An R Package for dealing with statistical uncertainty in regression models.

Installation
------------

You can install regsim from github with:

``` r
# install.packages("devtools")
devtools::install_github("altaf-ali/regsim")
```

You can also install the latest development version from:

``` r
devtools::install_github("altaf-ali/regsim@dev")
```

Example
-------

``` r
library(regsim)
```

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
```

``` r
x <- list(wt = seq(1, 5, 0.1))

sim <- regsim(model, x)
```

``` r
plot(sim)
```

![](figures/README-plot-1.png)
