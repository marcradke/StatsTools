# StatsTools

<!-- badges: start -->

[![R-CMD-check](https://github.com/marcradke/StatsTools/workflows/R-CMD-check/badge.svg)](https://github.com/marcradke/StatsTools/actions) [![codecov](https://codecov.io/gh/marcradke/StatsTools/branch/master/graph/badge.svg?token=USNB56P45B)](https://codecov.io/gh/marcradke/StatsTools)

<!-- badges: end -->

StatsTools is a package of statistics functions written as part of the STAT302 class at the University of Washington.

## Installation

You can install StatsTools from Github with:

``` r
# install.packages("devtools")
devtools::install_github("marcradke/StatsTools")
library(StatsTools)
```

## Use

For an overview of all functions in the package, create and view the vignette with:

``` r
devtools::install_github("marcradke/StatsTools", build_vignette = TRUE, build_opts = c())
library(StatsTools)
# Use this to view the vignette in the StatsTools HTML help
help(package = "StatsTools", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "StatsTools")
```

test line from local clone
