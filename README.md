
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/CorradoLanera/weektwo.svg?branch=master)](https://travis-ci.org/CorradoLanera/weektwo)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/CorradoLanera/weektwo?branch=master&svg=true)](https://ci.appveyor.com/project/CorradoLanera/weektwo)
[![Coverage
status](https://codecov.io/gh/CorradoLanera/weektwo/branch/master/graph/badge.svg)](https://codecov.io/github/CorradoLanera/weektwo?branch=master)

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/weektwo)](https://cran.r-project.org/package=weektwo)

# weektwo

The goal of **weektwo** is to demonstrate the ability to create a
package.

## Installation

You can install the develop version of **weektwo** from
[GITHUB](https://github.com) with:

``` r
# install.packages("devtools")
devtools::install_github("CorradoLanera/weektwo")
```

## Example

``` r
library(weektwo)

path_to_data <- system.file("sample-data", package = "weektwo")

fars_map_state(state.num = 1,
    year = 2013,
    path = path_to_data
)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

## Code of Conduct

Please note that the **weektwo** project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
