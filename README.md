
# cvasiUI

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

cvasiUI provides a [shiny](https://www.rstudio.com/products/shiny/) graphical user 
interface for the [cvasi](https://github.com/Bayer-Group/cvasi) R-package

## Installation

Install latest version from github
``` r
install.packages("remotes", dependencies=TRUE)
remotes::install_github("Bayer-Group/cvasi-ui", dependencies=TRUE, upgrade="never")
```

## Start the GUI

After successful installation you can start the GUI with
``` r
library(cvasiUI)
run_app()
```

