
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cvasi.ui

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

`cvasi.ui` is a graphical user interface and software package for the
language *[R](https://www.r-project.org/)*. It provides an user-friendly
interface for features of the
*[cvasi](https://github.com/cvasi-tktd/cvasi)* package to calibrate,
validate, and simulate TK/TD models in *R*. It is implemented using the
[Shiny](https://www.rstudio.com/products/shiny/) library.

## Installation

Install latest version from GitHub

``` r
install.packages("remotes", dependencies=TRUE)
remotes::install_github("cvasi-tktd/cvasi.ui", dependencies=TRUE)
```

## Usage

After installing the `cvasi.ui`package, start the GUI by:

``` r
library(cvasi.ui)
run_cvasi()
```

## Documentation

The user interface provides an interactive tutorial. The tutorial can be
started by clicking on the question mark in the top right corner of the
application.

## License

The package and its source code is free and open-source software
available under the [GPL-3.0
license](https://github.com/cvasi-tktd/cvasi.ui/blob/main/LICENSE.md).

## Issues

If you find any issues or bugs within the package, please create a [new
issue](https://github.com/cvasi-tktd/cvasi.ui/issues) on GitHub.

## Contributing

Contributions to the project are welcome! Please have a look at the
[Contribution
Guidelines](https://github.com/cvasi-tktd/cvasi.ui/blob/main/CONTRIBUTING.md)
before submitting a Pull Request.

## Acknowledgements

Financial support for creation and release of this software project was
provided by Bayer Crop Science. Thanks to all persons who contributed to
testing the tool and for their valuable feedback (in no particular
order):

Alexander Singer, Dietmar Warnecke, Gisela Wiedemann, Silke Laucht, Jens
Schabacker, Petra Moosmayer, Thomas Martin and Daniel Stengel at Rifcon.
As well as Jutta Hager, Thomas Preuss, David Heckmann, John Herrmann,
and Dominic Englert at Bayer Crop Science.
