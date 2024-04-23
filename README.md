<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/UCLouvain-CBIO/QFeaturesGUI/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/UCLouvain-CBIO/QFeaturesGUI/actions/workflows/check-bioc.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

# QFeaturesGUI

`QFeaturesGUI` is a R package that contains a suite of shiny app to offer a graphical interface for the QFeatures package.
Currently the package can be used to convert tables (csv and tsv) into a QFeatures object.
This works for both bulk and single-cell data.
The user can also apply several pre-processing steps to the imported QFeatures.
This process is performed with the readQFeatures function from the QFeatures package.

## Installation

Get the package:

```r
# Check if remotes is installed. Otherwise install it.
if (!require("remotes", quietly = TRUE)){
    install.packages("remotes")
}
# Install the package
remotes::install_github("UCLouvain-CBIO/QFeaturesGUI",
    build_manual = TRUE,
    build_vignettes = TRUE
)
# Load the package
library(QFeaturesGUI)
```
