<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![R-CMD-check-bioc](https://github.com/UCLouvain-CBIO/scpGUI/workflows/check-bioc.yml/badge.svg)](https://github.com/UCLouvain-CBIO/scpGUI/actions/workflows/check-bioc.yml)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

# scpGUI

`scpGUI` is a graphical interface for the scp package.
Currently the app can be used to convert two separate tables (Input and Sample) into a QFeatures object.
This process is performed with the readSCP function from the scp package.

## Installation 

Get the package:

```r
# Check if remotes is installed. Otherwise install it.
if (!require("remotes", quietly = TRUE)){
    install.packages("remotes")
}
# Install the package
remotes::install_github("UCLouvain-CBIO/scpGUI",
    build_manual = TRUE,
    build_vignettes = TRUE
)
# Load the package
library(scpGUI)
```