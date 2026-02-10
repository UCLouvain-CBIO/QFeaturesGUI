# A shiny app to import QFeatures objects.

importQFeatures is a simple graphical interface to import bulk and
single-cell proteomic data. The app use the
[`readQFeatures`](https://rformassspectrometry.github.io/QFeatures/reference/readQFeatures.html)
function from the QFeatures package to convert simple table (single or
multiple, csv or tsv) to a QFeatures object. The app allow to convert
tables to a QFeatures object.

## Usage

``` r
importQFeatures(colData = NULL, assayData = NULL, maxSize = 1000)
```

## Arguments

- colData:

  A dataframe that contains the sample table.

- assayData:

  A dataframe that contains the input table.

- maxSize:

  An integer that change shiny.maxRequestSize value, this value has to
  be in Mb.

## Value

Return the "importQFeatures" shiny app object.

## Examples

``` r
library(QFeaturesGUI)

data("sampleTable")
data("inputTable")
app <- importQFeatures(colData = sampleTable, assayData = inputTable, maxSize = 100)

if (interactive()) {
    shiny::runApp(app)
}
```
