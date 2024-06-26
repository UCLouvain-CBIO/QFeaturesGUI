% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sampleTable}
\alias{sampleTable}
\title{Single cell sample annotation}
\format{
An object of class \code{data.frame} with 64 rows and 6 columns.
}
\usage{
data("sampleTable")
}
\description{
A data frame with 48 observations on the following 6 variables.
\itemize{
\item Set: a character vector
\item Channel: a character vector
\item SampleType: a character vector
\item lcbatch: a character vector
\item sortday: a character vector
\item digest: a character vector
}
}
\details{
##' The dataset is a subset of the SCoPE2 dataset (version 2, Specht
et al. 2019,
\href{https://www.biorxiv.org/content/10.1101/665307v3}{BioRXiv}). The
input files \code{batch.csv} and \code{annotation.csv} were downloaded from a
\href{https://drive.google.com/drive/folders/1VzBfmNxziRYqayx3SP-cOe2gu129Obgx}{Google Drive repository}.
The two files were loaded and the columns names were adapted for
consistency with \code{mqScpData} table (see \code{?mqScpData}). The two
tables were filtered to contain only sets present in ``mqScpData\verb{. The tables were then merged based on the run ID, hence merging the sample annotation and the batch annotation. Finally, annotation for the blank run was added manually. The data is stored as a }data.frame`.
}
\seealso{
\code{\link[=readSCP]{readSCP()}} to see how this file is used.
}
\keyword{datasets}
