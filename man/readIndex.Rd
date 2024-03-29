% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readIndex.R
\name{readIndex}
\alias{readIndex}
\title{Get data set index}
\usage{
readIndex(...)
}
\arguments{
\item{...}{(optional) Arguments passed to \code{\link[data.table:fread]{data.table::fread()}}.}
}
\value{
An object of class \link[data.table:data.table]{data.table::data.table}.
}
\description{
Download the index of data sets published in the
\href{https://www.pedometria.org/febr/}{Data Repository of the Brazilian Soil}.
}
\examples{
index <- readIndex()
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
