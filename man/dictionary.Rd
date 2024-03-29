% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.R
\name{dictionary}
\alias{dictionary}
\title{Access the FEBR dictionary}
\usage{
dictionary(table, variable, unit, precision, active = NULL)
}
\arguments{
\item{table}{(optional) Character vector indicating one or more table IDs that should be used to
filter the dictionary. Accepted values: \code{"metadado"}, \code{"versionamento"}, \code{"observacao"}, and
\code{"camada"}.}

\item{variable}{(optional) Character vector indicating one or more variables that should be used
to filter the dictionary. Accepts both specific identification codes, e.g.
\code{"ferro_oxalato_icpoes"}, as well as general identification codes, e.g. \code{"ferro"}.}

\item{unit}{(optional) Character vector indicating one or more measurement units that should be
used to filter the dictionary. For example, ' \code{"g/kg"}, \code{"g/cm^3"}, and \code{"cmolc/kg"}.}

\item{precision}{(optional) Integer vector indicating one or more number of decimal places that
should be used to filter the dictionary.}

\item{active}{(optional) Logical value indicating whether active (\code{active = TRUE}), inactive
(\code{active = FALSE}) or any (\code{active = NULL}, default) field should be returned.}
}
\value{
An object of class \code{data.frame} with definitions for the selected fields.
}
\description{
Download definitions from the dictionary of the
\href{https://www.pedometria.org/febr/}{Data Repository of the Brazilian Soil}. For each field, the
dictionary includes an identification code and name, the standard
measurement unit, recommended number of decimal places, type of data, and description of the
respective analytical method. The dictionary is used to standardize the data contained in a
dataset.
}
\examples{
res <- dictionary(variable = "ferro")
head(res)
}
\references{
Teixeira, P. C., Donagemma, G. K., Fontana, A., Teixeira, W. G. (2017)
\emph{Manual de Métodos de Análise de Solo}. Brasília: Embrapa.
}
\seealso{
The FEBR dictionary at
\url{https://docs.google.com/spreadsheets/d/1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI}
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}
