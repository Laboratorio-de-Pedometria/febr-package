% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goto.R
\name{goto}
\alias{goto}
\title{Go to \emph{\strong{febr}}}
\usage{
goto(dataset, table, page)
}
\arguments{
\item{dataset}{Character vector indicating one dataset. The identification code should be as recorded
in \url{http://www.ufsm.br/febr/catalog/}.}

\item{table}{Character string indicating a table, i.e. the \emph{dataset} table, \code{"dataset"}, the \emph{observation}
table, \code{"observacao"}, the \emph{layer} table, \code{"camada"}, or the \emph{metadata} table, \code{"metadado"}.}

\item{page}{Character string indicating a web page of the \emph{\strong{febr}}. Options are: \code{"febr"}, \code{"view"},
\code{"catalog"}, \code{"search"}, \code{"book"}, \code{"package"}, \code{"github"}, \code{"forum"}, \code{"unit"}, and \code{"standard"}.}
}
\description{
Go to one of the web pages of the Free Brazilian Repository for Open Soil Data -- \emph{\strong{febr}} --, including
project and dataset web pages.
}
\examples{
\dontrun{
# Go to the main project page
goto(page = "febr")
}
}
\author{
Alessandro Samuel-Rosa \email{alessandrosamuelrosa@gmail.com}
}