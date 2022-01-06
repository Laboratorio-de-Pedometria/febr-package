#' @title Get data set index
#' @description Download the index of data sets published in the FEBR Soil Data Repository,
#' \url{https://www.pedometria.org/febr/}.
#' @param ... (optional) Arguments passed to [data.table::fread()].
#' @return An object of class [data.table::data.table].
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' index <- readIndex()
####################################################################################################
readIndex <- function(...) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table package is missing")
  if (!requireNamespace("curl", quietly = TRUE)) stop("curl package is missing")
  res <- data.table::fread(
    paste0(.opt()$owncloud, "&files=febr-indice.txt"),
    dec = ",", header = TRUE, stringsAsFactors = FALSE, ...)
  return(res)
}
