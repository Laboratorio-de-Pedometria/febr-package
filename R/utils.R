#' Utilities
#' 
#' @param ... (optional) Arguments passed to \code{\link[data.table]{fread}}.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
readIndex <- function(...) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table package is missing")
  if (!requireNamespace("curl", quietly = TRUE)) stop("curl package is missing")
  owncloud <- "https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F"
  febr_index <- data.table::fread(
    input = paste0(owncloud, "&files=febr-indice.txt"), dec = ",", ...)
  return(febr_index)
}
