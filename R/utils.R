#' Utilities
#' 
#' @param ... (optional) Arguments passed to [utils::read.table()].
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
readIndex <- function(...) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table package is missing")
  if (!requireNamespace("curl", quietly = TRUE)) stop("curl package is missing")
  
  owncloud <- "https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F"
  input <- paste0(owncloud, "&files=febr-indice.txt")
  febr_index <- data.table::fread(
    input, dec = ",", header = TRUE, stringsAsFactors = FALSE, ...)
  return(febr_index)
}
