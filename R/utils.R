#' Utilities
#' 
#' @param ... (optional) Arguments passed to [utils::read.table()].
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
readIndex <- function(...) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table package is missing")
  if (!requireNamespace("curl", quietly = TRUE)) stop("curl package is missing")
  
  febr_index <- data.table::fread(
    paste0(.opt()$owncloud, "&files=febr-indice.txt"),
    dec = ",", header = TRUE, stringsAsFactors = FALSE, ...)
  
  return(febr_index)
}
