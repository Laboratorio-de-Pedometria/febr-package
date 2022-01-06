#' @title Get controlled vocabulary
#' @description Download the controlled vocabulary used in the FEBR Soil Data Repository,
#' \url{https://www.pedometria.org/febr/}.
#' @param ... (optional) Arguments passed to [data.table::fread()].
#' @return An object of class [data.table::data.table].
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' vocab <- readVocabulary()
####################################################################################################
readVocabulary <- function(...) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table package is missing")
  if (!requireNamespace("curl", quietly = TRUE)) stop("curl package is missing")
  sheet_id <- "1yJ_XnsJhnhJSfC3WRimfu_3_owXxpfSKaoxCiMD2_Z0"
  url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv")
  res <- data.table::fread(url, dec = ",", header = TRUE, stringsAsFactors = FALSE, ...)
  return(res)
}
data.table::data.table