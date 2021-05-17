#' Defunct functions in the **febr** package
#' 
#' The functions listed here are no longer part of the **febr** package as they are not needed (any
#' more).
#' 
#' @param ... Not used.
#' 
#' @export
#' @rdname febr-defunct
#' @aliases dataset febr febr2spdf febr2xlsx standard header
dataset <- 
  function(...) {
    .Defunct(new = "identification", package = "febr")
  }
#' @export
#' @rdname febr-defunct
febr <- 
  function(...) {
    .Defunct(new = "readFEBR", package = "febr")
  }
#' @export
#' @rdname febr-defunct
febr2spdf <- 
  function(...) {
    .Defunct(new = "febr2sf", package = "febr")
  }
#' @export
#' @rdname febr-defunct
febr2xlsx <-
  function (...) {
    .Defunct(msg = "'febr2xlsx' is defunct.\nUse 'openxlsx::write.xlsx' instead.")
  }
#' @export
#' @rdname febr-defunct
header <- 
  function(...) { 
    .Defunct(new = "metadata", package = "febr")
  }
#' @export
#' @rdname febr-defunct
standard <- 
  function(...) {
    .Defunct(new = "dictionary", package = "febr")
  }
