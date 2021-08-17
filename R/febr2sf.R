#' Create an sf object
#'
#' Set spatial coordinates and coordinate reference system (CRS) to a set of soil observations.
#'
#' @param obj Object of class `data.frame` downloaded from the FEBR using [febr::observation()].
#'
#' @details
#' Create an sf object from the
#' *observation* ("observacao") table of one or more standardized datasets contained in the Free
#' Brazilian Repository for Open Soil Data (FEBR), \url{https://www.pedometria.org/febr/}.
#'
#' @return An object of class `sfc_POINT` (single soil observation) or `sfc_MULTIPOINT` (multiple
#' soil observations).
#'
#' @references
#' Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data.
#' The R Journal 10 (1), 439-446, \url{https://doi.org/10.32614/RJ-2018-009}
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' res <- observation(
#'   data.set = "ctb0013",
#'   progress = FALSE, verbose = FALSE)
#' res <- febr2sf(obj = res)
#' plot(res["geometry"], axes = TRUE, graticule = TRUE)
####################################################################################################
febr2sf <-
  function(obj) {
    if (!requireNamespace("sf")) stop("sf package is missing")
    # Verificar sistema de referência de coordenadas
    crs <- unique(obj[["coord_datum"]])
    if (length(crs) == 1) {
      obj <- sf::st_as_sf(x = obj, coords = c("coord_longitude", "coord_latitude"), crs = crs)
    } else {
      stop("coordinate reference system has not been standardized")
    }
    return(obj)
  }
