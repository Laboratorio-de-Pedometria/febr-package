#' Create an sf object
#' 
#' Set spatial coordinates and projection attributes to create an sf object from the *observation* 
#' ("observacao") table of one or more standardized datasets contained in the Free Brazilian Repository for 
#' Open Soil Data -- FEBR, \url{https://www.pedometria.org/febr/}.
#' 
#' @param obj Object of class `data.frame` downloaded from the FEBR using function
#' \code{\link[febr]{observation}}.
#' 
#' @return An sf object
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' res <- observation(dataset = "ctb0003", variable = "taxon",
#'                    progress = FALSE, verbose = FALSE)
#' res <- febr2sf(obj = res)
#' plot(res["taxon_sibcs_2009"], axes = TRUE, graticule = TRUE)
###############################################################################################################
# 2020-03-08: substitui 'sp' por 'sf'
febr2spdf <- 
  function (obj) {
    .Defunct(new = 'febr2sf')
  }
#' @rdname febr2spdf
#' @export
febr2sf <-
  function (obj) {
    # Verificar sistema de referência de coordenadas
    crs <- unique(obj$coord_sistema)
    n_crs <- length(crs)
    if (n_crs == 1) {
      obj <- sf::st_as_sf(x = obj, coords = c('coord_x', 'coord_y'), crs = as.integer(gsub('EPSG:', '', crs)))
    } else {
      stop ("coordinate reference system has not been standardized")
    }
    return (obj)
  }
