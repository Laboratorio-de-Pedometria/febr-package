#' Create an object of class `SpatialPointsDataFrame`
#' 
#' Set spatial coordinates and projection attributes to create an object of class `SpatialPointsDataFrame` from
#' the *observation* ("observacao") table of one or more standardized datasets contained in the Free Brazilian
#' Repository for Open Soil Data -- ___febr___, \url{http://www.ufsm.br/febr}.
#' 
#' @param obj Object of class `data.frame` downloaded from ___febr___ using function
#' \code{\link[febr]{observation}}.
#' 
#' @return An object of class `SpatialPointsDataFrame`
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
# \donttest{
#' library(magrittr)
#' observation(dataset = "ctb0003", variable = "taxon",
#'             progress = FALSE, verbose = FALSE) %>% 
#'   febr2spdf() %>%
#'   sp::spplot(zcol = "taxon_sibcs_2009", auto.key = list(columns = 3), scales = list(draw = TRUE))
# }
###############################################################################################################
febr2spdf <-
  function (obj) {
    
    # Verificar sistema de referência de coordenadas
    crs <- unique(obj$coord_sistema)
    n_crs <- length(crs)
    if (n_crs == 1) {
      sp::coordinates(obj) <- ~ coord_x + coord_y
      sp::proj4string(obj = obj) <- sp::CRS(glue::glue("+init={tolower(crs)}"))
      obj@data <- dplyr::select(obj@data, -coord_sistema)
    } else {
      stop ("spatial coordinates have not been standardized")
    }
    
    return (obj)
  }
