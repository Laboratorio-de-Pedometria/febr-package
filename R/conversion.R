#' Conversion of units
#'
#' Factors for converting measurement units of soil variables in the Free Brazilian Repository for Open Soil 
#' Data -- \url{http://www.ufsm.br/febr}.
#'
#' @param source Source measurement unit. Currently available options are \code{"g/kg"}, \code{"\%"},
#' \code{"mg/kg"} and \code{"mg/dm^3"}.
#' 
#' @param target Target measurement unit. Currently available options are \code{"g/kg"} and \code{"mg/kg"}.
#' 
#' @details
#' Target measurement units are defined according to the third edition of the Manual of Methods of Soil
#' Analysis published by Embrapa Soils in 2011.
#' 
#' @return A data.frame with source and target measurement units and their corresponding conversion factor.
#' 
#' @references
#' Donagemma, G. K., Campos, D. V. B., Calderano, S. B., Teixeira, W. G. and Viana, J. H. M. (2011)
#' \emph{Manual de Métodos de Análise de Solo}. Rio de Janeiro: Embrapa Solos.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
# @export
#' @examples
#' conversion(source = "%", target = "g/kg")
#' 
conversion <-
  function (source, target) {

    # Fatores de conversão entre unidades
    # Conversão de massa-volume para massa-massa é feita assumindo uma densidade da alíquota de solo usada
    # para as análises igual a 1.00 g/cm^3.
    switch(
      target,
      "mg/kg" = {
        res <- data.frame(
          source = c("g/kg", "%",   "mg/dm^3"),
          factor = c(1000,   10000, 1)
        )
        res <- res$factor[res$source %in% source]
      },
      "g/kg" = {
        res <- data.frame(
          source = c("mg/kg", "%", "mg/dm^3"),
          factor = c(1/1000,  10,  1/1000)
        )
        res <- res$factor[res$source %in% source]
      }
    )

    res <- data.frame(source, target, factor = res)
    return (res)
  }
