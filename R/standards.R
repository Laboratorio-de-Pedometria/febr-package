#' Standards
#'
#' Standards for soil variables in the Brazilian Soil Iron Data Repository (Fe-BR)  -- 
#' \url{http://www.ufsm.br/febr}.
#'
#' @param soil.var Identification code of the soil variable. The only currently available option is 
#' \code{soil.var = "fe"}.
#' 
#' @param extraction.method Identification code of the extraction method. See \sQuote{Details}.
#' 
#' @details
#' Standards for soil variables are defined according to the third edition of the Manual of Methods of Soil
#' Analysis published by Embrapa Soils in 2011. They include the measurement units and number of digital 
#' places that should be used with a soil variable as defined by the extraction method.
#' 
#' @return A data.frame.
#'
#' @references
#' Donagemma, G. K., Campos, D. V. B., Calderano, S. B., Teixeira, W. G., Viana, J. H. M. \emph{Manual de 
#' Métodos de Análise de Solo}. Rio de Janeiro: Embrapa Solos, p. 230, 2011.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' standards()
#' 
# Definir unidade e número de casas decimais para cada tipo de dado de ferro ##################################
standards <-
  function (soil.var = "fe", extraction.method) {

    # Verificar consistência dos parâmetros
    if(!soil.var %in% c("fe")) {
      stop (paste("Unknown value '", soil.var, "' passed to parameter soil.var", sep = ""))
    }
    
    switch(
      soil.var,
      "fe" = {
        # Segundo a terceira edição do Manual de Métodos de Análise de Solo (Donagemma et al. 2011), as
        # seguintes convenções devem ser aplicadas:
        # - Ferro no extrato sulfúrico: g/kg, com 1 casa decimal
        # - Ferro livre: g/kg, com 2 casas decimais
        # - Microelementos: mg/kg, sem casa decimal
        # - Ataque triácido: g/kg, com 1 casa decimal
        res <- matrix(
          c("aquaregia",   "g/kg",  1, "Ácido clorídrico + Ácido nítrico",
            "cloridrico",  "g/kg",  1, "Ácido clorídrico",
            "ditionito",   "g/kg",  2, "Citrato-ditionito-bicarbonato",
            "dtpa",        "mg/kg", 0, "DTPA",
            "mehlich",     "mg/kg", 0, "Mehlich",
            "oxalato",     "g/kg",  2, "Oxalato ácido de amônio",
            "pirofosfato", "g/kg",  2, "Pirofosfato de sódio",
            "rggh",        "-",     3, "Razão goethita/(goethita+hematita)",
            "sulfurico",   "g/kg",  1, "Ácido sulfúrico",
            "triacido",    "g/kg",  1, "Ácido perclórico + Ácido nítrico + Ácido fluorídrico"),
          ncol = 4, byrow = TRUE
        )
        res <- as.data.frame(res, stringsAsFactors = FALSE)
        colnames(res) <- c("code", "unit", "digits", "description")
        res$digits <- as.numeric(res$digits)
      }
    )

    if (!missing(extraction.method)) {
      res <- res[res$code %in% extraction.method, ]
    }

    return (res)
  }

