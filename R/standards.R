#' Standards for soil variables
#'
#' Definitions of standards for soil variables in the Brazilian Soil Iron Data Repository  -- 
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
#' Donagemma, G. K., Campos, D. V. B., Calderano, S. B., Teixeira, W. G. and Viana, J. H. M. (2011)
#' \emph{Manual de Métodos de Análise de Solo}. Rio de Janeiro: Embrapa Solos.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' # All standards
#' standards()
#' 
#' # Specific stantdard
#' standards(extraction.method = "sulfurico")
#' 
# Definir unidade e número de casas decimais para cada tipo de dado de ferro ##################################
standards <-
  function (soil.var = "fe", extraction.method) {

    # Verificar consistência dos parâmetros
    if(!soil.var %in% c("fe")) {
      stop (paste("Unknown value '", soil.var, "' passed to parameter soil.var", sep = ""))
    }
    
    # checking R files for non-ASCII characters ... WARNING
    # Found the following file with non-ASCII characters:
      # standards.R
    # Portable packages must use only ASCII characters in their R code, except perhaps in comments.
    # Use \uxxxx escapes for other characters.
    # Use sprintf("%X", as.integer(charToRaw("£"))) to know the hexadecimal number of your character.
    # http://www.javascripter.net/faq/accentedcharacters.htm
    # Á: \u00C1
    # á: \u00E1
    # ã: \u00E3
    # é: \u00E9
    # í: \u00ED
    # ó: \u00F3
    # ô: \u00F4
    # ú: \u00FA
    
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
          c(# Ácido clorídrico + Ácido nítrico
            "aquaregia",   "g/kg",  1, "\u00C1cido clor\u00EDdrico + \u00C1cido n\u00EDtrico",
            # Ácido clorídrico,
            "cloridrico",  "g/kg",  1, "\u00C1cido clor\u00EDdrico",
            "ditionito",   "g/kg",  2, "Citrato-ditionito-bicarbonato",
            "dtpa",        "mg/kg", 0, "DTPA",
            "mehlich",     "mg/kg", 0, "Mehlich",
            # Oxalato ácido de amônio
            "oxalato",     "g/kg",  2, "Oxalato \u00E1cido de am\u00F4nio",
            # Pirofosfato de sódio
            "pirofosfato", "g/kg",  2, "Pirofosfato de s\u00F3dio",
            # Razão goethita/(goethita+hematita)
            "rggh",        "-",     3, "Raz\u00E3o goethita/(goethita+hematita)",
            # Ácido sulfúrico
            "sulfurico",   "g/kg",  1, "\u00C1cido sulf\u00FArico",
            # Ácido perclórico + Ácido nítrico + Ácido fluorídrico
            "triacido",    "g/kg",  1, 
                         "\u00C1cido percl\u00F3rico + \u00C1cido n\u00EDtrico + \u00C1cido fluor\u00EDdrico"),
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

