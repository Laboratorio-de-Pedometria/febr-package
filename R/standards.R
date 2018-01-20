#' Standards for soil variables
#'
#' Definitions of standards for soil variables in the Free Brazilian Repository for Open Soil Data -- 
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
standard <-
  function (table, variable, unit, precision, expr) {

    # Descarregar tablela com padrões
    std <- .getTable(x = "1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI")

    # Selecionar por tabela_id
    if (!missing(table)) {
      idx <- which(std$tabela_id == table)
      std <- std[idx, ]
    }
    
    # Selecionar por campo_id
    if (!missing(variable)) {
      idx <- which(which(std$campo_id == variable))
      std <- std[idx, ]
    }
    
    # Selecionar por campo_unidade
    if (!missing(unit)) {
      idx <- which(std$campo_unidade == unit)
      std <- std[idx, ]
    }
    
    # Selecionar por campo_precisao
    if (!missing(precision)) {
      idx <- which(std$campo_precisao == precision)
      std <- std[idx, ]
    }
    
    # Selecionar usando expressão
    if (!missing(expr)) {
      std <- dplyr::filter(std, eval(parse(text = expr)))
    }
    
    return (std)
  }
