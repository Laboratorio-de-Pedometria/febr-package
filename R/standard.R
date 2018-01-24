#' Get *standard* table
#'
#' Download data from the *standard* ("padrao") table of the Free Brazilian Repository for Open Soil Data --
#' ___febr___, \url{http://www.ufsm.br/febr}. This includes codes and names of variable, measurement units, 
#' number of decimal places, type of data, and description of analytical methods. This is used to standardize 
#' the data contained in a dataset when downloading it via \code{\link[febr]{layer}} or 
#' \code{\link[febr]{observation}}.
#' 
#' @param table (optional) Character string indicating a table, i.e. the *layer* table, `"camada"`, or the 
#' *observation* table, `"observacao"`.
#' 
#' @param variable (optional) Character vector indicating one or more variables. Accepts both specific 
#' identification codes, e.g. `"ferro_oxalato_icpoes"` and `"carbono_dicromato_30min150_mohr"`, as well as 
#' general identification codes, e.g. `"ferro"` and `"carbono"`.
#' 
#' @param unit (optional) Character vector indicating one or more measurement units. For example, `"g/kg"`, 
#' `"g/cm^3"`, and `"cmolc/kg"`.
#' 
#' @param precision (optional) Integer vector indicating one or more number of decimal places.
#' 
#' @param expr (optional) Character string to be parsed and evaluated as a regular expression. For example, 
#' `"campo_precisao > 0"`. Usage requires some knowledge of the structure of the *standard* table.
#' 
#' @return A data frame with standards for selected variable(s).
#'
#' @references
#' Donagemma, G. K., Campos, D. V. B., Calderano, S. B., Teixeira, W. G. and Viana, J. H. M. (2011)
#' *Manual de Métodos de Análise de Solo*. Rio de Janeiro: Embrapa Solos.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso The *standard* table at \url{https://goo.gl/mwwZN9}
#' @export
#' @examples
#' \dontrun{
#' a <- standard(variable = "ferro")
#' }
###############################################################################################################
standard <-
  function (table, variable, unit, precision, expr) {
    
    # ARGUMENTOS
    ## table
    if (!missing(table) && !table %in% c("observacao", "camada")) {
      stop (glue::glue("unknown value '{table}' passed to argument 'table'"))
    }
    
    ## variable
    if (!missing(variable) && !is.character(variable)) {
      stop (glue::glue("object of class '{class(variable)}' passed to argument 'variable'"))
    }
    
    ## unit
    if (!missing(unit) && !is.character(unit)) {
      stop (glue::glue("object of class '{class(unit)}' passed to argument 'unit'"))
    }
    
    ## precision
    if (!missing(precision) && !pedometrics::isNumint(precision)) {
      stop (glue::glue("object of class '{class(precision)}' passed to argument 'precision'"))
    }
    
    ## expr
    if (!missing(expr) && !is.character(expr)) {
      stop (glue::glue("object of class '{class(expr)}' passed to argument 'expr'"))
    }
    
    # DESCARREGAMENTO
    ## Descarregar tabela com padrões
    std <- .getTable(x = "1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI")

    # PROCESSAMENTO
    ## Selecionar por tabela_id
    if (!missing(table)) {
      idx <- which(std$tabela_id == table)
      std <- std[idx, ]
    }
    
    ## Selecionar por campo_id
    if (!missing(variable)) {
      is_start <- all(grepl(pattern = "_", x = variable))
      if (!is_start) {
        variable <- glue::glue("^{variable}_")
        idx <- lapply(variable, function (pattern) grep(pattern = pattern, x = std$campo_id))
        idx <- unlist(idx)
      } else {
        idx <- which(std$campo_id %in% variable) 
      }
      std <- std[idx, ]
    }
    
    ## Selecionar por campo_unidade
    if (!missing(unit)) {
      idx <- which(std$campo_unidade %in% unit)
      std <- std[idx, ]
    }
    
    ## Selecionar por campo_precisao
    if (!missing(precision)) {
      idx <- which(std$campo_precisao %in% precision)
      std <- std[idx, ]
    }
    
    ## Selecionar usando expressão
    if (!missing(expr)) {
      std <- dplyr::filter(std, eval(parse(text = expr)))
    }
    
    # ERRO
    if (nrow(std) == 0) {
      stop ("function call did not return any results", call. = TRUE)
    }
    
    # FINAL
    return (std)
  }
