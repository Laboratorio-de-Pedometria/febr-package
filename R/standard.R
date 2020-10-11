#' Get *standards* table
#'
#' Download data from the *standards* ("padroes") table of the Free Brazilian Repository for Open Soil Data
#' -- FEBR, \url{https://www.pedometria.org/projeto/febr/}. This includes codes and names of variable,
#' measurement units, number of decimal places, type of data, and description of analytical methods. This is
#' used to standardize the data contained in a dataset when downloading it via \code{\link[febr]{layer}} or 
#' \code{\link[febr]{observation}}.
#' 
#' @param table (optional) Character string indicating a table, i.e. the *layer* table, `"camada"`, or the 
#' *observation* table, `"observacao"`.
#' 
#' @param variable (optional) Character vector indicating one or more variables. Accepts both specific 
#' identification codes, e.g. `"ferro_oxalato_icpoes"` and `"carbono_cromo_30min150_mohr"`, as well as 
#' general identification codes, e.g. `"ferro"` and `"carbono"`.
#' 
#' @param unit (optional) Character vector indicating one or more measurement units. For example, `"g/kg"`, 
#' `"g/cm^3"`, and `"cmolc/kg"`.
#' 
#' @param precision (optional) Integer vector indicating one or more number of decimal places.
#' 
#' @param expr (optional) Character string to be parsed and evaluated as a regular expression. For example, 
#' `"campo_precisao > 0"`. Usage requires some knowledge of the structure of the *standards* table.
#' 
#' @return A data frame with standards for selected variable(s).
#'
#' @references
#' Teixeira, P. C., Donagemma, G. K., Fontana, A., Teixeira, W. G. (2017)
#' *Manual de Métodos de Análise de Solo*. Brasília: Embrapa.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso The *standards* table at 
#' \url{https://docs.google.com/spreadsheets/d/1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI}
#' @export
#' @examples
# \donttest{
#' #res <- standard(variable = "ferro")
#' #head(res)
# }
###############################################################################################################
standard <-
  function (table, variable, unit, precision, expr) {
    
    # ARGUMENTOS
    ## table
    if (missing(table)) {
      stop ("argument 'table' is missing")
    } else if (!table %in% c("observacao", "camada")) {
      stop (paste("unknown value '", table, "' passed to argument 'table'", sep = ""))
    }
    
    ## variable
    if (!missing(variable) && !is.character(variable)) {
      stop (paste("object of class '", class(variable), "' passed to argument 'variable'", sep = ''))
    }
    
    ## unit
    if (!missing(unit) && !is.character(unit)) {
      stop (paste("object of class '", class(unit), "' passed to argument 'unit'", sep = ""))
    }
    
    ## precision
    if (!missing(precision) && !pedometrics::isNumint(precision)) {
      stop (paste("object of class '", class(precision), "' passed to argument 'precision'", sep = ""))
    }
    
    ## expr
    if (!missing(expr) && !is.character(expr)) {
      stop (paste("object of class '", class(expr), "' passed to argument 'expr'", sep = ""))
    }
    
    # DESCARREGAMENTO
    ## Descarregar tabela com padrões
    std <- .getStds()

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
        variable <- paste("^", variable, "_", sep = "")
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
    rownames(std) <- NULL
    return (std)
  }
