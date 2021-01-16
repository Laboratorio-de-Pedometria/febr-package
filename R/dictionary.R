#' Access the FEBR dictionary
#'
#' Download definitions from the dictionary of the Free Brazilian Repository for Open Soil Data
#' (FEBR). For each field, the dictionary includes an identification code and name, the standard
#' measurement unit, recommended number of decimal places, type of data, and description of the
#' respective analytical method. The dictionary is used to standardize the data contained in a
#' dataset.
#' @param table (optional) Character vector indicating one or more table IDs that should be used to
#' filter the dictionary. Accepted values: `"metadado"`, `"versionamento"`, `"observacao"`, and
#' `"camada"`.
#' @param variable (optional) Character vector indicating one or more variables that should be used
#' to filter the dictionary. Accepts both specific identification codes, e.g.
#' `"ferro_oxalato_icpoes"`, as well as general identification codes, e.g. `"ferro"`.
#' @param unit (optional) Character vector indicating one or more measurement units that should be
#' used to filter the dictionary. For example, ' `"g/kg"`, `"g/cm^3"`, and `"cmolc/kg"`.
#' @param precision (optional) Integer vector indicating one or more number of decimal places that
#' should be used to filter the dictionary.
# @param expr (optional) Character string to be parsed and evaluated as a regular expression. For
# example, `"campo_precisao > 0"`. Efficient usage requires some knowledge of the structure of the
# dictionary.
#' @return A data frame with definitions for the selected fields.
#' @references
#' Teixeira, P. C., Donagemma, G. K., Fontana, A., Teixeira, W. G. (2017)
#' _Manual de Métodos de Análise de Solo_. Brasília: Embrapa.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso The FEBR dictionary at
#' \url{https://docs.google.com/spreadsheets/d/1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI}
#' @export
#' @examples
# \donttest{
#' res <- dictionary(variable = "ferro")
#' head(res)
# }
####################################################################################################
dictionary <-
  function(table, variable, unit, precision) {
  # function(table, variable, unit, precision, expr) {
    # CHECK ARGUMENTS
    # table
    if (!missing(table)) {
      if (!table %in% c("observacao", "camada", "metadado", "versionamento")) {
        stop(paste0("unknown value '", table, "' passed to argument 'table'"))
      }
    }
    # variable
    if (!missing(variable) && !is.character(variable)) {
      stop(paste0("object of class '", class(variable), "' passed to argument 'variable'"))
    }
    ## unit
    if (!missing(unit) && !is.character(unit)) {
      stop(paste0("object of class '", class(unit), "' passed to argument 'unit'"))
    }
    ## precision
    if (!missing(precision) && !pedometrics::isNumint(precision)) {
      stop(paste0("object of class '", class(precision), "' passed to argument 'precision'"))
    }
    ## expr
    # if (!missing(expr) && !is.character(expr)) {
    #   stop(paste0("object of class '", class(expr), "' passed to argument 'expr'"))
    # }
    # DESCARREGAMENTO
    ## Descarregar tabela
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
        idx <- lapply(variable, function(pattern) grep(pattern = pattern, x = std[["campo_id"]]))
        idx <- unlist(idx)
      } else {
        idx <- which(std$campo_id %in% variable)
      }
      std <- std[idx, ]
    }
    ## Selecionar por campo_unidade
    if (!missing(unit)) {
      idx <- which(std[["campo_unidade"]] %in% unit)
      std <- std[idx, ]
    }
    ## Selecionar por campo_precisao
    if (!missing(precision)) {
      idx <- which(std[["campo_precisao"]] %in% precision)
      std <- std[idx, ]
    }
    ## Selecionar usando expressão
    # if (!missing(expr)) {
    #   std <- dplyr::filter(std, eval(parse(text = expr)))
    # }
    # ERRO
    if (nrow(std) == 0) {
      stop("function call did not return any results", call. = TRUE)
    }
    # FINAL
    rownames(std) <- NULL
    return(std)
  }
####################################################################################################
#' @rdname dictionary
#' @export
standard <- dictionary
