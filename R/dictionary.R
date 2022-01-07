#' @title Access the FEBR dictionary
#' @description Download definitions from the dictionary of the
#' [Data Repository of the Brazilian Soil](https://www.pedometria.org/febr/). For each field, the
#' dictionary includes an identification code and name, the standard
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
#' @return An object of class `data.frame` with definitions for the selected fields.
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
    #
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
    if (!missing(precision) && !.isNumint(precision)) {
      stop(paste0("object of class '", class(precision), "' passed to argument 'precision'"))
    }
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
        variable <- paste0("^", variable, "_")
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
    # ERRO
    if (nrow(std) == 0) {
      stop("function call did not return any results", call. = TRUE)
    }
    # FINAL
    rownames(std) <- NULL
    return(std)
  }
