#' Get *units* table
#' 
#' Download data from the *units* ("unidades") table of the Free Brazilian Repository for Open Soil Data --
#' FEBR, \url{https://www.pedometria.org/febr/}. This includes measurement units and conversion 
#' factors. This is used to standardize the data contained in a dataset when downloading it via
#' \code{\link[febr]{layer}} or \code{\link[febr]{observation}}.
#' 
#' @param source (optional) Character vector indicating one or more source measurement units.
#' 
#' @param target (optional) Character vector indicating one or more target measurement units.
#' 
#' @return A data.frame with source and target measurement units and their corresponding conversion factors.
#' 
#' @references
#' Teixeira, P. C., Donagemma, G. K., Fontana, A., Teixeira, W. G. (2017)
#' *Manual de Métodos de Análise de Solo*. Brasília: Embrapa.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso The *unit* table at 
#' \url{https://docs.google.com/spreadsheets/d/1tU4Me3NJqk4NH2z0jvMryGObSSQLCvGqdLEL5bvOflo}
#' @export
#' @examples
# \donttest{
# Sys.sleep(time = 10)
#' #res <- unit(source = c("%", "dag/kg"), target = "g/kg")
#' #res
# }
###############################################################################################################
unit <-
  function (source, target) {
    
    # DESCARREGAMENTO
    ## Descarregar tabela com unidades de medida
    # res <- .getUnits()
    res <- .readGoogleSheetCSV(sheet.name = 'unidades')
    
    # ARGUMENTOS
    ## source
    if (!missing(source) && any(!source %in% res$unidade_origem)) {
      source <- source[which(!source %in% res$unidade_origem)]
      stop (paste0("unknown value '", source, "' passed to argument 'source'"))
    }
    
    ## target
    if (!missing(target) && any(!target %in% res$unidade_destino)) {
      target <- target[which(!target %in% res$unidade_destino)]
      stop (paste0("unknown value '", target, "}' passed to argument 'target'"))
    }
    
    # PROCESSAMENTO
    ## Selecionar por 'unidade_origem'
    if (!missing(source)) {
      idx <- which(res$unidade_origem %in% source)
      res <- res[idx, ]
    }
    
    ## Selecionar por 'unidade_destino'
    if (!missing(target)) {
      idx <- which(res$unidade_destino %in% target)
      res <- res[idx, ]
    }
    
    # FINAL
    rownames(res) <- NULL
    return (res)
  }
