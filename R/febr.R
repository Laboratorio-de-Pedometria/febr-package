#' Get all dataset tables
#' 
#' Download data from the *identification* ("identificacao"), *observation* ("observacao"), *layer* ("camada"),
#' and *metadata* ("metadado") tables of a dataset contained in the Free Brazilian Repository for Open Soil 
#' Data -- FEBR, \url{https://www.pedometria.org/febr/}.
#' 
#' @param data.set Character vector indicating one dataset.
#' 
#' @template metadata_template
#' 
#' @param merge (optional) Logical value indicating if the *observation* ("observacao") and *layer* ("camada")
#' tables should be merged. Defaults to `merge = FALSE`. See \code{\link[base]{merge}} for more details.
#' 
#' @param ... (optional) Arguments passed to \code{\link[febr]{observation}} and \code{\link[febr]{layer}}.
#' 
#' @return A list of data frames with data on the chosen dataset.
#'
#' @note Check the new core data download function `readFEBR()`.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{dataset}}, \code{\link[febr]{observation}}, \code{\link[febr]{layer}}, 
#' \code{\link[febr]{metadata}}
#' @export
#' @examples
#' \donttest{ 
#' # res <- febr(data.set = "ctb0013")
#' }
####################################################################################################
febr <- 
  function (data.set, merge = FALSE, progress = TRUE, verbose = TRUE, ...) {
    
    # ARGUMENTOS
    ## data.set
    if (missing(data.set)) {
      stop ("argument 'data.set' is missing")
    } else if (!is.character(data.set)) {
      stop (paste("object of class", class(data.set), "passed to argument 'data.set'"))
    } else if (length(data.set) > 1 || data.set == "all") {
      stop ("cannot donwload data from more than on dataset")
    }
    
    ## merge
    if (!is.logical(merge)) {
      stop (paste("object of class", class(merge), "passed to argument 'merge'"))
    }
    
    # DESCARREGAMENTO
    ## 'identificacao'
    if (verbose) {
      message(paste("Downloading ", data.set, "-identificacao...", sep = ""))
    }
    dts <- dataset(data.set = data.set, progress = progress, verbose = FALSE)
    
    ## 'metadado'
    if (verbose) {
      message(paste("Downloading ", data.set, "-metadado...", sep = ""))
    }
    mtd <- try(metadata(data.set = data.set, progress = progress, verbose = FALSE))
    
    ## 'observacao'
    if (verbose) {
      message(paste("Downloading ", data.set, "-observacao...", sep = ""))
    }
    obs <- observation(data.set = data.set, progress = progress, verbose = FALSE, ...)
    
    ## 'camada'
    if (verbose) {
      message(paste("Downloading ", data.set, "-camada...", sep = ""))
    }
    lyr <- layer(data.set = data.set, progress = progress, verbose = FALSE, ...)
    
    # PROCESSAMENTO
    ## Fundir tabelas se necessário
    if (nrow(obs) >= 1 && nrow(lyr) >= 1 && merge) {
      a_obs <- attributes(obs)[["units"]]
      a_lyr <- attributes(lyr)[["units"]]
      a_mer <- c(a_obs, a_lyr[3:length(a_lyr)])
      res <- list(identificacao = dts, 
                  dados = merge(x = obs, y = lyr, by = c("dataset_id", "observacao_id")), 
                  metadado = mtd)
      a <- attributes(res[[2]])
      a$units <- a_mer
      attributes(res[[2]]) <- a
    } else {
      res <- list(identificacao = dts, observacao = obs, camada = lyr, metadado = mtd)
    }
    
    # FINAL
    return (res)
  }
