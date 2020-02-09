#' Get all dataset tables
#' 
#' Download data from the *dataset* ("dataset"), *observation* ("observacao"), *layer* ("camada"), and 
#' *metadata* ("metadado") tables of a dataset contained in the Free Brazilian Repository for Open Soil Data 
#' -- ___febr___, \url{http://www.ufsm.br/febr}.
#' 
#' Character vector indicating one dataset. The identification code should be as recorded in 
#' \url{http://www.ufsm.br/febr/catalog/}.
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
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{dataset}}, \code{\link[febr]{observation}}, \code{\link[febr]{layer}}, 
#' \code{\link[febr]{metadata}}
#' @export
#' @examples
#' \donttest{ 
#' res <- febr(dataset = "ctb0003", merge = TRUE, variable = "all")
#' str(res)
#' }
###############################################################################################################
febr <- 
  function (dataset, merge = FALSE, progress = TRUE, verbose = TRUE, ...) {
    
    # ARGUMENTOS
    ## dataset
    if (missing(dataset)) {
      stop ("argument 'dataset' is missing")
    } else if (!is.character(dataset)) {
      #stop (glue::glue("object of class '{class(dataset)}' passed to argument 'dataset'"))
      stop (paste("object of class", class(dataset), "passed to argument 'dataset'"))
    } else if (length(dataset) > 1 || dataset == "all") {
      stop ("cannot donwload data from more than on dataset")
    }
    
    ## merge
    if (!is.logical(merge)) {
      #stop (glue::glue("object of class '{class(merge)}' passed to argument 'merge'"))
      stop (paste("object of class", class(merge), "passed to argument 'merge'"))
    }
    
    # DESCARREGAMENTO
    ## Descarregar tabela 'dataset'
    if (verbose) {
      #message(glue::glue("Downloading table {dataset}-dataset..."))
      message(paste("Downloading ", dataset, "-dataset...", sep = ""))
    }
    dts <- dataset(dataset = dataset, progress = progress, verbose = FALSE)
    
    ## Descarregar tabela 'observacao'
    if (verbose) {
      #message(glue::glue("Downloading table {dataset}-observacao..."))
      message(paste("Downloading ", dataset, "-observacao...", sep = ""))
    }
    obs <- observation(dataset = dataset, progress = progress, verbose = FALSE, ...)
    
    ## Descarregar tabela 'camada'
    if (verbose) {
      #message(glue::glue("Downloading table {dataset}-camada..."))
      message(paste("Downloading ", dataset, "-camada...", sep = ""))
    }
    lyr <- layer(dataset = dataset, progress = progress, verbose = FALSE, ...)
    
    ## Descarregar tabela 'metadado'
    if (verbose) {
      #message(glue::glue("Downloading table {dataset}-metadado..."))
      message(paste("Downloading ", dataset, "-metadado...", sep = ""))
    }
    mtd <- try(metadata(dataset = dataset, progress = progress, verbose = FALSE))
    
    # PROCESSAMENTO
    ## Fundir tabelas se necessário
    if (nrow(obs) >= 1 && nrow(lyr) >= 1 && merge) {
      a_obs <- attributes(obs)[["units"]]
      a_lyr <- attributes(lyr)[["units"]]
      a_mer <- c(a_obs, a_lyr[3:length(a_lyr)])
      res <- list(dataset = dts, 
                  data = merge(x = obs, y = lyr, by = c("dataset_id", "observacao_id")), 
                  metadata = mtd)
      a <- attributes(res[[2]])
      a$units <- a_mer
      attributes(res[[2]]) <- a
    } else {
      res <- list(dataset = dts, observation = obs, layer = lyr, metadata = mtd)
    }
    
    # FINAL
    return (res)
  }
