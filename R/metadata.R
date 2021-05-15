#' Get 'metadata' table
#' 
#' Download data from the 'metadata' ("metadado") table of one or more datasets published in the 
#' Free Brazilian Repository for Open Soil Data (FEBR), \url{https://www.pedometria.org/febr/}.
#' This table includes data such as variable names, description of analytical methods, and
#' identification of analysis laboratories.
#'
#' @template metadata_template
#' 
#' @param febr.repo (optional) Defaults to the remote file directory of the Federal University of
#' Technology - Paraná at \url{https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso}. 
#' Alternatively, a local directory path can be informed if the user has a local copy of the data
#' repository.
#' 
#' @return A list of data frames or a data frame with metadata of the chosen dataset(s).
#'
#' @note Check the new core data download function `readFEBR()`.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
# \donttest{
#' res <- metadata(dataset = "ctb0003")
# }
####################################################################################################
metadata <-
  function (dataset, progress = TRUE, verbose = TRUE, febr.repo = NULL) {
    # ARGUMENTOS
    ## dataset
    if (missing(dataset)) {
      stop ("argument 'dataset' is missing")
    } else if (!is.character(dataset)) {
      stop (paste("object of class", class(dataset), "passed to argument 'dataset'"))
    }
    ## progress
    if (!is.logical(progress)) {
      stop (paste("object of class", class(progress), "passed to argument 'progress'"))
    }
    ## verbose
    if (!is.logical(verbose)) {
      stop (paste("object of class", class(verbose), "passed to argument 'verbose'"))
    }
    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)
    # Opções
    opts <- .opt()
    # Descarregar planilhas
    if (progress) {
      pb <- utils::txtProgressBar(
        min = 0, max = length(stats::na.omit(sheets_keys$metadado)), style = 3)
    }
    obs <- list()
    for (i in 1:length(stats::na.omit(sheets_keys$metadado))) {
      # Mensagens informativas
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Reading ", dts, "-metadado...", sep = ""))
      }
      tmp <- .readOwnCloud(ctb = sheets_keys[i, 'ctb'], table = 'metadado', febr.repo = febr.repo)
      # Dados processadas
      obs[[i]] <- cbind(dataset_id = sheets_keys$ctb[i], tmp, stringsAsFactors = FALSE)
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }
    if (n == 1) {
      obs <- obs[[1]]
    }
    return (obs)
  }
