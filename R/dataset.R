#' Get 'identification' table
#'
#' Download data from the 'identification' ("identificacao") table of one or more soil datasets
#' published in the Free Brazilian Repository for Open Soil Data (FEBR), 
#' \url{https://www.pedometria.org/febr/}. This table includes data such as dataset title and
#' description, author and institution, data license, and much more.
#'
#' @param febr.repo (optional) Defaults to the remote file directory of the Federal University of
#' Technology - Paraná at \url{https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso}. 
#' Alternatively, a local directory path can be informed if the user has a local copy of the data
#' repository.
#'
#' @template metadata_template
#'
#' @return A list of data frames or a data frame with data of the chosen dataset(s).
#'
#' @note Check the new core data download function `readFEBR()`.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' res <- identification(data.set = "ctb0003")
#' }
####################################################################################################
identification <-
  function (data.set, progress = TRUE, verbose = TRUE, febr.repo = NULL) {
    # ARGUMENTS
    ## data.set
    if (missing(data.set)) {
      stop ("argument 'data.set' is missing")
    } else if (!is.character(data.set)) {
      stop (paste("object of class", class(data.set), "passed to argument 'data.set'"))
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
    sheets_keys <- readIndex()[["dados_id"]]
    sheets_keys <- sheets_keys[sheets_keys %in% data.set]
    n_datasets <- length(sheets_keys)
    # Opções
    opts <- .opt()
    # Descarregar planilhas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = n_datasets, style = 3)
    }
    res <- list()
    for (i in seq_along(sheets_keys)) {
      # Mensagens informativas
      if (verbose) {
        message(paste0(ifelse(progress, "\n", ""), "Reading ", sheets_keys[i], "-identificacao..."))
      }
      # Dados processados
      res[[i]] <- .readFEBR(
        data.set = sheets_keys[i], data.table = 'identificacao', febr.repo = febr.repo)
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }
    if (n_datasets == 1) {
      res <- res[[1]]
    }
    return(res)
  }
#' @rdname identification
#' @export
dataset <- identification
