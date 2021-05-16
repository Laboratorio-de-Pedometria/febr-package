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
#' # res <- identification(data.set = c("ctb0003", "ctb0000"))
#' res <- metadata(data.set = c("ctb0003", "ctb0002"))
#' }
####################################################################################################
identification <-
  function(data.set, progress = TRUE, verbose = TRUE, febr.repo = NULL) {
    # ARGUMENT CHECK ----
    ## data.set
    if (missing(data.set)) {
      stop("Argument 'data.set' is missing")
    } else if (!is.character(data.set)) {
      stop(paste0("Object of class ", class(data.set), " passed to 'data.set'"))
    } else {
      dataset_ids <- readIndex()[["dados_id"]]
      if (data.set[1] != "all") {
        idx_out <- data.set %in% dataset_ids
        if (sum(idx_out) != length(data.set)) {
          stop(paste0("Unknown value '", data.set[!idx_out], "' passed to 'data.set'"))
        } else {
          dataset_ids <- data.set
        }
      }
    }
    n_datasets <- length(dataset_ids)
    ## progress
    if (!is.logical(progress)) {
      stop(paste0("object of class ", class(progress), " passed to 'progress'"))
    }
    ## verbose
    if (!is.logical(verbose)) {
      stop(paste0("object of class ", class(verbose), " passed to 'verbose'"))
    }
    # Options
    opts <- .opt()
    # Descarregar planilhas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = n_datasets, style = 3)
    }
    res <- list()
    for (i in seq_along(dataset_ids)) {
      # Mensagens informativas
      if (verbose) {
        message(paste0(ifelse(progress, "\n", ""), "Reading ", dataset_ids[i], "-identificacao..."))
      }
      # Dados processados
      res[[i]] <- .readFEBR(
        data.set = dataset_ids[i], data.table = "identificacao", febr.repo = febr.repo)
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }
    # PREPARE OUTPUT ----
    if (n_datasets == 1) {
      res <- res[[1]]
    } else {
      names(res) <- data.set
    }
    return(res)
  }
