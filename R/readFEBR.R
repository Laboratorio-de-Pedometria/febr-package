#' @title Get soil data
#' @description Download soil data from one or more data sets published in the
#' [Data Repository of the Brazilian Soil](https://www.pedometria.org/febr/).
#' @param data.set Character vector indicating the identification code of one or more data sets.
#' Use `data.set = "all"` to download all data sets.
#' @param data.table Character vector indicating one or more data tables, with supported values
#' `"identificacao"`, `"versionamento"`, `"metadado"`, `"observacao"`, and `"camada"`.
#' @param febr.repo (optional) Defaults to the remote file directory of the Federal University of
#' Technology - Paraná at \url{https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso}.
#' Alternatively, a local directory path can be informed if the user has a local copy of the data
#' repository.
#' @param verbose (optional) Logical value indicating if informative messages should be displayed.
#' Generally useful to identify issues—please report to \email{febr-forum@@googlegroups.com} if
#' you find any.
#' @param ... (optional) Arguments passed to [data.table::fread()].
#'
#' @return A list of data tables (data frames) with data from the chosen data sets.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' res <- readFEBR(data.set = "ctb0003")
####################################################################################################
readFEBR <-
  function(
    data.set,
    data.table = c("identificacao", "versionamento", "metadado", "observacao", "camada"),
    febr.repo = NULL, verbose = TRUE, ...) {
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
    ## data.table
    if (!is.character(data.table)) {
      stop(paste0("object of class '", class(data.table), "' passed to argument 'data.table'"))
    } else if (!all(data.table %in%
      c("identificacao", "versionamento", "metadado", "observacao", "camada"))) {
      stop("unsupported value passed to argument 'data.table'")
    }
    ## verbose
    if (!is.logical(verbose)) {
      stop(paste0("object of class '", class(verbose), "' passed to argument 'verbose'"))
    }
    # READ DATA ----
    # build file names
    path <- lapply(dataset_ids, function(x) {
      if (is.null(febr.repo)) {
        path <- paste0(.opt()$owncloud, x, "&files=", x, "-", data.table, ".txt")
      } else {
        path <- file.path(febr.repo, dataset_ids, paste0(dataset_ids, "-", data.table, ".txt"))
        path <- normalizePath(path = path, mustWork = TRUE)
      }
      return(path)
    })
    res <- lapply(path, function(x) {
      if (verbose) {
        message(paste0("Reading...\n", paste0(x, collapse = "\n")))
      }
      out <- lapply(x, data.table::fread, dec = ",", ...)
      names(out) <- data.table
      out
    })
    # PREPARE OUTPUT ----
    if (n_datasets == 1) {
      res <- res[[1]]
      if (length(data.table) == 1) {
        res <- res[[1]]
      }
    } else if (n_datasets > 1) {
      if (length(data.table) == 1) {
        res <- lapply(res, function(x) x[[1]])
      }
      names(res) <- dataset_ids
    }
    return(res)
  }
