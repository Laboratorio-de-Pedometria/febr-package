#' Get soil data
#'
#' Download soil data from one or more data sets published in the Free Brazilian Repository for
#' Open Soil Data -- FEBR, \url{https://www.pedometria.org/febr/}.
#'
#' @param data.set Character vector indicating the identification code of one or more data sets.
#' Use `data.set = "all"` to download all data sets.
#' 
#' @param data.table Character vector indicating one or more data tables, with supported values
#' `"identificacao"`, `"versionamento"`, `"metadado"`, `"observacao"`, and `"camada"`.
#' 
#' @param febr.repo (optional) Defaults to the remote file directory of the Federal University of
#' Technology - Paraná at \url{https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso}. 
#' Alternatively, a local directory path can be informed if the user has a local copy of the data
#' repository.
#' 
#' @param verbose (optional) Logical value indicating if informative messages should be displayed.
#' Generally useful to identify issues—please report to \email{febr-forum@@googlegroups.com} if
#' you find any.
#' 
#' @param ... (optional) Arguments passed to \code{\link[data.table]{fread}}.
#'
#' @return A list of data tables (data frames) with data from the chosen data sets.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @export
#' @examples
#' \donttest{
#' res <- readFEBR(data.set = "ctb0003")
#' }
####################################################################################################
readFEBR <-
  function(
    data.set,
    data.table = c("identificacao", "versionamento", "metadado", "observacao", "camada"),
    febr.repo = NULL, verbose = TRUE, ...) {
    # ARGUMENT CHECK ----
    ## data.set
    if (missing(data.set)) {
      stop("argument 'data.set' is missing")
    } else if (!is.character(data.set)) {
      stop(paste0("object of class '", class(data.set), "' passed to argument 'data.set'"))
    }
    ## data.table
    if (!is.character(data.table)) {
      stop(paste0("object of class '", class(data.table), "' passed to argument 'data.table'"))
    } else if (!all(data.table %in%
      c("identificacao", "versionamento", "metadado", "observacao", "camada"))) {
      stop("unsupported value passed to argument 'data.table'")
    }
    ## febr.repo
    if (is.null(febr.repo)) {
      owncloud <- "https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F"
      febr_index <- paste0(owncloud, "&files=febr-indice.txt")
    } else {
      febr_index <- normalizePath(file.path(febr.repo, "febr-indice.txt"), mustWork = TRUE)
    }
    ## verbose
    if (!is.logical(verbose)) {
      stop(paste0("object of class '", class(verbose), "' passed to argument 'verbose'"))
    }
    # READ DATA ----
    if ("all" %in% data.set) {
      # if all datasets are required, then start by reading the index file to get the dataset IDs.
      data.set <- data.table::fread(
        input = febr_index, header = TRUE, dec = ",", stringsAsFactors = FALSE)[["dados_id"]]
    }
    # build file names
    url <- lapply(data.set, function(x) {
      if (is.null(febr.repo)) {
        paste0(owncloud, x, "&files=", x, "-", data.table, ".txt")
      } else {
        path <- file.path(febr.repo, data.set, paste0(data.set, '-', data.table, ".txt"))
        normalizePath(path = path, mustWork = TRUE)
      }
    })
    res <- lapply(url, function(x){
      if (verbose) {
        message(paste0("Reading...\n", paste0(x, collapse = "\n")))
      }
      out <- lapply(x, data.table::fread, header = TRUE, dec = ",", stringsAsFactors = FALSE, ...)
      names(out) <- data.table
      out
    })
    # PREPARE OUTPUT ----
    if(length(data.set) == 1) {
      res <- res[[1]]
      if (length(data.table) == 1) {
        res <- res[[1]]
      }
    } else if (length(data.set) > 1) {
      if (length(data.table) == 1) {
        res <- lapply(res, function(x) x[[1]])
      }
      names(res) <- data.set
    }
    return(res)
  }
