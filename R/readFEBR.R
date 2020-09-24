#' Get soil data
#' 
#' Download soil data from one or more data sets published in the Free Brazilian Repository for Open Soil Data
#' -- FEBR, \url{https://www.pedometria.org/projeto/febr/}.
#' 
#' @param data.set Character vector indicating the identification code of one or more data sets. Identification
#' codes should be as recorded in \url{https://pedometria.shinyapps.io/febr/}. Use `data.set = "all"` to
#' download all data sets.
#' 
#' @param data.table Character vector indicating one or more data tables, with supported values 
#' `"identificacao"`, `"versionamento"`, `"metadado"`, `"observacao"`, e `"camada"`.
#' 
#' @param verbose (optional) Logical value indicating if informative messages should be displayed. Generally
#' useful to identify issues—please report to \email{febr-forum@@googlegroups.com} if you find any.
#' 
#' @param ... (optional) Arguments passed to \code{\link[utils]{read.table}}.
#' 
#' @return A list of data frames with data from the chosen data sets.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @export
#' @examples
#' \donttest{ 
#' res <- readFEBR(data.set = "ctb0003")
#' }
###############################################################################################################
readFEBR <- 
  function (
    data.set,
    data.table = c("identificacao", "versionamento", "metadado", "observacao", "camada"),
    verbose = TRUE, ...) {
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
    } else if (!all(data.table %in% c("identificacao", "versionamento", "metadado", "observacao", "camada"))) {
      stop("unsupported value passed to argument 'data.table'")
    }
    ## verbose
    if (!is.logical(verbose)) {
      stop(paste0("object of class '", class(verbose), "' passed to argument 'verbose'"))
    }
    # DOWNLOAD DATA ----
    if (data.set == "all") {
      keys <- "18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q"
      keys <- paste0('https://docs.google.com/spreadsheets/d/', keys, '/export?format=csv')
      data.set <- utils::read.table(
        file = keys, header = TRUE, sep = ',', dec = ',', comment.char = '', stringsAsFactors = FALSE)[, "ctb"]
      data.set <- sort(data.set)
    }
    server <- "https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso"
    url <- lapply(data.set, function(x) {
      paste0(server, "/download?path=%2F", x, "&files=", x, "-", data.table, ".txt")
    })
    res <- lapply(url, function(x){
      if (verbose) {
        message(paste0("Downloading...\n", paste0(x, collapse = "\n")))
      }
      out <- sapply(x, utils::read.table, header = TRUE, dec = ',', stringsAsFactors = FALSE, ...)
      names(out) <- data.table
      out
    })
    # PREPARE OUTPUT ----
    if(length(res) == 1) {
      res <- res[[1]]
    } else {
      names(res) <- data.set
    }
    return(res)
  }
