#' Get *metadata* table
#' 
#' Download data from the *metadata* ("metadado") table of one or more datasets contained in the Free Brazilian
#' Repository for Open Soil Data -- ___febr___, \url{http://www.ufsm.br/febr}. This includes variable names,
#' description of analytical methods, and identification of analysis laboratories.
#'
#' @template metadata_template
#' 
#' @return A list of data frames or a data frame with metadata of the chosen dataset(s).
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
# \donttest{
#' res <- metadata(dataset = "ctb0003")
#' head(res)
# }
###############################################################################################################
metadata <-
  function (dataset, progress = TRUE, verbose = TRUE) {

    # ARGUMENTOS
    ## dataset
    if (missing(dataset)) {
      stop ("argument 'dataset' is missing")
    } else if (!is.character(dataset)) {
      stop (glue::glue("object of class '{class(dataset)}' passed to argument 'dataset'"))
    }
    
    ## progress
    if (!is.logical(progress)) {
      stop (glue::glue("object of class '{class(progress)}' passed to argument 'progress'"))
    }
    
    ## verbose
    if (!is.logical(verbose)) {
      stop (glue::glue("object of class '{class(verbose)}' passed to argument 'verbose'"))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    if (length(stats::na.omit(sheets_keys$metadado)) == 0) {
      m <- glue::glue(
        "metadata for dataset {dataset} has not been published yet
        Contact febr-forum@googlegroups.com for more info and learn how to help")
      stop (m)
    }
    n <- nrow(sheets_keys)
    
    # Opções
    opts <- .opt()

    # Descarregar planilhas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(stats::na.omit(sheets_keys$metadado)), style = 3)
    }
    obs <- list()
    for (i in 1:length(stats::na.omit(sheets_keys$metadado))) {
      
      # Mensagens informativas
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading ", dts, "-metadado...", sep = ""))
      }
      
      # tmp <- googlesheets::gs_key(stats::na.omit(sheets_keys$metadado)[i], verbose = FALSE)
      # tmp <- suppressMessages(
      #   googlesheets::gs_read_csv(
      #     ss = tmp, ws = 'metadado', # identifica Sheet por seu nome
      #     na = opts$gs$na, locale = opts$gs$locale, verbose = opts$gs$verbose)
      # )
      tmp <- suppressMessages(
        googlesheets4::read_sheet(ss = sheets_keys$metadado[i], sheet = 'metadado', na = opts$gs$na))

      # Dados processadas
      obs[[i]] <- cbind(dataset_id = as.character(sheets_keys$ctb[i]), tmp)
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
