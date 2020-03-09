#' Get *dataset* table
#'
#' Download data from the *dataset* ("dataset") table of one or more datasets contained in the Free Brazilian
#' Repository for Open Soil Data -- ___febr___, \url{http://www.ufsm.br/febr}. This includes dataset title and
#' description, author and institution identification, dataset license, and much more.
#'
#' @template metadata_template
#'
#' @return A list of data frames or a data frame with data of the chosen dataset(s).
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
# \donttest{
#' res <- dataset(dataset = "ctb0003")
#' head(res)
# }
###############################################################################################################
dataset <-
  function (dataset, progress = TRUE, verbose = TRUE) {

    googlesheets4::sheets_deauth()
    
    # ARGUMENTOS
    ## dataset
    if (missing(dataset)) {
      stop ("argument 'dataset' is missing")
    } else if (!is.character(dataset)) {
      #stop (glue::glue("object of class '{class(dataset)}' passed to argument 'dataset'"))
      stop (paste("object of class", class(dataset), "passed to argument 'dataset'"))
    }
    
    ## progress
    if (!is.logical(progress)) {
      #stop (glue::glue("object of class '{class(progress)}' passed to argument 'progress'"))
      stop (paste("object of class", class(progress), "passed to argument 'progress'"))
    }
    
    ## verbose
    if (!is.logical(verbose)) {
      #stop (glue::glue("object of class '{class(verbose)}' passed to argument 'verbose'"))
      stop (paste("object of class", class(verbose), "passed to argument 'verbose'"))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)

    # Opções
    opts <- .opt()
    
    # Descarregar planilhas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$dataset), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$dataset)) {
      
      # Mensagens informativas
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading ", dts, "-dataset...", sep = ""))
      }
      
      # tmp <- googlesheets::gs_key(sheets_keys$dataset[i], verbose = FALSE)
      # tmp <- suppressMessages(
      #   googlesheets::gs_read_csv(
      #     ss = tmp, ws = 'dataset', # identifica Sheet por seu nome
      #     na = opts$gs$na, locale = opts$gs$locale, verbose = opts$gs$verbose
      #   )
      # )
      tmp <- suppressMessages(
        googlesheets4::read_sheet(ss = sheets_keys$dataset[i], sheet = 'dataset', na = opts$gs$na))

      # Dados processados
      obs[[i]] <- as.data.frame(tmp)
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
