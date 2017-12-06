#' Get soil datasets
#'
#' Download soil dataset-specific data contained in the Free Brazilian Repository for Open Soil Data --
#' \url{http://www.ufsm.br/febr}.
#'
#' @param dataset Identification code of the dataset (or datasets) for which soil dataset-specific data should 
#' be downloaded -- see \url{http://www.ufsm.br/febr/book}. Use \code{dataset = "all"} to download data from 
#' all existing datasets.
#' 
#' @param progress Show progress bar?
#'
#' @return A list with dataset-specific data.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' \dontrun{
#' res <- datasets()
#' str(res)
#' }
###############################################################################################################
datasets <-
  function (dataset, progress = TRUE) {

    # Verificar consistência dos parâmetros
    if (!is.logical(progress)) {
      stop (paste("unknown value '", progress, "' passed to parameter progress", sep = ""))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    
    # Which datasets should be downloaded?
    # if (!"all" %in% dataset) {
    #   idx_out <- which(!dataset %in% sheets_keys$ctb)
    #   if (length(idx_out) >= 1) {
    #     stop (paste("Unknown value '", dataset[idx_out], "' passed to parameter dataset", sep = ""))
    #   }
    #   sheets_keys <- sheets_keys[sheets_keys$ctb %in% dataset, ]
    # }
    n <- nrow(sheets_keys)

    # Opções
    opts <- .opt()
    
    # Descarregar planilhas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$dataset), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$dataset)) {
      tmp <- googlesheets::gs_key(sheets_keys$dataset[i], verbose = FALSE)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(tmp, na = opts$gs$na, locale = opts$gs$locale, verbose = opts$gs$verbose)
      )

      # Observações processadas
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
