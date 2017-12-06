#' Get soil metadata
#'
#' Download metadata-specific data contained in the Free Brazilian Repository for Open Soil Data --
#' \url{http://www.ufsm.br/febr}.
#'
#' @param dataset Identification code of the dataset (or datasets) for which soil metadata-specific data should
#' be downloaded -- see \url{http://www.ufsm.br/febr/book}. Use \code{dataset = "all"} to download data from 
#' all existing datasets.
#' 
#' @param progress Show progress bar?
#'
#' @return A list with metadata-specific data.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' \dontrun{
#' res <- metadata()
#' str(res)
#' }
###############################################################################################################
metadata <-
  function (dataset, progress = TRUE) {

    # Verificar consistência dos parâmetros
    if (!is.logical(progress)) {
      stop (paste("unknown value '", progress, "' passed to parameter progress", sep = ""))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)

    # Definir opções de local
    locale <- readr::locale(decimal_mark = ",")

    # Descarregar planilhas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(stats::na.omit(sheets_keys$metadata)), style = 3)
    }
    obs <- list()
    for (i in 1:length(stats::na.omit(sheets_keys$metadata))) {
      tmp <- googlesheets::gs_key(stats::na.omit(sheets_keys$metadata)[i], verbose = FALSE)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(tmp, na = c("NA", "-", ""), locale = locale, verbose = FALSE)
      )

      # Observações processadas
      obs[[i]] <- cbind(dataset_id = as.character(sheets_keys$n[i]), tmp)
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }

    return (obs)
  }
