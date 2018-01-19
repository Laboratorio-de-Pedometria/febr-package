#' Get soil metadata
#'
#' Download metadata-specific data contained in the Free Brazilian Repository for Open Soil Data --
#' \url{http://www.ufsm.br/febr}.
#'
#' @template metadata_template
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
  function (dataset = 'ctb0001', progress = TRUE, verbose = TRUE) {

    # Verificar consistência dos parâmetros
    if (!is.logical(progress)) {
      stop (paste("unknown value '", progress, "' passed to parameter progress", sep = ""))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)
    
    # Opções
    opts <- .opt()

    # Descarregar planilhas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(stats::na.omit(sheets_keys$metadado)), style = 3)
    }
    obs <- list()
    for (i in 1:length(stats::na.omit(sheets_keys$metadado))) {
      
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", dts, "...", sep = ""))
      }
      
      tmp <- googlesheets::gs_key(stats::na.omit(sheets_keys$metadado)[i], verbose = FALSE)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(tmp, na = opts$gs$na, locale = opts$gs$locale, verbose = opts$gs$verbose)
      )

      # Observações processadas
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
