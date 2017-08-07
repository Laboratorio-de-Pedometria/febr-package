#' Get soil datasets
#'
#' Download dataset-specific data contained in the Brazilian Soil Iron Data Repository  --
#' \url{http://www.ufsm.br/febr}.
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
  function (progress = TRUE) {

    # Verificar consistência dos parâmetros
    if (!is.logical(progress)) {
      stop (paste("unknown value '", progress, "' passed to parameter progress", sep = ""))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- googlesheets::gs_key("18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q", verbose = FALSE)
    sheets_keys <- suppressMessages(
      googlesheets::gs_read(sheets_keys, verbose = FALSE)
    )

    # Definir opções de local
    locale <- readr::locale(decimal_mark = ",")

    # Descarregar planilhas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$dataset), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$dataset)) {
      tmp <- googlesheets::gs_key(sheets_keys$dataset[i], verbose = FALSE)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(tmp, na = c("NA", "-", ""), locale = locale, verbose = FALSE)
      )

      # Observações processadas
      obs[[i]] <- tmp
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }

    return (obs)
  }
