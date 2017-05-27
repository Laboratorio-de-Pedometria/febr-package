#' Get soil layers
#'
#' Download layer-specific data contained in the Brazilian Soil Iron Data Repository (Fe-BR).
#'
#' @param which.cols Which columns should be returned? Options are \code{"standard"} (default) and
#' \code{"all"}.
#'
#' @param progress Show progress bar?
#'
#' @details Standard columns are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification code of the respective dataset.
#' \item \code{observation_id}. Identification code of the respective observation.
#' \item \code{layer_number}. Sequential layer number, from top to bottom.
#' \item \code{layer_name}. Layer designation according to some standard soil description guidelines.
#' \item \code{sample_code}. Laboratory number of the soil sample.
#' \item \code{upper_depth}. Limite superior da camada.
#' \item \code{lower_depth}. Limite inferior da camada.
#' \item \code{fe_xxx_yyy}. Soil iron content data, with \code{xxx} being a given extractant and \code{yyy}
#' being a given determination technique/equipment.
#' }
#'
#' @return A list with some or all of the data of the soil layers contained in Fe-BR.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' \dontrun{
#' res <- layers()
#' str(res)
#' }
###############################################################################################################
layers <-
  function (which.cols = "standard", progress = TRUE) {

    # Verificar consistência dos parâmetros
    if(!which.cols %in% c("standard", "all")) {
      stop (paste("unknown value '", which.cols, "' passed to parameter which.cols", sep = ""))
    }
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
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$layer), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$layer)) {
      tmp <- googlesheets::gs_key(sheets_keys$layer[i], verbose = FALSE)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(tmp, na = c("NA", "-", ""), locale = locale, verbose = FALSE)
      )

      # Definir as colunas a serem mantidas
      if (which.cols == "standard") {
        cols <-
          c("observation_id",	"layer_number",	"layer_name",	"sample_code", "upper_depth", "lower_depth",
            colnames(tmp)[grep("^fe_", colnames(tmp))])
        tmp <- tmp[, cols]
      }

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
