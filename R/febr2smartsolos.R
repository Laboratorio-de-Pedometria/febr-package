#' Conversion between FEBR and SMARTSolos soil profile data formats
#' 
#' Export FEBR soil profile data to the JSON file format required by the SMARTSolos API—and the other way
#' around.
#' 
#' @param profiles Data frame with soil profile data, i.e. observation locations.
#' 
#' @param horizons Data frame with soil horizon data, i.e. sampling layers.
#' 
#' @param file Character string naming the JSON file to be read from or written to disk.
#' 
#' @param ... (optional) Arguments passed to \code{\link[base]{writeLines}} and \code{\link[jsonlite]{fromJSON}}.
#' 
#' @export
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @examples 
#' \donttest{
#' soil <- febr::readFEBR("ctb0770", c("observacao", "camada"))[[1]]
#' febr2smartsolos(profiles = soil$observacao, horizons = soil$camada, file = "febr2smartsolos.json")
#' }
###############################################################################################################
febr2smartsolos <-
  function (profiles, horizons, file, ...) {
    # falta incluir funções para renomear colunas
    # https://docs.google.com/spreadsheets/d/1mc5S-HsoCcxLeue97eMoWLMse4RzFZ1_MCQyQhfzXUg/edit
    profiles$HORIZONTES <- NA
    horizons <- split(x = horizons, f = horizons$observacao_id)
    for (i in seq_along(horizons)) {
      profiles$HORIZONTES[i] <- list(horizons[[i]])
    }
    profiles <- list(items = profiles)
    ss <- jsonlite::toJSON(profiles, pretty = TRUE)
    writeLines(text = ss, con = file, ...)
  }
###############################################################################################################
#' @rdname febr2smartsolos
#' @export
smartsolos2febr <-
  function (file, ...) {
    profiles <- jsonlite::fromJSON(txt = file, ...)
    horizons <- profiles$items$HORIZONTES
    horizons <- lapply(horizons, data.table::as.data.table)
    horizons <- data.table::rbindlist(horizons, fill = TRUE)
    profiles <- profiles$items[, !colnames(profiles$items) == "HORIZONTES"]
    # falta incluir funções para renomear colunas
    # https://docs.google.com/spreadsheets/d/1mc5S-HsoCcxLeue97eMoWLMse4RzFZ1_MCQyQhfzXUg/edit
    return(list(observacao = profiles, camada = horizons))
  }
