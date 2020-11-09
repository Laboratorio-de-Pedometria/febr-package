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
#' soil <- readFEBR("ctb0770", c("observacao", "camada"))[[1]]
#' febr2smartsolos(profiles = soil$observacao, horizons = soil$camada, file = "febr2smartsolos.json")
#' }
###############################################################################################################
febr2smartsolos <-
  function (profiles, horizons, file, ...) {
    # Tradução dos nomes das variáveis
    gs <- "1mc5S-HsoCcxLeue97eMoWLMse4RzFZ1_MCQyQhfzXUg"
    sheet <- "dados"
    https_request <- paste0("https://docs.google.com/spreadsheets/d/", gs, "/gviz/tq?tqx=out:csv&sheet=", sheet)
    translation <- suppressWarnings(
      utils::read.table(file = https_request, sep = ",", header = TRUE, stringsAsFactors = FALSE))
    # profiles
    idx_old <- which(colnames(profiles) %in% translation$febr_var_name)
    idx_new <- match(colnames(profiles)[idx_old], translation$febr_var_name)
    colnames(profiles)[idx_old] <- translation$ss_var_name[idx_new]
    # horizons
    idx_old <- which(colnames(horizons) %in% translation$febr_var_name)
    idx_new <- match(colnames(horizons)[idx_old], translation$febr_var_name)
    colnames(horizons)[idx_old] <- translation$ss_var_name[idx_new]
    # Conversão para JSON
    profiles$HORIZONTES <- NA
    horizons <- split(x = horizons, f = horizons$ID_PONTO)
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
