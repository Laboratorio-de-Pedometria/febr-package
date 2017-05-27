#' Get soil observations
#'
#' Download observation-specific data contained in the Brazilian Soil Iron Data Repository (Fe-BR).
#'
#' @param which.cols Which columns should be returned? Options are \code{"standard"} (default) and
#' \code{"all"}.
#'
#' @param stack.obs Should observations from different datasets be stacked on a single data frame for output?
#' Used only with \code{which.cols = "standard"}. Defaults to \code{stack.obs = TRUE}.
#'
#' @param missing.coords What should be done with observations missing spatial coordinates? Options are
#' \code{"drop"} (default) and \code{"keep"}.
#'
#' @param progress Show progress bar?
#'
#' @details Standard columns are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification code of the respective dataset.
#' \item \code{observation_id}. Identification code of the respective observation.
#' \item \code{observation_date}. Date of observation.
#' \item \code{coord_system}. Coordinate reference system.
#' \item \code{coord_x}. Longitude or Easting.
#' \item \code{coord_y}. Latitude or Northing.
#' \item \code{coord_accuracy}. Accuracy with which coordinates were determined.
#' \item \code{coord_source}. Source of the coordinates.
#' \item \code{country_id}. Country code (ISO 3166-1 alpha-2).
#' \item \code{state_id}. Code of the federative unit.
#' \item \code{city_name}. Name of the city where the observation was taken.
#' \item \code{sample_type}. Type of soil sample, i.e. simple or composed.
#' \item \code{sample_number}. Number of soil samples taken.
#' \item \code{sample_area}. Sampling area.
#' }
#'
#' @return A list or data.frame with some or all of the data of the soil observations contained in Fe-BR.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' \dontrun{
#' res <- observations(which.cols = "standard", stack.obs = TRUE)
#' str(res)
#' }
###############################################################################################################
observations <-
  function (which.cols = "standard", stack.obs = TRUE, missing.coords = "drop", progress = TRUE) {

    # Verificar consistência dos parâmetros
    if(!which.cols %in% c("standard", "all")) {
      stop (paste("unknown value '", which.cols, "' passed to parameter which.cols", sep = ""))
    }
    if (!is.logical(stack.obs)) {
      stop (paste("unknown value '", stack.obs, "' passed to parameter stack.obs", sep = ""))
    }
    if (which.cols == "all" && stack.obs == TRUE) {
      message("stack.obs can only be used with standard columns... switching to FALSE")
      stack.obs <- FALSE
    }
    if(!missing.coords %in% c("drop", "keep")) {
      stop (paste("unknown value '", missing.coords, "' passed to parameter missing.coords", sep = ""))
    }
    if (!is.logical(progress)) {
      stop (paste("unknown value '", progress, "' passed to parameter progress", sep = ""))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- googlesheets::gs_key("18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q", verbose = FALSE)
    sheets_keys <- suppressMessages(
      googlesheets::gs_read(sheets_keys, range = cellranger::cell_cols(3), verbose = FALSE)
    )

    # Definir opções de local
    locale <- readr::locale(decimal_mark = ",")

    # Descarregar planilhas com observações
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$observation), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$observation)) {
      tmp <- googlesheets::gs_key(sheets_keys$observation[i], verbose = FALSE)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(tmp, na = c("NA", "-", ""), locale = locale, verbose = FALSE)
      )

      # Definir as colunas a serem mantidas
      if (which.cols == "standard") {
        cols <-
          c("observation_id", "observation_date", "coord_system", "coord_x", "coord_y", "coord_accuracy",
            "coord_source", "country_id", "state_id", "city_id", "sample_type", "sample_number", "sample_area")
        tmp <- tmp[, cols]
      }

      # Se necessário, descartar observações sem coordenadas
      if (missing.coords == "drop") {
        tmp <- tmp[!is.na(tmp$coord_x), ]
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

    # Empilhar dados se necessário
    if (stack.obs) {
      obs <- do.call(what = rbind, args = obs)
    }

    return (obs)
  }
