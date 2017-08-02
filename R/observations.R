#' Get soil observations
#'
#' Download soil observation-specific data contained in the Brazilian Soil Iron Data Repository (Fe-BR).
#'
#' @param which.cols Which columns should be returned? Options are \code{"standard"} (default) and
#' \code{"all"}.
#'
#' @param stack.obs Should soil observations from different datasets be stacked on a single data frame for
#' output? Used only with \code{which.cols = "standard"}. Defaults to \code{stack.obs = TRUE}.
#'
#' @param missing.coords What should be done with soil observations missing spatial coordinates? Options are
#' \code{"drop"} (default) and \code{"keep"}.
#'
#' @param progress Show progress bar?
#'
#' @details Standard columns and their content are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification code of the datasets in Fe-BR to which soil observations belong.
#' \item \code{observacao_id}. Identification code of soil observations in Fe-BR.
#' \item \code{sisb_id}. Identification code of soil observations in the Brazilian Soil Information System
#' maintained by the Brazilian Agricultural Research Corporation (EMBRAPA) at
#' \url{https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html}.
#' \item \code{ibge_id}. Identification code of soil observations in the database of the Brazilian Institute
#' of Geography and Statistics (IBGE) at \url{http://www.downloads.ibge.gov.br/downloads_geociencias.htm#}.
#' \item \code{observacao_data}. Date (dd-mm-yyyy) in which soil observations were made.
#' \item \code{coord_sistema}. Coordinate reference system used.
#' \item \code{coord_x}. Longitude (°) or Easting (m).
#' \item \code{coord_y}. Latitude (°) or Northing (m).
#' \item \code{coord_precisao}. Precision with which x- and y-coordinates were determined.
#' \item \code{coord_fonte}. Source of the x- and y-coordinates.
#' \item \code{pais_id}. Country code (ISO 3166-1 alpha-2), i.e. \code{"BR"}.
#' \item \code{estado_id}. Code of the Brazilian federative units.
#' \item \code{municipio_id}. Name of the county where soil observations were made.
#' \item \code{amostra_tipo}. Type of soil sample taken, i.e. simple or composed.
#' \item \code{amostra_quanti}. Number of soil samples taken.
#' \item \code{amostra_area}. Sampling area.
#' }
#'
#' @return A list or data.frame with some or all of the data of soil observations contained in Fe-BR.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' \dontrun{
#' res <- observations(which.cols = "standard", stack.obs = TRUE, missing.coords = "drop", progress = TRUE)
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
    sheets_keys <- suppressMessages(googlesheets::gs_read(sheets_keys, verbose = FALSE))

    # Definir opções de local
    locale <- readr::locale(decimal_mark = ",")

    # Definir as colunas padrão
    if (which.cols == "standard") {
      target_cols <-
        c("observacao_id", "sisb_id", "ibge_id", "observacao_data", "coord_sistema", "coord_x", "coord_y",
          "coord_precisao", "coord_fonte", "pais_id", "estado_id", "municipio_id", "amostra_tipo",
          "amostra_quanti", "amostra_area")
    }

    # Descarregar planilhas com observações
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$observacao), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$observacao)) {
      tmp <- googlesheets::gs_key(sheets_keys$observacao[i], verbose = FALSE)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(tmp, na = c("NA", "-", ""), locale = locale, verbose = FALSE)
      )

      # Definir as colunas a serem mantidas
      if (which.cols == "standard") {
        cols <- colnames(tmp) %in% target_cols
        tmp <- tmp[, cols]

        # 'observacao_id', 'sisb_id' e 'ibge_id' precisam estar no formato de caracter para evitar erros
        # durante a fusão das tabelas devido ao tipo de dado.
        tmp$observacao_id <- as.character(tmp$observacao_id)
        if ("sisb_id" %in% colnames(tmp)) {
          tmp$sisb_id <- as.character(tmp$sisb_id)
        }
        if ("ibge_id" %in% colnames(tmp)) {
          tmp$ibge_id <- as.character(tmp$ibge_id)
        }
      }

      # Se necessário, descartar observações sem coordenadas
      if (missing.coords == "drop") {
        tmp <- tmp[!is.na(tmp$coord_x), ]
      }

      # Adicionar 'dataset_id' às observações processadas.
      # Verificar se, com a eliminação das observações sem coordenadas, ainda restou alguma observação
      if (nrow(tmp) >= 1) {
        obs[[i]] <- cbind(dataset_id = as.character(sheets_keys$ctb[i]), tmp)
      }
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }

    # Empilhar dados se necessário
    if (stack.obs) {
      # obs <- do.call(what = rbind, args = obs)
      obs <- suppressWarnings(dplyr::bind_rows(obs))
    }

    return (obs)
  }
