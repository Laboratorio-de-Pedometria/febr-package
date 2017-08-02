#' Get soil observations
#'
#' Download soil observation-specific data contained in the Brazilian Soil Iron Data Repository (Fe-BR) --
#' \url{http://www.ufsm.br/febr}.
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
#' @param target.crs EPSG code defining the target coordinate reference system to which spatial coordinates
#' should be transformed. Used only with \code{stack.obs = TRUE}. Defaults to \code{target.crs = "EPSG:4674"},
#' i.e. SIRGAS 2000, the standard CRS for Brazil -- see more at \url{http://spatialreference.org/ref/epsg/}.
#' If set to \code{target.crs = NULL} then the native spatial coordinates are returned.
#'
#' @param progress Show download progress bar?
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
#' \item \code{coord_sistema}. EPSG code of the coordinate reference system -- see more at
#' \url{http://spatialreference.org/ref/epsg/}.
#' \item \code{coord_x}. Longitude (°) or Easting (m).
#' \item \code{coord_y}. Latitude (°) or Northing (m).
#' \item \code{coord_precisao}. Precision with which x- and y-coordinates were determined (m).
#' \item \code{coord_fonte}. Source of the x- and y-coordinates.
#' \item \code{pais_id}. Country code (ISO 3166-1 alpha-2), i.e. \code{"BR"}.
#' \item \code{estado_id}. Code of the Brazilian federative units.
#' \item \code{municipio_id}. Name of the Brazilian county where soil observations were made.
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
  function (which.cols = "standard", stack.obs = TRUE, missing.coords = "drop", target.crs = "EPSG:4674",
            progress = TRUE) {

    # Verificar consistência dos parâmetros
    if(!which.cols %in% c("standard", "all")) {
      stop (paste("Unknown value '", which.cols, "' passed to parameter which.cols", sep = ""))
    }
    if (!is.logical(stack.obs)) {
      stop (paste("Unknown value '", stack.obs, "' passed to parameter stack.obs", sep = ""))
    }
    if (which.cols == "all" && stack.obs == TRUE) {
      message("stack.obs can only be used with standard columns... switching to FALSE")
      stack.obs <- FALSE
    }
    if (!missing.coords %in% c("drop", "keep")) {
      stop (paste("Unknown value '", missing.coords, "' passed to parameter missing.coords", sep = ""))
    }
    if (!is.null(target.crs) && stack.obs == FALSE || which.cols == "all") {
      message(paste(
        "Coordinate transformation only works for stacked observations with standard columns\n",
        "Setting target CRS to NULL...", sep = "")
        )
      target.crs <- NULL
    }
    crs_list <- paste("EPSG:", c(
      # Córrego Alegre
      4225, 22521, 22522, 22523, 22524, 22525,
      # SAD69
      4618 , 29168, 29188, 29169, 29189, 29170, 29190, 29191, 29192, 29193, 29194, 29195,
      # WGS 84
      4326, 32618, 32718, 32619, 32719, 32620, 32720, 32721, 32722, 32723, 32724, 32725,
      # SIRGAS 2000
      4674, 31972, 31978, 31973, 31979, 31974, 31980, 31981, 31982, 31983, 31984, 31985
    ), sep = "")
    if (!toupper(target.crs) %in% crs_list) {
      stop (paste("Unknown value '", target.crs, "' passed to parameter target.crs", sep = ""))
    }
    if (!is.logical(progress)) {
      stop (paste("Unknown value '", progress, "' passed to parameter progress", sep = ""))
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
        # durante o empilhamento das tabelas devido ao tipo de dado.
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
      # Verificar se, com a eliminação das observações sem coordenadas, restou alguma observação
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

    # Se necessário, empilhar tabelas
    if (stack.obs) {
      obs <- suppressWarnings(dplyr::bind_rows(obs))

      # Transformar SRC
      if (!is.null(target.crs)) {
        # Até que se prove o contrário, sempre haverá diversos SRC. Contudo, para evitar surpresas, é melhor
        # inserir desde já o código para o caso de um único SRC.
        if (nlevels(as.factor(obs$coord_sistema)) > 1) {
          obs <- split(obs, as.factor(obs$coord_sistema))
          if (toupper(target.crs) %in% names(obs)) {
            j <- which(!names(obs) %in% toupper(target.crs))
          } else {
            j <- 1:length(obs)
          }
          obs <- lapply(obs[j], function (x) {
            sp::coordinates(x) <- c("coord_x", "coord_y")
            sp::proj4string(x) <- sp::CRS(paste("+init=", tolower(x$coord_sistema[1]), sep = ""))
            x <- sp::spTransform(x, sp::CRS(paste("+init=", tolower(target.crs), sep = "")))
            as.data.frame(x)
          })
          obs <- suppressWarnings(dplyr::bind_rows(obs))
          obs$coord_sistema <- toupper(target.crs)
        } else if (unique(obs$coord_sistema) != toupper(target.crs)) {
          sp::coordinates(obs) <- c("coord_x", "coord_y")
          sp::proj4string(obs) <- sp::CRS(paste("+init=", tolower(obs$coord_sistema[1]), sep = ""))
          obs <- sp::spTransform(obs, sp::CRS(paste("+init=", tolower(target.crs), sep = "")))
          obs <- as.data.frame(obs)
          obs$coord_sistema <- toupper(target.crs)
        }
      }

      # Colocar colunas na ordem padrão
      obs <- obs[, target_cols]
    }

    return (obs)
  }
