#' Get soil observations
#'
#' Download soil observation-specific data contained in the Free Brazilian Repository for Open Soil Data --
#' \url{http://www.ufsm.br/febr}.
#'
#' @param dataset Identification code of the dataset (or datasets) for which soil layer-specific data should be
#' downloaded -- see \url{http://www.ufsm.br/febr/data}. Use \code{dataset = "all"} to download data from all
#' existing datasets.
#' 
#' @param which.cols Which columns should be returned? Options are \code{"standard"} (default) and
#' \code{"all"}. See \sQuote{Details} for a description of the standard columns.
#' 
#' @param stack.obs Should soil observations from different datasets be stacked on a single data frame for
#' output? Used only with \code{which.cols = "standard"}. Defaults to \code{stack.obs = TRUE}.
#'
#' @param missing.coords What should be done with soil observations missing spatial coordinates? Options are
#' \code{"drop"} (default) and \code{"keep"}.
#'
#' @param target.crs EPSG code defining the target coordinate reference system to which spatial coordinates
#' should be transformed. Defaults to \code{target.crs = "EPSG:4674"}, i.e. SIRGAS 2000, the standard CRS for
#' Brazil -- see more at \url{http://spatialreference.org/ref/epsg/}. If set to \code{target.crs = NULL} then
#' the native spatial coordinates are returned.
#'
#' @param progress Show download progress bar?
#'
#' @param verbose Show informative messages? Generally useful to identify datasets with any inconsistent data. 
#' Please report to \email{febr-forum@@googlegroups.com} if you find any issue.
#' 
#' @details 
#' \subsection{Standard columns}{
#' Standard columns and their content are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification code of the datasets in febr to which soil observations belong.
#' \item \code{observacao_id}. Identification code of soil observations in febr.
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
#' \item \code{estado_id}. Code of the Brazilian federative unit where soil observations were made.
#' \item \code{municipio_id}. Name of the Brazilian county where soil observations were made.
#' \item \code{amostra_tipo}. Type of soil sample taken, i.e. simple or composed.
#' \item \code{amostra_quanti}. Number of soil samples taken.
#' \item \code{amostra_area}. Sampling area.
#' }
#' }
#'
#' @return A list or data.frame with soil observation-specific data.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{layers}}
#' @export
#' @examples
#' \dontrun{
#' res <- observations(dataset = paste("ctb000", 4:9, sep = ""))
#' str(res)
#' }
###############################################################################################################
observations <-
  function (dataset, which.cols = "standard", stack.obs = TRUE, missing.coords = "drop", 
            target.crs = "EPSG:4674", progress = TRUE, verbose = TRUE) {

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
    
    # Options
    opts <- .opt()

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- 
      googlesheets::gs_key("18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q", verbose = opts$gs$verbose)
    sheets_keys <- suppressMessages(
      googlesheets::gs_read(sheets_keys, na = opts$gs$na, verbose = opts$gs$verbose))
    sheets_keys <- .getDataset(sheets_keys = sheets_keys, dataset = dataset)

    # Which datasets should be downloaded?
    if (!"all" %in% dataset) {
      idx_out <- which(!dataset %in% sheets_keys$ctb)
      if (length(idx_out) >= 1) {
        stop (paste("Unknown value '", dataset[idx_out], "' passed to parameter dataset", sep = ""))
      }
      sheets_keys <- sheets_keys[sheets_keys$ctb %in% dataset, ]
    }
    n <- nrow(sheets_keys)

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
      
      # Informative messages
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", sheets_keys$ctb[i], "...", sep = ""))
      }
      
      tmp <- googlesheets::gs_key(sheets_keys$observacao[i], verbose = opts$gs$verbose)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(
          tmp, na = opts$gs$na, locale = opts$gs$locale, verbose = opts$gs$verbose, comment = opts$gs$comment)
      )
      n_obs <- nrow(tmp)
      
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
        
        # 'coord_precisao' precisa estar no formato numérico ao invés de inteiro
        if ("coord_precisao" %in% colnames(tmp)) {
          tmp$coord_precisao <- as.numeric(tmp$coord_precisao)
        }
      }
      
      # OBSERVAÇÕES SEM COORDENADAS
      ## Verificar se existem observações sem coordenadas
      n_missing <- sum(is.na(tmp$coord_x))
      if (n_missing > 0) {
      
        ## Se necessário, descartar as observações sem coordenadas
        if (missing.coords == "drop") {
          
          ## Alerta-se no caso de não haver quaisquer observações com coordenadas
          if (n_missing == n_obs) {
            m <- glue::glue("All observations in {dataset} are missing coordinates. None will be returned.")
            message(m)
            n_obs <- 0
          }
          tmp <- tmp[!is.na(tmp$coord_x), ]
        }
      }
      
      # OBSERVAÇÕES RESTANTES (COM E/OU SEM COORDENADAS)
      if (n_obs >= 1) {
        
        ## Adicionar a identificação do dataset às observações
        obs[[i]] <- cbind(dataset_id = as.character(sheets_keys$ctb[i]), tmp, stringsAsFactors = FALSE)
        
        ## Sistema de referência de coordenadas
        ## Verificar se existem observações com coordenadas e identificá-las.
        if (n_obs > n_missing) {
          id_coords <- which(!is.na(obs[[i]]$coord_x))
          
          ## Verificar se o sistema de referência de coordenadas deve ser transformado
          if (!is.null(target.crs)) {
            tmp_obs <- obs[[i]][id_coords, ]
            
            ## Verificar quantos são os sistemas de referência de coordenadas usados no dataset
            n_crs <- nlevels(as.factor(tmp_obs$coord_sistema))
            
            if (n_crs > 1) {
              tmp_obs <- split(tmp_obs, as.factor(tmp_obs$coord_sistema))
              
              ## Verificar se algum dos sistemas de referência de coordenadas é igual ao alvo
              if (toupper(target.crs) %in% names(tmp_obs)) {
                j <- which(!names(tmp_obs) %in% toupper(target.crs))
              } else {
                j <- 1:n_crs
              }
              
            } else {
              
            }
          }
        }
      }
      
      
      
      # Verificar se, com a eliminação das observações sem coordenadas, restou alguma observação
      if (nrow(tmp) >= 1) {
        # Transformar SRC
        if (!is.null(target.crs)) {
          # Muitas vezes há diversos SRC...
          if (nlevels(as.factor(obs[[i]]$coord_sistema)) > 1) {
            obs[[i]] <- split(obs[[i]], as.factor(obs[[i]]$coord_sistema))
            if (toupper(target.crs) %in% names(obs[[i]])) {
              j <- which(!names(obs[[i]]) %in% toupper(target.crs))
            } else {
              j <- 1:length(obs[[i]])
            }
            
            obs[[i]][j] <- lapply(obs[[i]][j], function (x) {
              sp::coordinates(x) <- c("coord_x", "coord_y")
              sp::proj4string(x) <- sp::CRS(paste("+init=", tolower(x$coord_sistema[1]), sep = ""))
              x <- sp::spTransform(x, sp::CRS(paste("+init=", tolower(target.crs), sep = "")))
              as.data.frame(x)
            })
            obs[[i]] <- suppressWarnings(dplyr::bind_rows(obs[[i]]))
            obs[[i]]$coord_sistema <- toupper(target.crs)
            
            # No caso de um único SRC... Diferente do SRC alvo...
          } else if (unique(obs[[i]]$coord_sistema) != toupper(target.crs)) {
            sp::coordinates(obs[[i]]) <- c("coord_x", "coord_y")
            sp::proj4string(obs[[i]]) <- sp::CRS(paste("+init=", tolower(obs[[i]]$coord_sistema[1]), sep = ""))
            obs[[i]] <- sp::spTransform(obs[[i]], sp::CRS(paste("+init=", tolower(target.crs), sep = "")))
            obs[[i]] <- as.data.frame(obs[[i]])
            obs[[i]]$coord_sistema <- toupper(target.crs)
          }
        } 
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
    } else if (n == 1) {
      obs <- obs[[1]]
    }
    
    return (obs)
  }
