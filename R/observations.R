#' Get soil observations
#'
#' Download soil observation-specific data contained in the Free Brazilian Repository for Open Soil Data --
#' \url{http://www.ufsm.br/febr}.
#'
#' @param dataset Identification code of the dataset (or datasets). Use \code{dataset = "all"} to download 
#' all datasets.
#' 
#' @param variable Name(s) of the variable(s). If missing, then a set of standard columns is downloaded. Use
#' \code{variable = "all"} to download all variables. See \sQuote{Details} for more information.
#' 
#' @param stack Should soil observations from different datasets be stacked on a single data frame for
#' output? Defaults to \code{stack = FALSE} and the output is a list of data frames.
#'
#' @param missing.coords What should be done with soil observations missing spatial coordinates? Options are
#' \code{"keep"} (default) and \code{"drop"}.
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
#' res <- observations(dataset = paste("ctb000", 4:9, sep = ""), variable = "taxon")
#' str(res)
#' }
# obs <- observations(dataset = c("ctb0029", "ctb0003"))
# head(obs)
###############################################################################################################
observations <-
  function (dataset, variable,
            stack = FALSE, missing.coords = "keep", target.crs = "EPSG:4674",
            progress = TRUE, verbose = TRUE) {

    # Options
    opts <- .opt()
    
    # Verificar consistência dos parâmetros
    if (!is.logical(stack)) {
      stop (paste("Unknown value '", stack, "' passed to parameter stack", sep = ""))
    }
    if (!missing(variable)) {
      if (variable == "all" && stack == TRUE) {
        stop ("data cannot be stacked when downloading all variables")
      }
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
    if (!is.null(target.crs) && !toupper(target.crs) %in% crs_list) {
      stop (paste("Unknown value '", target.crs, "' passed to parameter target.crs", sep = ""))
    }
    if (!is.logical(progress)) {
      stop (paste("Unknown value '", progress, "' passed to parameter progress", sep = ""))
    }
    
    # Variáveis padrão
    std_cols <- opts$observations$std.cols

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)

    # Descarregar planilhas com observações
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$observacao), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$observacao)) {
      # i <- 1
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", dts, "...", sep = ""))
      }
      
      tmp <- googlesheets::gs_key(sheets_keys$observacao[i], verbose = opts$gs$verbose)
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(
          tmp, na = opts$gs$na, locale = opts$gs$locale, verbose = opts$gs$verbose, comment = opts$gs$comment)
      )
      n_obs <- nrow(tmp)
      
      # Definir as colunas a serem mantidas
      if (missing(variable) || variable != "all") {
        
        # Manter colunas padrão
        in_cols <- colnames(tmp)
        cols <- in_cols %in% std_cols
        cols <- in_cols[cols]
        
        # Manter colunas adicionais
        # Verifica-se se algum dos nomes das colunas inicia com 'variable'.
        # Nomes duplicados entre as colunas padrão e as colunas adicionais são removidos.
        if (!missing(variable)) {
          extra_cols <- lapply(variable, function (x) in_cols[grep(paste("^", x, sep = ""), in_cols)]) 
          extra_cols <- unlist(extra_cols)
          cols <- c(cols, extra_cols)
          idx <- !duplicated(cols)
          cols <- cols[idx]
        }
        tmp <- tmp[, cols]
      }
      
      # Definição do tipo de dados
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
      
      # OBSERVAÇÕES SEM COORDENADAS
      ## Verificar se existem observações sem coordenadas
      n_missing <- sum(is.na(tmp$coord_x))
      if (n_missing > 0) {
      
        ## Se necessário, descartar as observações sem coordenadas
        if (missing.coords == "drop") {
          
          ## Alerta-se no caso de não haver quaisquer observações com coordenadas
          if (n_missing == n_obs) {
            m <- glue::glue("All observations in {dts} are missing coordinates. None will be returned.")
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
            
            ## Verificar se o sistema de referência de coordenadas está faltando
            is_na_crs <- is.na(tmp_obs$coord_sistema)
            if (any(is_na_crs)) {
              is_degree <- nchar(round(abs(tmp_obs$coord_x))) <= 2
              is_na_crs <- which(is_na_crs[is_degree])
              tmp_obs$coord_sistema[is_na_crs] <- target.crs
            }
            
            ## Verificar se o sistema de referência de coordenadas é o SAD69
            is_sad69 <- tmp_obs$coord_sistema %in% "SAD69"
            if (any(is_sad69)) {
              tmp_obs$coord_sistema[is_sad69] <- "EPSG:4618" 
            }
            
            ## Verificar se o sistema de referência de coordenadas é o SIRGAS
            is_sirgas <- tmp_obs$coord_sistema %in% "SIRGAS"
            if (any(is_sirgas)) {
              tmp_obs$coord_sistema[is_sirgas] <- target.crs 
            }
            
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
              
              ## Transformar os sistemas de referência de coordenadas
              tmp_obs[j] <- lapply(tmp_obs[j], function (x) {
                sp::coordinates(x) <- c("coord_x", "coord_y")
                sp::proj4string(x) <- sp::CRS(paste("+init=", tolower(x$coord_sistema[1]), sep = ""))
                x <- sp::spTransform(x, sp::CRS(paste("+init=", tolower(target.crs), sep = "")))
                as.data.frame(x)
              })
              tmp_obs <- suppressWarnings(dplyr::bind_rows(tmp_obs))
              tmp_obs$coord_sistema <- toupper(target.crs)
              
            } else if (tmp_obs$coord_sistema[1] != toupper(target.crs)) {
              
              ## Transformar o sistema de referência de coordenadas
              sp::coordinates(tmp_obs) <- c("coord_x", "coord_y")
              sp::proj4string(tmp_obs) <- sp::CRS(paste("+init=", tolower(tmp_obs$coord_sistema[1]), sep = ""))
              tmp_obs <- sp::spTransform(tmp_obs, sp::CRS(paste("+init=", tolower(target.crs), sep = "")))
              tmp_obs <- as.data.frame(tmp_obs)
              tmp_obs$coord_sistema <- toupper(target.crs)
            }
            
            ## Agrupar observações com e sem coordenadas
            obs[[i]] <- rbind(tmp_obs, obs[[i]][-id_coords, ])
          }
        }
      }
      
      # Organize column names
      # obs[[i]] <- obs[[i]][cols]
      
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }

    # Se necessário, empilhar tabelas
    if (stack) {
      obs <- suppressWarnings(dplyr::bind_rows(obs))
    } else if (n == 1) {
      obs <- obs[[1]]
    }
    
    return (obs)
  }
