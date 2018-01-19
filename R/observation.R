#' Get observation data
#'
#' Download observation-specific data (spatial coordinates, observation date, among others) contained in the
#' Free Brazilian Repository for Open Soil Data -- \pkg{febr}, \url{http://www.ufsm.br/febr}. In \pkg{febr},
#' observation-specific data are stored using a table named \code{"observacao"}. Use \code{\link[febr]{header}}
#' if you want to check what are the variables contained in a dataset before downloading it.
#'
#' @template data_template
#' @template metadata_template
#' 
#' @param missing List with named sub-arguments specifying what should be done with an observation missing 
#' spatial coordinates, \code{coord}, or data on variables, \code{data}? Options are \code{"keep"} (default) 
#' and \code{"drop"}.
#'
#' @param standardization List named sub-arguments specifying how to \emph{standardize} observation-specific
#' data.
#' \itemize{
#' \item \code{crs} EPSG code defining the target coordinate reference system (CRS) to which spatial 
#'       coordinates should be transformed. For example, \code{crs = "EPSG:4674"}, i.e. SIRGAS 2000, the 
#'       standard CRS for Brazil -- see more at \url{http://spatialreference.org/ref/epsg/}. Defaults to
#'       \code{crs = NULL}, i.e. no transformation is performed.
#' \item \code{units} Should the values of the real and integer variable(s) be converted to the standard 
#'       measurement unit(s)? Defaults to \code{units = FALSE}, i.e. no conversion is performed.
#' \item \code{round} Should the values of the real and integer variable(s) be rounded to the standard number 
#'       of decimal places? Effective only when \code{units = TRUE}. Defaults to \code{round = FALSE}, i.e. 
#'       no rounding is performed.
#' }
#' 
#' @details 
#' \subsection{Standard columns}{
#' Standard columns and their content are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification code of the dataset in \pkg{febr} to which an observation belongs.
#' \item \code{observacao_id}. Identification code of an observation in \pkg{febr}.
#' \item \code{sisb_id}. Identification code of an observation in the Brazilian Soil Information System
#' maintained by the Brazilian Agricultural Research Corporation (EMBRAPA) at
#' \url{https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html}.
#' \item \code{ibge_id}. Identification code of an observation in the database of the Brazilian Institute
#' of Geography and Statistics (IBGE) at \url{http://www.downloads.ibge.gov.br/downloads_geociencias.htm#}.
#' \item \code{observacao_data}. Date (dd-mm-yyyy) in which an observation was made.
#' \item \code{coord_sistema}. EPSG code of the coordinate reference system.
#' \item \code{coord_x}. Longitude (°) or Easting (m).
#' \item \code{coord_y}. Latitude (°) or Northing (m).
#' \item \code{coord_precisao}. Precision with which x- and y-coordinates were determined (m).
#' \item \code{coord_fonte}. Source of the x- and y-coordinates.
#' \item \code{pais_id}. Country code (ISO 3166-1 alpha-2).
#' \item \code{estado_id}. Code of the Brazilian federative unit where an observation was made.
#' \item \code{municipio_id}. Name of the Brazilian county where as observation was made.
#' \item \code{amostra_tipo}. Type of sample taken.
#' \item \code{amostra_quanti}. Number of samples taken.
#' \item \code{amostra_area}. Sampling area.
#' }
#' Further details about the content of the standard columns can be found in \url{http://www.ufsm.br/febr/book/}
#' (in Portuguese).
#' }
#'
#' @return A list of data frames or a data frame with observation-specific data on the chosen variable(s) of 
#' the chosen dataset(s).
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{layers}}
#' @export
#' @examples
#' \dontrun{
#' res <- observation(dataset = paste("ctb000", 4:9, sep = ""), variable = "taxon")
#' str(res)
#' }
###############################################################################################################
observation <-
  function (dataset, variable, 
            stack = FALSE, missing = list(coord = "keep", data = "keep"),
            standardization = list(crs = NULL, units = FALSE, round = FALSE),
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
    if (!missing$coord %in% c("drop", "keep")) {
      stop (paste("Unknown value '", missing$coord, "' passed to parameter missing$coord", sep = ""))
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
    if (!is.null(crs) && !toupper(crs) %in% crs_list) {
      stop (paste("Unknown value '", crs, "' passed to parameter crs", sep = ""))
    }
    if (!is.logical(progress)) {
      stop (paste("Unknown value '", progress, "' passed to parameter progress", sep = ""))
    }
    
    # Variáveis padrão
    std_cols <- opts$observations$std.cols
    
    # CHAVES
    ## Descarregar chaves de identificação das tabelas
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)
    
    # Descarregar planilhas com observações
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$observacao), style = 3)
    }
    res <- list()
    for (i in 1:length(sheets_keys$observacao)) {
      
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", dts, "...", sep = ""))
      }
      
      # DESCARREGAMENTO
      ## Cabeçalho com unidades de medida
      unit <- .getHeader(x = sheets_keys$observacao[i])
      ## Dados
      tmp <- .getTable(x = sheets_keys$observacao[i])
      n_rows <- nrow(tmp)
      
      # PROCESSAMENTO I
      ## A decisão pelo processamento dos dados começa pela verificação de dados faltantes nas coordenadas.
      na_coord <- max(apply(tmp[c("coord_x", "coord_y")], 2, function (x) sum(is.na(x))))
      if (missing$coord == "keep" || missing$coord == "drop" && na_coord < n_rows) {
        
        # COLUNAS
        ## Definir as colunas a serem mantidas
        ## As colunas padrão são sempre mantidas.
        ## No caso das colunas adicionais, é possível que algumas não contenham quaisquer dados, assim sendo
        ## ocupadas por 'NA'. Nesse caso, as respectivas colunas são descartadas.  
        in_cols <- colnames(tmp)
        cols <- in_cols[in_cols %in% std_cols]
        extra_cols <- vector()
        if (!missing(variable)) {
          if (variable == "all") {
            extra_cols <- in_cols[!in_cols %in% std_cols]
            idx_na <- apply(tmp[extra_cols], 2, function (x) all(is.na(x)))
            extra_cols <- extra_cols[!idx_na]
          } else {
            extra_cols <- lapply(variable, function (x) in_cols[grep(paste("^", x, sep = ""), in_cols)])
            extra_cols <- unlist(extra_cols)
            extra_cols <- extra_cols[!extra_cols %in% std_cols]
            idx_na <- apply(tmp[extra_cols], 2, function (x) all(is.na(x)))
            extra_cols <- extra_cols[!idx_na]
          }
        }
        cols <- c(cols, extra_cols)
        tmp <- tmp[, cols]
        
        # LINHAS
        ## Definir as linhas a serem mantidas
        tmp <- .cleanRows(obj = tmp, missing = missing, extra_cols = extra_cols)
        n_rows <- nrow(tmp)
        
        # PROCESSAMENTO II
        ## A continuação do processamento dos dados depende das presença de dados após a eliminação de colunas
        ## e linhas com NAs.
        if (n_rows >= 1 && missing(variable) || length(extra_cols) >= 1) {
          
          # TIPO DE DADOS
          ## 'observacao_id', 'sisb_id' e 'ibge_id' precisam estar no formato de caracter para evitar erros
          ## durante o empilhamento das tabelas devido ao tipo de dado.
          ## Nota: esse processamento deve ser feito via Google Sheets.
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
          
          # PADRONIZAÇÃO I
          ## Sistema de referência de coordenadas
          
          ## Verificar se existem observações com coordenadas e se o SRC deve ser transformado
          na_coord <- max(apply(tmp[c("coord_x", "coord_y")], 2, function (x) sum(is.na(x))))
          if (n_rows > na_coord && !is.null(standardization$crs)) {
            
            ## Identificar as observações com coordenadas
            id_coords <- which(!is.na(tmp$coord_x))
            tmp_obs <- tmp[id_coords, ]
            
            ## Verificar se o SRC está faltando
            is_na_crs <- is.na(tmp_obs$coord_sistema)
            if (any(is_na_crs)) {
              is_degree <- nchar(round(abs(tmp_obs$coord_x))) <= 2
              is_na_crs <- which(is_na_crs[is_degree])
              tmp_obs$coord_sistema[is_na_crs] <- crs
            }
            
            ## Verificar se o SRC é o SAD69
            ## Nota: Isso deve ser feito no Google Sheets
            is_sad69 <- tmp_obs$coord_sistema %in% "SAD69"
            if (any(is_sad69)) {
              tmp_obs$coord_sistema[is_sad69] <- "EPSG:4618" 
            }
            
            ## Verificar se o SRC é o SIRGAS
            ## Nota: Isso deve ser feito no Google Sheets
            is_sirgas <- tmp_obs$coord_sistema %in% "SIRGAS"
            if (any(is_sirgas)) {
              tmp_obs$coord_sistema[is_sirgas] <- crs 
            }
            
            ## Verificar quantos são os SRC usados
            n_crs <- nlevels(as.factor(tmp_obs$coord_sistema))
            
            if (n_crs > 1) {
              tmp_obs <- split(tmp_obs, as.factor(tmp_obs$coord_sistema))
              
              ## Verificar se algum dos SRC é igual ao alvo
              if (toupper(crs) %in% names(tmp_obs)) {
                j <- which(!names(tmp_obs) %in% toupper(crs))
              } else {
                j <- 1:n_crs
              }
              
              ## Transformar os SRC
              tmp_obs[j] <- lapply(tmp_obs[j], function (x) {
                sp::coordinates(x) <- c("coord_x", "coord_y")
                sp::proj4string(x) <- sp::CRS(paste("+init=", tolower(x$coord_sistema[1]), sep = ""))
                x <- sp::spTransform(x, sp::CRS(paste("+init=", tolower(crs), sep = "")))
                as.data.frame(x)
              })
              tmp_obs <- suppressWarnings(dplyr::bind_rows(tmp_obs))
              tmp_obs$coord_sistema <- toupper(crs)
              
            } else if (tmp_obs$coord_sistema[1] != toupper(crs)) {
              
              ## Transformar o SRC
              sp::coordinates(tmp_obs) <- c("coord_x", "coord_y")
              sp::proj4string(tmp_obs) <- sp::CRS(paste("+init=", tolower(tmp_obs$coord_sistema[1]), sep = ""))
              tmp_obs <- sp::spTransform(tmp_obs, sp::CRS(paste("+init=", tolower(crs), sep = "")))
              tmp_obs <- as.data.frame(tmp_obs)
              tmp_obs$coord_sistema <- toupper(crs)
            }
            
            ## Agrupar observações com e sem coordenadas
            tmp <- rbind(tmp_obs, tmp[-id_coords, ])
            
          }
          
          # PADRONIZAÇÃO II
          ## Unidade de medida e número de casas decimais
          if (standardization$units) {
            if (standardization$round) {
              #
            }
          }
          
          # IDENTIFICAÇÃO
          ## Código de identificação do conjunto de dados
          res[[i]] <- cbind(dataset_id = as.character(sheets_keys$ctb[i]), tmp, stringsAsFactors = FALSE)[cols]
          
          if (progress) {
            utils::setTxtProgressBar(pb, i)
          }
          
        } else {
          res[[i]] <- data.frame()
          m <- glue::glue("All observations in {dts} are missing data. None will be returned.")
          message(m)
        }
      } else {
        res[[i]] <- data.frame()
        m <- glue::glue("All observations in {dts} are missing coordinates. None will be returned.")
        message(m)
      }
    }
    if (progress) {
      close(pb)
    }
    
    # FINAL
    ## Empilhar conjuntos de dados
    if (stack) {
      res <- suppressWarnings(dplyr::bind_rows(res))
    } else if (n == 1) {
      res <- res[[1]]
    }
    
    return (res)
  }
