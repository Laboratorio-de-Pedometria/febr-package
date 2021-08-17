#' Get 'observation' table
#'
#' Download data from the 'observation' ("observacao") table of one or more datasets published in
#' the Free Brazilian Repository for Open Soil Data (FEBR), \url{https://www.pedometria.org/febr/}.
#' This table includes data such as latitude, longitude, date of observation, underlying geology,
#' land use and vegetation, local topography, soil classification, and much more.
#' 
#' @template data_template
#' @template metadata_template
#' 
#' @param febr.repo (optional) Defaults to the remote file directory of the Federal University of
#' Technology - Paraná at \url{https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso}. 
#' Alternatively, a local directory path can be informed if the user has a local copy of the data
#' repository.
#' 
#' @param missing (optional) List with named sub-arguments indicating what should be done with an
#' observation missing spatial coordinates, `coord`, date of observation, `time`, or data on
#' variables, `data`. Options are `"keep"` (default) and `"drop"`.
#'
#' @param standardization (optional) List with named sub-arguments indicating how to perform data 
#' standardization.
#' \itemize{
#' \item `crs` Character string indicating the EPSG code of the coordinate reference system (CRS)
#' to which spatial coordinates should be transformed. For example, `crs = "EPSG:4674"`, i.e.
#' SIRGAS 2000, the standard CRS for Brazil. Defaults to `crs = NULL`, i.e. no transformation 
#' is performed.
#' 
#' \item `time.format` Character string indicating how to format dates. For example, 
#' \code{time.format = "\%d-\%m-\%Y"}, i.e. dd-mm-yyyy such as in 31-12-2001. Defaults to 
#' `time.format = NULL`, i.e. no formatting is performed. See [base::as.Date()] for more 
#' details.
#' 
#' \item `units` Logical value indicating if the measurement unit(s) of the continuous variable(s)
#' should be converted to the standard measurement unit(s). Defaults to `units = FALSE`, i.e. no
#' conversion is performed. See [febr::dictionary()] for more information.
#' 
#' \item `round` Logical value indicating if the values of the continuous variable(s) should be
#' rounded to the standard number of decimal places. Requires `units = TRUE`. Defaults to
#' `round = FALSE`, i.e. no rounding is performed. See [febr::dictionary()] for more
#' information.
#' }
#' 
#' @param harmonization (optional) List with named sub-arguments indicating if and how to perform
#' data harmonization.
#' \itemize{
#' \item `harmonize` Logical value indicating if data should be harmonized. Defaults to
#' `harmonize = FALSE`, i.e. no harmonization is performed.
#' 
#' \item `level` Integer value indicating the number of levels of the identification code of the
#' variable(s) that should be considered for harmonization. Defaults to `level = 2`. See
#' \sQuote{Details} for more information.
#' }
#'
#' @details 
#' \subsection{Standard identification variables}{
#' Standard identification variables and their content are as follows:
#' \itemize{
#' \item `dataset_id`. Identification code of the dataset in the FEBR to which an observation
#' belongs.
#' \item `observacao_id`. Identification code of an observation in a dataset.
#' \item `sisb_id`. Identification code of an observation in the Brazilian Soil Information System
#' maintained by the Brazilian Agricultural Research Corporation (EMBRAPA).
#' \item `ibge_id`. Identification code of an observation in the database of the Brazilian Institute
#' of Geography and Statistics (IBGE).
#' \item `observacao_data`. Date (dd-mm-yyyy) in which an observation was made.
#' \item `coord_sistema`. EPSG code of the coordinate reference system.
#' \item `coord_x`. Longitude (deg) or easting (m).
#' \item `coord_y`. Latitude (deg) or northing (m).
#' \item `coord_precisao`. Precision with which x- and y-coordinates were determined (m).
#' \item `coord_fonte`. Source of the x- and y-coordinates.
#' \item `pais_id`. Country code (ISO 3166-1 alpha-2).
#' \item `estado_id`. Code of the Brazilian federative unit where an observation was made.
#' \item `municipio_id`. Name of the Brazilian municipality where as observation was made.
#' \item `amostra_tipo`. Type of sample taken.
#' \item `amostra_quanti`. Number of samples taken.
#' \item `amostra_area`. Sampling area.
#' }
#' Further details about the content of the standard identification variables can be found in 
#' \url{https://docs.google.com/document/d/1Bqo8HtitZv11TXzTviVq2bI5dE6_t_fJt0HE-l3IMqM}
#' (in Portuguese).
#' }
#' 
#' \subsection{Harmonization}{
#' Data harmonization consists of converting the values of a variable determined using some method
#' *B* so that they are (approximately) equivalent to the values that would have been obtained if
#' the standard method *A* had been used instead. For example, converting carbon content values
#' obtained using a wet digestion method to the standard dry combustion method is data
#' harmonization.
#'
#' A heuristic data harmonization procedure is implemented in the __febr__ package. It consists of
#' grouping variables based on a chosen number of levels of their identification code. For example,
#' consider a variable with an identification code composed of four levels, `aaa_bbb_ccc_ddd`, where
#' `aaa` is the first level and `ddd` is the fourth level. Now consider a related variable,
#' `aaa_bbb_eee_fff`. If the harmonization is to consider all four coding levels (`level = 4`),
#' then these two variables will remain coded as separate variables. But if `level = 2`, then both
#' variables will be re-coded as `aaa_bbb`, thus becoming the same variable.
#' }
#'
#' @return A list of data frames or a data frame with data on the chosen variable(s) of the chosen
#' dataset(s).
#'
#' @note Check the new core data download function [febr::readFEBR()].
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso [febr::layer()], [febr::dictionary()], [febr::unit()]
#' @export
#' 
#' @examples
#' res <- observation(data.set = "ctb0013")
#' 
#' if (interactive()) {
#' # Download two data sets and standardize CRS
#' res <- observation(
#'   data.set = paste("ctb000", 4:5, sep = ""),
#'   variable = "taxon",
#'   standardization = list(crs = "EPSG:4674"))
#'
#' # Try to download a data set that is not available yet
#' res <- observation(data.set = "ctb0020")
#' 
#' # Try to download a non existing data set
#' #res <- observation(data.set = "ctb0000")
#' 
#' # Try to read all files from local directory
#' febr.repo <- "~/ownCloud/febr-repo/publico"
#' febr.repo <- ifelse(dir.exists(febr.repo), febr.repo, NULL)
#' res <- observation(data.set = "all", febr.repo = febr.repo)
#' }
####################################################################################################
observation <-
  function(data.set, variable, stack = FALSE,
           missing = list(coord = "keep", time = "keep", data = "keep"),
           standardization = list(crs = NULL, time.format = NULL, units = FALSE, round = FALSE),
           harmonization = list(harmonize = FALSE, level = 2),
           progress = TRUE, verbose = TRUE, febr.repo = NULL) {
    
    # OPÇÕES E PADRÕES
    opts <- .opt()
    std_cols <- opts$observation$std.cols
    # ARGUMENT CHECK ----
    ## data.set
    if (missing(data.set)) {
      stop("Argument 'data.set' is missing")
    } else if (!is.character(data.set)) {
      stop(paste0("Object of class ", class(data.set), " passed to 'data.set'"))
    } else {
      dataset_ids <- readIndex()[["dados_id"]]
      if (data.set[1] != "all") {
        idx_out <- data.set %in% dataset_ids
        if (sum(idx_out) != length(data.set)) {
          stop(paste0("Unknown value '", data.set[!idx_out], "' passed to 'data.set'"))
        } else {
          dataset_ids <- data.set
        }
      }
    }
    n_datasets <- length(dataset_ids)
    ## variable
    if (!missing(variable) && !is.character(variable)) {
      stop(paste0("object of class '", class(variable), "' passed to 'variable'"))
    }
    ## stack
    if (!is.logical(stack)) {
      stop(paste0("object of class '", class(stack), "' passed to 'stack'"))
    }
    ## missing
    if (!missing(missing)) {
      if (is.null(missing$coord)) {
        missing$coord <- "keep"
      } else if (!missing$coord %in% c("drop", "keep")) {
        stop(paste0("unknown value '", missing$coord, "' passed to 'missing$coord'"))
      }
      if (is.null(missing$time)) {
        missing$time <- "keep"
      } else if (!missing$time %in% c("drop", "keep")) {
        stop(paste0("unknown value '", missing$time,  "' passed to 'missing$time'"))
      }
      if (is.null(missing$data)) {
        missing$data <- "keep"
      } else if (!missing$data %in% c("drop", "keep")) {
        stop(paste0("unknown value '", missing$data,  "' passed to 'missing$data'"))
      }
    }
    ## standardization
    if (!missing(standardization)) {
      if (is.null(standardization$crs)) {
        standardization$crs <- NULL
      } else if (!is.character(standardization$crs)) {
        y <- class(standardization$crs)
        stop(paste0("object of class '", y, "' passed to 'standardization$crs'"))
      } else if (!toupper(standardization$crs) %in% opts$crs) {
        y <- standardization$crs
        stop(paste0("unknown value '", y, "' passed to 'standardization$crs'"))
      }
      if (is.null(standardization$time.format)) {
        standardization$time.format <- NULL
      } else if (!is.character(standardization$time.format)) {
        y <- class(standardization$time.format)
        stop(paste0("object of class '", y, "' passed to 'standardization$time.format'"))
      }
      if (is.null(standardization$units)) {
        standardization$units <- FALSE
      } else if (!is.logical(standardization$units)) {
        y <- class(standardization$units)
        stop(paste0("object of class '", y, "' passed to 'standardization$units'"))
      }
      if (is.null(standardization$round)) {
        standardization$round <- FALSE
      } else if (!is.logical(standardization$round)) {
        y <- class(standardization$round)
        stop(paste0("object of class '", y, "' passed to 'standardization$round'"))
      }
      if (is.null(standardization$units)) {
        standardization$units <- FALSE
      } else if (!is.logical(standardization$units)) {
        y <- class(standardization$units)
        stop(paste0("object of class '", y, "' passed to 'standardization$units'"))
      }
      if (is.null(standardization$round)) {
        standardization$round <- FALSE
      } else if (!is.logical(standardization$round)) {
        y <- class(standardization$round)
        stop(paste0("object of class '", y, "' passed to 'standardization$round'"))
      }
    }
    ## harmonization
    if (!missing(harmonization)) {
      if (is.null(harmonization$harmonize)) {
        harmonization$harmonize <- FALSE
      } else if (!is.logical(harmonization$harmonize)) {
        y <- class(harmonization$harmonize)
        stop(paste0("object of class '", y, "' passed to 'harmonization$harmonize'"))
      }
      if (is.null(harmonization$level)) {
        harmonization$level <- 2
      } else if (!.isNumint(harmonization$level)) {
        y <- class(harmonization$level)
        stop(paste0("object of class '", y, "' passed to 'harmonization$level'"))
      }
    }
    ## progress
    if (!is.logical(progress)) {
      stop(paste0("object of class '", class(progress), "' passed to 'progress'"))
    }
    ## verbose
    if (!is.logical(verbose)) {
      stop(paste0("object of class '", class(verbose), "' passed to 'verbose'"))
    }
    ## variable + stack || variable + harmonization
    if (!missing(variable) && all(variable == "all")) {
      if (stack) {
        stop("data cannot be stacked when downloading all variables")
      }
      if (harmonization$harmonize) {
        stop("data cannot be harmonized when downloading all variables")
      }
    }
    ## data.set + stack
    if (stack && length(data.set) == 1 && data.set != "all") {
      message("A single dataset is being downloaded... setting stack = FALSE")
      stack <- FALSE
    }
    # PADRÕES
    ## Descarregar tabela com unidades de medida e número de casas decimais quando padronização é
    ## solicitada ou quando empilhamento é solicitado
    if (standardization$units || stack) {
      febr_stds <- .getStds()
      febr_unit <- .readGoogleSheetCSV(sheet.name = "unidades")
    }
    ## stack + stadardization
    ## Padronização não precisa ser feita no caso de descarregamento apenas das variáveis padrão
    ## Também não precisa ser feita no caso de variáveis de tipo 'texto'
    if (stack && !standardization$units && !missing(variable) && variable != "all") {
      tmp_var <- paste("^", variable, sep = "")
      idx <- lapply(tmp_var, function(pattern) grep(pattern = pattern, x = febr_stds$campo_id))
      idx <- unlist(idx)
      is_all_text <- all(febr_stds$campo_tipo[idx] == "texto")
      if (!is_all_text) {
        stop("Data cannot be stacked when measurement units are not standardized")
      }
    }
    # Descarregar planilhas com observações
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = n_datasets, style = 3)
    }
    res <- list()
    for (i in seq_along(dataset_ids)) {
      # Informative messages
      dts <- dataset_ids[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Reading ", dts, "-observacao...", sep = ""))
      }
      # DESCARREGAMENTO
      tmp <- .readFEBR(
        data.set = dataset_ids[i], data.table = "observacao", febr.repo = febr.repo)
      if (inherits(tmp, "data.frame")) {
        unit <- .readFEBR(
          data.set = dataset_ids[i], data.table = "metadado", febr.repo = febr.repo)
        unit$campo_unidade[is.na(unit$campo_unidade)] <- "-"
        unit <- unit[unit$tabela_id == "observacao", c("campo_id", "campo_nome", "campo_unidade")]
        unit <- as.data.frame(t(unit), stringsAsFactors = FALSE)
        colnames(unit) <- unlist(unit[1, ])
        unit <- unit[-1, ]
        n_rows <- nrow(tmp)
        # PROCESSAMENTO I
        ## A decisão pelo processamento dos dados começa pela verificação de dados faltantes nas
        ## coordenadas e na data.
        na_coord <- max(apply(tmp[, c("coord_x", "coord_y")], 2, function(x) sum(is.na(x))))
        na_time <- is.na(tmp$observacao_data)
        n_na_time <- sum(na_time)
        if (missing$coord == "keep" && missing$time == "keep" ||
            missing$coord == "drop" && na_coord < n_rows && missing$time == "keep" |
            missing$time == "drop" ||
            missing$coord == "keep" | missing$coord == "drop" && missing$time == "drop" &&
            n_na_time < n_rows) {
          # COLUNAS
          ## Definir as colunas a serem mantidas
          ## As colunas padrão são sempre mantidas.
          ## No caso das colunas adicionais, é possível que algumas não contenham quaisquer dados,
          ## assim sendo ocupadas por 'NA'. Nesse caso, as respectivas colunas são descartadas.
          in_cols <- colnames(tmp)
          cols <- in_cols[in_cols %in% std_cols]
          extra_cols <- vector()
          if (!missing(variable)) {
            if (length(variable) == 1 && variable == "all") {
              extra_cols <- in_cols[!in_cols %in% std_cols]
              idx_na <- apply(tmp[extra_cols], 2, function(x) all(is.na(x)))
              extra_cols <- extra_cols[!idx_na]
            } else {
              extra_cols <- lapply(variable, function(x) {
                in_cols[grep(paste("^", x, sep = ""), in_cols)]
              })
              extra_cols <- unlist(extra_cols)
              extra_cols <- extra_cols[!extra_cols %in% std_cols]
              idx_na <- apply(tmp[extra_cols], 2, function(x) all(is.na(x)))
              extra_cols <- extra_cols[!idx_na]
            }
          }
          cols <- c(cols, extra_cols)
          tmp <- tmp[cols]
          unit <- unit[cols]
          
          # LINHAS I
          ## Avaliar limpeza das linhas
          tmp_clean <- .cleanRows(obj = tmp, missing = missing, extra_cols = extra_cols)
          n_rows <- nrow(tmp_clean)
          
          # PROCESSAMENTO II
          ## A continuação do processamento dos dados depende das presença de dados após a eliminação
          ## de colunas e linhas com NAs.
          if (n_rows >= 1 && missing(variable) || missing$data == "keep") {
            # LINHAS II
            ## Definir as linhas a serem mantidas
            ## É preciso considerar todas as possibilidades de remoção de dados
            if (missing$data == "drop" || missing$coord == 'drop' || missing$time == 'drop') {
              tmp <- tmp_clean
            }
            
            # TIPO DE DADOS
            ## 'observacao_id', 'sisb_id' e 'ibge_id' precisam estar no formato de caracter para
            ## evitar erros durante o empilhamento das tabelas devido ao tipo de dado.
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
            ## Primeiro verificar se existem observações com coordenadas e se o SRC deve ser
            ## transformado
            na_coord <- max(apply(tmp[, c("coord_x", "coord_y")], 2, function(x) sum(is.na(x))))
            if (n_rows > na_coord && !is.null(standardization$crs)) {
              tmp <- .crsTransform(obj = tmp, crs = standardization$crs)
            }
            
            # PADRONIZAÇÃO II
            ## Data de observação
            if (n_rows > n_na_time && !is.null(standardization$time.format)) {
              # if (n_rows > na_time && !is.null(standardization$time.format)) {
              tmp <- .formatObservationDate(obj = tmp, time.format = standardization$time.format)
            }
            
            # PADRONIZAÇÃO III
            ## Unidade de medida e número de casas decimais das colunas adicionais
            if (standardization$units && length(extra_cols) >= 1) {
              
              ## Identificar variáveis contínuas (classe 'numeric' e 'integer'), excluíndo variáveis
              ## de  identificação padrão
              ## TODO: EXCETO 'coord_precisao'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              id_class <- sapply(tmp, class)
              cont_idx <- which(id_class %in% c("numeric", "integer") &
                                  !names(id_class) %in% std_cols)
              if (length(cont_idx) >= 1) {
                
                # Tabela com padrões das variáveis contínuas identificadas
                tmp_stds <- match(cols[cont_idx], febr_stds$campo_id)
                tmp_stds <- febr_stds[tmp_stds, c("campo_id", "campo_unidade", "campo_precisao")]
                
                ## 1. Se necessário, padronizar unidades de medida
                ## # verifica a 2ª linha de metadados
                need_idx <- unit[2, cols[cont_idx]] != tmp_stds$campo_unidade
                if (any(need_idx)) {
                  need_name <- cols[cont_idx][need_idx]
                  source <- unit[2, need_name]
                  target <- tmp_stds$campo_unidade[match(need_name, tmp_stds$campo_id)]
                  
                  ## Identificar constante
                  k <- lapply(seq_along(source), function(i) {
                    # i <- 2
                    idx <- febr_unit$unidade_origem %in% source[i] +
                      febr_unit$unidade_destino %in% target[i]
                    febr_unit[idx == 2, ] 
                  })
                  k <- do.call(rbind, k)
                  
                  ## Processar dados
                  tmp[need_name] <- mapply(`*`, tmp[need_name], k$unidade_constante)
                  unit[2, need_name] <- k$unidade_destino
                }
                
                ## 2. Se necessário, padronizar número de casas decimais
                if (standardization$round) {
                  tmp[tmp_stds$campo_id] <- 
                    sapply(seq(nrow(tmp_stds)), function(i) 
                      round(x = tmp[tmp_stds$campo_id[i]], digits = tmp_stds$campo_precisao[i]))
                }
              }
            }
            
            # ATTRIBUTOS I
            ## Processar unidades de medida
            unit[2, ] <- as.character(unit[2, names(unit) %in% cols])
            unit[2, ] <- gsub("^-$", "unitless", unit[2, ])
            # https://en.wikipedia.org/wiki/List_of_Unicode_characters
            unit["observacao_id"] <- c("Identifica\u00E7\u00E3o da observa\u00E7\u00E3o", "unitless")
            dataset_id <- c("Identifica\u00E7\u00E3o do conjunto de dados", "unitless")
            unit <- cbind(dataset_id, unit)
            
            # HARMONIZAÇÃO I
            ## Harmonização dos dados das colunas adicionais
            if (harmonization$harmonize && length(extra_cols) >= 1) {
              
              ## Harmonização baseada nos níveis dos códigos de identificação
              tmp <- .harmonizeByName(obj = tmp, extra_cols = extra_cols,
                                      harmonization = harmonization)
              
            }
            
            # IDENTIFICAÇÃO
            ## Código de identificação do conjunto de dados
            res[[i]] <- cbind(dataset_id = dataset_ids[i], tmp, stringsAsFactors = FALSE)
            
            # ATTRIBUTOS II
            a <- attributes(res[[i]])
            
            ## Adicionar nomes reais
            a$field_name <- as.vector(t(unit)[, 1])
            
            ## Adicionar unidades de medida
            a$field_unit <- as.vector(t(unit)[, 2])
            attributes(res[[i]]) <- a
            if (progress) {
              utils::setTxtProgressBar(pb, i)
            }
            
          } else {
            res[[i]] <- data.frame()
            m <- paste("All observations in {dts} are missing data. None will be returned.")
            message(m)
          }
        } else {
          res[[i]] <- data.frame()
          if (na_coord == n_rows) {
            m <-
              paste0("All observations in", dts, "are missing coordinates. None will be returned.") 
          } else if (n_na_time == n_rows) {
            m <- paste("All observations in", dts, "are missing date. None will be returned.")  
          }
          message(m)
        }
      } else {
        res[[i]] <- tmp
      }
    }
    if (progress) {
      close(pb)
    }
    # FINAL
    ## Empilhar conjuntos de dados
    ## Adicionar unidades de medida
    if (stack) {
      res <- .stackTables(obj = res)
    } else if (n_datasets == 1 & inherits(res, "list")) {
      res <- res[[1]]
    } else {
      names(res) <- data.set
    }
    return(res)
  }
