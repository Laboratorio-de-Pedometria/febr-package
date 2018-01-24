#' Get *observation* table
#'
#' Download data from the *observation* ("observacao") table of one or more datasets contained in the Free
#' Brazilian Repository for Open Soil Data -- ___febr___, \url{http://www.ufsm.br/febr}. This includes spatial
#' coordinates, observation date, and variables such as geology, land use and vegetation, local topography, and
#' much more. Use \code{\link[febr]{header}} if you want to check what are the variables contained in the 
#' *observation* table of a dataset before downloading it.
#' 
#' @template data_template
#' @template metadata_template
#' 
#' @param missing (optional) List with named sub-arguments indicating what should be done with an observation
#' missing spatial coordinates, `coord`, or data on variables, `data`? Options are `"keep"` (default) and
#' `"drop"`.
#'
#' @param standardization (optional) List with named sub-arguments indicating how to perform data 
#' standardization.
#' \itemize{
#' \item `crs` Character string indicating the EPSG code of the coordinate reference system (CRS) to which
#'       spatial coordinates should be transformed. For example, `crs = "EPSG:4674"`, i.e. SIRGAS 2000, the
#'       standard CRS for Brazil -- see more at \url{http://spatialreference.org/ref/epsg/}. Defaults to 
#'       `crs = NULL`, i.e. no transformation is performed.
#' \item `units` Logical value indicating if the measurement units of the real and integer variable(s) should be
#'       converted to the standard measurement unit(s). Defaults to `units = FALSE`, i.e. no conversion is
#'       performed. See \code{\link[febr]{standard}} for more information. (NOT AVAILABLE AT THE MOMENT!)
#' \item `round` Logical value indicating if the values of the real and integer variable(s) should be rounded  
#'       to the standard number of decimal places. Effective only when `units = TRUE`. Defaults to 
#'       `round = FALSE`, i.e. no rounding is performed. See \code{\link[febr]{standard}} for more information.
#'       (NOT AVAILABLE AT THE MOMENT!)
#' }
#' 
#' @param harmonization (optional) List with named sub-arguments indicating if and how to perform data 
#' harmonization.
#' \itemize{
#' \item `harmonize` Logical value indicating if data should be harmonized? Defaults to `harmonize = FALSE`,
#'       i.e. no harmonization is performed.
#' \item `level` Integer value indicating the number of levels of the identification code of the variable(s) 
#'       that should be considered for harmonization. Defaults to `level = 2`. See \sQuote{Details} for more
#'       information.
#' }
#'
#' @details 
#' \subsection{Standard columns}{
#' Standard columns and their content (in Portuguese) are as follows:
#' \itemize{
#' \item `dataset_id`. Identification code of the dataset in ___febr___ to which an observation belongs.
#' \item `observacao_id`. Identification code of an observation in ___febr___.
#' \item `sisb_id`. Identification code of an observation in the Brazilian Soil Information System
#' maintained by the Brazilian Agricultural Research Corporation (EMBRAPA) at
#' \url{https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html}.
#' \item `ibge_id`. Identification code of an observation in the database of the Brazilian Institute
#' of Geography and Statistics (IBGE) at \url{http://www.downloads.ibge.gov.br/downloads_geociencias.htm#}.
#' \item `observacao_data`. Date (dd-mm-yyyy) in which an observation was made.
#' \item `coord_sistema`. EPSG code of the coordinate reference system.
#' \item `coord_x`. Longitude (°) or Easting (m).
#' \item `coord_y`. Latitude (°) or Northing (m).
#' \item `coord_precisao`. Precision with which x- and y-coordinates were determined (m).
#' \item `coord_fonte`. Source of the x- and y-coordinates.
#' \item `pais_id`. Country code (ISO 3166-1 alpha-2).
#' \item `estado_id`. Code of the Brazilian federative unit where an observation was made.
#' \item `municipio_id`. Name of the Brazilian county where as observation was made.
#' \item `amostra_tipo`. Type of sample taken.
#' \item `amostra_quanti`. Number of samples taken.
#' \item `amostra_area`. Sampling area.
#' }
#' Further details about the content of the standard columns can be found in \url{http://www.ufsm.br/febr/book/}
#' (in Portuguese).
#' }
#' 
#' \subsection{Harmonization}{
#' Data harmonization consists of converting the values of a variable determined using some method *B* so 
#' that they are (approximately) equivalent to the values that would have been obtained if the standard method
#' *A* had been used instead. For example, converting carbon content values obtained using a wet digestion
#' method to the standard dry combustion method is data harmonization.
#' 
#' A heuristic data harmonization procedure is implemented **febr**. It consists of grouping variables 
#' based on a chosen number of levels of their identification code. For example, consider a variable with an 
#' identification code composed of four levels, `aaa_bbb_ccc_ddd`, where `aaa` is the first level and
#' `ddd` is the fouth level. Now consider a related variable, `aaa_bbb_eee_fff`. If the harmonization
#' is to consider all four coding levels (`level = 4`), then these two variables will remain coded as
#' separate variables. But if `level = 2`, then both variables will be recoded to `aaa_bbb`, thus becoming the
#' same variable.
#' }
#'
#' @return A list of data frames or a data frame with data on the chosen variable(s) of the chosen dataset(s).
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{layer}}, \code{\link[febr]{standard}}
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
            standardization = list(
              crs = NULL, 
              units = FALSE, round = FALSE),
            harmonization = list(harmonize = FALSE, level = 2),
            progress = TRUE, verbose = TRUE) {
    
    # OPÇÕES E PADRÕES
    opts <- .opt()
    std_cols <- opts$observation$std.cols
    
    # ARGUMENTOS
    ## dataset
    if (missing(dataset)) {
      stop ("argument 'dataset' is missing")
    } else if (!is.character(dataset)) {
      stop (glue::glue("object of class '{class(dataset)}' passed to argument 'dataset'"))
    }
    
    ## variable
    if (!missing(variable) && !is.character(variable)) {
      stop (glue::glue("object of class '{class(variable)}' passed to argument 'variable'"))
    }
    
    ## stack
    if (!is.logical(stack)) {
      stop (glue::glue("object of class '{class(stack)}' passed to argument 'stack'"))
    }
    
    ## missing
    if (!missing(missing)) {
      if (is.null(missing$coord)) {
        missing$coord <- "keep"
      } else if (!missing$coord %in% c("drop", "keep")) {
        stop (glue::glue("unknown value '{missing$coord}' passed to sub-argument 'missing$coord'"))
      }
      if (is.null(missing$data)) {
        missing$data <- "keep"
      } else if (!missing$data %in% c("drop", "keep")) {
        stop (glue::glue("unknown value '{missing$data}' passed to sub-argument 'missing$data'"))
      }
    }
    
    ## standardization
    if (!missing(standardization)) {
      if (is.null(standardization$crs)) {
        standardization$crs <- NULL
      } else if (!is.character(standardization$crs)) {
        y <- class(standardization$crs)
        stop (glue::glue("object of class '{y}' passed to sub-argument 'standardization$crs'"))
      } else if (!toupper(standardization$crs) %in% opts$crs) {
        y <- standardization$crs
        stop (glue::glue("unknown value '{y}' passed to sub-argument 'standardization$crs'"))
      }
      if (is.null(standardization$units)) {
        standardization$units <- FALSE
      } else if (!is.logical(standardization$units)) {
        y <- class(standardization$units)
        stop (glue::glue("object of class '{y}' passed to sub-argument 'standardization$units'"))
      }
      if (is.null(standardization$round)) {
        standardization$round <- FALSE
      } else if (!is.logical(standardization$round)) {
        y <- class(standardization$round)
        stop (glue::glue("object of class '{y}' passed to sub-argument 'standardization$round'"))
      }
    }
    
    ## harmonization
    if (!missing(harmonization)) {
      if (is.null(harmonization$harmonize)) {
        harmonization$harmonize <- FALSE
      } else if (!is.logical(harmonization$harmonize)) {
        y <- class(harmonization$harmonize)
        stop (glue::glue("object of class '{y}' passed to sub-argument 'harmonization$harmonize'"))
      }
      if (is.null(harmonization$level)) {
        harmonization$level <- 2
      } else if (!pedometrics::isNumint(harmonization$level)) {
        y <- class(harmonization$level)
        stop (glue::glue("object of class '{y}' passed to sub-argument 'harmonization$level'"))
      }
    }
    
    ## progress
    if (!is.logical(progress)) {
      stop (glue::glue("object of class '{class(progress)}' passed to argument 'progress'"))
    }
    
    ## verbose
    if (!is.logical(verbose)) {
      stop (glue::glue("object of class '{class(verbose)}' passed to argument 'verbose'"))
    }
    
    ## variable + stack || variable + harmonization
    if (!missing(variable) && variable == "all") {
      if (stack) {
        stop ("data cannot be stacked when downloading all variables")
      }
      if (harmonization$harmonize) {
        stop ("data cannot be harmonized when downloading all variables")
      }
    }
    
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
          ## Primeiro verificar se existem observações com coordenadas e se o SRC deve ser transformado
          na_coord <- max(apply(tmp[c("coord_x", "coord_y")], 2, function (x) sum(is.na(x))))
          if (n_rows > na_coord && !is.null(standardization$crs)) {
            tmp <- .crsTransform(obj = tmp, crs = standardization$crs)
          }
          
          # PADRONIZAÇÃO II
          ## Unidade de medida e número de casas decimais
          if (standardization$units) {
            message("Standardization of measurement units is not available yet")
            if (standardization$round) {
              message("Standardization of decimal places is not available yet")
            }
          }
          
          # HARMONIZAÇÃO I
          ## Harmonização dos dados das colunas adicionais
          if (harmonization$harmonize) {
            
            ## Harmonização baseada nos níveis dos códigos de identificação
            tmp <- .harmonizeByName(obj = tmp, extra_cols = extra_cols, harmonization = harmonization)
            
          }
          
          # IDENTIFICAÇÃO
          ## Código de identificação do conjunto de dados
          res[[i]] <- cbind(dataset_id = as.character(sheets_keys$ctb[i]), tmp, stringsAsFactors = FALSE)
          
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
