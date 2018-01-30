#' Get *layer* table
#'
#' Download data from the *layer* ("camada") table of one or more datasets contained in the Free Brazilian
#' Repository for Open Soil Data -- ___febr___, \url{http://www.ufsm.br/febr}. This includes sampling depth,
#' horizon designation, and variables such as pH, carbon content, clay content, and much more. Use 
#' \code{\link[febr]{header}} if you want to check what are the variables contained in the *layer* table of a
#' dataset before downloading it.
#' 
#' @template data_template
#' @template metadata_template
#'
#' @param missing (optional) List with named sub-arguments indicating what should be done with a layer missing
#' data on sampling depth, `depth`, or data on variable(s), `data`? Options are `"keep"` (default) and `"drop"`.
#' 
#' @param standardization (optional) List with named sub-arguments indicating how to perform data 
#' standardization.
#' \itemize{
#' \item `plus.sign` Character string indicating what should be done with the plus sign (`+`) commonly used
#'        along with the inferior limit of the bottom layer of an observation. Options are `"keep"` (default),
#'        `"add"`, and `"remove"`.
#' \item `plus.depth` Numeric value indicating the depth increment (in centimetres) when processing the plus
#'        sign (`+`) with `plus.sign = "add"`. Defaults to `plus.depth = 2.5`.
#' \item `transition` Character string indicating what should be done about wavy, irregular, and broken
#'        transitions between layers in an observation? Options are `"keep"` (default) and `"smooth"`.
#' \item `smoothing.fun` Character string indicating the function that should be used to smooth wavy and
#'       irregular transitions between layers in an observation when `transition = "smooth"`. Options are 
#'       `"mean"` (default), `"min"`, `"max"`, and `"median"`.
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
#' \item \code{dataset_id}. Identification code of the dataset in ___febr___ to which an observation belongs.
#' \item \code{observacao_id}. Identification code of an observation in ___febr___.
#' \item \code{camada_numero}. Sequential layer number, from top to bottom.
#' \item \code{camada_nome}. Layer designation according to some standard description guide.
#' \item \code{amostra_codigo}. Laboratory number of a sample.
#' \item \code{profund_sup}. Upper boundary of a layer (cm).
#' \item \code{profund_inf}. Lower boundary of a layer (cm).
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
#' @seealso \code{\link[febr]{observation}}, \code{\link[febr]{standard}}
# \code{\link[febr]{conversion}}
#' @export
#' @examples
#' \dontrun{
#' res <- layer(dataset = paste("ctb000", 4:9, sep = ""))
#' str(res)
#' }
###############################################################################################################
layer <-
  function (dataset, variable,
            stack = FALSE, missing = list(depth = "keep", data = "keep"),
            standardization = list(
              plus.sign = "keep", plus.depth = 2.5, 
              transition = "keep", smoothing.fun = "mean",
              units = FALSE, round = FALSE),
            harmonization = list(harmonize = FALSE, level = 2),
            progress = TRUE, verbose = TRUE) {
    
    # OPÇÕES E PADRÕES
    opts <- .opt()
    std_cols <- opts$layer$std.cols
    
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
      if (is.null(missing$depth)) {
        missing$depth <- "keep"
      } else if (!missing$depth %in% c("drop", "keep")) {
        stop (glue::glue("unknown value '{missing$depth}' passed to sub-argument 'missing$depth'"))
      }
      if (is.null(missing$data)) {
        missing$data <- "keep"
      } else if (!missing$data %in% c("drop", "keep")) {
        stop (glue::glue("unknown value '{missing$data}' passed to sub-argument 'missing$data'"))
      }
    }
    
    ## standardization
    if (!missing(standardization)) {
      if (is.null(standardization$plus.sign)) {
        standardization$plus.sign <- "keep"
      } else if (!standardization$plus.sign %in% c("add", "remove", "keep")) {
        y <- standardization$plus.sign
        stop (glue::glue("unknown value '{y}' passed to sub-argument 'standardization$plus.sign'"))
      }
      if (is.null(standardization$plus.depth)) {
        standardization$plus.depth <- 2.5  
      } else if (standardization$plus.depth > 100 || standardization$plus.depth < 0) {
        y <- standardization$plus.depth
        stop (glue::glue("unlikely value '{y}' passed to sub-argument 'standardization$plus.depth'"))
      }
      if (is.null(standardization$transition)) {
        standardization$transition <- "keep"
      } else if (!standardization$transition %in% c("smooth", "keep")) {
        y <- standardization$transition
        stop (glue::glue("unknown value '{y}' passed to sub-argument 'standardization$transition'"))
      }
      if (is.null(standardization$smoothing.fun)) {
        standardization$smoothing.fun <- "mean"
      } else if (!standardization$smoothing.fun %in% c("mean", "min", "max", "median")) {
        y <- standardization$smoothing.fun
        stop(glue::glue("unknown value '{y}' passed to sub-argument 'standardization$smoothing.fun'"))
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
    
    # Descarregar tabelas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$camada), style = 3)
    }
    res <- list()
    for (i in 1:length(sheets_keys$camada)) {
      
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", dts, "...", sep = ""))
      }
      
      # DESCARREGAMENTO
      ## Cabeçalho com unidades de medida
      unit <- .getHeader(x = sheets_keys$camada[i])
      ## Dados
      tmp <- .getTable(x = sheets_keys$camada[i])
      n_rows <- nrow(tmp)
      
      # PROCESSAMENTO I
      ## A decisão pelo processamento dos dados começa pela verificação de dados faltantes nas profundidades
      na_depth <- max(apply(tmp[c("profund_sup", "profund_inf")], 2, function (x) sum(is.na(x))))
      if (missing$depth == "keep" || missing$depth == "drop" && na_depth < n_rows) {
        
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
        if (n_rows >= 1 && missing(variable) || length(extra_cols) >= 1 || missing$data == "keep") {
          
          # TIPO DE DADOS
          ## "observacao_id", "camada_numero", "camada_nome", "amostra_codigo", "profund_sup" e "profund_inf"
          ## precisam estar no formato de carácter para evitar erros durante o empilhamento das tabelas
          ## devido ao tipo de dado.
          ## Nota: esse processamento deve ser feito via Google Sheets.
          tmp[std_cols] <- sapply(tmp[std_cols], as.character)
          
          # PADRONIZAÇÃO I
          ## Profundidade e transição entre as camadas
          
          ## Sinal positivo em 'profund_inf' indicando maior profundidade do abaixo da última camada
          ## O padrão consiste em manter o sinal positivo.
          if (standardization$plus.sign != "keep") {
            tmp <- .setMaximumObservationDepth(
              obj = tmp, plus.sign = standardization$plus.sign, plus.depth = standardization$plus.depth)
          }
          
          ## Transição ondulada ou irregular
          ## O padrão consiste em manter a transição ondulada ou irregular.
          if (standardization$transition != "keep") {
            tmp <- .solveIrregularLayerTransition(obj = tmp, smoothing.fun = standardization$smoothing.fun)
            
            # What to do with broken layer transitions?
            # tmp <- .solveBrokenLayerTransition(obj = tmp)
            
          }
          
          ## Se a profundidade foi padronizada, então os dados devem ser definidos como tipo 'Real'
          if (standardization$plus.sign != "keep" || standardization$transition != "keep") {
            tmp[c("profund_sup", "profund_inf")] <- sapply(tmp[c("profund_sup", "profund_inf")], as.numeric)
          }
          
          # PADRONIZAÇÃO II
          ## Unidade de medida e número de casas decimais
          if (standardization$units) {
            message("Standardization of measurement units is not available yet")
            # fe_type <- stringr::str_split_fixed(soil_vars, "_", n = 3)[, 2]
            # fe_stand <- lapply(fe_type, function (y) standards(soil.var = "fe", extraction.method = y))
            # fe_stand <- do.call(rbind, fe_stand)
            # 
            # # 1. Se necessário, padronizar unidades de medida
            # idx_unit <- which(!unit[, soil_vars] %in% unique(standards(soil.var = "fe")$unit))
            # if (length(idx_unit) >= 1) {
            #   conv_factor <- lapply(1:length(fe_type[idx_unit]), function (j) {
            #     conversion(source=unlist(unit[, soil_vars[idx_unit]])[[j]],target=fe_stand$unit[idx_unit][j])
            #   })
            #   conv_factor <- do.call(rbind, conv_factor)
            #   tmp[soil_vars[idx_unit]] <- t(t(tmp[soil_vars[idx_unit]]) * conv_factor$factor)
            # }
            # 
            # 2. Padronizar número de casas decimais
            if (standardization$round) {
              message("Standardization of decimal places is not available yet")
            #   tmp[, soil_vars] <- sapply(1:length(soil_vars), function (j) {
            #     round(tmp[, soil_vars[j]], digits = fe_stand$digits[j])
            #   }) 
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
          
          # ATTRIBUTOS
          ## Se os conjuntos de dados não são empilhados, adicionar unidades de medida
          if (!stack) {
            a <- attributes(res[[i]])
            a$units <- c(rep("unitless", 2), gsub("^-$", "unitless", as.character(unit)[-1]))
            attributes(res[[i]]) <- a
          }
          
          if (progress) {
            utils::setTxtProgressBar(pb, i)
          }
          
        } else {
          res[[i]] <- data.frame()
          m <- glue::glue("All layers in {dts} are missing data. None will be returned.")
          message(m)
        }
      } else {
        res[[i]] <- data.frame()
        m <- glue::glue("All layers in {dts} are missing depth. None will be returned.")
        message(m)
      }
    }
    if (progress) {
      close(pb)
    }
    
    # FINAL
    ## Empilhar conjuntos de dados
    ## Adicionar unidades de medida
    if (stack) {
      res <- suppressWarnings(dplyr::bind_rows(res))
      # soil_vars <- colnames(res)[grep("^fe_", colnames(res))]
      # fe_type <- stringr::str_split_fixed(soil_vars, "_", n = 3)[, 2]
      # fe_stand <- sapply(fe_type, function (y) standards(soil.var = "fe", extraction.method = y)$unit)
      # a <- attributes(res)
      # a$units <- c(rep("unitless", 5), rep("cm", 2), as.character(fe_stand))
      # attributes(res) <- a
    } else if (n == 1) {
      res <- res[[1]]
    }
    
    return (res)
  }
