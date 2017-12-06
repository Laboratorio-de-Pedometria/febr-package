#' Get soil layers
#'
#' Download soil layer-specific data contained in the Free Brazilian Repository for Open Soil Data --
#' \url{http://www.ufsm.br/febr}.
#' 
#' @param dataset Identification code of the dataset or datasets for which soil layer-specific data should be
#' downloaded -- see \url{http://www.ufsm.br/febr/book}. Use \code{dataset = "all"} to download data from all
#' existing datasets.
#' 
#' @param which.cols Which columns should be returned? Options are \code{"standard"} (default) and
#' \code{"all"}. See \sQuote{Details} for a description of the standard columns.
#' 
#' @param soil.vars Identification code of the soil variables for which soil layer-specific data should be
#' downloaded.
#'
#' @param stack.datasets Should soil layers from different datasets be stacked on a single data frame for
#' output? Used only with \code{which.cols = "standard"}. Defaults to \code{stack.datasets = TRUE}.
#'
#' @param missing.data What should be done with soil layers missing any iron data? Options are \code{"drop"}
#' (default) and \code{"keep"}.
#' 
#' @param standardization List with definitions on how to \emph{standardize} soil layer-specific data. Only 
#' works when \code{soil.vars = 'fe'}.
#' \itemize{
#' \item \code{plus.sign} What should be done with the plus sign ('+') commonly used along with the inferior 
#'       limit of the bottom layer of soil observations? Options are \code{"add"} (default), \code{"remove"},
#'       and \code{"keep"}.
#' \item \code{plus.depth} Depth increment (in centimetres) when processing the plus sign ('+') with 
#'       \code{plus.sign = "add"}. Defaults to \code{plus.depth = 2.5}.
#' \item \code{transition} What should be done about wavy, irregular, and broken transitions between layers in
#'       a soil observation? Options are \code{"smooth"} (default) and \code{"keep"}.
#' \item \code{smoothing.fun} Function that should be used to smooth wavy and irregular transitions between 
#'       layers in a soil observation when \code{transition = "smooth"}. Options are \code{"mean"} (default),
#'       \code{"min"}, \code{"max"}, and \code{"median"}. 
#' }
#'
#' @param harmonization List with definitions on how to \emph{harmonize} soil layer-specific data.
#' \itemize{
#' \item \code{level} Should data on soil variables be harmonized based only on the extraction method, 
#'       \code{level = 1} (default), or on both extraction and measurement methods, \code{level = 2}? See 
#'       \code{\link[febr]{standards}}.
#' }
#'
#' @param progress Show download progress bar?
#' 
#' @param verbose Show informative messages? Generally useful identify datasets with any inconsistent data. 
#' Please report to \email{febr-forum@@googlegroups.com} if you find any issue.
#'
#' @details
#' \subsection{Standard columns}{
#' Standard columns and their content are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification code of the datasets in febr to which soil observations belong.
#' \item \code{observacao_id}. Identification code of soil observations in febr.
#' \item \code{camada_numero}. Sequential layer number, from top to bottom.
#' \item \code{camada_nome}. Layer designation according to some standard soil description guidelines.
#' \item \code{amostra_codigo}. Laboratory number of the soil samples.
#' \item \code{profund_sup}. Upper boundary of soil layers (cm).
#' \item \code{profund_inf}. Lower boundary of soil layers (cm).
# \item \code{fe_xxx_yyy}. Soil iron content data, with \code{xxx} being a given extraction method and
# \code{yyy} being a given measurement method.
#' }
#' }
#' @return A list or data.frame with soil layer-specific data.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{observations}}, \code{\link[febr]{standards}}, \code{\link[febr]{conversion}}
#' @export
#' @examples
#' \dontrun{
#' res <- layers(dataset = paste("ctb000", 4:9, sep = ""))
#' str(res)
#' }
###############################################################################################################
layers <-
  function (dataset, which.cols = "standard", soil.vars,
            stack.datasets = TRUE, missing.data = "drop",
            standardization = list(
              plus.sign = "add", plus.depth = 2.5, transition = "smooth", smoothing.fun = "mean"),
            harmonization = list(level = 1),
            progress = TRUE, verbose = TRUE) {
     
    # Opções
    opts <- .opt()
    
    # CHECKS ----
    if(!which.cols %in% c("standard", "all")) {
      stop (paste("Unknown value '", which.cols, "' passed to 'which.cols'", sep = ""))
    }
    # if(!soil.vars %in% opts$layers$soil.vars) {
    #   stop (paste("Unknown value '", soil.vars, "' passed to 'soil.vars'", sep = ""))
    # }
    soil.vars <- paste(soil.vars, "_", sep = "")
    if (!is.logical(stack.datasets)) {
      stop (paste("Unknown value '", stack.datasets, "' passed to 'stack.datasets'", sep = ""))
    }
    if (which.cols == "all" && stack.datasets == TRUE) {
      message("stack.datasets can only be used with standard columns... switching to FALSE")
      stack.datasets <- FALSE
    }
    if (!missing.data %in% c("drop", "keep")) {
      stop (paste("Unknown value '", missing.data, "' passed to 'missing.data", sep = ""))
    }
    if (!standardization$plus.sign %in% c("add", "remove", "keep")) {
      stop (
        paste("Unknown value '", standardization$plus.sign, "' passed to 'standardization$plus.sign'", 
          sep = ""))
    }
    if (standardization$plus.depth > 100 || standardization$plus.depth < 0) {
      stop (
        paste("Unlikely value '", standardization$plus.depth, "' passed to 'standardization$plus.depth'", 
              sep = "")
      )
    }
    if (!standardization$transition %in% c("smooth", "keep")) {
      stop (
        paste("Unknown value '", standardization$transition, "' passed to 'standardization$transition'", 
              sep = ""))
    }
    if (!standardization$smoothing.fun %in% c("mean", "min", "max", "median")) {
      stop(
        paste("Unknown value '", standardization$smoothing.fun, "' passed to 'standardization$smoothing.fun'", 
            sep = ""))
    }
    if (!harmonization$level %in% c(1, 2)) {
      stop (
        paste("Unknown value '", harmonization$level, "' passed to 'harmonization$level'", sep = ""))
    }
    if (!is.logical(progress)) {
      stop (paste("Unknown value '", progress, "' passed to 'progress'", sep = ""))
    }
    
    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)
    
    # Definir as colunas padrão
    if (which.cols == "standard") {
      target_cols <- c(opts$layers$id.cols, opts$layers$depth.cols)
    }

    # Descarregar planilhas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$camada), style = 3)
    }
    res <- list()
    for (i in 1:length(sheets_keys$camada)) {
      # i <- 1
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", dts, "...", sep = ""))
      }
      
      tmp <- googlesheets::gs_key(sheets_keys$camada[i], verbose = opts$gs$verbose)
      unit <- suppressMessages(
        googlesheets::gs_read_csv(tmp, locale = opts$gs$locale, verbose = opts$gs$verbose, n_max = 1))
      # Solução rápida e suja enquanto as unidade de medida não são padronizadas
      unit <- as.data.frame(unit)
      unit[which(unit %in% "???")] <- "g/kg"
      unit[which(unit %in% "mg/dm3")] <- "mg/dm^3" 
      
      tmp <- suppressMessages(
        googlesheets::gs_read_csv(
          tmp, na = opts$gs$na, locale = opts$gs$locale, verbose = opts$gs$verbose, comment = opts$gs$comment)
      )
      tmp <- as.data.frame(tmp)
      
      # Quais colunas/variáveis?
      # No caso de which.cols == "standard", então precisamos identificar as colunas que contém os dados da
      # variável do solo escolhida. Note que algumas dessas colunas podem não conter quaiquer dados, assim 
      # sendo ocupadas por 'NA'. Nesse caso, as respectivas colunas são descartadas e, se sobrar alguma coluna
      # com dados, continua-se com os passos seguintes do processamento. Nesse caso, o nome das variáveis 
      # 'soil_vars' também é atualizado.
      soil_vars <- lapply(soil.vars, function (x) colnames(tmp)[grep(paste("^", x, sep = ""), colnames(tmp))])
      soil_vars <- unlist(soil_vars)
      idx_na <- which(apply(tmp[soil_vars], 2, function (x) all(is.na(x))))
      
      if (length(idx_na) < length(soil_vars)) {
        
        # Update the names of soil variables
        if (length(idx_na) >= 1) {
          tmp <- tmp[, !colnames(tmp) %in% soil_vars[idx_na]]
          soil_vars <- soil_vars[-idx_na]
        }
        
        # Definir as colunas a serem mantidas
        if (which.cols == "standard") {
          tmp <- tmp[, c(target_cols, soil_vars)]
        }
        
        # STANDARDIZATION ----
        # I. What to do with the plus sign?
        if (standardization$plus.sign != "keep") {
          tmp <- .setMaximumObservationDepth(
            obj = tmp, id.col = target_cols[1], depth.cols = opts$layers$depth.cols,
            plus.sign = standardization$plus.sign, plus.depth = standardization$plus.depth)
        }
        
        if (standardization$transition != "keep") {
          # II. What to do with wavy and irregular layer transitions?
          tmp <- .solveIrregularLayerTransition(
            obj = tmp, id.col = target_cols[1], depth.cols = opts$layers$depth.cols,
            smoothing.fun = standardization$smoothing.fun)
          
          # III. What to do with broken layer transitions?
          tmp <- .solveBrokenLayerTransition(
            obj = tmp, id.cols = opts$layers$id.cols, depth.cols = opts$layers$depth.cols)
        }
        
        # CLEAN UP ----
        # Se necessário, descartar camadas sem qualquer tipo de dado de ferro
        if (missing.data == "drop") {
          idx_keep <- is.na(tmp[soil_vars])
          idx_keep <- rowSums(idx_keep) < ncol(idx_keep)
          tmp <- tmp[idx_keep, ]
        }
        
        # Verificar se, com a eliminação das camadas sem quaisquer dados de ferro, restou alguma camada
        # Também é possível que o conjunto de dados não possua quaisquer dados de ferro
        if (nrow(tmp) >= 1) {
          
          # STANDARDIZATION (Fe) ----
          # Identificar tipos de dado de ferro para padronização da unidade de medida e número de casas decimais
          if (soil.vars == 'fe') {
            fe_type <- stringr::str_split_fixed(soil_vars, "_", n = 3)[, 2]
            fe_stand <- lapply(fe_type, function (y) standards(soil.var = "fe", extraction.method = y))
            fe_stand <- do.call(rbind, fe_stand)
            
            # 1. Se necessário, padronizar unidades de medida
            idx_unit <- which(!unit[, soil_vars] %in% unique(standards(soil.var = "fe")$unit))
            if (length(idx_unit) >= 1) {
              conv_factor <- lapply(1:length(fe_type[idx_unit]), function (j) {
                conversion(source = unlist(unit[, soil_vars[idx_unit]])[[j]], target = fe_stand$unit[idx_unit][j])
              })
              conv_factor <- do.call(rbind, conv_factor)
              tmp[soil_vars[idx_unit]] <- t(t(tmp[soil_vars[idx_unit]]) * conv_factor$factor)
            }
            # 2. Padronizar número de casas decimais
            tmp[, soil_vars] <- sapply(1:length(soil_vars), function (j) {
              round(tmp[, soil_vars[j]], digits = fe_stand$digits[j])
            })
          }
          
          # HARMONIZATION ----
          # Harmonizar dados de ferro
          if (harmonization$level == 1) {
            new_colnames <- matrix(stringr::str_split_fixed(colnames(tmp[soil_vars]), "_", 3)[, 1:2], ncol = 2)
            new_colnames <- apply(new_colnames, 1, paste, collapse = "_", sep = "")
            
            # Nomes idênticos são gerados para variáveis definidas pelo mesmo extrator. Nesses casos mantém-se
            # os nomes originais das respectivas colunas.
            if (any(duplicated(new_colnames))) {
              idx <- c(which(duplicated(new_colnames)), which(duplicated(new_colnames, fromLast = TRUE)))
              new_colnames[idx] <- soil_vars[idx]
            }
            colnames(tmp)[colnames(tmp) %in% soil_vars] <- new_colnames
          }
          
          # STACKING ----
          # If tables are to be stacked, then id.cols must be of type character
          if (stack.datasets) {
            tmp[opts$layers$id.cols] <- sapply(tmp[opts$layers$id.cols], as.character)
            
            # If tables are to be stacked and depth data has not been standardized, then depth.cols must be 
            # of type character. Otherwise depth.cols must be of type numeric.
            if (standardization$plus.sign == "keep" || standardization$transition == "keep") {
              tmp[opts$layers$depth.cols] <- sapply(tmp[opts$layers$depth.cols], as.character)
            } else {
              tmp[opts$layers$depth.cols] <- sapply(tmp[opts$layers$depth.cols], as.numeric)
            }
          }
          
          # Adicionar 'dataset_id' às camadas processadas.
          res[[i]] <- cbind(dataset_id = as.character(sheets_keys$ctb[i]), tmp)
          
          # Se as tabelas não são empilhadas, adicionar informação sobre unidade de medida
          if (!stack.datasets) {
            a <- attributes(res[[i]])
            a$units <- c(rep("unitless", 2), gsub("-", "unitless", as.character(unit)[-1]))
            attributes(res[[i]]) <- a
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
    # Se necessário, empilhar tabelas, adicionando informações sobre as unidades de medida
    if (stack.datasets) {
      res <- suppressWarnings(dplyr::bind_rows(res))
      soil_vars <- colnames(res)[grep("^fe_", colnames(res))]
      fe_type <- stringr::str_split_fixed(soil_vars, "_", n = 3)[, 2]
      fe_stand <- sapply(fe_type, function (y) standards(soil.var = "fe", extraction.method = y)$unit)
      a <- attributes(res)
      a$units <- c(rep("unitless", 5), rep("cm", 2), as.character(fe_stand))
      attributes(res) <- a
    }
    return (res)
  }
