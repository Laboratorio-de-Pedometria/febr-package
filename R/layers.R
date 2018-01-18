#' Get soil layers
#'
#' Download soil layer-specific data contained in the Free Brazilian Repository for Open Soil Data --
#' \url{http://www.ufsm.br/febr}.
#' 
#' @param dataset Identification code of the dataset or datasets for which soil layer-specific data should be
#' downloaded -- see \url{http://www.ufsm.br/febr/book}. Use \code{dataset = "all"} to download data from all
#' existing datasets.
#' 
#' @param variable Name(s) of the variable(s). If missing, then a set of standard columns is downloaded. Use
#' \code{variable = "all"} to download all variables. See \sQuote{Details} for more information.
#'
#' @param stack Should soil layers from different datasets be stacked on a single data frame for
#' output? Defaults to \code{stack = FALSE}, the output being a list of data frames.
#'
#' @param missing List with named arguments specifying what should be done with layers missing depth, 
#' \code{depth}, or data, \code{data}? Options are \code{"keep"} (default) and \code{"drop"}.
#' 
#' @param missing List with named a argument specifying what should be done with layers missing data, 
#' \code{data}? Options are \code{"keep"} (default) and \code{"drop"}.
#' 
#' @param standardization List with definitions on how to \emph{standardize} soil layer-specific data. Only 
#' works when \code{soil.vars = 'fe'}.
#' \itemize{
#' \item \code{plus.sign} What should be done with the plus sign ('+') commonly used along with the inferior 
#'       limit of the bottom layer of soil observations? Options are \code{"keep"} (default), \code{"add"},
#'       and \code{"remove"}.
#' \item \code{plus.depth} Depth increment (in centimetres) when processing the plus sign ('+') with 
#'       \code{plus.sign = "add"}. Defaults to \code{plus.depth = 0}.
#' \item \code{transition} What should be done about wavy, irregular, and broken transitions between layers in
#'       a soil observation? Options are \code{"keep"} (default) and \code{"smooth"}.
#' \item \code{smoothing.fun} Function that should be used to smooth wavy and irregular transitions between 
#'       layers in a soil observation when \code{transition = "smooth"}. Options are \code{"mean"} (default),
#'       \code{"min"}, \code{"max"}, and \code{"median"}. 
#' }
#'
#' @param harmonization List with definitions on how to \emph{harmonize} soil layer-specific data.
#' \itemize{
#' \item \code{level} Level of harmonization. Defautls to \code{level = 5}. See \code{\link[febr]{standards}}.
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
# dataset <- "ctb0029"
###############################################################################################################
layers <-
  function (dataset, variable,
            stack = FALSE, missing = list(depth = "keep", data = "keep"),
            standardization = list(
              plus.sign = "keep", plus.depth = 0, transition = "keep", smoothing.fun = "mean"),
            harmonization = list(level = 5),
            progress = TRUE, verbose = TRUE) {
    
    # Opções
    opts <- .opt()
    
    # Verificar consistência dos parâmetros
    if (!missing(variable)) {
      if (variable == "all" && stack == TRUE) {
        stop ("data cannot be stacked when downloading all variables")
      }
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
    
    # Variáveis padrão
    std_cols <- opts$layers$std.cols
    
    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)
    
    # Descarregar planilhas com camadas
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
        if (length(extra_cols) >= 1 && missing$data == "drop") {
          idx_keep <- is.na(tmp[extra_cols])
          idx_keep <- rowSums(idx_keep) < ncol(idx_keep)
          tmp <- tmp[idx_keep, ]
        }
        if (missing$depth == "drop") {
          na_depth_id <- apply(tmp[c("profund_sup", "profund_inf")], 1, function (x) sum(is.na(x))) >= 1
          tmp <- tmp[!na_depth_id, ]
        }
        n_rows <- nrow(tmp)
        
        # PROCESSAMENTO II
        ## A continuação do processamento dos dados depende das presença de dados após a eliminação de colunas
        ## e linhas com NAs.
        if (n_rows >= 1 && missing(variable) || length(extra_cols) >= 1) {
          
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
          
          ## Se a profundidade foi padronizada, então deve ser definida como tipo 'Real'
          if (standardization$plus.sign != "keep" || standardization$transition != "keep") {
            tmp[c("profund_sup", "profund_inf")] <- sapply(tmp[c("profund_sup", "profund_inf")], as.numeric)
          }
          
          # PADRONIZAÇÃO II
          ## Unidade de medida e número de casas decimais
          
          # if (soil.vars == 'fe') {
          #   fe_type <- stringr::str_split_fixed(soil_vars, "_", n = 3)[, 2]
          #   fe_stand <- lapply(fe_type, function (y) standards(soil.var = "fe", extraction.method = y))
          #   fe_stand <- do.call(rbind, fe_stand)
          #   
          #   # 1. Se necessário, padronizar unidades de medida
          #   idx_unit <- which(!unit[, soil_vars] %in% unique(standards(soil.var = "fe")$unit))
          #   if (length(idx_unit) >= 1) {
          #     conv_factor <- lapply(1:length(fe_type[idx_unit]), function (j) {
          #       conversion(source = unlist(unit[, soil_vars[idx_unit]])[[j]], target = fe_stand$unit[idx_unit][j])
          #     })
          #     conv_factor <- do.call(rbind, conv_factor)
          #     tmp[soil_vars[idx_unit]] <- t(t(tmp[soil_vars[idx_unit]]) * conv_factor$factor)
          #   }
          #   # 2. Padronizar número de casas decimais
          #   tmp[, soil_vars] <- sapply(1:length(soil_vars), function (j) {
          #     round(tmp[, soil_vars[j]], digits = fe_stand$digits[j])
          #   })
          # }
          
          # HARMONIZAÇÃO I
          ## Harmonização baseada nos níveis dos códigos de identificação
          new_colnames <- stringr::str_split_fixed(string = extra_cols, pattern = "_", n = Inf)
          n_new_colnames <- seq(min(harmonization$level, ncol(new_colnames)))
          new_colnames <- new_colnames[, n_new_colnames]
          if (n_new_colnames > 1) {
            new_colnames <- apply(new_colnames, 1, function (x) paste(x[!x == ""], collapse = "_", sep = ""))  
          }
          
          ## No caso de nomes idênticos, manter o nome original
          if (any(duplicated(new_colnames))) {
            idx <- c(which(duplicated(new_colnames)), which(duplicated(new_colnames, fromLast = TRUE)))
            new_colnames[idx] <- extra_cols[idx]
          }
          cols[cols %in% extra_cols] <- new_colnames
          colnames(tmp) <- cols
          
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
