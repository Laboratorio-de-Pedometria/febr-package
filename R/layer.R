#' Get *layer* table
#'
#' Download data from the *layer* ("camada") table of one or more datasets contained in the Free Brazilian
#' Repository for Open Soil Data -- FEBR, \url{https://www.pedometria.org/febr/}. This includes 
#' sampling depth, horizon designation, and variables such as pH, carbon content, clay content, and much more.
#' Use \code{\link[febr]{header}} if you want to check what are the variables contained in the *layer* table 
#' of a dataset before downloading it.
#' @template data_template
#' @template metadata_template
#' @param febr.repo (optional) Character vector indicating where the data should be read. Defaults to
#' `febr.repo = "remote"`, i.e. the remote web server. Alternatively, a local directory path can be passed to
#' `febr.repo` if the user has a local copy of the data repository.
#' @param missing (optional) List with named sub-arguments indicating what should be done with a layer missing
#' data on sampling depth, `depth`, or data on variable(s), `data`. Options are `"keep"` (default) and 
#' `"drop"`.
#' @param standardization (optional) List with named sub-arguments indicating how to perform data 
#' standardization.
#' \itemize{
#' \item `plus.sign` Character string indicating what should be done with the plus sign (`+`) commonly used
#'       along with the inferior limit of the bottom layer of an observation. Options are `"keep"` (default),
#'       `"add"`, and `"remove"`.
#' \item `plus.depth` Numeric value indicating the depth increment (in centimeters) when processing the plus
#'       sign (`+`) with `plus.sign = "add"`. Defaults to `plus.depth = 2.5`.
#'       
#' \item `lessthan.sign` Character string indicating what should be done with the less-than sign (`<`) used
#'       to indicate that the value of a variable is below the lower limit of detection. Options are `"keep"`
#'       (default), `"subtract"`, and `"remove"`.
#' \item `lessthan.frac` Numeric value between 0 and 1 (a fraction) by which the lower limit of detection 
#'       should be subtracted when `lessthan.sign = "subtract"`. Defaults to `lessthan.frac = 0.5`, i.e. 
#'       subtract 50\% from the lower limit of detection.
#'       
#' \item `repetition` Character string indicating what should be done with repetitions, i.e. repeated
#'       measurements of layers in an observation. Options are `"keep"` (default) and `"combine"`. In the 
#'       latter case, it is recommended to set `lessthan.sign = "subtract"` or `lessthan.sign = "remove"`.
#' \item `combine.fun` Character string indicating the function that should be used to combine repeated 
#'       measurements of layers in an observation when `repetition = "combine"`. Options are `"mean"` 
#'       (default), `"min"`, `"max"`, and `"median"`.
#'       
#' \item `transition` Character string indicating what should be done about the wavy and irregular 
#'       transition between subsequent layers in an observation. Options are `"keep"` (default) and 
#'       `"smooth"`.
#' \item `smoothing.fun` Character string indicating the function that should be used to smooth wavy and
#'       irregular transitions between subsequent layers in an observation when `transition = "smooth"`.
#'       Options are `"mean"` (default), `"min"`, `"max"`, and `"median"`.
#'       
# \item `broken.transition` Character string indicating what should be done about the broken transition
#       between intermingled, disrupted layers in an observation. Options are `"keep"` (default) and
#       `"merge"`.
# \item `merge.fun` Character string indicating the function that should be used to merge intermingled,
#       disrupted layers (also called broken transition) in an observation when `broken.transition = "merge"`.
#       Options are `"weighted.mean"` (default), `"mean"`, `"min"`, `"max"`, and `"median"`.
#'       
#' \item `units` Logical value indicating if the measurement unit(s) of the continuous variable(s) should
#'       be converted to the standard measurement unit(s). Defaults to `units = FALSE`, i.e. no conversion is
#'       performed. See \code{\link[febr]{standard}} for more information.
#' \item `round` Logical value indicating if the values of the continuous variable(s) should be rounded  
#'       to the standard number of decimal places. Requires `units = TRUE`. Defaults to `round = FALSE`, i.e. 
#'       no rounding is performed. See \code{\link[febr]{standard}} for more information.
#' }
#' @param harmonization (optional) List with named sub-arguments indicating if and how to perform data 
#' harmonization.
#' \itemize{
#' \item `harmonize` Logical value indicating if data should be harmonized. Defaults to `harmonize = FALSE`, 
#'       i.e. no harmonization is performed.
#' \item `level` Integer value indicating the number of levels of the identification code of the variable(s) 
#'       that should be considered for harmonization. Defaults to `level = 2`. See \sQuote{Details} for more
#'       information.
#' }
#' @details
#' \subsection{Standard identification variables}{
#' Standard identification variables and their content are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification of the dataset in FEBR to which an observation belongs.
#' \item \code{observacao_id}. Identification code of an observation in a dataset.
#' \item \code{camada_id}. Sequential layer number, from top to bottom.
#' \item \code{camada_nome}. Layer designation according to some standard description guide.
#' \item \code{amostra_id}. Laboratory number of a sample.
#' \item \code{profund_sup}. Upper boundary of a layer (cm).
#' \item \code{profund_inf}. Lower boundary of a layer (cm).
#' }
#' Further details about the content of the standard identification variables can be found in
#' \url{https://docs.google.com/document/d/1Bqo8HtitZv11TXzTviVq2bI5dE6_t_fJt0HE-l3IMqM} (in Portuguese).
#' }
#' \subsection{Harmonization}{
#' Data harmonization consists of converting the values of a variable determined using some method *B* so 
#' that they are (approximately) equivalent to the values that would have been obtained if the standard method
#' *A* had been used instead. For example, converting carbon content values obtained using a wet digestion
#' method to the standard dry combustion method is data harmonization.
#'
#' A heuristic data harmonization procedure is implemented in the __febr__ package. It consists of grouping
#' variables based on a chosen number of levels of their identification code. For example, consider a variable
#' with an identification code composed of four levels, `aaa_bbb_ccc_ddd`, where `aaa` is the first level and
#' `ddd` is the fourth level. Now consider a related variable, `aaa_bbb_eee_fff`. If the harmonization
#' is to consider all four coding levels (`level = 4`), then these two variables will remain coded as
#' separate variables. But if `level = 2`, then both variables will be re-coded as `aaa_bbb`, thus becoming the
#' same variable.
#' }
#' @return A list of data frames or a data frame with data on the chosen variable(s) of the chosen dataset(s).
#' @note Check the new core data download function `readFEBR()`.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{observation}}, \code{\link[febr]{standard}}, \code{\link[febr]{unit}}
#' @export
#' @examples
#' res <- layer(dataset = "ctb0003")
####################################################################################################
layer <-
  function(dataset, variable,
            stack = FALSE,
            missing = list(depth = "keep", data = "keep"),
            standardization = list(
              plus.sign = "keep", plus.depth = 2.5,
              lessthan.sign = "keep", lessthan.frac = 0.5,
              repetition = "keep", combine.fun = "mean",
              transition = "keep", smoothing.fun = "mean",
              # broken.transition = "keep", merge.fun = "weighted.mean",
              units = FALSE, round = FALSE),
            harmonization = list(harmonize = FALSE, level = 2),
            progress = TRUE, verbose = TRUE, febr.repo = "remote") {
    # OPÇÕES E PADRÕES
    opts <- .opt()
    std_cols <- opts$layer$std.cols
    # ARGUMENTOS
    ## dataset
    if (missing(dataset)) {
      stop("argument 'dataset' is missing")
    } else if (!is.character(dataset)) {
      stop(paste0("object of class ", class(dataset), " passed to argument 'dataset'"))
    }
    ## variable
    if (!missing(variable) && !is.character(variable)) {
      stop(paste0("object of class '", class(variable), "' passed to argument 'variable'"))
    }
    ## stack
    if (!is.logical(stack)) {
      stop(paste0("object of class '", class(stack), "' passed to argument 'stack'"))
    }
    ## missing
    if (!missing(missing)) {
      if (is.null(missing$depth)) {
        missing$depth <- "keep"
      } else if (!missing$depth %in% c("drop", "keep")) {
        stop(paste0("unknown value '", missing$depth, "' passed to argument 'missing$depth'"))
      }
      if (is.null(missing$data)) {
        missing$data <- "keep"
      } else if (!missing$data %in% c("drop", "keep")) {
        stop(paste0("unknown value '", missing$data, "' passed to argument 'missing$data'"))
      }
    }
    ## standardization
    if (!missing(standardization)) {
      if (is.null(standardization$plus.sign)) {
        standardization[["plus.sign"]] <- "keep"
      } else if (!standardization$plus.sign %in% c("add", "remove", "keep")) {
        y <- standardization[["plus.sign"]]
        stop(paste0("unknown value '", y, "' passed to argument 'standardization$plus.sign'"))
      }
      if (is.null(standardization$plus.depth)) {
        standardization[["plus.depth"]] <- 2.5
      } else if (standardization$plus.depth > 100 || standardization$plus.depth < 0) {
        y <- standardization$plus.depth
        stop(paste0("unlikely value '", y, "' passed to argument 'standardization$plus.depth'"))
      }
      if (is.null(standardization$lessthan.sign)) {
        standardization[["lessthan.sign"]] <- "keep"
      } else if (!standardization$lessthan.sign %in% c("subtract", "remove", "keep")) {
        y <- standardization$lessthan.sign
        stop(paste0("unknown value '", y, "' passed to argument 'standardization$lessthan.sign'"))
      }
      if (is.null(standardization$lessthan.frac)) {
        standardization[["lessthan.frac"]] <- 0.5
      } else if (standardization$lessthan.frac > 1 || standardization$lessthan.frac < 0) {
        y <- standardization$lessthan.frac
        stop(paste0("unlikely value '", y, "' passed to argument 'standardization$lessthan.frac'"))
      }
      if (is.null(standardization$repetition)) {
        standardization$repetition <- "keep"
      } else if (!standardization$repetition %in% c("combine", "keep")) {
        y <- standardization$repetition
        stop(paste0("unknown value '", y, "' passed to argument 'standardization$repetition'"))
      }
      if (is.null(standardization$combine.fun)) {
        standardization$combine.fun <- "mean"
      } else if (!standardization$combine.fun %in% c("mean", "min", "max", "median")) {
        y <- standardization$combine.fun
        stop(paste0("unknown value '", y, "' passed to argument 'standardization$combine.fun'"))
      }
      if (is.null(standardization$transition)) {
        standardization$transition <- "keep"
      } else if (!standardization$transition %in% c("smooth", "keep")) {
        y <- standardization$transition
        stop(paste0("unknown value '", y, "' passed to argument 'standardization$transition'"))
      }
      if (is.null(standardization$smoothing.fun)) {
        standardization$smoothing.fun <- "mean"
      } else if (!standardization$smoothing.fun %in% c("mean", "min", "max", "median")) {
        y <- standardization$smoothing.fun
        stop(paste0("unknown value '", y, "' passed to argument 'standardization$smoothing.fun'"))
      }
      # if (is.null(standardization$broken.transition)) {
      #   standardization$broken.transition <- "keep"
      # } else if (!standardization$broken.transition %in% c("merge", "keep")) {
      #   y <- standardization$broken.transition
      #   stop(paste("unknown value '", y, "'passed to sub-argument 'standardization$broken.transition'", sep = ""))
      # }
      # if (is.null(standardization$merge.fun)) {
      #   standardization$merge.fun <- "weighted.mean"
      # } else if (!standardization$merge.fun %in% c("weighted.mean", "mean", "min", "max", "median", sep = "")) {
      #   y <- standardization$merge.fun
      #   stop(paste("unknown value '", y, "'passed to sub-argument 'standardization$merge.fun'", sep = ""))
      # }
      if (is.null(standardization$units)) {
        standardization$units <- FALSE
      } else if (!is.logical(standardization$units)) {
        y <- class(standardization$units)
        stop(paste0("object of class '", y, "' passed to argument 'standardization$units'"))
      }
      if (is.null(standardization$round)) {
        standardization$round <- FALSE
      } else if (!is.logical(standardization$round)) {
        y <- class(standardization$round)
        stop(paste0("object of class '", y, "' passed to argument 'standardization$round'"))
      }
    }
    ## harmonization
    if (!missing(harmonization)) {
      if (is.null(harmonization$harmonize)) {
        harmonization$harmonize <- FALSE
      } else if (!is.logical(harmonization$harmonize)) {
        y <- class(harmonization$harmonize)
        stop(paste0("object of class '", y, "' passed to argument 'harmonization$harmonize'"))
      }
      if (is.null(harmonization$level)) {
        harmonization$level <- 2
      } else if (!pedometrics::isNumint(harmonization$level)) {
        y <- class(harmonization$level)
        stop(paste0("object of class '", y, "' passed to argument 'harmonization$level'"))
      }
    }
    ## progress
    if (!is.logical(progress)) {
      stop(paste0("object of class '", class(progress), "'' passed to argument 'progress'"))
    }
    ## verbose
    if (!is.logical(verbose)) {
      stop(paste0("object of class '", class(verbose), "' passed to argument 'verbose'"))
    }
    ## variable + stack || variable + harmonization
    if (!missing(variable) && variable == "all") {
      if (stack) {
        stop("data cannot be stacked when downloading all variables")
      }
      if (harmonization$harmonize) {
        stop("data cannot be harmonized when downloading all variables")
      }
    }
    ## dataset and stack
    if (stack && length(dataset) == 1 && dataset != "all") {
      # Por razões óbvias, não há como empilhar conjuntos de dados quando apenas um conjunto de
      # dados está sendo descarregado. Assim, se o usuário especificar stack = TRUE, o argumento é
      # reconfigurado para stack = FALSE.
      message("a single dataset is being downloaded... setting stack = FALSE")
      stack <- FALSE
    }
    # PADRÕES
    # Descarregar tabela com unidades de medida e número de casas decimais quando padronização é
    # solicitada ou quando empilhamento é solicitado. A tabela está disponível em:
    # https://docs.google.com/spreadsheets/d/1tU4Me3NJqk4NH2z0jvMryGObSSQLCvGqdLEL5bvOflo/
    if (standardization$units || stack) {
      febr_stds <- .getStds()
      febr_unit <- .readGoogleSheetCSV(sheet.name = "unidades")
    }
    ## stack and stadardization
    ## Padronização não precisa ser feita no caso de descarregamento apenas das variáveis padrão
    ## Também não precisa ser feita no caso de variáveis de tipo 'texto'
    if (stack && !standardization$units && !missing(variable) && variable != "all") {
      tmp_var <- paste("^{variable}")
      idx <- lapply(tmp_var, function(pattern) grep(pattern = pattern, x = febr_stds[["campo_id"]]))
      idx <- unlist(idx)
      is_all_text <- all(febr_stds[["campo_tipo"]][idx] == "texto")
      if (!is_all_text) {
        stop("data cannot be stacked when measurement units are not standardized")
      }
    }
    # CHAVES
    ## Descarregar chaves de identificação das tabelas
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)
    # Descarregar tabelas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys[["camada"]]), style = 3)
    }
    res <- list()
    for (i in 1:length(sheets_keys$camada)) {
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste0(par, "Downloading ", dts, "-camada..."))
      }
      # DESCARREGAMENTO
      tmp <- .readOwnCloud(ctb = sheets_keys[i, "ctb"], table = "camada", febr.repo = febr.repo)
      unit <- .readOwnCloud(ctb = sheets_keys[i, "ctb"], table = "metadado", febr.repo = febr.repo)
      unit[["campo_unidade"]][is.na(unit[["campo_unidade"]])] <- "-"
      unit <- unit[unit$tabela_id == "camada", c("campo_id", "campo_nome", "campo_unidade")]
      rownames(unit) <- unit[["campo_id"]]
      unit <- unit[, -1]
      unit <- as.data.frame(t(unit), stringsAsFactors = FALSE)
      n_rows <- nrow(tmp)
      # PROCESSAMENTO I
      # A decisão pelo processamento dos dados começa pela verificação de dados faltantes nas
      # profundidades
      na_depth <- max(apply(tmp[c("profund_sup", "profund_inf")], 2, function(x) sum(is.na(x))))
      if (missing$depth == "keep" || missing$depth == "drop" && na_depth < n_rows) {
        # COLUNAS
        # Definir as colunas a serem mantidas
        # As colunas padrão são sempre mantidas.
        # No caso das colunas adicionais, é possível que algumas não contenham quaisquer dados,
        # assim sendo ocupadas por 'NA'. Nesse caso, as respectivas colunas são descartadas.
        in_cols <- colnames(tmp)
        cols <- in_cols[in_cols %in% std_cols]
        extra_cols <- vector()
        if (!missing(variable)) {
          if (length(variable) == 1 && variable == "all") {
          # if (variable == "all") {
            extra_cols <- in_cols[!in_cols %in% std_cols]
            idx_na <- apply(tmp[extra_cols], 2, function(x) all(is.na(x)))
            extra_cols <- extra_cols[!idx_na]
          } else {
            extra_cols <- lapply(variable, function(x) in_cols[grep(paste0("^", x), in_cols)])
            extra_cols <- unlist(extra_cols)
            extra_cols <- extra_cols[!extra_cols %in% std_cols]
            idx_na <- apply(tmp[extra_cols], 2, function(x) all(is.na(x)))
            extra_cols <- extra_cols[!idx_na]
          }
        }
        cols <- c(cols, extra_cols)
        tmp <- tmp[, cols]
        # unit <- unit[names(unit) %in% cols]
        unit <- unit[, cols]
        # LINHAS I
        ## Avaliar limpeza das linhas
        tmp_clean <- .cleanRows(obj = tmp, missing = missing, extra_cols = extra_cols)
        n_rows <- nrow(tmp_clean)
        # PROCESSAMENTO II
        # A continuação do processamento dos dados depende das presença de dados após a eliminação
        # de colunas e linhas com NAs.
        if (n_rows >= 1 && missing(variable) ||
            # length(extra_cols) >= 1 ||
            missing$data == "keep") {
          # LINHAS II
          ## Definir as linhas a serem mantidas
          ## É preciso considerar todas as possibilidades de remoção de dados
          if (missing$data == "drop" || missing$dept == 'drop') {
            tmp <- tmp_clean
          }
          # TIPO DE DADOS
          # "observacao_id", "camada_id", "camada_nome", "amostra_id", "profund_sup" e "profund_inf"
          # devem estar no formato de caracter para evitar erros durante o empilhamento das tabelas
          # devido ao tipo de dado.
          if (stack) {
            tmp[std_cols] <- sapply(tmp[std_cols], as.character)
          }
          # PADRONIZAÇÃO I
          # Profundidade e transição entre as camadas
          # Sinal positivo em 'profund_inf' indicando maior profundidade abaixo da última camada
          # O padrão consiste em manter o sinal positivo. Os dados não são definidos como classe
          # 'numeric' porque pode haver transição ondulada ou irregular entre as camadas --
          # solucionada abaixo. Por enquanto se assume que a profundidade está em centímetros a
          # partir da superfície.
          if (standardization$plus.sign != "keep") {
            tmp <- .setMaximumObservationDepth(
              obj = tmp, plus.sign = standardization$plus.sign,
              plus.depth = standardization$plus.depth)
          }
          # Símbolo indicador do limite inferior de detecção do método de determinação (<)
          # O padrão consiste em manter o símbolo. Do contrário, o resultado é convertido para
          # classe 'numeric' a fim de que seja possível, se demandado, padronizar as unidades de
          # medida e o número de casas decimais.
          if (standardization$lessthan.sign != "keep") {
            tmp <- .setLowestMeasuredValue(
              obj = tmp, lessthan.sign = standardization$lessthan.sign,
              lessthan.frac = standardization$lessthan.frac)
          }
          # Repetições de laboratório
          # O padrão consiste em manter as repetições de laboratório. Do contrário, a coluna
          # 'camada_id' é a chave para o processamento dos dados. Note que é necessário que o tipo
          # de dado das variáveis esteja corretamente definido, sobretudo no caso de variáveis
          # contínuas. A solução prévia do símbolo indicador do limite inferior de detecção
          # geralmente é necessária.
          if (standardization$repetition != "keep") {
            tmp <- .solveLayerRepetition(obj = tmp, combine.fun = standardization$combine.fun)
          }
          ## Transição ondulada ou irregular
          ## O padrão consiste em manter a transição ondulada ou irregular.
          if (standardization$transition != "keep") {
            tmp <- .solveWavyLayerTransition(obj = tmp,
            smoothing.fun = standardization$smoothing.fun)
          }
          ## Transição quebrada
          ## O padrão consiste em manter a transição quebrada
          # if (standardization$broken.transition != "keep") {
          #   tmp <- .solveBrokenLayerTransition(obj = tmp, merge.fun = standardization$merge.fun) 
          # }
          # Se a profundidade foi padronizada e as tabelas serão empilhadas, então os dados de
          # profundidade devem ser definidos como classe 'numeric'
          if (standardization$plus.sign != "keep" && standardization$transition != "keep") {
            tmp[c("profund_sup", "profund_inf")] <-
              sapply(tmp[c("profund_sup", "profund_inf")], as.numeric)
          }
          # PADRONIZAÇÃO II
          # Unidade de medida e número de casas decimais de colunas adicionais
          if (standardization$units && length(extra_cols) >= 1) {
            # Identificar variáveis contínuas (classe 'numeric' e 'integer'), excluíndo variáveis de
            # identificação padrão.
            id_class <- sapply(tmp, class)
            cont_idx <-
              which(id_class %in% c("numeric", "integer") & !names(id_class) %in% std_cols)
            if (length(cont_idx) >= 1) {
              # Tabela com padrões das variáveis contínuas identificadas
              tmp_stds <- match(cols[cont_idx], febr_stds[["campo_id"]])
              tmp_stds <- febr_stds[tmp_stds, c("campo_id", "campo_unidade", "campo_precisao")]
              ## 1. Se necessário, padronizar unidades de medida
              # idx_unit <- unit[cols[cont_idx]] != tmp_stds$campo_unidade
              # idx_unit <- unit[, cols[cont_idx]] != tmp_stds$campo_unidade
              # verifica 2ª linha de metadados
              need_idx <- unit[2, cols[cont_idx]] != tmp_stds[["campo_unidade"]]
              if (any(need_idx)) {
                # idx_unit <- colnames(idx_unit)[idx_unit]
                need_name <- cols[cont_idx][need_idx]
                # source <- unit[idx_unit]
                # source <- unit[2, idx_unit]
                source <- unit[2, need_name]
                target <- tmp_stds[["campo_unidade"]][match(need_name, tmp_stds[["campo_id"]])]
                ## Identificar constante
                k <- lapply(seq_along(source), function(i) {
                  idx <- febr_unit$unidade_origem %in% source[i] + febr_unit$unidade_destino %in% target[i]
                  febr_unit[idx == 2, ]
                })
                k <- do.call(rbind, k)
                ## Processar dados
                # tmp[idx_unit] <- mapply(`*`, tmp[idx_unit], k$unidade_constante)
                tmp[need_name] <- mapply(`*`, tmp[need_name], k$unidade_constante)
                # unit[idx_unit] <- k$unidade_destino
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
          # Processar unidades de medida
          # 'sem unidade' significa que uma variável não possui unidade de medida (unitless)
          # '-' significa que a unidade de medida é desconhecida ou não foi informada (NA).
          unit[2, ] <- as.character(unit[2, names(unit) %in% cols])
          unit[2, ] <- gsub("^sem unidade$", "unitless", unit[2, ])
          # unit[2, ] <- gsub("^-$", "unitless", unit[2, ])
          # https://en.wikipedia.org/wiki/List_of_Unicode_characters
          unit["observacao_id"] <-
            c("Identifica\u00E7\u00E3o da observa\u00E7\u00E3o", # Identificação da observação
              "unitless")
          dataset_id <-
            c("Identifica\u00E7\u00E3o do conjunto de dados", # Identificação do conjunto de dados
              "unitless")
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
          res[[i]] <- cbind(dataset_id = sheets_keys$ctb[i], tmp, stringsAsFactors = FALSE)
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
          m <- paste0("All layers in ", dts, " are missing data. None will be returned.")
          message(m)
        }
      } else {
        res[[i]] <- data.frame()
        m <- paste0("All layers in ", dts, " are missing depth. None will be returned.")
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
      res <- .stackTables(obj = res)
    } else if (n == 1) {
      res <- res[[1]]
    }
    return(res)
  }
