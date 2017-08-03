#' Get soil layers
#'
#' Download soil layer-specific data contained in the Brazilian Soil Iron Data Repository (Fe-BR)  --
#' \url{http://www.ufsm.br/febr}.
#'
#' @param which.cols Which columns should be returned? Options are \code{"standard"} (default) and
#' \code{"all"}.
#'
#' @param stack.layers Should soil layers from different datasets be stacked on a single data frame for
#' output? Used only with \code{which.cols = "standard"}. Defaults to \code{stack.layers = TRUE}.
#'
#' @param missing.data What should be done with soil layers missing iron data? Options are \code{"drop"}
#' (default) and \code{"keep"}.
#'
#' @param progress Show progress bar?
#'
#' @details Standard columns and their content are as follows:
#' \itemize{
#' \item \code{dataset_id}. Identification code of the datasets in Fe-BR to which soil observations belong.
#' \item \code{observacao_id}. Identification code of soil observations in Fe-BR.
#' \item \code{camada_numero}. Sequential layer number, from top to bottom.
#' \item \code{camada_nome}. Layer designation according to some standard soil description guidelines.
#' \item \code{amostra_codigo}. Laboratory number of the soil samples.
#' \item \code{profund_sup}. Upper boundary of soil layers (cm).
#' \item \code{profund_inf}. Lower boundary of soil layers (cm).
#' \item \code{fe_xxx_yyy}. Soil iron content data, with \code{xxx} being a given extractant and \code{yyy}
#' being a given determination technique/equipment.
#' }
#'
#' @return A list with some or all of the data of the soil layers contained in Fe-BR.
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \url{http://www.ufsm.br/febr}
#' @export
#' @examples
#' \dontrun{
#' res <- layers(which.cols = "standard", stack.layers = TRUE, missing.data = "drop", progress = TRUE)
#' str(res)
#' }
###############################################################################################################
layers <-
  function (which.cols = "standard", stack.layers = TRUE, missing.data = "drop",
            harmonization = list(level = 1), progress = TRUE) {

    # Verificar consistência dos parâmetros
    if(!which.cols %in% c("standard", "all")) {
      stop (paste("unknown value '", which.cols, "' passed to parameter which.cols", sep = ""))
    }
    if (!is.logical(stack.layers)) {
      stop (paste("Unknown value '", stack.layers, "' passed to parameter stack.layers", sep = ""))
    }
    if (which.cols == "all" && stack.layers == TRUE) {
      message("stack.layers can only be used with standard columns... switching to FALSE")
      stack.layers <- FALSE
    }
    if (!is.logical(progress)) {
      stop (paste("unknown value '", progress, "' passed to parameter progress", sep = ""))
    }

    # Descarregar chaves de identificação das planilhas do repositório
    sheets_keys <- googlesheets::gs_key("18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q", verbose = FALSE)
    sheets_keys <- suppressMessages(googlesheets::gs_read(sheets_keys, verbose = FALSE))

    # Definir opções de local
    locale <- readr::locale(decimal_mark = ",")

    # Definir as colunas padrão
    if (which.cols == "standard") {
      target_cols <-
        c("observacao_id", "camada_numero", "camada_nome", "amostra_codigo", "profund_sup", "profund_inf")
    }

    # Descarregar planilhas com camadas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$camada), style = 3)
    }
    obs <- list()
    for (i in 1:length(sheets_keys$camada)) {
      tmp <- googlesheets::gs_key(sheets_keys$camada[i], verbose = FALSE)
      unit <- suppressMessages(googlesheets::gs_read_csv(tmp, locale = locale, verbose = FALSE, n_max = 1))

      # Solução rápida para conjuntos de dados para os quais ainda não sabemos a unidade de medida
      unit[which(unit %in% "???")] <- "g/kg"

      tmp <- suppressMessages(
        googlesheets::gs_read_csv(
          tmp, na = c("NA", "-", ""), locale = locale, verbose = FALSE, comment = "unidade")
      )

      # Identificar colunas com dados de ferro
      fe_cols <- colnames(tmp)[grep("^fe_", colnames(tmp))]

      # Se houver, descartar colunas sem dados de ferro, atualizando a lista dos nomes das colunas com dados
      # de ferro
      if (any(apply(tmp[, fe_cols], 2, function (x) all(is.na(x))))) {
        idx <- which(apply(tmp[, fe_cols], 2, function (x) all(is.na(x))))
        tmp <- tmp[, !colnames(tmp) %in% fe_cols[idx]]
        fe_cols <- fe_cols[-idx]
      }

      # Definir as colunas a serem mantidas
      if (which.cols == "standard") {
        tmp <- tmp[, c(target_cols, fe_cols)]
      }

      # Se necessário, descartar camadas sem qualquer tipo de dado de ferro
      if (missing.data == "drop") {
        idx_keep <- is.na(tmp[, fe_cols])
        idx_keep <- rowSums(idx_keep) < ncol(idx_keep)
        tmp <- tmp[idx_keep, ]
      }

      # Verificar se, com a eliminação das camadas sem quaisquer dados de ferro, restou alguma camada
      # Também é possível que o conjunto de dados não possua quaisquer dados de ferro
      if (nrow(tmp) >= 1) {

        # Identificar tipos de dado de ferro para padronização da unidade de medida e número de casas decimais
        fe_type <- stringr::str_split_fixed(fe_cols, "_", n = 3)[, 2]
        fe_stand <- lapply(fe_type, function (y) standards(x = "fe", code = y))
        fe_stand <- do.call(rbind, fe_stand)

        # Se necessário, padronizar unidades de medida
        if (any(!unit[, fe_cols] %in% unique(standards(x = "fe")$unit))) {
          idx <- which(!unit[, fe_cols] %in% unique(standards(x = "fe")$unit))
          conv_factor <- lapply(1:length(fe_type[idx]), function (j) {
            conversions(source = unlist(unit[, fe_cols[idx]])[[j]], target = fe_stand$unit[idx][j])
          })
          conv_factor <- do.call(rbind, conv_factor)
          tmp[fe_cols[idx]] <- t(t(tmp[fe_cols[idx]]) * conv_factor$factor)
        }

        # Padronizar número de casas decimais
        tmp[, fe_cols] <- sapply(1:length(fe_cols), function (j) {
          round(tmp[, fe_cols[j]], digits = fe_stand$digits[j])
        })

        # Harmonizar dados de ferro
        if (harmonization$level == 1) {
          new_colnames <- matrix(stringr::str_split_fixed(colnames(tmp[, fe_cols]), "_", 3)[, 1:2], ncol = 2)
          new_colnames <- apply(new_colnames, 1, paste, collapse = "_", sep = "")

          # Nomes idênticos são gerados para variáveis definidas pelo mesmo extrator. Nesses casos mantém-se
          # os nomes originais das respectivas colunas.
          if (any(duplicated(new_colnames))) {
            idx <- c(which(duplicated(new_colnames)), which(duplicated(new_colnames, fromLast = TRUE)))
            new_colnames[idx] <- fe_cols[idx]
          }
          colnames(tmp)[colnames(tmp) %in% fe_cols] <- new_colnames
        }

        # Se as tabelas forem empilhadas, então 'observacao_id', 'profund_sup' e 'profund_inf' precisam estar
        # no formato de caracter para evitar erros devido ao tipo de dado.
        if (stack.layers) {
          tmp$observacao_id <- as.character(tmp$observacao_id)
          tmp$profund_sup <- as.character(tmp$profund_sup)
          tmp$profund_inf <- as.character(tmp$profund_inf)
        }

        # Adicionar 'dataset_id' às camadas processadas.
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
    if (stack.layers) {
      obs <- suppressWarnings(dplyr::bind_rows(obs))
    }

    return (obs)
  }

# Definir unidade e número de casas decimais para cada tipo de dado de ferro ##################################
standards <-
  function (x = "fe", code) {

    switch(
      x,
      "fe" = {
        # Segundo a terceira edição do Manual de Métodos de Análise de Solo (Donagemma et al. 2011), as
        # seguintes convenções devem ser aplicadas:
        # - Ferro no extrato sulfúrico: g/kg, com 1 casa decimal
        # - Ferro livre: g/kg, com 2 casas decimais
        # - Microelementos: mg/kg, sem casa decimal
        # - Ataque triácido: g/kg, com 1 casa decimal
        res <- matrix(
          c("aquaregia",   "g/kg",  1, "Ácido clorídrico + Ácido nítrico",
            "cloridrico",  "g/kg",  1, "Ácido clorídrico",
            "ditionito",   "g/kg",  2, "Citrato-ditionito-bicarbonato",
            "dtpa",        "mg/kg", 0, "DTPA",
            "mehlich",     "mg/kg", 0, "Mehlich",
            "oxalato",     "g/kg",  2, "Oxalato ácido de amônio",
            "pirofosfato", "g/kg",  2, "Pirofosfato de sódio",
            "rggh",        "-",     3, "Razão goethita/(goethita+hematita)",
            "sulfurico",   "g/kg",  1, "Ácido sulfúrico",
            "triacido",    "g/kg",  1, "Ácido perclórico + Ácido nítrico + Ácido fluorídrico"),
          ncol = 4, byrow = TRUE
        )
        res <- as.data.frame(res, stringsAsFactors = FALSE)
        colnames(res) <- c("code", "unit", "digits", "description")
        res$digits <- as.numeric(res$digits)
      }
    )

    if (!missing(code)) {
      res <- res[res$code %in% code, ]
    }

    return (res)
  }

# Conversion of units #########################################################################################
conversions <-
  function (source, target) {

    # Fatores de conversão entre unidades
    switch(
      target,
      "mg/kg" = {
        res <- data.frame(
          source = c("g/kg", "%",   "mg/dm3"),
          factor = c(1000,   10000, 1)
        )
        res <- res$factor[res$source %in% source]
      },
      "g/kg" = {
        res <- data.frame(
          source = c("mg/kg", "%", "mg/dm3"),
          factor = c(1/1000,  10,  1/1000)
        )
        res <- res$factor[res$source %in% source]
      }
    )

    res <- data.frame(source, target, factor = res)
    return (res)
  }
