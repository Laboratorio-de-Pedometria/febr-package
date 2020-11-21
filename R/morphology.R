#' Soil morphology
#' 
#' Extract and process soil morphological properties from field soil morphology descriptions.
#' 
#' @param x Character string with field soil morphology description (in Portuguese).
#' 
#' @param variable Character string defining the soil morphological property of interest. Options: `color`, 
#' `structure`.
#' 
#' @export
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @examples 
#' \donttest{
#' horizons <- layer(dataset = "ctb0025", variable = "morfologia_descricao")
#' color <- morphology(x = horizons$morfologia_descricao, variable = "color")
#' structure <- morphology(x = horizons$morfologia_descricao, variable = "structure")
#' }
#' x <- "cinzento rosado (7.5YR 6/2, seco), bruno escuro (7.5YR 3/2, úmido)"
#' color <- morphology(x = x, variable = "color")
###############################################################################################################
morphology <-
  function (x, variable = "color") {
    switch (
      variable,
      color = {
        # Fonte: https://stackoverflow.com/questions/8613237/
        res <- gregexpr(pattern = "(?<=\\().*?(?=\\))", text = x, perl = TRUE)
        res <- regmatches(x = x, m = res)
        no_color <- sapply(res, length) == 0
        for (i in seq_along(res[!no_color])) {
          # Fonte: https://stackoverflow.com/questions/8020848/
          wet_color <- grep(pattern = "(\u00famido|\U00FAmida)", x = res[!no_color][[i]]) # ú --> \u00fa
          dry_color <- grep(pattern = "(seco|seca)", x = res[!no_color][[i]])
          res[!no_color][[i]] <- suppressWarnings(res[!no_color][[i]][c(min(wet_color), min(dry_color))])
        }
        res[no_color] <- lapply(res[no_color], function (x) c(NA_character_, NA_character_))
        res <- do.call(rbind, res)
        colnames(res) <- c("cor_matriz_umido_munsell", "cor_matriz_seco_munsell")
        # Limpeza dos dados
        res[, 1] <- sub("(\u00famido|\U00FAmida)", "", res[, 1]) # ú --> \u00fa
        res[, 2] <- sub("(seco|seca)", "", res[, 2])
        res <- sub(", ", "", res)
        res <- trimws(res)
        res <- sub(",", ".", res)
        res <- sub("\u00be", "3/4", res) # ¾ --> \u00be
      },
      structure = {
        res <- strsplit(x, ";")
        res <- sapply(res, function (x) x[3])
        res <- trimws(res)
        # Grau da estrutura
        str_degree <- c("gr\u00e3os simples", "maci\u00e7a", "fraca", "moderada", "forte") # ã -->  \u00e3
        idx <- list()
        for (i in seq_along(str_degree)) {
          idx[[i]] <- grepl(str_degree[i], res)
        }
        idx <- do.call(cbind, idx)
        estrutura_grau <- apply(idx, 1, function (x) str_degree[x][1])
        # Tipo de estrutura
        str_type <- c("laminar", "prism\u00e1tica", "colunar", "blocos angulares", "blocos subangulares",
                      "granular")
        no_str <- estrutura_grau %in% c("gr\u00e3os simples", "maci\u00e7a") # ç --> \u00e7
        idx <- list()
        for (i in seq_along(str_type)) {
          idx[[i]] <- grepl(str_type[i], res[!no_str])
        }
        idx <- do.call(cbind, idx)
        estrutura_tipo <- rep(NA, length(res))
        estrutura_tipo[!no_str] <- apply(idx, 1, function (x) str_type[!no_str][x][1])
        # Classe de diâmetro da estrutura
        str_size <- rev(c("muito pequena", "pequena", "m\u00e9dia", "grande", "muito grande")) # é --> \u00e9
        idx <- list()
        for (i in seq_along(str_size)) {
          idx[[i]] <- grepl(str_size[i], res[!no_str])
        }
        idx <- do.call(cbind, idx)
        estrutura_cdiam <- rep(NA, length(res))
        estrutura_cdiam[!no_str] <- apply(idx, 1, function (x) str_size[!no_str][x][1])
        # Resultado
        res <- data.frame(estrutura_tipo, estrutura_grau, estrutura_cdiam, stringsAsFactors = FALSE)
      }
    )
    return(res)
  }
