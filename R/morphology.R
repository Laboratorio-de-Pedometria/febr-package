#' Soil morphology
#' Extract and process soil morphological properties from field soil morphology descriptions.
#' @param x Character string with field soil morphology description (in Portuguese).
#' @param variable Character string defining the soil morphological property of interest. Options:
#' `color`, `structure`, `consistency`.
#' @export
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @examples
#' \donttest{
#' horizons <- layer(dataset = "ctb0025", variable = "morfologia_descricao")
#' color <- morphology(x = horizons$morfologia_descricao, variable = "color")
#' structure <- morphology(x = horizons$morfologia_descricao, variable = "structure")
#' consistency <- morphology(x = horizons$morfologia_descricao, variable = "consistency")
#' }
#' x <- "cinzento rosado (7.5YR 6/2, seco), bruno escuro (7.5YR 3/2, úmido)"
#' color <- morphology(x = x, variable = "color")
####################################################################################################
morphology <-
  function(x, variable = "color") {
    switch(
      variable,
      color = {
        # Fonte: https://stackoverflow.com/questions/8613237/
        res <- gregexpr(pattern = "(?<=\\().*?(?=\\))", text = x, perl = TRUE)
        res <- regmatches(x = x, m = res)
        no_color <- sapply(res, length) == 0
        for (i in seq_along(res[!no_color])) {
          # Fonte: https://stackoverflow.com/questions/8020848/
          # ú --> \u00fa
          wet_color <- grep(pattern = "(\u00famido|\U00FAmida)", x = res[!no_color][[i]])
          # Adicionar regras para demais condições:
          # - o termo 'úmido' é omitido da descrição porque a cor do solo seco não foi determinada
          dry_color <- grep(pattern = "(seco|seca)", x = res[!no_color][[i]])
          res[!no_color][[i]] <-
            suppressWarnings(res[!no_color][[i]][c(min(wet_color), min(dry_color))])
        }
        res[no_color] <- lapply(res[no_color], function(x) c(NA_character_, NA_character_))
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
        # Dados sobre estrutura geralmente são o terceiro item da lista
        res <- sapply(res, function(x) x[3])
        res <- trimws(res)
        # Grau da estrutura
        # ã -->  \u00e3
        str_degree <- c("gr\u00e3os simples", "maci\u00e7a", "fraca", "moderada", "forte")
        idx <- list()
        for (i in seq_along(str_degree)) {
          idx[[i]] <- grepl(str_degree[i], res)
        }
        idx <- do.call(cbind, idx)
        estrutura_grau <- apply(idx, 1, function(x) str_degree[x][1])
        # Tipo de estrutura
        str_type <-
          c("laminar", "prism\u00e1tica", "colunar", "blocos angulares", "blocos subangulares",
            "granular")
        no_str <- estrutura_grau %in% c("gr\u00e3os simples", "maci\u00e7a") # ç --> \u00e7
        idx <- list()
        for (i in seq_along(str_type)) {
          idx[[i]] <- grepl(str_type[i], res[!no_str])
        }
        idx <- do.call(cbind, idx)
        estrutura_tipo <- rep(NA, length(res))
        estrutura_tipo[!no_str] <- apply(idx, 1, function(x) str_type[!no_str][x][1])
        # Classe de diâmetro da estrutura
        # é --> \u00e9
        str_size <- rev(c("muito pequena", "pequena", "m\u00e9dia", "grande", "muito grande"))
        idx <- list()
        for (i in seq_along(str_size)) {
          idx[[i]] <- grepl(str_size[i], res[!no_str])
        }
        idx <- do.call(cbind, idx)
        estrutura_cdiam <- rep(NA, length(res))
        estrutura_cdiam[!no_str] <- apply(idx, 1, function(x) str_size[!no_str][x][1])
        # Resultado
        res <- data.frame(estrutura_tipo, estrutura_grau, estrutura_cdiam, stringsAsFactors = FALSE)
      },
      consistency = {
        res0 <- strsplit(x, ";")
        # Dados sobre estrutura geralmente são o quarto item da lista
        # Contudo, se houver cerosidade, costumam se o quinto item da lista
        res <- sapply(res0, function(x) x[4])
        idx <- grepl("cerosidade", res)
        res[idx] <- sapply(res0[idx], function(x) x[5])
        res <- trimws(res)
        # Consistência do solo úmido (friabilidade)
        friabi <- 
          c("solt(o|a)", "muito fri\u00e1vel", "fri\u00e1vel", "firme", "muito firme",
            "extremamente firme")
        idx <- list()
        for (i in seq_along(friabi)) {
          idx[[i]] <- grepl(friabi[i], res)
        }
        idx <- do.call(cbind, idx)
        consistencia_friabi <- rep(NA, length(res))
        consistencia_friabi <- apply(idx, 1, function(x) friabi[x][1])
        consistencia_friabi <- gsub("(o|a)", "o", consistencia_friabi, fixed = TRUE)
        # Consistência do solo seco (dureza)
        dureza <-
          c("solt(o|a)", "maci(o|a)", "ligeiramente dur(o|a)", "dur(o|a)", "muito dur(o|a)",
            "extremamente dur(o|a)")
        idx <- list()
        for (i in seq_along(dureza)) {
          idx[[i]] <- grepl(dureza[i], res)
        }
        idx <- do.call(cbind, idx)
        consistencia_dureza <- rep(NA, length(res))
        consistencia_dureza <- apply(idx, 1, function(x) dureza[x][1])
        consistencia_dureza <- gsub("(o|a)", "o", consistencia_dureza, fixed = TRUE)
        # Consistência do solo molhado (plasticidade)
        plasticidade <- 
          c("n\u00e3o-pl\u00e1stic(o|a)", "ligeiramente pl\u00e1stic(o|a)", "pl\u00e1stic(o|a)",
            "muito pl\u00e1stic(o|a)")
        idx <- list()
        for (i in seq_along(plasticidade)) {
          idx[[i]] <- grepl(plasticidade[i], res)
        }
        idx <- do.call(cbind, idx)
        consistencia_plasti <- rep(NA, length(res))
        consistencia_plasti <- apply(idx, 1, function(x) plasticidade[x][1])
        consistencia_plasti <- gsub("(o|a)", "o", consistencia_plasti, fixed = TRUE)
        # Consistência do solo molhado (pegajosidade)
        pegajo <- 
          c("n\u00e3o pegajos(o|a)", "ligeiramente pegajos(o|a)", "pegajos(o|a)",
            "muito pegajos(o|a)")
        idx <- list()
        for (i in seq_along(pegajo)) {
          idx[[i]] <- grepl(pegajo[i], res)
        }
        idx <- do.call(cbind, idx)
        consistencia_pegajo <- rep(NA, length(res))
        consistencia_pegajo <- apply(idx, 1, function(x) pegajo[x][1])
        consistencia_pegajo <- gsub("(o|a)", "o", consistencia_pegajo, fixed = TRUE)
        # Resultado
        res <- data.frame(
          consistencia_friabi, consistencia_dureza, consistencia_plasti, consistencia_pegajo,
          stringsAsFactors = FALSE)
      }
    )
    return(res)
  }
