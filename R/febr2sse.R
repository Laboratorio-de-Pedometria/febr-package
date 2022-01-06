#' @title Conversion between FEBR and SmartSolos Expert (SSE) soil profile data formats
#' @description Export FEBR soil profile data to the JSON file format required by the SmartSolos
#' Expert API.
#' @param profiles Data frame with soil profile data, i.e. observation locations.
#' @param horizons Data frame with soil horizon data, i.e. sampling layers.
#' @param file (optional) Character string naming the JSON file to be written to disk.
#' @param ... (optional) Arguments passed to [base::writeLines()].
#' @return An object of class `character` containing a unicode JSON string.
#' @references
#' Jeroen Ooms (2014). The jsonlite Package: A Practical and Consistent
#' Mapping Between JSON Data and R Objects. arXiv:1403.2805 \[stat.CO\]
#' URL \url{https://arxiv.org/abs/1403.2805}.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' # Toy example
#' pro <- data.frame(
#'   observacao_id = "Perfil-1",
#'   taxon_sibcs_1999 = "Argissolo",
#'   stringsAsFactors = FALSE
#' )
#' hor <- data.frame(
#'   observacao_id = rep("Perfil-1", 3),
#'   camada_id = c("Ap", "B1", "B2"),
#'   profund_sup = c(0, 16, 32),
#'   profund_inf = c(16, 32, 50),
#'   cor_matriz_umido_munsell = rep("5YR 3/3", 3),
#'   cor_matriz_seco_munsell = rep("5YR 3/3", 3),
#'   estrutura_tipo = rep("blocos subangulares", 3),
#'   estrutura_grau = rep("moderada", 3),
#'   estrutura_cdiam = rep("média", 3),
#'   consistencia_umido = rep("friável", 3),
#'   consistencia_seco = rep("duro", 3),
#'   plasticidade = rep("plástico", 3),
#'   pegajosidade = rep("pegajoso", 3),
#'   stringsAsFactors = FALSE
#' )
#' febr2sse(pro, hor, tempfile(fileext = ".json"))
#'
#' # Real example
#' if (interactive()) {
#' profiles <- observation(
#'   data.set = "ctb0025", variable = c("taxon_sibcs", "relevo_drenagem"),
#'   standardization = list(units = TRUE, round = TRUE))
#' idx <- profiles$observacao_id[1]
#' profiles <- profiles[profiles$observacao_id %in% idx, ]
#' horizons <- layer(
#'   data.set = "ctb0025", variable = "all",
#'   standardization =
#'     list(plus.sign = "remove", lessthan.sign = "remove",
#'          transition = "smooth", units = TRUE, round = TRUE))
#' horizons <- horizons[horizons$observacao_id %in% idx, ]
#' horizons[, 9:48] <- lapply(horizons[, 9:48], as.numeric)
#' horizons <- cbind(
#'   horizons,
#'   morphology(x = horizons$morfologia_descricao, variable = "color"),
#'   morphology(x = horizons$morfologia_descricao, variable = "structure"),
#'   morphology(x = horizons$morfologia_descricao, variable = "consistence"),
#'   stringsAsFactors = FALSE)
#' file <- ifelse(
#'   dir.exists("tmp"),
#'   paste0("tmp/febr2smartsolos-", idx, ".json"),
#'   paste0(tempdir(), "/febr2smartsolos-", idx, ".json"))
#' febr2sse(profiles, horizons, file)
#' }
####################################################################################################
febr2sse <-
  function(profiles, horizons, file, ...) {
    #
    if (!requireNamespace("jsonlite")) stop("jsonlite package is missing")
    #
    # Mapeamento de metadados
    gs <- "1mc5S-HsoCcxLeue97eMoWLMse4RzFZ1_MCQyQhfzXUg"
    sheet <- "dados"
    https_request <- paste0(
      "https://docs.google.com/spreadsheets/d/", gs, "/gviz/tq?tqx=out:csv&sheet=", sheet)
    translation <- suppressWarnings(
      utils::read.table(file = https_request, sep = ",", header = TRUE, stringsAsFactors = FALSE))
    sheet <- "vocabulario"
    https_request <- paste0(
      "https://docs.google.com/spreadsheets/d/", gs, "/gviz/tq?tqx=out:csv&sheet=", sheet)
    vocabulary <- suppressWarnings(
      utils::read.table(file = https_request, sep = ",", header = TRUE, stringsAsFactors = FALSE))
    #
    # Processar classificação taxonômica
    taxon <- profiles[, startsWith(colnames(profiles), "taxon_sibcs")]
    taxon <- taxonomy(text = taxon, method = "decompose",
      sep = " ", pattern = c(", ", " A ", " textura "))
    colnames(taxon) <- c("ORDEM", "SUBORDEM", "GDE_GRUPO", "SUBGRUPO")
    profiles <- cbind(profiles, taxon)
    #
    # Processar cor do solo úmido
    cor <- strsplit(horizons[["cor_matriz_umido_munsell"]], " ")
    horizons[["COR_UMIDA_MATIZ"]] <- sapply(cor, function(x) x[1])
    cor <- sapply(cor, function(x) x[2])
    cor <- strsplit(cor, "/")
    horizons[["COR_UMIDA_VALOR"]] <- as.integer(sapply(cor, function(x) x[1]))
    horizons[["COR_UMIDA_CROMA"]] <- as.integer(sapply(cor, function(x) x[2]))
    #
    # Processar cor do solo seco
    cor <- strsplit(horizons[["cor_matriz_seco_munsell"]], " ")
    horizons[["COR_SECA_MATIZ"]] <- sapply(cor, function(x) x[1])
    cor <- sapply(cor, function(x) x[2])
    cor <- strsplit(cor, "/")
    horizons[["COR_SECA_VALOR"]] <- as.integer(sapply(cor, function(x) x[1]))
    horizons[["COR_SECA_CROMA"]] <- as.integer(sapply(cor, function(x) x[2]))
    #
    # Processar estrutura do solo
    idx <- match(
      horizons[["estrutura_tipo"]],
      vocabulary[vocabulary[["sse_var_name"]] == "ESTRUTURA_TIPO", "febr_var_value"])
    horizons[["estrutura_tipo"]] <-
      vocabulary[vocabulary[["sse_var_name"]] == "ESTRUTURA_TIPO", "sse_var_code"][idx]
    idx <- match(
      horizons[["estrutura_grau"]],
      vocabulary[vocabulary[["sse_var_name"]] == "ESTRUTURA_GRAU", "febr_var_value"])
    horizons[["estrutura_grau"]] <-
      vocabulary[vocabulary[["sse_var_name"]] == "ESTRUTURA_GRAU", "sse_var_code"][idx]
    idx <- match(
      horizons[["estrutura_cdiam"]],
      vocabulary[vocabulary[["sse_var_name"]] == "ESTRUTURA_TAMANHO", "febr_var_value"])
    horizons[["estrutura_cdiam"]] <-
    vocabulary[vocabulary[["sse_var_name"]] == "ESTRUTURA_TAMANHO", "sse_var_code"][idx]
    #
    # Processar consistência do solo
    idx <- match(
      horizons[["consistencia_umido"]],
      vocabulary[vocabulary[["sse_var_name"]] == "CONSISTENCIA_UMIDO", "febr_var_value"])
    horizons[["consistencia_umido"]] <-
      vocabulary[vocabulary[["sse_var_name"]] == "CONSISTENCIA_UMIDO", "sse_var_code"][idx]
    idx <- match(
      horizons[["consistencia_seco"]],
      vocabulary[vocabulary[["sse_var_name"]] == "CONSISTENCIA_SECO", "febr_var_value"])
    horizons[["consistencia_seco"]] <-
      vocabulary[vocabulary[["sse_var_name"]] == "CONSISTENCIA_SECO", "sse_var_code"][idx]
    idx <- match(
      horizons[["plasticidade"]],
      vocabulary[vocabulary[["sse_var_name"]] == "PLASTICIDADE", "febr_var_value"])
    horizons[["plasticidade"]] <-
      vocabulary[vocabulary[["sse_var_name"]] == "PLASTICIDADE", "sse_var_code"][idx]
    idx <- match(
      horizons[["pegajosidade"]],
      vocabulary[vocabulary[["sse_var_name"]] == "PEGAJOSIDADE", "febr_var_value"])
    horizons[["pegajosidade"]] <-
      vocabulary[vocabulary[["sse_var_name"]] == "PEGAJOSIDADE", "sse_var_code"][idx]
    # profiles
    idx_old <- which(colnames(profiles) %in% translation[["febr_var_name"]])
    idx_new <- match(colnames(profiles)[idx_old], translation[["febr_var_name"]])
    colnames(profiles)[idx_old] <- translation[["sse_var_name"]][idx_new]
    # horizons
    idx_old <- which(colnames(horizons) %in% translation[["febr_var_name"]])
    idx_new <- match(colnames(horizons)[idx_old], translation[["febr_var_name"]])
    colnames(horizons)[idx_old] <- translation[["sse_var_name"]][idx_new]
    # Conversão para JSON
    profiles[["HORIZONTES"]] <- NA
    horizons <- split(x = horizons, f = horizons[["ID_PONTO"]])
    for (i in seq_along(horizons)) {
      profiles[["HORIZONTES"]][i] <- list(horizons[[i]])
    }
    profiles <- list(items = profiles)
    #
    # Saída: arquivo ou string JSON
    ss <- jsonlite::toJSON(profiles, pretty = TRUE)
    if (!missing(file)) {
      writeLines(text = ss, con = file, ...)
    } else {
      return(ss)
    }
  }
####################################################################################################
# @rdname febr2smartsolos
# @export
# smartsolos2febr <-
#   function(file, ...) {
#     profiles <- jsonlite::fromJSON(txt = file, ...)
#     horizons <- profiles$items$HORIZONTES
#     horizons <- lapply(horizons, data.table::as.data.table)
#     horizons <- data.table::rbindlist(horizons, fill = TRUE)
#     profiles <- profiles$items[, !colnames(profiles$items) == "HORIZONTES"]
#     # falta incluir funções para renomear colunas
#     # https://docs.google.com/spreadsheets/d/1mc5S-HsoCcxLeue97eMoWLMse4RzFZ1_MCQyQhfzXUg/edit
#     return(list(observacao = profiles, camada = horizons))
#   }
