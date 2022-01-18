#' @title Process soil taxonomy
#'
#' @description Extract and process soil taxonomic data from textual soil classification
#' descriptions of the Brazilian Soil Classification System (SiBCS).
#'
#' @param text Character string(s) with soil classification description(s) (in Portuguese).
#'
#' @param method Character string defining the string processing method. Options:
#' * `"decompose"`: decompose the Brazilian soil classification into its four higher categorical
#' levels (order, suborder, large group, and subgroup).
#'
#' @param sep Character string. Defaults to `sep = " "`.
#'
#' @param pattern Character string (in Portuguese). Defaults to
#' `pattern = c(", ", " A ", " textura ")`.
#'
#' @return An object of class [base::data.frame] with four named columns: `ordem` (UPPER CASE),
#' `subordem` (UPPER CASE), `grangrupo` (Title Case), and `subgrupo` (lower case).
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#'
#' @references
#' Santos, H. G. dos, Jacomine, P. K. T., Anjos, L. H. C. dos, Oliveira, V. Á. de, Lumbreras, J.
#' F., Coelho, M. R., Almeida, J. A. de, Araújo Filho, J. C. de, Oliveira, J. B. de, & Cunha, T. J.
#' F. (2018). Sistema Brasileiro de Classificação de Solos (5th ed., p. 531). Embrapa.
#' \url{https://www.infoteca.cnptia.embrapa.br/infoteca/handle/doc/1094003}.
#'
#' IBGE. (2015). Manual Técnico de Pedologia (3rd ed., p. 430). Instituto Brasileiro de Geografia
#' e Estatística. \url{https://biblioteca.ibge.gov.br/visualizacao/livros/liv95017.pdf}
#'
#' @export
#'
#' @examples
#' text <-
#'   c("CAMBISSOLO HÁPLICO Ta Eutrófico léptico A proeminente textura média",
#'     "PLANOSSOLO HÁPLICO Distrófico solódico êndico plintossólico, textura média, Tb",
#'     "CHERNOSSOLO ARGILÚVICO Órtico típico textura média com cascalho/argilosa com cascalho",
#'     "ARGISSOLO VERMELHO-AMARELO",
#'     "Latossolo"
#'   )
#' taxonomy(text)
####################################################################################################
taxonomy <-
  function(text, method = "decompose", sep = " ", pattern = c(", ", " A ", " textura ")) {
    switch(
      method,
      decompose = {
        # Create object to hold the data
        na <- rep(NA_character_, length(text))
        res <- data.frame(
          ordem = na, subordem = na, grangrupo = na, subgrupo = na, stringsAsFactors = FALSE)
        # Clean string considering various string patterns
        for (i in seq_along(pattern)) {
          text <- strsplit(x = text, split = pattern[i])
          text <- sapply(text, function(x) x[1])
        }
        text <- strsplit(x = text, split = sep)
        # Get soil classes
        res[["ordem"]] <- sapply(text, function(x) x[1])
        res[["ordem"]] <- toupper(res[["ordem"]]) # order is upper case
        res[["subordem"]] <- sapply(text, function(x) x[2])
        res[["subordem"]] <- toupper(res[["subordem"]]) # suborder is upper case
        res[["grangrupo"]] <- sapply(text, function(x) x[3])
        idx <- sapply(res[["grangrupo"]], function(x) x %in% c("Ta", "Tb"))
        res[["grangrupo"]][idx] <- sapply(text[idx], function(x) paste0(x[3:4], collapse = " "))
        res[["grangrupo"]] <- as.character(res[["grangrupo"]])
        n <- 10
        res[["subgrupo"]][idx] <- sapply(text[idx], function(x) {
          paste0(stats::na.omit(x[5:n]), collapse = " ")
        })
        res[["subgrupo"]][!idx] <- sapply(text[!idx], function(x) {
          paste0(stats::na.omit(x[4:n]), collapse = " ")
        })
        res[["subgrupo"]] <- as.character(res[["subgrupo"]])
        res[["subgrupo"]] <- tolower(res[["subgrupo"]]) # sub group is lower case
        res[["subgrupo"]][res[["subgrupo"]] == ""] <- NA_character_
      }
    )
    # Output
    return(res)
  }
