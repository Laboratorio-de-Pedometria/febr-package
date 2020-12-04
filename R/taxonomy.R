#' Soil taxonomy
#' Extract and process soil taxonomic data from textual soil classification description.
#' @param text Character string with soil classification description (in Portuguese).
#' @param method Character string defining the string processing method. Options: `decompose`.
#' @param sep Character string.
#' Defaults to `sep = " "`.
#' @param pattern Character string (in Portuguese).
#' Defaults to `pattern = c(", ", " A ", " textura ")`.
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
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
        res[["ordem"]] <- toupper(res[["ordem"]])
        res[["subordem"]] <- sapply(text, function(x) x[2])
        res[["subordem"]] <- toupper(res[["subordem"]])
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
        res[["subgrupo"]] <- tolower(res[["subgrupo"]])
        res[["subgrupo"]][res[["subgrupo"]] == ""] <- NA_character_
      }
    )
    # Output
    return(res)
  }
