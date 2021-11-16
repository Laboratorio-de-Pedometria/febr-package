#' Go to FEBR
#'
#' Visit FEBR assets on the web.
#'
#' @param data.set (optional) Character vector indicating a (unique) dataset whose
#' metadata web page you wish to visit.
#'
#' @param page (optional) Character string indicating a web page of the FEBR, with
#' options:
#' * `"febr"`: FEBR main web page,
#' * `"dictionary"`: FEBR data dictionary and vocabulary,
#' * `"forum"`: FEBR public forum at Google Groups,
#' * `"github"`: **febr** package source code repository on GitHub,
#' * `"index"`: FEBR data set index,
#' * `"search"`: data set search web page,
#' * `"package"`: **febr** package on CRAN,
#' * `"template"`: FEBR spreadsheet template on Google Sheets,
#' * `"units"`: units and conversion factors used in FEBR.
#'
#' @return Load a given URL into an HTML browser via [utils::browseURL()].
#'
#' @seealso [utils::browseURL()]
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' if (interactive()) {
#' # Go to the FEBR web page
#' goto(page = "febr")
#'
#' # Go to the data set index
#' goto(page = "index")
#'
#' # Go to the GitHub repository
#' goto(page = "github")
#' }
####################################################################################################
goto <-
  function(data.set, page) {
    # ARGUMENTS
    ## data.set
    if (!missing(data.set)) {
      if (!is.character(data.set)) {
        stop(paste0("object of class '", class(data.set), "' passed to argument 'data.set'"))
      }
      if (length(data.set) > 1) {
        stop("a single identification code must be passed to argument 'data.set'")
      }
    }
    ## page
    if (!missing(page)) {
      if (!is.character(page)) {
        stop(paste0("object of class '", class(page), "}' passed to argument 'page'"))
      }
      ops <-
        c("febr", "search", "package", "github", "forum", "units", "dictionary", "index",
        "template")
      if (!page %in% ops) {
        stop (paste0("unknown value '", page, "' passed to argument 'page'"))
      }
    }
    ## Identificar URL
    if (missing(data.set) && !missing(page)) { # Ir para alguma página do projeto
      gs <- "https://docs.google.com/spreadsheets/d/"
      switch (page,
        febr = {
          url <- "https://www.pedometria.org/febr/"
          },
        search = {
          url <- "https://www.pedometria.org/febr/buscar/"
        },
        package = {
          url <- "https://CRAN.R-project.org/package=febr"
        },
        github = {
          url <- "https://github.com/laboratorio-de-pedometria/febr-package"
        },
        forum = {
          url <- "https://groups.google.com/forum/#!forum/febr-forum"
        },
        units = {
          url <- paste0(gs, "1tU4Me3NJqk4NH2z0jvMryGObSSQLCvGqdLEL5bvOflo")
        },
        dictionary = {
          url <- paste0(gs, "1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI")
        },
        index = {
          url <- "https://cloud.utfpr.edu.br/index.php/apps/onlyoffice/s/JDcb8XBvkpQeyXm"
        },
        template = {
          url <- paste0(gs, "1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI")
        }
      )
    } else if (missing(page) && !missing(data.set)) { # Ir para diretório do conjunto de dados
      # url <- paste0("https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F", data.set)
      url <- paste0(.opt()$owncloud, data.set)
    }
    ## Lançar navegador
    if (!is.null(url)) {
      utils::browseURL(url = url)
    }
  }
