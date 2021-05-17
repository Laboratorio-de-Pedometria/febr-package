#' Go to FEBR
#' 
#' Go to one of the web pages of the Free Brazilian Repository for Open Soil Data (FEBR),
#' including project and dataset web pages.
#' 
#' @param data.set (optional) Character vector indicating a single data set.
#' 
#' @param page (optional) Character string indicating a web page of the FEBR. Options are:
#' `"febr"` (FEBR web page),
#' `"search"` (data set search web page),
#' `"package"` (**febr** package on CRAN),
#' `"github"` (**febr** package source code repository on GitHub),
#' `"gitlab"` (FEBR source code repository on GitLab),
#' `"forum"` (FEBR public forum at Google Groups),
#' `"units"` (units and conversion factors used in FEBR),
#' `"dictionary"` (dictionary used in FEBR),
#' `"index"` (data set index), and
#' `"template"` (spreadsheet data template).
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
        c("febr", "search", "package", "github", "gitlab", "forum", "units", "dictionary", "index",
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
          url <- "https://github.com/samuel-rosa/febr-package"
        },
        gitlab = {
          url <- "https://gitlab.com/laboratorio-de-pedometria/febr"
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
      url <- paste0("https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F", data.set)
    }
    ## Lançar navegador
    if (!is.null(url)) {
      utils::browseURL(url = url)
    }
  }
