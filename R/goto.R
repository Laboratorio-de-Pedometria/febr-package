#' Go to FEBR
#' 
#' Go to one of the web pages of the Free Brazilian Repository for Open Soil Data -- FEBR --,
#' including project and dataset web pages.
#' @param dataset (optional) Character vector indicating one dataset.
#' @param page (optional) Character string indicating a web page of the FEBR. Options are: `"febr"`,
#' `"search"`, `"package"`, `"github"`, `"forum"`, `"units"`, `"standards"`, `"index"`, and `"template"`.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # Go to the main project page
#' goto(page = "febr")
#' }
###############################################################################################################
goto <-
  function (dataset, page) {
    
    # ARGUMENTOS
    ## dataset
    if (!missing(dataset)) {
      if (!is.character(dataset)) {
        stop(paste0("object of class '", class(dataset), "' passed to argument 'dataset'"))
      }
      if (length(dataset) > 1) {
        stop("a single identification code must be passed to argument 'dataset'")
      }
    }
    
    ## page
    if (!missing(page)) {
      if (!is.character(page)) {
        stop(paste0("object of class '", class(page), "}' passed to argument 'page'"))
      }
      ops <- c("febr", "search", "package", "github", "forum", "units", "standards", "index", "template")
      if (!page %in% ops) {
        stop (paste0("unknown value '", page, "' passed to argument 'page'"))
      }
    }
    
    ## Identificar URL
    if (missing(dataset) && !missing(page)) { # Ir para alguma página do projeto
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
        forum = {
          url <- "https://groups.google.com/forum/#!forum/febr-forum"
        },
        units = {
          url <- "https://docs.google.com/spreadsheets/d/1tU4Me3NJqk4NH2z0jvMryGObSSQLCvGqdLEL5bvOflo"
        },
        standards = {
          url <- "https://docs.google.com/spreadsheets/d/1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI"
        },
        index = {
          url <- "https://cloud.utfpr.edu.br/index.php/apps/onlyoffice/s/JDcb8XBvkpQeyXm"
        },
        template = {
          url <- "https://docs.google.com/spreadsheets/d/1rXIiT1zSYhFegSdAvE0yJX16q-bvXVNpYIYdd5YgjhI"
        }
      )
    } else if (missing(page) && !missing(dataset)) { # Ir para diretório do conjunto de dados
      url <- paste0("https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F", dataset)
    }
    
    ## Lançar navegador
    if (!is.null(url)) {
      utils::browseURL(url)
    }
  }
