#' Go to FEBR
#' 
#' Go to one of the web pages of the Free Brazilian Repository for Open Soil Data -- FEBR --, including
#' project and dataset web pages.
#' 
#' @param dataset (optional) Character vector indicating one dataset. The identification code should be as
#' recorded in \url{https://www.pedometria.shinyapps.io/febr/}.
#' 
#' @param table (optional) Character string indicating a table, i.e. the *identification* table, 
#' `"identificacao"`, the *observation* table, `"observacao"`, the *layer* table, `"camada"`, or the 
#' *metadata* table, `"metadado"`.
#' 
#' @param page (optional) Character string indicating a web page of the FEBR. Options are: `"febr"`, `"view"`,
#' `"catalog"`, `"search"`, `"book"`, `"package"`, `"github"`, `"forum"`, `"units"`, `"standards"`, and
#' `"index"`.
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
  function (dataset, table, page) {
    
    # ARGUMENTOS
    ## dataset
    if (!missing(dataset)) {
      if (!is.character(dataset)) {
        stop (glue::glue("object of class '{class(dataset)}' passed to argument 'dataset'"))
      }
      if (length(dataset) > 1) {
        stop ("a single identification code must be passed to argument 'dataset'")
      }
    }
    
    ## table
    if (!missing(table)) {
      if (!is.character(table)) {
        stop (glue::glue("object of class '{class(table)}' passed to argument 'table'"))
      }
      if (!table %in% c("dataset", "observacao", "camada", "metadado")) {
        stop (glue::glue("unknown value '{table}' passed to argument 'table'"))
      }
    }
    
    ## page
    if (!missing(page)) {
      if (!is.character(page)) {
        stop (glue::glue("object of class '{class(page)}' passed to argument 'page'"))
      }
      ops <- c("febr", "view", "catalog", "search", "book", "package", "github", "forum", "units", "standards",
               "index", "template")
      if (!page %in% ops) {
        stop (glue::glue("unknown value '{page}' passed to argument 'page'"))
      }
    }
    
    ## Identificar URL
    if (missing(dataset) && missing(table) && !missing(page)) { # Ir para alguma página do projeto
      switch (page,
        febr = {
          url <- "https://www.pedometria.org/projeto/febr/"
          },
        view = {
          url <- "https://www.pedometria.shinyapps.io/febr/"
        },
        catalog = {
          url <- "https://www.pedometria.shinyapps.io/febr/"
        },
        search = {
          url <- "https://www.pedometria.shinyapps.io/febr/"
        },
        book = {
          url <- "https://www.pedometria.org/projeto/febr/"
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
      
    } else if (missing(table) && missing(page) && !missing(dataset)) { # Ir para diretório do conjunto de dados
      url <- paste0("https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso?path=%2F", dataset)
      
    } else { # Ir para tabela do conjunto de dados
      sheets_keys <- .getSheetsKeys(dataset = dataset)
      url <- paste('https://docs.google.com/spreadsheets/d/', sheets_keys[[table]], sep = '')
    }
    
    ## Lançar navegador
    if (!is.null(url)) {
      utils::browseURL(url)
    }
  }
