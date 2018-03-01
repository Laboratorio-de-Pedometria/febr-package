#' Go to ___febr___
#' 
#' Go to one of the web pages of the Free Brazilian Repository for Open Soil Data -- ___febr___ --, including
#' project and dataset web pages.
#' 
#' @param dataset Character vector indicating one dataset. The identification code should be as recorded
#' in \url{http://www.ufsm.br/febr/catalog/}.
#' 
#' @param table Character string indicating a table, i.e. the *dataset* table, `"dataset"`, the *observation*
#' table, `"observacao"`, the *layer* table, `"camada"`, or the *metadata* table, `"metadado"`.
#' 
#' @param page Character string indicating a web page of the ___febr___. Options are: `"febr"`, `"view"`,
#' `"catalog"`, `"search"`, `"book"`, `"package"`, `"github"`, `"forum"`, `"unit"`, and `"standard"`.
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @export
#' @examples
#' \dontrun{
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
      ops <- c("febr", "view", "catalog", "search", "book", "package", "github", "forum", "unit", "standard")
      if (!page %in% ops) {
        stop (glue::glue("unknown value '{page}' passed to argument 'page'"))
      }
    }
    
    ## Identificar URL
    if (missing(dataset) && missing(table) && !missing(page)) { # Ir para alguma página do projeto
      switch (page,
        febr = {
          url <- "http://coral.ufsm.br/febr/"
          },
        view = {
          url <- "http://coral.ufsm.br/febr/view"
        },
        catalog = {
          url <- "http://coral.ufsm.br/febr/catalog/"
        },
        search = {
          url <- "http://coral.ufsm.br/febr/search"
        },
        book = {
          url <- "http://coral.ufsm.br/febr/book/"
        },
        package = {
          url <- "https://febr-team.github.io/febr-package/"
        },
        github = {
          url <- "https://github.com/febr-team"
        },
        forum = {
          url <- "https://groups.google.com/forum/#!forum/febr-forum"
        },
        unit = {
          url <- "https://goo.gl/Vvvsf2"
        },
        standard = {
          url <- "https://goo.gl/hi77sB"
        }
      )
      
    } else if (missing(table) && missing(page) && !missing(dataset)) { # Ir para página do conjunto de dados
      url <- glue::glue("http://coral.ufsm.br/febr/catalog/{dataset}.html")
      
    } else { # Ir para tabela do conjunto de dados no GoogleDrive
      sheets_keys <- .getSheetsKeys(dataset = dataset)
      key <- googlesheets::gs_key(x = sheets_keys[[table]], verbose = FALSE)
      url <- key$browser_url
    }
    
    ## Lançar navegador
    if (!is.null(url)) {
      browseURL(url)
    }
  }
