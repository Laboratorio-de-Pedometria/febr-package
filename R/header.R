#' Get table header
#'
#' Download header data (column names and measurement units) from the *layer* ("camada") or *observation* 
#' ("observacao") table of one or more datasets contained in the Free Brazilian Repository for Open Soil Data 
#' -- FEBR, \url{https://www.pedometria.org/febr/}. This is useful to check what are the variables
#' contained in a dataset before downloading it via \code{\link[febr]{layer}} or 
#' \code{\link[febr]{observation}}.
#'
#' @template data_template
#' @template metadata_template
#' 
#' @param table Character string indicating a table, i.e. the *layer* table, `"camada"`, or the *observation*
#' table, `"observacao"`.
#' 
#' @details 
#' \subsection{Standard identification variables}{
#' Standard identification variables and their content depend on the chosen `table`. See documentation of 
#' \code{\link[febr]{layer}} and \code{\link[febr]{observation}}.
#' }
#' 
#' @return A list of data frames or a data frame with table header data (column names and measurement units) on
#' the chosen variable(s) of the chosen dataset(s).
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{layer}}, \code{\link[febr]{observation}}
#' @export
###############################################################################################################
header <-
  function (dataset, table, variable, stack = FALSE, progress = TRUE, verbose = TRUE) {
    
    .Deprecated(new = 'metadata', package = 'febr')
    
    # ARGUMENTOS
    ## dataset
    if (missing(dataset)) {
      stop ("argument 'dataset' is missing")
    } else if (!is.character(dataset)) {
      stop (paste("object of class", class(dataset), "passed to argument 'dataset'"))
    }
    
    ## table
    if (missing(table)) {
      stop ("argument 'table' is missing")
    } else if (!table %in% c("observacao", "camada")) {
      stop (paste("unknown value '", table, "' passed to argument 'table'", sep = ""))
    }
    
    ## variable
    if (!missing(variable) && !is.character(variable)) {
      stop (paste("object of class '", class(variable), "' passed to argument 'variable'", sep = ''))
    }
    
    ## stack
    if (!is.logical(stack)) {
      stop (paste("object of class '", class(stack), "' passed to argument 'stack'", sep = ""))
    }
    
    ## progress
    if (!is.logical(progress)) {
      stop (paste("object of class", class(progress), "passed to argument 'progress'"))
    }
    
    ## verbose
    if (!is.logical(verbose)) {
      stop (paste("object of class", class(verbose), "passed to argument 'verbose'"))
    }
    
    ## variable + stack
    if (!missing(variable) && variable == "all") {
      if (stack) {
        stop ("data cannot be stacked when downloading all variables")
      }
    }
    
    # Variáveis padrão
    if (table == "observacao") {
      std_cols <- .opt()[["observation"]]$std.cols
    } else if (table == "camada") {
      std_cols <- .opt()[["layer"]]$std.cols
    }
    
    # Descarregar chaves de identificação das tabelas
    sheets_keys <- .getSheetsKeys(dataset = dataset)
    n <- nrow(sheets_keys)
    
    # Descarregar tabelas
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(sheets_keys$ctb), style = 3)
    }
    res <- list()
    for (i in seq(n)) {
      # i <- 1
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", dts, "...", sep = ""))
      }
      
      # DESCARREGAMENTO
      ## Cabeçalho com unidades de medida
      tmp <- .getHeader(x = sheets_keys[[table]][i], ws = table) # identifica Sheet com seu nome
      
      # COLUNAS
      ## Definir as colunas a serem mantidas
      ## As colunas padrão são sempre mantidas.
      in_cols <- colnames(tmp)
      cols <- in_cols[in_cols %in% std_cols]
      extra_cols <- vector()
      if (!missing(variable)) {
        if (length(variable) == 1 && variable == "all") {
          extra_cols <- in_cols[!in_cols %in% std_cols]
        } else {
          extra_cols <- lapply(variable, function (x) in_cols[grep(paste("^", x, sep = ""), in_cols)])
          extra_cols <- unlist(extra_cols)
          extra_cols <- extra_cols[!extra_cols %in% std_cols]
        }
      }
      cols <- c(cols, extra_cols)
      tmp <- tmp[, cols]
      
      # TIPO DE DADOS
      # if (table == "observacao") {
        ## 'observacao_id', 'sisb_id' e 'ibge_id' precisam estar no formato de caracter para evitar erros
        ## durante o empilhamento das tabelas devido ao tipo de dado.
        ## Nota: esse processamento deve ser feito via Google Sheets.
        # tmp$observacao_id <- as.character(tmp$observacao_id)
        # if ("sisb_id" %in% cols) {
        #   tmp$sisb_id <- as.character(tmp$sisb_id)
        # }
        # if ("ibge_id" %in% cols) {
        #   tmp$ibge_id <- as.character(tmp$ibge_id)
        # }
        # 'coord_precisao' precisa estar no formato numérico ao invés de inteiro
        # if ("coord_precisao" %in% colnames(tmp)) {
          # tmp$coord_precisao <- as.numeric(tmp$coord_precisao)
        # }
      # } else if (table == "camada") {
        ## "observacao_id", "camada_id", "camada_nome", "amostra_id", "profund_sup" e "profund_inf"
        ## precisam estar no formato de carácter para evitar erros durante o empilhamento das tabelas
        ## devido ao tipo de dado.
        ## Nota: esse processamento deve ser feito via Google Sheets.
        # tmp[std_cols] <- sapply(tmp[std_cols], as.character)
      # }
      
      # IDENTIFICAÇÃO
      ## Código de identificação do conjunto de dados
      res[[i]] <- cbind(dataset_id = as.character(sheets_keys$ctb[i]), tmp, stringsAsFactors = FALSE)
      
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }
    
    # FINAL
    ## Empilhar tabelas
    if (stack) {
      res <- suppressWarnings(dplyr::bind_rows(res))
    } else if (n == 1) {
      res <- res[[1]]
    }
    
    return (res)
  }
