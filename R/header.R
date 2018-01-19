#' Get table header
#'
#' Download header data (column names and measurement units) from table(s) containing layer or 
#' observation-specific data. This is useful to check what are the variables contained in a dataset before
#' downloading it via \code{\link[febr]{layers}} or \code{\link[febr]{observations}}.
#'
#' @template data_template
#' @template metadata_template
#' 
#' @param table Table from which header data should be downloaded, with options \code{"camada"} 
#' (layer-specific header data) and \code{"observacao"} (observation-specific header data).
#' 
#' @return A list or data.frame with table header data (column names and measurement units) of the chosen 
#' dataset(s).
#'
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' @seealso \code{\link[febr]{layers}}, \code{\link[febr]{observations}}
#' @export
#' @examples
#' \dontrun{
#' res <- header(dataset = "all", table = "camada", variable = "ferro", stack = TRUE)
#' id <- grep("ferro_", colnames(res))
#' col <- colnames(res)[id]
#' col[order(col)]
#' }
header <-
  function (dataset, table, variable, stack = FALSE, progress = TRUE, verbose = TRUE) {
    
    # Variáveis padrão
    if (table == "observacao") {
      std_cols <- .opt()[["observations"]]$std.cols
    } else if (table == "camada") {
      std_cols <- .opt()[["layers"]]$std.cols
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
      
      # Informative messages
      dts <- sheets_keys$ctb[i]
      if (verbose) {
        par <- ifelse(progress, "\n", "")
        message(paste(par, "Downloading dataset ", dts, "...", sep = ""))
      }
      
      # DESCARREGAMENTO
      ## Cabeçalho com unidades de medida
      tmp <- .getHeader(x = sheets_keys[[table]][i])
      
      # COLUNAS
      ## Definir as colunas a serem mantidas
      ## As colunas padrão são sempre mantidas.
      in_cols <- colnames(tmp)
      cols <- in_cols[in_cols %in% std_cols]
      extra_cols <- vector()
      if (!missing(variable)) {
        if (variable == "all") {
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
      if (table == "observacao") {
        ## 'observacao_id', 'sisb_id' e 'ibge_id' precisam estar no formato de caracter para evitar erros
        ## durante o empilhamento das tabelas devido ao tipo de dado.
        ## Nota: esse processamento deve ser feito via Google Sheets.
        tmp$observacao_id <- as.character(tmp$observacao_id)
        if ("sisb_id" %in% cols) {
          tmp$sisb_id <- as.character(tmp$sisb_id)
        }
        if ("ibge_id" %in% cols) {
          tmp$ibge_id <- as.character(tmp$ibge_id)
        }
        # 'coord_precisao' precisa estar no formato numérico ao invés de inteiro
        if ("coord_precisao" %in% colnames(tmp)) {
          tmp$coord_precisao <- as.numeric(tmp$coord_precisao)
        }
      } else if (table == "camada") {
        ## "observacao_id", "camada_numero", "camada_nome", "amostra_codigo", "profund_sup" e "profund_inf"
        ## precisam estar no formato de carácter para evitar erros durante o empilhamento das tabelas
        ## devido ao tipo de dado.
        ## Nota: esse processamento deve ser feito via Google Sheets.
        tmp[std_cols] <- sapply(tmp[std_cols], as.character)
      }
      
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
