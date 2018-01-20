.opt <- 
  function () {
    list(
      observations = list(
        std.cols =
          c("observacao_id", "sisb_id", "ibge_id", "observacao_data", 
            "coord_sistema", "coord_x", "coord_y", "coord_precisao", "coord_fonte", 
            "pais_id", "estado_id", "municipio_id", 
            "amostra_tipo", "amostra_quanti", "amostra_area")
        ),
      layers = list(
        std.cols = 
          c("observacao_id", "camada_numero", "camada_nome", "amostra_codigo",
            "profund_sup", "profund_inf")
      ),
      gs = list(
        comment = "#unidade",
        locale = readr::locale(date_names = "pt", decimal_mark = ","),
        na = c("NA", "-", ""),
        verbose = FALSE
        
      )
    )
  }
# Harmonização baseada nos níveis dos códigos de identificação ----
.harmonizeByName <-
  function (obj, extra_cols, harmonization) {
    
    # Alterar nomes das colunas
    new_colnames <- stringr::str_split_fixed(string = extra_cols, pattern = "_", n = Inf)
    n_new_colnames <- seq(min(harmonization$level, ncol(new_colnames)))
    new_colnames <- matrix(new_colnames[, n_new_colnames], nrow = nrow(new_colnames))
    
    # if (n_new_colnames > 1) {
      new_colnames <- 
        apply(new_colnames, 1, function (x) paste(x[!x == ""], collapse = "_", sep = ""))
    # }
    
    # No caso de nomes idênticos, manter o nome original
    if (any(duplicated(new_colnames))) {
      idx <- c(which(duplicated(new_colnames)), which(duplicated(new_colnames, fromLast = TRUE)))
      new_colnames[idx] <- extra_cols[idx]
    }
    
    # Definir novos nomes das colunas
    colnames(obj)[colnames(obj) %in% extra_cols] <- new_colnames
    return (obj)
  }
# Eliminação de linhas sem dados nas tabelas 'camada' e 'observacao' ----
.cleanRows <-
  function (obj, missing, extra_cols) {
    
    # Remover linhas sem dados em uma ou mais colunas
    if (length(extra_cols) >= 1 && missing$data == "drop") {
      idx_keep <- is.na(obj[extra_cols])
      idx_keep <- rowSums(idx_keep) < ncol(idx_keep)
      obj <- obj[idx_keep, ]
    }
    
    # Tabela 'observacao': remover linhas sem dados de coordenadas
    if (!is.null(missing$coord) && missing$coord == "drop") {
      na_coord_id <- apply(obj[c("coord_x", "coord_y")], 1, function (x) sum(is.na(x))) >= 1
      obj <- obj[!na_coord_id, ]
    }
    
    # Tabela 'camada': remover linhas sem dados de profundidade
    if (!is.null(missing$depth) && missing$depth == "drop") {
      na_depth_id <- apply(obj[c("profund_sup", "profund_inf")], 1, function (x) sum(is.na(x))) >= 1
      obj <- obj[!na_depth_id, ]
    }
    
    return (obj)
  }

# Descarregar cabeçalho das tabelas 'camada' e observacao' ----
.getHeader <- 
  function (x) {
    res <- googlesheets::gs_key(x = x, verbose = .opt()$gs$verbose)
    res <- suppressMessages(
      googlesheets::gs_read_csv(
        ss = res, locale = .opt()$gs$locale, verbose = .opt()$gs$verbose, n_max = 1))
    res <- as.data.frame(res)
    return (res)
  }

# Descarregar tabela 'camada' e 'observacao' ----
.getTable <-
  function (x) {
    res <- googlesheets::gs_key(x = x, verbose = .opt()$gs$verbose)
    res <- suppressMessages(
      googlesheets::gs_read_csv(
        res, na = .opt()$gs$na, locale = .opt()$gs$locale, verbose = .opt()$gs$verbose, 
        comment = .opt()$gs$comment))
    res <- as.data.frame(res)
    return (res)
  }

# Which datasets should be downloaded? ----
.getDataset <-
  function (sheets_keys, dataset) {
    
    if ("all" %in% dataset) {
      # Some datasets are not ready for download
      sheets_keys <- sheets_keys[!is.na(sheets_keys$camada), ]
      
    } else {
      idx_out <- which(!dataset %in% sheets_keys$ctb)
      if (length(idx_out) >= 1) {
        stop (paste("Unknown value '", dataset[idx_out], "' passed to parameter dataset", sep = ""))
      }
      sheets_keys <- sheets_keys[sheets_keys$ctb %in% dataset, ]
      
      # Some datasets might not be ready for download
      idx_na <- which(is.na(sheets_keys$camada))
      if (length(idx_na) >= 1) {
        stop (paste("Cannot download dataset '", dataset[idx_na], "'. See https://goo.gl/tVC8dH", sep = ""))
      }
    }
    return (sheets_keys)
  }
# Download sheets keys ----
.getSheetsKeys <- 
  function (key = "18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q", dataset) {
    
    # Options
    opts <- .opt()
    
    sheets_keys <- googlesheets::gs_key(x = key, verbose = opts$gs$verbose)
    sheets_keys <- suppressMessages(
      googlesheets::gs_read(sheets_keys, na = opts$gs$na, verbose = opts$gs$verbose))
    sheets_keys <- .getDataset(sheets_keys = sheets_keys, dataset = dataset)
    
    # Descarregar chaves de identificação das planilhas do repositório
    return (sheets_keys)
  }
