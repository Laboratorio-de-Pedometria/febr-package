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
            "profund_sup", "profund_inf"),
        soil.vars = c(
          # Variáveis químicas
          # c(alumínio = 'al', cálcio = 'ca', carbono = 'c', 
            ferro = 'fe'
            # magnésio = 'mg', potássio = 'k', sódio = 'na'),
          # Variáveis físicas
          # c(areia = 'areia', argila = 'argila', densidade = 'densidade')
        )
      ),
      gs = list(
        comment = "#unidade",
        locale = readr::locale(decimal_mark = ","),
        na = c("NA", "-", ""),
        verbose = FALSE
        
      )
    )
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
