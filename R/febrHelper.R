.opt <-
  function() {
    list(
      observation = list(
        std.cols =
          c("observacao_id", "sisb_id", "ibge_id", "observacao_data",
            "coord_sistema", "coord_x", "coord_y", "coord_precisao", "coord_fonte",
            "pais_id", "estado_id", "municipio_id",
            "amostra_tipo", "amostra_quanti", "amostra_area")
      ),
      layer = list(
        std.cols =
          c("observacao_id", "camada_id", "camada_nome", "amostra_id", "profund_sup", "profund_inf")
      ),
      gs = list(
        comment = "#metadado>",
        # locale = readr::locale(date_names = "pt", decimal_mark = ","),
        na = c("NA", "-", "", "na", "tr", "#VALUE!", "#N/A"),
        verbose = FALSE
      ),
      crs =
        paste("EPSG:", c(
          # Córrego Alegre
          4225, 22521, 22522, 22523, 22524, 22525,
          # SAD69
          4618, 29168, 29188, 29169, 29189, 29170, 29190, 29191, 29192, 29193, 29194, 29195,
          # WGS 84
          4326, 32618, 32718, 32619, 32719, 32620, 32720, 32721, 32722, 32723, 32724, 32725,
          # SIRGAS 2000
          4674, 31972, 31978, 31973, 31979, 31974, 31980, 31981, 31982, 31983, 31984, 31985
        ), sep = "")
    )
  }
# Descarregar e ler arquivo do onwCloud ############################################################
# O repositório de leitura é remoto ou local?
.readOwnCloud <-
  function(ctb, table, febr.repo, ...) {
    if (febr.repo == "remote") {
      url <- paste0(
        'https://cloud.utfpr.edu.br/index.php/s/Df6dhfzYJ1DDeso/download?path=%2F', ctb, '&files=',
        ctb, '-', table, '.txt')
    } else {
      if (!grepl('./$', febr.repo)) {
        febr.repo <- paste(febr.repo, "/", sep = "")
      }
      url <- paste(path.expand(febr.repo), ctb, '/', ctb, '-', table, '.txt', sep = '')
    }
    res <- utils::read.table(
      file = url, header = TRUE, dec = ",", na.strings = .opt()$gs$na, stringsAsFactors = FALSE,
      ...)
    return(res)
  }
# Empilhar tabelas ----
.stackTables <-
  function(obj) {
    
    # Organizar unidades de medida
    # stack_unit <- lapply(obj, function(x) do.call(rbind, attributes(x)[c("names", "units")]))
    stack_unit <- 
      lapply(obj, function(x) do.call(rbind, attributes(x)[c("names", "field_name", "field_unit")]))
    stack_unit <- do.call(cbind, stack_unit)
    stack_unit <- stack_unit[, !duplicated(stack_unit["names", ])]
    
    # Empilhar tabelas
    res <- suppressWarnings(dplyr::bind_rows(obj))
    
    # Definir novos atributos
    a <- attributes(res)
    # a$units <- stack_unit["units", ][match(stack_unit["names", ], colnames(res))]
    a$field_unit <- stack_unit["field_unit", ][match(stack_unit["names", ], colnames(res))]
    a$field_name <- stack_unit["field_name", ][match(stack_unit["names", ], colnames(res))]
    attributes(res) <- a
    
    # Resultado
    return(res)
  }
# Transformação do sistema de referência de coordenadas -------------------------------------------------------
# 2020-03-08: substitui 'sp' por 'sf'; substitui 'dplyr' por 'base'
.getEPSGcode <- 
  function(x) {
    as.integer(gsub(pattern = 'EPSG:', replacement = '', x = x))
  }
.crsTransform <- 
  function(obj, crs, xy = c("coord_x", "coord_y")) {
    
    # crs_lower <- tolower(crs)
    crs_upper <- toupper(crs)
    
    # Registrar ordem das colunas
    col_names <- colnames(obj)
    
    ## Identificar as observações com coordenadas
    id_coords <- which(apply(obj[xy], 1, function(x) sum(is.na(x))) == 0)
    tmp_obj <- obj[id_coords, ]
    
    ## Verificar se o SRC está faltando
    ## Caso esteja faltando, e as coordendas forem geográficas, então atribui-se o SRC usado como padrão
    is_na_crs <- is.na(tmp_obj$coord_sistema)
    if (any(is_na_crs)) {
      is_degree <- nchar(round(abs(tmp_obj$coord_x))) <= 2
      is_na_crs <- which(is_na_crs[is_degree])
      tmp_obj$coord_sistema[is_na_crs] <- crs
    }
    
    ## Verificar se o SRC é o SAD69
    ## Nota: Isso deve ser feito no Google Sheets
    is_sad69 <- tmp_obj$coord_sistema %in% "SAD69"
    if (any(is_sad69)) {
      tmp_obj$coord_sistema[is_sad69] <- "EPSG:4618" 
    }
    
    ## Verificar se o SRC é o SIRGAS
    ## Nota: Isso deve ser feito no Google Sheets
    is_sirgas <- tmp_obj$coord_sistema %in% "SIRGAS"
    if (any(is_sirgas)) {
      tmp_obj$coord_sistema[is_sirgas] <- crs 
    }
    
    ## Verificar quantos são os SRC usados
    n_crs <- nlevels(as.factor(tmp_obj$coord_sistema))
    
    if (n_crs > 1) {
      tmp_obj <- split(tmp_obj, as.factor(tmp_obj$coord_sistema))
      
      ## Verificar se algum dos SRC é igual ao alvo
      if (crs_upper %in% names(tmp_obj)) {
        j <- which(!names(tmp_obj) %in% crs_upper)
      } else {
        j <- 1:n_crs
      }
      
      ## Transformar os SRC
      tmp_obj[j] <- lapply(tmp_obj[j], function(x) {
        x <- sf::st_as_sf(x = x, coords = xy, crs = .getEPSGcode(x$coord_sistema[1]))
        x <- sf::st_transform(x = x, crs = .getEPSGcode(crs))
        x_coords <- sf::st_coordinates(x = x)
        colnames(x_coords) <- xy
        x <- cbind(x_coords, sf::st_drop_geometry(x))
      })
      tmp_obj <- do.call(rbind, tmp_obj)
      tmp_obj$coord_sistema <- crs_upper
      
    } else if (tmp_obj$coord_sistema[1] != crs_upper) {
      
      ## Transformar o SRC
      tmp_obj <- sf::st_as_sf(x = tmp_obj, coords = xy, crs = .getEPSGcode(tmp_obj$coord_sistema[1]))
      tmp_obj <- sf::st_transform(crs = .getEPSGcode(crs), x = tmp_obj)
      tmp_obj_coords <- sf::st_coordinates(x = tmp_obj)
      colnames(tmp_obj_coords) <- xy
      tmp_obj <- cbind(tmp_obj_coords, sf::st_drop_geometry(tmp_obj))
      tmp_obj$coord_sistema <- crs_upper
    }
    
    ## 1. Agrupar observações com e sem coordenadas
    ## 2. Organizar as colunas na ordem original de entrada
    ## 3. Ordenar as linhas em função de 'observacao_id'
    res <- rbind(tmp_obj, obj[-id_coords, ])
    res <- res[, col_names]
    res <- res[order(res$observacao_id), ]
    
    return(res)
  }
# Harmonização baseada nos níveis dos códigos de identificação ----
.harmonizeByName <-
  function(obj, extra_cols, harmonization) {
    
    # Alterar nomes das colunas
    new_colnames <- stringr::str_split_fixed(string = extra_cols, pattern = "_", n = Inf)
    n_new_colnames <- seq(min(harmonization$level, ncol(new_colnames)))
    new_colnames <- matrix(new_colnames[, n_new_colnames], nrow = nrow(new_colnames))
    
    # if (n_new_colnames > 1) {
    new_colnames <- 
      apply(new_colnames, 1, function(x) paste(x[!x == ""], collapse = "_", sep = ""))
    # }
    
    # No caso de nomes idênticos, manter o nome original
    if (any(duplicated(new_colnames))) {
      idx <- c(which(duplicated(new_colnames)), which(duplicated(new_colnames, fromLast = TRUE)))
      new_colnames[idx] <- extra_cols[idx]
    }
    
    # Definir novos nomes das colunas
    colnames(obj)[colnames(obj) %in% extra_cols] <- new_colnames
    return(obj)
  }
# Formatar data de observação ----
.formatObservationDate <- 
  function(obj, time.format) {
    
    # Identificar formatação da data
    time_sep <- ifelse(all(grepl("/", stats::na.omit(obj$observacao_data))), "/", "-")
    time_form0 <- glue::glue("%d{time_sep}%m{time_sep}%Y")
    
    # Verificar se falta data para alguma observação
    time_miss <- grepl("xx", obj$observacao_data)
    if (any(time_miss)) {
      
      ## Falta dia
      miss_day <- grepl(glue::glue("^xx{time_sep}"), obj$observacao_data)
      if (any(miss_day)) {
        obj$observacao_data[miss_day] <-
          gsub(pattern = glue::glue("^xx{time_sep}"),
               replacement = glue::glue("{format(Sys.Date(), '%d')}{time_sep}"),
               x = obj$observacao_data[miss_day])
      }
      
      # Falta mês
      miss_month <- grepl(glue::glue("{time_sep}xx{time_sep}"), obj$observacao_data)
      if (any(miss_month)) {
        obj$observacao_data[miss_month] <-
          gsub(pattern = glue::glue("{time_sep}xx{time_sep}"),
               replacement = glue::glue("{time_sep}{format(Sys.Date(), '%m')}{time_sep}"),
               x = obj$observacao_data[miss_month])
      }
    }
    
    # Formatar data
    obj$observacao_data <- as.Date(x = obj$observacao_data, format = time_form0)
    obj$observacao_data <- as.Date(x = obj$observacao_data, format = time.format)
    
    return(obj)
  }
# Eliminação de linhas sem dados nas tabelas 'camada' e 'observacao' ----
.cleanRows <-
  function(obj, missing, extra_cols) {
    
    # Remover linhas sem dados em uma ou mais colunas
    if (length(extra_cols) >= 1 && missing$data == "drop") {
      idx_keep <- is.na(obj[extra_cols])
      # idx_keep <- rowSums(idx_keep) < ncol(idx_keep)
      idx_keep <- rowSums(idx_keep) == 0
      obj <- obj[idx_keep, ]
    }
    
    # Continuar processamento apenas se restaram linhas
    if (nrow(obj) >= 1) {
      # Tabela 'observacao'
      is_obs <- all(c("coord_x", "coord_y", "observacao_data") %in% colnames(obj))
      if (is_obs) {
        
        ## remover linhas sem dados de coordenadas
        if (!is.null(missing$coord) && missing$coord == "drop") {
          na_coord_id <- apply(obj[c("coord_x", "coord_y")], 1, function(x) sum(is.na(x))) >= 1
          obj <- obj[!na_coord_id, ]
        }
        
        ## remover linhas sem dados de data de observação
        if (!is.null(missing$time) && missing$time == "drop") {
          na_time_id <- is.na(obj$observacao_data)
          obj <- obj[!na_time_id, ]
        }
      }
      
      # Continuar processamento apenas se restaram linhas
      if (nrow(obj) >= 1) {
        
        # Tabela 'camada'
        is_lyr <- all(c("profund_sup", "profund_inf") %in% colnames(obj))
        if (is_lyr) {
          
          ## remover linhas sem dados de profundidade
          if (!is.null(missing$depth) && missing$depth == "drop") {
            na_depth_id <- apply(obj[c("profund_sup", "profund_inf")], 1, function(x) sum(is.na(x))) >= 1
            obj <- obj[!na_depth_id, ]
          }
        } 
      }
    }
    
    return(obj)
  }
# Descarregar e ler planilha do Google Sheets ######################################################
.readGoogleSheetCSV <-
  function(sheet.id, sheet.name) {
    if (sheet.name == "unidades") {
      sheet.id <- "1tU4Me3NJqk4NH2z0jvMryGObSSQLCvGqdLEL5bvOflo"
    }
    # descarregar planilha
    url <- paste0("https://docs.google.com/spreadsheets/d/", sheet.id, "/export?format=csv")
    destfile <- tempfile(pattern = sheet.id, tmpdir = tempdir(), fileext = ".csv")
    utils::download.file(url = url, destfile = destfile, quiet = TRUE)
    # ler planilha
    if (sheet.name %in% c("observacao", "camada")) { # observacao e camada
      res <- list(header = NA, table = NA)
      res[["header"]] <- utils::read.table(
        file = destfile,
        header = TRUE,
        sep = ",",
        dec = ",",
        comment.char = "",
        nrows = 2,
        na.strings = .opt()$gs$na[.opt()$gs$na != "-"], # cannot evaluate '-' as NA
        stringsAsFactors = FALSE
      )
      res[["table"]] <- utils::read.table(
        file = destfile,
        header = FALSE,
        sep = ",",
        dec = ",",
        comment.char = "",
        skip = 3,
        na.strings = .opt()$gs$na,
        stringsAsFactors = FALSE
      )
      colnames(res[["table"]]) <- colnames(res[["header"]])
    } else if (sheet.name %in% c("dataset", "metadado")) { # dataset e metadado
      res <- utils::read.table(
        file = destfile,
        header = TRUE,
        sep = ",",
        dec = ",",
        comment.char = "",
        na.strings = .opt()$gs$na,
        stringsAsFactors = FALSE
      )
    } else if (sheet.name == "unidades") { # febr-unidades
      res <- utils::read.table(
        file = destfile,
        header = TRUE,
        sep = ",",
        dec = ",",
        comment.char = "",
        na.strings = .opt()$gs$na[.opt()$gs$na != "-"], # cannot evaluate '-' as NA
        stringsAsFactors = FALSE
      )
    }
    # saída ---
    return(res)
  }
# Descarregar cabeçalho das tabelas 'camada' e observacao' #########################################
.getHeader <-
  function(x, ws) {
    url <- paste('https://docs.google.com/spreadsheets/d/', x, '/export?format=csv', sep ='')
    destfile <- tempfile(pattern = x, tmpdir = tempdir(), fileext = '.csv')
    utils::download.file(url = url, destfile = destfile, quiet = TRUE)
    res <- utils::read.table(
      file = destfile, header = TRUE, sep = ',', dec = ',', nrows = 2, comment.char = '',
      na.strings = 'NA', stringsAsFactors = FALSE)
    # output ---
    return(res)
  }
# Download FEBR dictionary #########################################################################
.getStds <-
  function(x = "1Dalqi5JbW4fg9oNkXw5TykZTA39pR5GezapVeV0lJZI", ws) {
    # O símbolo '-' é usado para indicar variáveis que não possuem unidade de medida. Portanto, não
    # pode ser lido como NA. Na prática, '-' é lido como uma unidade de medida. Do contrário, não é
    # possível realizar a padronização das unidades de medida quando descarregamos variáveis sem
    # unidades de medida. Contudo, no campo 'campo_precisao', '-' significa NA.
    na <- .opt()$gs$na
    na <- na[-which(na == "-")]
    url <- paste0("https://docs.google.com/spreadsheets/d/", x, "/export?format=csv")
    # destfile <- tempfile(pattern = x, tmpdir = tempdir(), fileext = ".csv")
    # utils::download.file(url = url, destfile = destfile, quiet = TRUE)
    res <- utils::read.table(
      # file = destfile,
      file = url,
      header = TRUE, sep = ",", dec = ",", comment.char = "", na.strings = na,
      stringsAsFactors = FALSE)
    # saída ---
    res[["campo_precisao"]] <- gsub(pattern = "-", NA, res[["campo_precisao"]])
    res[["campo_precisao"]] <- suppressWarnings(as.numeric(res[["campo_precisao"]]))
    return(res)
  }
# Quais conjuntos de dados devem ser descarregados? ###########################################################
.getDataset <-
  function(sheets_keys, dataset) {
    
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
    return(sheets_keys)
  }
# Descarregar tabela 'febr-chaves' ############################################################################
.getSheetsKeys <- 
  function(x = "18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q", dataset, engine = 'utils') {
    
    # motor de descarregamento e leitura ---
    switch(
      engine,
      gs = { # googlesheets ---
        # res <- googlesheets::gs_key(x = x, verbose = FALSE)
        # res <- suppressMessages(googlesheets::gs_read(res, na = '', verbose = FALSE))
      },
      gs4 = { # googlesheets4 ---
        # res <- suppressMessages(googlesheets4::read_sheet(ss = x, sheet = 'chaves'))
      },
      utils = { # utils ---
        url <- paste('https://docs.google.com/spreadsheets/d/', x, '/export?format=csv', sep ='')
        destfile <- tempfile(pattern = x, tmpdir = tempdir(), fileext = '.csv')
        utils::download.file(url = url, destfile = destfile, quiet = TRUE)
        res <- utils::read.table(
          file = destfile, header = TRUE, sep = ',', dec = ',', comment.char = '', 
          # na.strings = '',
          stringsAsFactors = FALSE)
      }
    )
    # saída ---
    res <- .getDataset(sheets_keys = res, dataset = dataset)
    res <- res[order(res$ctb), ]
    return(res)
  }
