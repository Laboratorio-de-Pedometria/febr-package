.opt <- 
  function () {
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
          c("observacao_id", "camada_numero", "camada_nome", "amostra_codigo",
            "profund_sup", "profund_inf")
      ),
      gs = list(
        comment = "#unidade",
        locale = readr::locale(date_names = "pt", decimal_mark = ","),
        na = c("NA", "-", "", "na"),
        verbose = FALSE
      ),
      crs = 
        paste("EPSG:", c(
          # Córrego Alegre
          4225, 22521, 22522, 22523, 22524, 22525,
          # SAD69
          4618 , 29168, 29188, 29169, 29189, 29170, 29190, 29191, 29192, 29193, 29194, 29195,
          # WGS 84
          4326, 32618, 32718, 32619, 32719, 32620, 32720, 32721, 32722, 32723, 32724, 32725,
          # SIRGAS 2000
          4674, 31972, 31978, 31973, 31979, 31974, 31980, 31981, 31982, 31983, 31984, 31985
        ), sep = "")
    )
  }
# Empilhar tabelas ----
.stackTables <-
  function (obj) {
    
    # Organizar unidades de medida
    stack_unit <- lapply(obj, function (x) do.call(rbind, attributes(x)[c("names", "units")]))
    stack_unit <- do.call(cbind, stack_unit)
    stack_unit <- stack_unit[, !duplicated(stack_unit["names", ])]
    
    # Empilhar tabelas
    res <- suppressWarnings(dplyr::bind_rows(obj))
    
    # Definir novos atributos
    a <- attributes(res)
    a$units <- stack_unit["units", ][match(stack_unit["names", ], colnames(res))]
    attributes(res) <- a
    
    # Resultado
    return (res)
  }
# Transformação do sistema de referência de coordenadas ----
.crsTransform <- 
  function (obj, crs, xy = c("coord_x", "coord_y")) {
    
    crs_lower <- tolower(crs)
    crs_upper <- toupper(crs)
    
    # Registrar ordem das colunas
    col_names <- colnames(obj)
    
    ## Identificar as observações com coordenadas
    id_coords <- which(apply(obj[xy], 1, function (x) sum(is.na(x))) == 0)
    tmp_obj <- obj[id_coords, ]
    
    ## Verificar se o SRC está faltando
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
      tmp_obj[j] <- lapply(tmp_obj[j], function (x) {
        sp::coordinates(x) <- c("coord_x", "coord_y")
        sp::proj4string(x) <- sp::CRS(paste("+init=", tolower(x$coord_sistema[1]), sep = ""))
        x <- sp::spTransform(x, sp::CRS(paste("+init=", crs_lower, sep = "")))
        as.data.frame(x)
      })
      tmp_obj <- suppressWarnings(dplyr::bind_rows(tmp_obj))
      tmp_obj$coord_sistema <- crs_upper
      
    } else if (tmp_obj$coord_sistema[1] != crs_upper) {
      
      ## Transformar o SRC
      sp::coordinates(tmp_obj) <- xy
      sp::proj4string(tmp_obj) <- sp::CRS(paste("+init=", tolower(tmp_obj$coord_sistema[1]), sep = ""))
      tmp_obj <- sp::spTransform(tmp_obj, sp::CRS(paste("+init=", crs_lower, sep = "")))
      tmp_obj <- as.data.frame(tmp_obj)
      tmp_obj$coord_sistema <- crs_upper
    }
    
    ## Agrupar observações com e sem coordenadas
    ## Em seguida, organizar as colunas na ordem original de entrada
    res <- rbind(tmp_obj, obj[-id_coords, ])
    res <- dplyr::select(res, col_names)
    
    return (res)
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
# Formatar data de observação ----
.formatObservationDate <- 
  function (obj, time.format) {
    
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
    
    return (obj)
  }
# Eliminação de linhas sem dados nas tabelas 'camada' e 'observacao' ----
.cleanRows <-
  function (obj, missing, extra_cols) {
    
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
          na_coord_id <- apply(obj[c("coord_x", "coord_y")], 1, function (x) sum(is.na(x))) >= 1
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
            na_depth_id <- apply(obj[c("profund_sup", "profund_inf")], 1, function (x) sum(is.na(x))) >= 1
            obj <- obj[!na_depth_id, ]
          }
        } 
      }
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
    res[1, ] <- gsub("#unidade", "-", res[1, ])
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
