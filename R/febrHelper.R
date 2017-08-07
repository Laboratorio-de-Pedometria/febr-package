.opt <- 
  function () {
    list(
      layers = list(
        id.cols = 
          c("observacao_id", "camada_numero", "camada_nome", "amostra_codigo"),
        depth.cols = c("profund_sup", "profund_inf")
      ),
      gs = list(
        comment = "unidade",
        locale = readr::locale(decimal_mark = ","),
        na = c("NA", "-", ""),
        verbose = FALSE
        
      )
    )
  }

