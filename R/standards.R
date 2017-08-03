# Definir unidade e número de casas decimais para cada tipo de dado de ferro ##################################
standards <-
  function (x = "fe", code) {

    switch(
      x,
      "fe" = {
        # Segundo a terceira edição do Manual de Métodos de Análise de Solo (Donagemma et al. 2011), as
        # seguintes convenções devem ser aplicadas:
        # - Ferro no extrato sulfúrico: g/kg, com 1 casa decimal
        # - Ferro livre: g/kg, com 2 casas decimais
        # - Microelementos: mg/kg, sem casa decimal
        # - Ataque triácido: g/kg, com 1 casa decimal
        res <- matrix(
          c("aquaregia",   "g/kg",  1, "Ácido clorídrico + Ácido nítrico",
            "cloridrico",  "g/kg",  1, "Ácido clorídrico",
            "ditionito",   "g/kg",  2, "Citrato-ditionito-bicarbonato",
            "dtpa",        "mg/kg", 0, "DTPA",
            "mehlich",     "mg/kg", 0, "Mehlich",
            "oxalato",     "g/kg",  2, "Oxalato ácido de amônio",
            "pirofosfato", "g/kg",  2, "Pirofosfato de sódio",
            "rggh",        "-",     3, "Razão goethita/(goethita+hematita)",
            "sulfurico",   "g/kg",  1, "Ácido sulfúrico",
            "triacido",    "g/kg",  1, "Ácido perclórico + Ácido nítrico + Ácido fluorídrico"),
          ncol = 4, byrow = TRUE
        )
        res <- as.data.frame(res, stringsAsFactors = FALSE)
        colnames(res) <- c("code", "unit", "digits", "description")
        res$digits <- as.numeric(res$digits)
      }
    )

    if (!missing(code)) {
      res <- res[res$code %in% code, ]
    }

    return (res)
  }