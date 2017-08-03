# Conversion of units #########################################################################################
conversions <-
  function (source, target) {

    # Fatores de conversão entre unidades
    # Conversão de massa-volume para massa-massa é feita assumindo uma densidade da alíquota de solo usada
    # para as análises igual a 1.00 g/cm^3.
    switch(
      target,
      "mg/kg" = {
        res <- data.frame(
          source = c("g/kg", "%",   "mg/dm^3"),
          factor = c(1000,   10000, 1)
        )
        res <- res$factor[res$source %in% source]
      },
      "g/kg" = {
        res <- data.frame(
          source = c("mg/kg", "%", "mg/dm^3"),
          factor = c(1/1000,  10,  1/1000)
        )
        res <- res$factor[res$source %in% source]
      }
    )

    res <- data.frame(source, target, factor = res)
    return (res)
  }
