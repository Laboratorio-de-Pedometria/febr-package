key <- '1l_ag2vVnEjKSUYyWsy89a6LYCjqQN8QKGolZcoms4NY'
tb <- "observacao"
sheet <-
  .readGoogleSheet(
    sheet.id = key,
    # return = 'https.request',
    sheet.name = tb, sheet.headers = 3,
    # sheet.query = "select * where A != '#metadado>'",
    stringsAsFactors = FALSE, dec = ',', header = TRUE,
    na.strings = c("NA", "-", "", "na", "tr", "#VALUE!"))
colnames(sheet) <- sapply(colnames(sheet), function (x) strsplit(x, split = '.', fixed = TRUE)[[1]][1])
sheet1 <- .getTable(x = key, ws = tb)
cbind(new = sapply(sheet, class), old = sapply(sheet1, class))

lapply(list(sheet, sheet1), head)
