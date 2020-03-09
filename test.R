key <- '1U1RlF6-2FuCpuVgZtcOkfX9qM3nfmzjhSLQXhnSPJFg'
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
