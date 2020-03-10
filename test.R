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

keys <- list('1FQpmLSeVsbVDeFEwBzLQO1j6_m_G5PZ58DRPr32fhAc', '1U1RlF6-2FuCpuVgZtcOkfX9qM3nfmzjhSLQXhnSPJFg',
             '1_sX8qx_zzHQhxaV3Bh6irU9ceFBuDY9O_ar68xp4aE4', '1jnY-USD_39eqXIMVuPXDEEaiaGlXxZimrYvmr_1nEi4',
             '1xJU9T5TjQRakVqbDKR35Bh22CMDkm3XCC9F9wbgZNus', '1tP0c-oStomBFqyXqJn1vUmE__w2rZd6e6zQftSfQCU8')
teste <- lapply(keys, function (x) .readGoogleSheet(
  sheet.id = x,
  # return = 'https.request',
  sheet.name = tb, sheet.headers = 3,
  # sheet.query = "select * where A != '#metadado>'",
  stringsAsFactors = FALSE, dec = ',', header = TRUE,
  na.strings = c("NA", "-", "", "na", "tr", "#VALUE!")))


###############################################################################################################
# Descarregamento do cabeçalho
key <- '1l_ag2vVnEjKSUYyWsy89a6LYCjqQN8QKGolZcoms4NY'
tb <- "observacao"
cab_old <- .getHeader(x = key, ws = tb)
cab_new <- .readGoogleSheet(return = 'https.request',
  sheet.id = key, sheet.name = tb, sheet.query = 'limit 2',
  sheet.headers = 1, stringsAsFactors = FALSE, 
  dec = ',', header = TRUE)

cab_old == cab_new

url <- 'https://docs.google.com/spreadsheets/d/1l_ag2vVnEjKSUYyWsy89a6LYCjqQN8QKGolZcoms4NY/export?format=csv&id=1l_ag2vVnEjKSUYyWsy89a6LYCjqQN8QKGolZcoms4NY&sheet=observacao'
download.file(url = url, destfile = 'tmp.csv')
utils::read.table(file = 'tmp.csv', nrows = 2, dec = ',', sep = ',', header = T, comment.char = '')

https://docs.google.com/spreadsheets/d/1l_ag2vVnEjKSUYyWsy89a6LYCjqQN8QKGolZcoms4NY/gviz/tq?tqx=out:csv&sheet=observacao&headers=1&tq=limit%202

a <- httr::GET('https://docs.google.com/spreadsheets/d/1l_ag2vVnEjKSUYyWsy89a6LYCjqQN8QKGolZcoms4NY/gviz/tq?tqx=out:csv&sheet=observacao&headers=1&tq=limit%202')
a <- a$content
a <- readBin(con = a, what = character())

