library(magrittr)

key <- '1jLQqkDjIT_N86mmRRbaAClU21UBbcmXJI7MiA8zg_Ig'
sheet_name <- 'camada'
headers <- 1
glue::glue("https://docs.google.com/spreadsheets/d/{key}/gviz/tq?tqx=out:csv&sheet={sheet_name}&headers={headers}&tq=where%20observacao_id%20!%3D%20'%23metadado%3E'") %>% 
  read.table(header = TRUE, sep = ',', dec = ',', check.names = FALSE, stringsAsFactors = FALSE, na.strings = c('NA', '-')) %>% 
  tibble::as_tibble() %>% 
  colnames()

#' https://stackoverflow.com/questions/33713084/download-link-for-google-spreadsheets-csv-export-with-multiple-sheets
#' https://developers.google.com/chart/interactive/docs/spreadsheets
#' https://developers.google.com/chart/interactive/docs/querylanguage
#' https://developers.google.com/chart/interactive/docs/dev/implementing_data_source
###############################################################################################################
readGoogleSheet <- 
  function (sheet.key, sheet.name, sheet.query, sheet.headers = 1, sheet.out = 'csv', sep = ',', ...) {
    
    if (!missing(sheet.query)) {
      sheet.query <- paste("&tq=", utils::URLencode(sheet.query, reserved = TRUE), sep = "")
    }
    https_call <-
      paste(
        "https://docs.google.com/spreadsheets/d/", sheet.key, "/gviz/tq?tqx=", 
        "out:", sheet.out, 
        "&sheet=", sheet.name, 
        "&headers=", as.integer(sheet.headers),
        sheet.query, 
        sep = "")
    res <- utils::read.table(file = https_call, sep = sep, ...)
    return (res)
  }
readGoogleSheet(
  sheet.key = key, sheet.name = "camada", sheet.query = "select * where A <> '#metadado>'",
  stringsAsFactors = FALSE, dec = ',', na.strings = c("NA", "-", "", "na", "tr", "#VALUE!"), 
  header = TRUE) %>%
  colnames()

