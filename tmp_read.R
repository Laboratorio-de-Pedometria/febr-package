library(magrittr)
# https://stackoverflow.com/questions/33713084/download-link-for-google-spreadsheets-csv-export-with-multiple-sheets

key <- '1jLQqkDjIT_N86mmRRbaAClU21UBbcmXJI7MiA8zg_Ig'
sheet_name <- 'camada'
headers <- 1
glue::glue("https://docs.google.com/spreadsheets/d/{key}/gviz/tq?tqx=out:csv&sheet={sheet_name}&headers={headers}?tq=where%20observacao_id%20!%3D%20'%23metadado%3E'") %>% 
  read.table(header = TRUE, sep = ',', dec = ',', check.names = FALSE, stringsAsFactors = FALSE, na.strings = c('NA', '-')) %>% 
  tibble::as_tibble() %>% 
  colnames()

