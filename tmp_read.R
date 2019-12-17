library(magrittr)
# https://stackoverflow.com/questions/33713084/download-link-for-google-spreadsheets-csv-export-with-multiple-sheets

key <- '1ewJgirY3h4c5X5DApGGfkb1z8XALKoBiSh8vKd5JZhA'
sheet_name <- 'camada'
glue::glue('https://docs.google.com/spreadsheets/d/{key}/gviz/tq?tqx=out:csv&sheet={sheet_name}&headers=3') %>% 
  read.table(header = TRUE, sep = ',', dec = ',', check.names = FALSE, stringsAsFactors = FALSE) %>% 
  tibble::as_tibble() %>% 
  colnames()

