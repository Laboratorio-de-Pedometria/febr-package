library(magrittr)
# https://stackoverflow.com/questions/33713084/download-link-for-google-spreadsheets-csv-export-with-multiple-sheets

key <- '1ewJgirY3h4c5X5DApGGfkb1z8XALKoBiSh8vKd5JZhA'
sheet_name <- 'camada'
sheet <- glue::glue('https://docs.google.com/spreadsheets/d/{key}/gviz/tq?tqx=out:csv&sheet={sheet_name}')
read.table(file = sheet, header = TRUE, sep = ',', dec = ',', check.names = FALSE, stringsAsFactors = FALSE) %>% 
  tibble::as_tibble() %>% 
  colnames()

