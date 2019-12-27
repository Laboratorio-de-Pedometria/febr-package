#' Data input from a Google spreadsheet
#' 
#' Reads a Google spreadsheet and creates a data frame from it, with cases corresponding to lines and variables
#' to fields in the spreadsheet.
#' 
#' @param sheet.id character string; the ID of the Google spreadsheet holding the data. This ID is the value 
#' between the `/d/` and the `/edit` in the URL of the spreadsheet.
#' 
#' @param sheet.name (optional) character string; specifies which sheet in a multi-sheet document you are
#' linking to, if you are not linking to the first sheet. `sheet.name` is the display name of the sheet.
#' 
#' @param sheet.headers (optional) integer value; specifies how many rows are header rows, where 
#' `sheet.headers` is an integer zero or greater. These will be excluded from the data and assigned as column
#' labels in the data table. If you don't specify this argument, the spreadsheet will guess how many rows are
#' header rows. Note that if all your columns are string data, the spreadsheet might have difficulty 
#' determining which rows are header rows without this argument.
#' 
#' @param sheet.query (optional) character string; query string attached to a data source request. The syntax
#' of the query language is similar to SQL. If your request does not include a query string, the default
#' behavior for a data source is to return all rows and columns using its default row/column order and
#' formatting. You can change that by including a query string in your request to a data source. (Note that
#' column IDs in spreadsheets are always letters; the column heading text shown in the published spreadsheet
#' are labels, not IDs. You must use the ID, not the label, in your query string.)
#' 
#' @param return character string; specifies the funtion call output, with options `data.frame` and 
#' `https.request`. Defaults to `return = data.frame`.
#' 
#' @param ... Further arguments to be passed to \code{\link[utils]{read.table}}. (Note that the field separator
#' character is hard-coded as `sep = ','`.)
#' 
#' @return A data frame (if `return = 'data.frame'`) or a character string with the https request (if 
#' `return = 'https.request'`).
#' 
#' @author Alessandro Samuel-Rosa \email{alessandrosamuelrosa@@gmail.com}
#' 
#' @seealso \code{\link[utils]{read.table}}
#' 
#' @references 
#' Stack Overflow. Download link for Google Spreadsheets CSV export - with Multiple Sheets. https://stackoverflow.com/a/33727897/3365410
#' Google Charts. Google Spreadsheets. https://developers.google.com/chart/interactive/docs/spreadsheets
#' Google Charts. Query Language Reference (Version 0.7). https://developers.google.com/chart/interactive/docs/querylanguage
#' Google Charts. Implementing the Chart Tools Datasource Protocol (V0.6). https://developers.google.com/chart/interactive/docs/dev/implementing_data_source
###############################################################################################################
readGoogleSheet <- 
  function (
    sheet.id, sheet.name, sheet.headers, sheet.query, return = 'data.frame', ...) {
    
    # ARGUMENTS
    ## sheet.id
    if (missing(sheet.id)) {
      stop ("argument 'sheet.id' is missing")
    } else if (!is.character(sheet.id)) {
      stop (paste("object of class '", class(sheet.id), "' passed to argument 'sheet.id'"))
    }
    
    ## sheet.name (optional)
    if (!missing(sheet.name)) {
      if (!is.character(sheet.name)) {
        stop (paste("object of class '", class(sheet.name), "' passed to argument 'sheet.name'"))
      } else {
        sheet.name <- paste("&sheet=", sheet.name, sep = "")
      }
    } else {
      sheet.name <- ""
    }

    ## sheet.headers (optional)
    if (!missing(sheet.headers)) {
      if (round(sheet.headers) != sheet.headers) {
        stop(paste("object of class '", class(sheet.headers), "' passed to argument 'sheet.headers'"))
      } else {
        sheet.headers <- paste("&headers=", as.integer(sheet.headers), sep = "")
      }
    } else {
      sheet.headers <- ""
    }
    
    ## sheet.query (optional)
    if (!missing(sheet.query)) {
      if (!is.character(sheet.query)) {
        stop (paste("object of class '", class(sheet.query), "' passed to argument 'sheet.query'"))
      } else {
        sheet.query <- paste("&tq=", utils::URLencode(sheet.query, reserved = TRUE), sep = "")
      }
    } else {
      sheet.query <- ""
    }
    
    ## return
    if (!return %in% c('data.frame', 'https.request')) {
      stop (paste("unknown value '", return, "' passed to argument 'return'"))
    }
    
    # PREPARE HTTPS REQUEST
    https_request <-
      paste(
        "https://docs.google.com/spreadsheets/d/", sheet.id, "/gviz/tq?tqx=out:csv",
        sheet.name,
        sheet.headers,
        sheet.query, 
        sep = "")
    
    # RESULT
    switch (
      return,
      data.frame = {
        res <- utils::read.table(file = https_request, sep = ",", ...)
      },
      https.request = {
        res <- https_request
      }
    )
    return (res)
  }
readGoogleSheet(
  sheet.id = '1GMkY54sJAGBqcPLMiQes_gjDJ70RsfXzYoQWPzc37Dc', 
  sheet.name = "camada",
  sheet.headers = 1,
  # return = 'https.request',
  sheet.query = "select * where A != '-'",
  # sheet.query = "select * where A != '#metadado>'",
  stringsAsFactors = FALSE, dec = ',', 
  na.strings = c("NA", "-", "", "na", "tr", "#VALUE!"), 
  header = TRUE)
