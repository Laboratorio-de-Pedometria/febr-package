#' @param variable (optional) Character vector indicating one or more variables. Accepts only general 
#' identification codes, e.g. `"ferro"` and `"carbono"`. If missing, then a set of standard identification 
#' variables is downloaded. Use `variable = "all"` to download all variables. See \sQuote{Details} for 
#' more information.
#' 
#' @param stack (optional) Logical value indicating if tables from different datasets should be stacked on a 
#' single table for output. Requires `standardization = list(units = TRUE)` -- see below. Defaults to 
#' `stack = FALSE`, the output being a list of tables.
