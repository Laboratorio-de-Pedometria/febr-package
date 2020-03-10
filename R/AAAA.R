# Supress R CMD check note 'no visible binding for global variable ...' ########
# Source: http://stackoverflow.com/a/12429344/3365410
# These variables are generated within the functions created with 'autofun'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c("coord_sistema"))
}

# Import functions from default packages other than `base` #####################
# Source: http://stackoverflow.com/a/31314870/3365410
#' @importFrom utils setTxtProgressBar txtProgressBar browseURL
#' @importFrom stats na.omit weighted.mean
#' @importFrom pedometrics isNumint
