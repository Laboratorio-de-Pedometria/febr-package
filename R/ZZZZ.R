.onAttach <- function (lib, pkg) {
  
  pkg.info <- drop(
    read.dcf(file = system.file("DESCRIPTION", package = "febr"), fields = c("Title", "Version", "Date")))
  
  packageStartupMessage(
    paste("---------------------------------------------------------------------\n",
          pkg.info["Title"],                                                  " \n",
          "febr version ", pkg.info["Version"],                               " \n",
          "(built on ", pkg.info["Date"], ") is now loaded                      \n",
          "                                                                     \n",
          "Making the access to the Free Brazilian Repository for Open Soil Data\n",
          "as easy as possible. Access http://www.ufsm.br/febr/.                \n",
          "Contact febr-forum@googlegroups.com for more information             \n",
          "and to learn how you can help!                                       \n",
          "---------------------------------------------------------------------\n",
          sep = "")
  )
  
  # Verificar se os pacotes sugeridos estão instalados
  pkg <- c("cellranger", "dplyr", "glue", "googlesheets", "pedometrics", "readr", "sp", "stringr")
  id <- !sapply(pkg, requireNamespace, quietly = TRUE)
  if (any(id)) {
    pkg <- paste(pkg[which(id)], collapse = " ")
    stop(paste("package(s) needed for febr to work but not installed: ", pkg, sep = ""), call. = FALSE)
  }
}


