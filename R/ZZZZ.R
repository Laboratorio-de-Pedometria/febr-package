.onAttach <- function (lib, pkg) {
  
  pkg.info <- drop(
    read.dcf(file = system.file("DESCRIPTION", package = "febr"), fields = c("Title", "Version", "Date")))
  
  packageStartupMessage(
    paste("---------------------------------------------------------------------\n",
          pkg.info["Title"],                                                  " \n",
          "febr version ", pkg.info["Version"],                               " \n",
          "(built on ", pkg.info["Date"], ") is now loaded                      \n",
          "                                                                     \n",
          "Check the new core data download function readFEBR()                 \n",
          "                                                                     \n",
          "Visit https://www.pedometria.org/projeto/febr for more information   \n",
          "---------------------------------------------------------------------\n",
          sep = "")
  )
  
  # Verificar se os pacotes importados estão instalados
  pkg <- c("dplyr", "glue", "pedometrics", "sf", "stringr")
  id <- !sapply(pkg, requireNamespace, quietly = TRUE)
  if (any(id)) {
    pkg <- paste(pkg[which(id)], collapse = " ")
    stop(paste("package(s) needed for febr to work but not installed: ", pkg, sep = ""), call. = FALSE)
  }
}


