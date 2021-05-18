.onAttach <- function (lib, pkg) {
  pkg.info <- drop(
    read.dcf(file = system.file("DESCRIPTION", package = "febr"),
             fields = c("Title", "Version", "Date")))
  packageStartupMessage(
    paste("---------------------------------------------------------------------\n",
          pkg.info["Title"],                                                  " \n",
          "febr version ", pkg.info["Version"],                               " \n",
          "(built on ", pkg.info["Date"], ") is now loaded                      \n",
          "                                                                     \n",
          "Check the new data download function readFEBR()                      \n",
          "                                                                     \n",
          "Visit https://www.pedometria.org/febr for more information           \n",
          "---------------------------------------------------------------------\n",
          sep = "")
  )
}
