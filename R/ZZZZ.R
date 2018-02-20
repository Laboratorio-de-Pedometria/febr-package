.onAttach <- function (lib, pkg) {
  
  pkg.info <- drop(
    read.dcf(file = system.file("DESCRIPTION", package = "febr"), fields = c("Title", "Version", "Date")))
  
  packageStartupMessage(
    paste("---------------------------------------------------------------------\n",
          pkg.info["Title"],                                                  " \n",
          "febr version ", pkg.info["Version"],                               " \n",
          "(built on ", pkg.info["Date"], ") is now loaded                      \n",
          "Making the access to the Free Brazilian Repository for Open Soil Data\n",
          "as easy as possible. Access http://www.ufsm.br/febr/.                \n",
          "Contact febr-forum@googlegroups.com for more information             \n",
          "and to learn how you can help!                                       \n",
          "---------------------------------------------------------------------\n",
          sep = "")
  )
}
