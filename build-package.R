# Build package

# Dependencies
update(remotes::package_deps(packages = "febr"))
update(remotes::package_deps(packages = "devtools"))

# Reverse dependency tools
devtools::revdep()

# turn on/off development mode
# devtools::dev_mode()

# check examples and documentation
devtools::check_man()
devtools::run_examples()
devtools::spell_check()
devtools::check_rhub()

# check the package for Linux and Windows
devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)

devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

devtools::build()

# Load package
devtools::load_all()

# Build package site
pkgdown::build_site()
# pkgdown::build_articles()
# pkgdown::build_home()
# pkgdown::build_reference()
# pkgdown::build_news()


# turn on/off development mode
# devtools::dev_mode()

# upload to CRAN
devtools::release(check = FALSE)

# febr2xlsx
# WARNING: An illegal reflective access operation has occurred
# WARNING: Illegal reflective access by org.apache.poi.util.SAXHelper (file:/home/alessandro/Rlibs/xlsxjars/java/poi-ooxml-3.10.1-20140818.jar) to method com.sun.org.apache.xerces.internal.util.SecurityManager.setEntityExpansionLimit(int)
# WARNING: Please consider reporting this to the maintainers of org.apache.poi.util.SAXHelper
# WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
# WARNING: All illegal access operations will be denied in a future release
