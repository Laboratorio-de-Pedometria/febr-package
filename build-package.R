# Build package

# Dependencies
update(remotes::package_deps(packages = "febr"))
update(remotes::package_deps(packages = "devtools"))

# Reverse dependency tools
devtools::revdep()

# turn on/off development mode
devtools::dev_mode()

# check documentation ----
roxygen2::roxygenise()
devtools::check_man()
devtools::spell_check()
# spelling::update_wordlist()

# check examples ----
devtools::run_examples()

# check for Linux (local) ----
devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)

# check for Windows (remote) ----
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

# check in R-hub ----
rhub::validate_email(email = 'alessandrosamuelrosa@gmail.com')
devtools::check_rhub()
# devtools::check_rhub(env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "false")) # scape missing suggested packages

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
