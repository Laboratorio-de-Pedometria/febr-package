# Build package

# Dependencies
update(remotes::package_deps(packages = "febr"))

# turn on/off development mode
# devtools::dev_mode()

# check examples and documentation
devtools::check_man()
devtools::run_examples()

# check the package for Linux and Windows
devtools::check(document = TRUE, manual = TRUE, force_suggests = TRUE, run_dont_test = TRUE)

devtools::check_win_devel()
devtols::check_win_release()
devtols::check_win_oldrelease()

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
