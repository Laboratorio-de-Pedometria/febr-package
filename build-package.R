# Build package

# Dependencies
update(devtools::package_deps())

# turn on/off development mode
# devtools::dev_mode()

# check examples and documentation
devtools::check_man()
devtools::run_examples()

# check the package for Linux and Windows
devtools::check(
  document = TRUE, manual = TRUE, check_version = TRUE, force_suggests = TRUE, 
  args = "--use-valgrind --run-donttest")

devtools::build_win(version = c("R-release", "R-devel"))

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
