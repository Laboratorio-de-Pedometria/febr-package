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
  document = TRUE, manual = TRUE, check_version = TRUE, force_suggests = TRUE, args = "--use-valgrind")

devtools::build_win()

devtools::build()

# Load package
devtools::load_all()

# Build package site
pkgdown::build_site()

# turn on/off development mode
# devtools::dev_mode()
