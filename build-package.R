# Data Repository of the Brazilian Soil
# Alessandro Samuel-Rosa

# Test package #####################################################################################
# Dependencies
update(remotes::package_deps(packages = "febr"))
update(remotes::package_deps(packages = "devtools"))
# Reverse dependency tools
devtools::revdep()
# Render README
rmarkdown::render("febr-package/README.Rmd")
# check documentation ----
roxygen2::roxygenise("febr-package/")
devtools::check_man("febr-package/")
devtools::spell_check("febr-package/", vignettes = FALSE)
# spelling::update_wordlist("febr-package/")

# check examples ----
devtools::run_examples("febr-package/")

# check for Linux (local) ----
devtools::check("febr-package/", manual = TRUE, run_dont_test = TRUE)

# check for Windows (remote) ----
devtools::check_win_oldrelease("febr-package/")
devtools::check_win_release("febr-package/")
devtools::check_win_devel("febr-package/")

# check in R-hub ----
# rhub::validate_email(email = "alessandrosamuelrosa@gmail.com")
# rhub::check_on_windows()
devtools::check_rhub("febr-package/")

devtools::build()

# Load package
devtools::load_all("febr-package")

# Build package site
if (!require(pkgdown)) {
  install.packages("pkgdown")
}
pkgdown::build_site()
html_files <- list.files(path = "docs", pattern = ".html", recursive = TRUE, full.names = TRUE)
for (con in html_files) {
  x <- readLines(con)
  x <- sub("View on CRAN", "Ver no CRAN", x)
  x <- sub("Browse source code", "Navegue pelo código fonte", x)
  x <- sub("Report a bug", "Reporte um problema", x)
  x <- sub("License", "Licença", x)
  x <- sub("Developers", "Desenvolvedores", x)
  x <- sub("Authors", "Autores", x)
  x <- sub("Author", "Autor", x)
  x <- sub("maintainer", "mantenedor", x)
  x <- sub("Funder", "Financiador", x)
  x <- sub("Contributor", "Contribuinte", x)
  x <- sub("More about authors", "Mais sobre autores", x)
  x <- sub("Developed by", "Desenvolvido por", x)
  x <- sub("Site built with", "Página construída com", x)
  x <- sub("Contents", "Conteúdo", x)
  x <- sub("Citation", "Citação", x)
  x <- sub("Citing", "Citando", x)
  x <- sub("Reference", "Referência", x)
  x <- sub("Source:", "Fonte:", x)
  writeLines(x, con)
}
if (dir.exists("~/projects/web/pedometria.org/static/software/febr")) {
  system("cp -a docs/. ~/projects/web/pedometria.org/static/software/febr")
}
# pkgdown::preview_site()
# pkgdown::build_articles()
# pkgdown::build_home()
# pkgdown::build_reference()
# pkgdown::build_news()

# turn on/off development mode
# devtools::dev_mode()

# upload to CRAN
devtools::release(check = FALSE)

dados <- jsonlite::fromJSON("/home/alessandrorosa/Downloads/SmartSolos-Teste-01-ARGI-VERMELHO-Eutrofico-abruptico.json")
dados <- write.table(dados$items$HORIZONTES, "/home/alessandrorosa/Downloads/SmartSolos-Teste-01-ARGI-VERMELHO-Eutrofico-abruptico.txt", row.names = FALSE)
str(dados)
