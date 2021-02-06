# Build package

# Dependencies
update(remotes::package_deps(packages = "febr"))
update(remotes::package_deps(packages = "devtools"))
install.packages("rmarkdown")

# Reverse dependency tools
devtools::revdep()

# turn on/off development mode
devtools::dev_mode()

# Render README
rmarkdown::render("README.Rmd")

# check documentation ----
roxygen2::roxygenise()
devtools::check_man()
devtools::spell_check(vignettes = FALSE)
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
html_files <- list.files(path = "docs", pattern = ".html", recursive = TRUE, full.names = TRUE)
for (con in html_files) {
  x <- readLines(con)
  x <- sub("Download from CRAN at ", "Descarregue do CRAN em ", x)
  x <- sub("Browse source code at ", "Navegue pelo código fonte em ", x)
  x <- sub("Report a bug at ", "Reporte um problema em ", x)
  x <- sub("<h2>License</h2>", "<h2>Licença</h2>", x)
  x <- sub("<h2>Developers</h2>", "<h2>Desenvolvedores</h2>", x)
  x <- sub(" Author, maintainer ", " Autor, mantenedor ", x)
  x <- sub(" Funder ", " Financiador ", x)
  x <- sub("Developed by ", "Desenvolvido por ", x)
  x <- sub("Site built with ", "Página construída com ", x)
  x <- sub("<h2>Contents</h2>", "<h2>Conteúdo</h2>", x)
  x <- sub("<h1>Reference</h1>", "<h1>Referência</h1>", x)
  x <- sub(">Source: ", ">Fonte: ", x)
  writeLines(x, con)
}
if (dir.exists("~/projects/web/pedometria.org/static/software/febr")) {
  system("cp -a docs/. ~/projects/web/pedometria.org/static/software/febr")
}
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
