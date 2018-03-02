## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(magrittr)
library(dplyr)
library(glue)
library(pander)
panderOptions("table.alignment.default", "left")
panderOptions("table.split.table", Inf)
library(sp)
library(lattice)
library(latticeExtra)
febr <- glue::glue('<font face="Comfortaa">febr</font>')

## ---- eval=FALSE---------------------------------------------------------
#  if (!require(devtools)) {
#    install.packages(pkgs = "devtools")
#  }
#  devtools::install_github(repo = "febr-team/febr-package")

## ------------------------------------------------------------------------
library("febr")

## ---- echo=FALSE, results='asis'-----------------------------------------
rbind(
  c("`header`", "Estrutural", "Descarregar o cabeçalho das tabelas de dados"),
  c("`standard`", "Estrutural", glue("Descarregar os padrões de codificação e nomenclatura do {febr}")),
  c("`unit`", "Estrutural", glue("Descarregar os padrões de unidades de medida do {febr}")),
  c("`dataset`", "De trabalho", "Descarregar informações gerais sobre um conjunto de dados"),
  c("`observation`", "De trabalho", "Descarregar dados das observações do solo de um conjunto de dados"),
  c("`layer`", "De trabalho", "Descarregar dados das camadas das observações do solo de um conjunto de dados"),
  c("`metadata`", "De trabalho", "Descarregar metadados de um conjunto de dados"),
  c("`febr`", "De trabalho", "Descarregar todos os dados e metadados de um conjunto de dados"),
  c("`febr2sp`", "Auxiliar", "Criar objeto de classe `SpatialPointsDataFrame`."),
  c("`febr2xlsx`", "Auxiliar", "Escrever dados para arquivo XLSX.")
  ) %>% 
  pandoc.table(
    caption = "Descrição sumária das funções do pacote `febr`.", 
    col.names = c("Função", "Grupo", "Descrição"))

## ------------------------------------------------------------------------
cab <- 
  febr::header(
    dataset = "ctb0003", 
    table = "camada", 
    variable = "all",
    progress = FALSE, verbose = FALSE) %>%
  t() 
cab 

## ------------------------------------------------------------------------
febr::header(
  dataset = c("ctb0003", "ctb0036"),
  table = "camada",
  variable = c("argila", "densidade", "carbono", "ph"),
  stack = TRUE, progress = FALSE, verbose = FALSE) %>%
  t()

## ---- results='asis'-----------------------------------------------------
febr::standard(
  variable = c("argila", "densidade", "carbono", "ph")) %>%
  dplyr::select(campo_id, campo_nome, campo_descricao) %>%
  pander(row.names = FALSE)

## ---- results='asis'-----------------------------------------------------
febr::standard(
  variable = c(
    "argila_naoh_esferas_pipeta", "densidade_solo_anel", "carbono_dicromato_30min150_mohr", "ph_h2o_25")) %>%
  dplyr::select(campo_id, campo_unidade, campo_precisao) %>%
  pander()

## ------------------------------------------------------------------------
febr::unit(source = "Mg/m^3", target = "kg/dm^3")

## ---- results='asis', echo=FALSE-----------------------------------------
febr::unit() %>%
  dplyr::select(unidade_origem, unidade_destino, unidade_constante) %>%
  DT::datatable(
    rownames = FALSE, colnames = c("Unidade de origem", "Unidade de destino", "Constante de transformação"),
    options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
    ))

## ---- results='asis'-----------------------------------------------------
febr::dataset(
  dataset = "ctb0003", progress = FALSE, verbose = FALSE) %>%
  na.omit() %>%
  pandoc.table(
    row.names = FALSE, caption = "Dados sobre o conjunto de dados `ctb0003` contidos na tabela  `dataset`.")

## ---- results='asis'-----------------------------------------------------
set.seed(2001)
febr::metadata(
  dataset = "ctb0003", progress = FALSE, verbose = FALSE) %>%
  dplyr::select(dataset_id, tabela_id, campo_id, campo_nome, campo_descricao) %>% 
  group_by(tabela_id) %>%
  sample_n(5) %>%
  pandoc.table(
    row.names = FALSE, caption = "Alguns dos dados sobre os dados contidos no conjunto de dados `ctb0003`.")

## ------------------------------------------------------------------------
febr::header(
  dataset = "ctb0003", 
  table = "observacao", 
  variable = "all",
  progress = FALSE, verbose = FALSE) %>%
  t()

## ---- fig.asp=1----------------------------------------------------------
febr::observation(
  dataset = "ctb0003",
  variable = "taxon",
  progress = FALSE, verbose = FALSE) %>% 
  febr2sp() %>%
  spplot(zcol = "taxon_sibcs_2009", auto.key = list(columns = 3), scales = list(draw = TRUE)) +
  latticeExtra::layer(panel.grid(v = -1, h = -1))

## ---- fig.asp=1----------------------------------------------------------
febr::observation(
  dataset = "ctb0036",
  missing = list(coord = "drop", time = "drop"),
  progress = FALSE, verbose = FALSE) %>%
  febr2sp() %>%
  plot();box();grid();axis(1);axis(2)

