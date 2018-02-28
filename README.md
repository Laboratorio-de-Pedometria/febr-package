# Repositório Brasileiro Livre para Dados Abertos do Solo - pacote para R

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/febr)](https://cran.r-project.org/package=febr)
[![Licence](https://img.shields.io/badge/licence-GPL (>= 2)-lightgrey.svg)](http://choosealicense.com/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/febr-team/febr-package.svg?branch=main)](https://travis-ci.org/febr-team/febr-package)

Olá! Esse é o ambiente de desenvolvimento do *pacote para R* do Repositório Brasileiro Livre para Dados Abertos do Solo, também conhecido como <font face="Comfortaa">febr</font>. Sinta-se à vontade para propor melhorias e estudar o código fonte. Afinal de contas, o <font face="Comfortaa">febr</font> é um repositório livre!

# Instalação

Use o seguinte comando para instalar a última versão de desenvolvimento do pacote `febr`:

    devtools::install_github("febr-team/febr-package")
    
# Utilização

O pacote `febr` possui cinco funções para descarregamento de dados.

* `dataset`, para descarregar os dados sobre um conjunto de dados;
* `observation`, para descarregar os dados das observações do solo de um conjunto de dados;
* `layer`, para descarrgar os dados das camadas das observações do solo de um conjunto de dados;
* `metadata`, para descarregar os dados sobre os dados de um conjunto de dados;
* `febr`, para descarregar todos os dados e metadados de um conjunto de dados.
