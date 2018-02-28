# Repositório Brasileiro Livre para Dados Abertos do Solo - pacote para R

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/febr-team/febr-package.svg?branch=master)](https://travis-ci.org/febr-team/febr-package)
[![CRAN](https://www.r-pkg.org/badges/version/febr)](https://cran.r-project.org/package=febr)
[![Rdoc](http://www.rdocumentation.org/badges/version/febr)](http://www.rdocumentation.org/packages/febr)

Olá! Esse é o ambiente de desenvolvimento do *pacote para R* do Repositório Brasileiro Livre para Dados Abertos do Solo, também conhecido como <font face="Comfortaa">febr</font>. Sinta-se à vontade para propor melhorias e estudar o código fonte. Afinal de contas, o <font face="Comfortaa">febr</font> é um repositório livre!

# Instalação

O pacote `febr` ainda não está no CRAN. A versão atual de desenvolvimento, disponível no GitHub, pode ser instalada -- usando o pacote `devtools` -- da seguinte maneira:

```R
if (!require(devtools)) {
  install.packages(pkgs = "devtools")
}
devtools::install_github(repo = "febr-team/febr-package")
```

# Utilização

O pacote `febr` possui cinco funções para descarregamento de dados:

* `dataset`, para descarregar os dados sobre um conjunto de dados;
* `observation`, para descarregar os dados das observações do solo de um conjunto de dados;
* `layer`, para descarregar os dados das camadas das observações do solo de um conjunto de dados;
* `metadata`, para descarregar os dados sobre os dados de um conjunto de dados;
* `febr`, para descarregar todos os dados e metadados de um conjunto de dados.

O principal argumento dessas funções é `dataset`. Para esse argumento é passado o código de identificação de um ou mais conjuntos de dados publicados no <font face="Comfortaa">febr</font>. Isso descarrega os dados da tabela de dados indicada pela função escolhida. Por exemplo, `observation(dataset = "ctb0003")` descarrega os dados das observações do solo do conjunto de dados `ctb0003`. O segundo principal argumento das funções `observation`, `layer` e `febr` é `variable`. Para esse argumento é passado o código de identificação da(s) variável(is) cujos dados devem ser retornados pela função. Por exemplo, `layers(dataset = "ctb0003", variable = "argila")` retorna os dados de argila do conjunto de dados `ctb0003`.

# Como colaborar

Nós usamos o modelo de desenvolvimento colaborativo *fork & pull*. Isso significa que qualquer pessoa pode fazer um cópia paralela -- *fork* -- deste repositório, realizar alterações e depois empurrá-las -- *push* -- para a sua cópia pessoal do repositório. Isso tudo sem que seja necessário pedir qualquer autorização aos mantenedores deste repositório. Caso as alterações realizadas sejam interessantes, pode-se fazer um pedido para que as mesmas sejam puxadas -- *pull request* -- para o repositório original. Depois de uma revisão das alterações, nós decidiremos se elas devem ser fundidas -- *merge* -- com o código fonte do repositório original.
