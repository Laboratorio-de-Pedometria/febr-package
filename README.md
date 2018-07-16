# Repositório Brasileiro Livre para Dados Abertos do Solo -- pacote para R

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/febr-team/febr-package.svg?branch=master)](https://travis-ci.org/febr-team/febr-package)
[![CRAN](https://www.r-pkg.org/badges/version/febr)](https://cran.r-project.org/package=febr)
[![Rdoc](http://www.rdocumentation.org/badges/version/febr)](http://www.rdocumentation.org/packages/febr)
[![Downloads](http://cranlogs.r-pkg.org/badges/febr?color=brightgreen)](http://www.r-pkg.org/pkg/febr)

Olá! Esta é a página do *pacote para R* do Repositório Brasileiro Livre para Dados Abertos do Solo, também conhecido como [___febr___][febr] (/ˈfɛ.bɾe/). Sinta-se à vontade para propor melhorias e estudar o código 
fonte. Afinal de contas, o ___febr___ é um repositório livre!

[febr]: http://www.ufsm.br/febr/

## Instalação

O pacote `febr` foi criado para facilitar o acesso aos dados dos conjuntos de dados publicados no ___febr___. Ele pode ser descarregado do [CRAN][cran] da seguinte maneira:

```R
install.packages(pkgs = "febr")
```

Já a versão de desenvolvimento, disponível no [GitHub][github], pode ser instalada -- usando o pacote `devtools` -- da seguinte maneira:

[cran]: https://CRAN.R-project.org/package=febr
[github]: https://github.com/febr-team/febr-package

```R
if (!require(devtools)) {
  install.packages(pkgs = "devtools")
}
devtools::install_github(repo = "febr-team/febr-package")
```

## Utilização básica

O pacote `febr` possui cinco funções para descarregamento de dados:

1. `dataset`, para descarregar os dados sobre um ou mais conjuntos de dados;
2. `observation`, para descarregar os dados das observações do solo de um ou mais conjuntos de dados;
3. `layer`, para descarregar os dados das camadas das observações do solo de um ou mais conjuntos de dados;
4. `metadata`, para descarregar os dados sobre os dados de um ou mais conjuntos de dados;
5. `febr`, para descarregar todos os dados e metadados de um único conjunto de dados.

O principal argumento dessas cinco funções é `dataset`. Para esse argumento é passado o código de identificação de um ou mais conjuntos de dados publicados no ___febr___. Isso descarrega os dados da tabela de dados indicada pela função escolhida. Por exemplo,

```R
observation(dataset = "ctb0003")
```

descarrega os dados das observações do solo do conjunto de dados `ctb0003`. O código de identificação de todos os conjuntos de dados publicados no ___febr___ estão catalogados em http://www.ufsm.br/febr/catalog/.

No caso das funções `observation`, `layer` e `febr`, o segundo principal argumento é `variable`. Para esse argumento é passado o código de identificação da(s) variável(is) cujos dados devem ser retornados pela função escolhida. Por exemplo,

```R
layers(dataset = "ctb0003", variable = "argila")
```

retorna os dados da variável `argila` do conjunto de dados `ctb0003`. O código de identificação de todas as variáveis contidas nos conjuntos de dados publicados no ___febr___ estão catalogados em https://goo.gl/hi77sB.

## Como colaborar

Nós usamos o modelo de desenvolvimento colaborativo *fork & pull*. Isso significa que você tem liberdade para fazer um cópia paralela -- *fork* -- deste repositório, alterar o código fonte conforme julgar necessário e depois empurrar -- *push* -- as alterações para a sua cópia pessoal deste repositório. Isso tudo sem que seja necessário pedir qualquer autorização. Caso as alterações que você realizou na sua cópia pessoal deste repositório sejam interessantes e você tem interesse em compartilhar as mesmas conosco, então basta solicitar que sejam puxadas -- *pull request* -- para este repositório. Depois de uma revisão das alterações, nós decidiremos se elas podem ser fundidas -- *merge* -- com o código fonte deste repositório.
