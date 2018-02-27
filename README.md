# Repositório Brasileiro Livre para Dados Abertos do Solo - pacote para R

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
