# Repositório Brasileiro Livre para Dados Abertos do Solo - pacote para R

Olá! Esse é o ambiente de desenvolvimento do *pacote para R* do Repositório Brasileiro Livre para Dados
Abertos do Solo, também conhecido como **febr**. Sinta-se à vontade para propor melhorias e estudar o código 
fonte. Afinal de contas, o **febr** é um repositório livre!

# Instalação

Use o seguinte comando para instalar a última versão de desenvolvimento do pacote `febr`:  

    devtools::install_github("samuel-rosa/febr-package")
    
# Utilização

O pacote `febr` possui quatro funções principais.

Para descarregar as informações básicas de todos os conjuntos de dados, use

    dts <- datasets()

Já para descarregar os dados pontuais de todas as observações do solo, a função é

    obs <- observations()
    
Descarrgar os dados das camadas/horizontes do solo é feito usando

    lrs <- layers()

Por fim, para descarregar os metadados, use

    mtd <- metadata()
