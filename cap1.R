## ---

## title: compR: Inventário Florestal
## subtitle: Capítulo 1 - Cubagem

## author:
## - Sollano Rabelo Braga 
## - Marcio Leles Romarco de Oliveira

## date: Outubro, 2016

##    html_document:

##      toc: true

##      toc_depth: 4

##      df_print: kable

##      reference_docx: ~/R/mystyles.docx

## ---
## \pagebreak

#+include=FALSE
library(knitr)

## O pacote dplyr será utilizado neste script. Caso ele não esteja instalado na máquina ou esteja desatualizado, utilize a função install.packages para instalá-lo, como na linha de comando abaixo:
#install.packages("dplyr", dependencies = T)

## Com o pacote instalado, basta rodar o comando library para carregá-lo na sua seção atual. Isto deve ser feito sempre que se iniciar uma nova seção, ou seja, sempre que se abrir o R ou reiniciá-lo.
#+message=FALSE
library(dplyr)

## Os dados utilizados neste capítulo estão disponíveis no site github, e podem ser baixados e carregados no R utilizando a função read.csv2. Vamos realizar o download desses dados direto pelo R por meio da função, e salvar em um objeto chamado dados_sma:
dados_sma <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_sma.csv")

## Com a função head é possível visualizar as primeiras 6 linhas dos dados, e garantir que foram carregados corretamente:
#+results="hide"
head(dados_sma)

#+echo=FALSE
kable(head(dados_sma), "html")
