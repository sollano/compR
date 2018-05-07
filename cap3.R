## ---

## title: CompR - Inventário Florestal
## subtitle: Capítulo 3 - Processamento de dados de inventário florestal

## author:
## - Sollano Rabelo Braga 
## - Marcio Leles Romarco de Oliveira
## - Eric Bastos Gorgens

## date: 2018

## output:

##    pdf_document:

##      toc: true

##      toc_depth: 4

##      highlight: tango

##    word_document:

##      toc: true

##      toc_depth: 4

##      highlight: tango

##      df_print: kable

##      reference_docx: ~/R/mystyles.docx

##    html_document:

##      toc: true

##      toc_float: true

##      df_print: kable

##      toc_depth: 4

##      highlight: default

## ---
## \pagebreak

#+include=FALSE
library(knitr)
library(kableExtra)

#+include=FALSE
library(dplyr)



## # Preparação dos dados ####
## Os dados utilizados neste capítulo estão disponíveis no site github, e podem ser baixados e carregados no R utilizando a função read.csv2. Vamos realizar o download desses dados direto pelo R por meio da função, e salvar em um objeto chamado dados_sma:
dados_invt <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_invt.csv")

#+results="hide"
head(dados_invt)

#+echo=FALSE
kable(head(dados_invt), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0)) %>%
column_spec(1:ncol(head(dados_invt)), width = "2cm")

#+echo=TRUE
dados_invt[dados_invt==0] <- NA 

#+results="hide"
head(dados_invt)

#+echo=FALSE
kable(head(dados_invt), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0)) %>%
  column_spec(1:ncol(head(dados_invt)), width = "2cm")

#+echo=TRUE
class(dados_invt$DATA_PL)

class(dados_invt$DATA_MD)

dados_invt$DATA_PL <- as.Date(dados_invt$DATA_PL, format = "%d/%m/%Y")
dados_invt$DATA_MD <- as.Date(dados_invt$DATA_MD, format = "%d/%m/%Y")

#+results="hide"
head(dados_invt)

#+echo=FALSE
kable(head(dados_invt), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0)) %>%
  column_spec(1:ncol(head(dados_invt)), width = "2cm")

#+echo=TRUE
class(dados_invt$DATA_PL)

class(dados_invt$DATA_MD)

## # Estimativa das árvores não medidas ####

## $$ Ln(Ht) = \beta_0 + \beta_1*\frac{1}{dap} + \beta_2*Ln(HD) $$ 
##

## ## Estimativa da altura dominante ####
library(dplyr)

dados_invt <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_invt2.csv")

#+results="hide"
head(dados_invt)

#+echo=FALSE
kable(head(dados_invt), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0)) %>%
column_spec(1:ncol(head(dados_invt)), width = "2cm")

#+echo=TRUE
tabhd <- dados_invt %>% 
  filter( OBS == "D") %>% 
  group_by(TALHAO, PARCELA) %>% 
  summarise(HD = mean(HT) )

#+results="hide"
tabhd

#+echo=FALSE
kable(tabhd, "html",digits=c(1,1,1)) %>%
column_spec(1:ncol(tabhd), width = "2cm")

#+echo=TRUE
dados_invt_hd <- left_join(dados_invt, tabhd, by = c("TALHAO", "PARCELA") )

#+results="hide"
head(dados_invt_hd)

#+echo=FALSE
kable(head(dados_invt_hd), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,1)) %>%
column_spec(1:ncol(head(dados_invt_hd)), width = "2cm")

#+echo=TRUE
dados_invt_hd <- dados_invt %>% 
  filter( OBS == "D") %>% 
  group_by(TALHAO, PARCELA) %>% 
  summarise(HD = mean(HT) ) %>% 
  left_join(dados_invt, ., by = c("TALHAO", "PARCELA") )

#+results="hide"
head(dados_invt_hd)

#+echo=FALSE
kable(head(dados_invt_hd), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,1)) %>%
  column_spec(1:ncol(head(dados_invt_hd)), width = "2cm")

## ## Ajuste de um modelo hipsométrico ####

dados_invt_hd <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_invt_hd.csv")

#+results="hide"
head(dados_invt_hd)

#+echo=FALSE
kable(head(dados_invt_hd), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0)) %>%
  column_spec(1:ncol(head(dados_invt_hd)), width = "2cm")







## ## Estimativa de altura ####

## # Estimativa de volume ####

## # Quantificação de parcelas ####

## #####



