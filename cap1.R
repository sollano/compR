## ---

## title: CompR - Inventário Florestal
## subtitle: Capítulo 1 - Cubagem

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

## O pacote dplyr será utilizado neste script. Caso ele não esteja instalado na máquina ou esteja desatualizado, utilize a função install.packages para instalá-lo, como na linha de comando abaixo:
#install.packages("dplyr", dependencies = T)

## Com o pacote instalado, basta rodar o comando library para carregá-lo na sua seção atual. Isto deve ser feito sempre que se iniciar uma nova seção, ou seja, sempre que se abrir o R ou reiniciá-lo.
#+message=FALSE
library(dplyr)

## Os dados utilizados neste capítulo estão disponíveis no site github, e podem ser baixados e carregados no R utilizando a função read.csv2. Vamos realizar o download desses dados direto pelo R por meio da função, e salvar em um objeto chamado dados_sma:
dados_sma <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_sma.csv")

## Com a função 
#+results="hide"
head(dados_sma)

#+echo=FALSE
kable(forestr::round_df(head(dados_sma), 2), "html") %>% column_spec(5:ncol(dados_sma), width = "20em")%>% column_spec(1:4, width = "15em")

## É important
str(dados_sma)

## Após 
##

## # Cálculo do volume por seção ####

## Existem diversos métodos de cálculo de volume de árvores cubadas. Aqui serão abordados dois dos mais utilizados. Utilizando eles como base, é possível fazer modificações para fazer o cálculo para os outros métodos, caso o usuário deseje.
##

## ## Método de Smalian ####

## O volume p
## $$ V_{secao} = \frac{AS_{i} + AS_{i+1}}{2} . L $$

## Antes de 
#+results="hide"
head(dados_sma)
#+echo=FALSE
kable(forestr::round_df(head(dados_sma), 2), "html") %>% column_spec(5:ncol(dados_sma), width = "20em")%>% column_spec(1:4, width = "15em")

## Observando os dados, p

dados_vol_secao <- dados_sma %>% 
  group_by(ARVORE) %>% # grupo
  mutate( # função para adicionar novas variáveis
    AS_CC = (di_cc^2 * pi) / 40000, # Cálculo da AS com casca
    VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(hi) - hi) ) # Cálculo do volume com casca
# lead: acessa a linha seguinte
#+results="hide"
head(dados_vol_secao,20)
#+echo=FALSE
kable(forestr::round_df(head(dados_vol_secao,20), 2), "html") %>% column_spec(5:ncol(dados_vol_secao), width = "20em")%>% column_spec(1:4, width = "15em")

dados_vol_secao <- dados_vol_secao %>% 
  group_by(ARVORE) %>% 
  mutate(
    
  )

######

library(dplyr)

dados_vol_secao <- dados_sma %>% 
  mutate( # função para adicionar novas variáveis
    AS_CC = (di_cc^2 * pi) / 40000) # Cálculo da AS com casca

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(forestr::round_df(head(dados_vol_secao), 3), "html") %>% column_spec(5:ncol(dados_vol_secao), width = "20em")%>% column_spec(1:4, width = "15em")



dados_vol_secao <- dados_vol_secao  %>% 
  group_by(ARVORE) %>% # grupo
  mutate( # função para adicionar novas variáveis
    VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(hi) - hi) ) # Cálculo do volume com casca
# lead: acessa o valor seguinte

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(forestr::round_df(head(dados_vol_secao,20), 3), "html") %>% column_spec(5:ncol(dados_vol_secao), width = "20em")%>% column_spec(1:4, width = "15em")

dados_vol_secao <- dados_vol_secao  %>% 
  mutate( # função para adicionar novas variáveis
    di_sc = di_cc-2*(e_casca/10) ) # cálculo do diâmetro sem casca

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(forestr::round_df(head(dados_vol_secao), 3), "html") %>% column_spec(5:ncol(dados_vol_secao), width = "20em")%>% column_spec(1:4, width = "15em")

dados_vol_secao <- dados_vol_secao %>% 
  mutate( # função para adicionar novas variáveis
    AS_SC = (di_sc^2 * pi) / 40000, # Cálculo da AS sem casca
    VSC   = ( (AS_SC + lead(AS_SC) )/2 ) * (lead(hi) - hi) ) # Cálculo do volume sem casca

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(forestr::round_df(head(dados_vol_secao), 3), "html") %>% column_spec(5:ncol(dados_vol_secao), width = "20em")%>% column_spec(1:4, width = "15em")

head(as.data.frame(dados_vol_secao))

dados_vol_secao <- dados_sma %>% # definição do df
  group_by(ARVORE) %>% # grupo
  mutate( # função para adicionar novas variáveis
    di_sc = di_cc-2*(e_casca/10), # cálculo do diâmetro sem casca
    AS_CC = (di_cc^2 * pi) / 40000, # AS com casca
    AS_SC = (di_sc^2 * pi) / 40000,  # AS sem casca
    VCC   = ( (AS_CC + lead(AS_CC) )/2) * (lead(hi) - hi), # Volume com casca
    VSC   = ( (AS_SC + lead(AS_SC) )/2) * (lead(hi) - hi) ) # Volume sem casca
# lead: acessa a linha seguinte

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(forestr::round_df(head(dados_vol_secao), 3), "html") %>% column_spec(5:ncol(dados_vol_secao), width = "20em")%>% column_spec(1:4, width = "15em")

head(as.data.frame(dados_vol_secao))





######
## ## Método de Huber ####

## $$ V_{secao} = AS_{1/2} . L $$
##

## # Cálculo do volume por árvore ####

## $$ V_{arvore} = V_{secao1} + V_{secao2} + ... V_n $$

##




