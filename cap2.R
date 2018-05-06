## ---

## title: CompR - Inventário Florestal
## subtitle: Capítulo 2 - Cubagem
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


## # Cálculo do volume por seção ####

## Existem diversos métodos de cálculo de volume de árvores cubadas. Aqui serão abordados dois dos mais utilizados. Utilizando eles como base, é possível fazer modificações para fazer o cálculo para os outros métodos, caso o usuário deseje.
##

## ## Método de Smalian ####

## O volume p
## $$ V_{secao} = \frac{AS_{i} + AS_{i+1}}{2} . L $$
#+message=FALSE
library(dplyr)

## Os dados utilizados neste capítulo estão disponíveis no site github, e podem ser baixados e carregados no R utilizando a função read.csv2. Vamos realizar o download desses dados direto pelo R por meio da função, e salvar em um objeto chamado dados_sma:
dados_sma <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_sma.csv")

## Com a função 
#+results="hide"
head(dados_sma)

#+echo=FALSE
kable(forestr::round_df(head(dados_sma), 2), "html") %>% 
  column_spec(1:ncol(dados_sma), width = "2cm")

library(dplyr)

dados_vol_secao <- dados_sma %>% 
  mutate( # função para adicionar novas variáveis
    AS_CC = (di_cc^2 * pi) / 40000) # Cálculo da AS com casca

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(head(dados_vol_secao), "html",digits=c(1,2,1,1,2,1,4)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")

#+echo=TRUE
dados_vol_secao <- dados_vol_secao  %>% 
  group_by(ARVORE) %>% # grupo
  mutate( # função para adicionar novas variáveis
    VCC   = ( (AS_CC + lead(AS_CC) )/2 ) * (lead(hi) - hi) ) # Cálculo do volume com casca
# lead: acessa o valor seguinte

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(head(dados_vol_secao,20), "html",digits=c(1,2,1,1,2,1,4,4)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")
#+echo=TRUE
dados_vol_secao <- dados_vol_secao  %>% 
  mutate( # função para adicionar novas variáveis
    di_sc = di_cc-2*(e_casca/10) ) # cálculo do diâmetro sem casca

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(head(dados_vol_secao), "html",digits=c(1,2,1,1,2,1,4,4,2)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")

#+echo=TRUE
dados_vol_secao <- dados_vol_secao %>% 
  mutate( # função para adicionar novas variáveis
    AS_SC = (di_sc^2 * pi) / 40000, # Cálculo da AS sem casca
    VSC   = ( (AS_SC + lead(AS_SC) )/2 ) * (lead(hi) - hi) ) # Cálculo do volume sem casca

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(head(dados_vol_secao), "html",digits=c(1,2,1,1,2,1,4,4,2,4,4)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")

#+echo=TRUE
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
kable(head(dados_vol_secao), "html",digits=c(1,2,1,1,2,1,4,4,2,4,4)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")



## ## Método de Huber ####

## $$ V_{secao} = AS_{1/2} . L $$
##

#+message=FALSE
library(dplyr)

dados_hub <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_hub.csv")

#+results="hide"
head(dados_hub)

#+echo=FALSE
kable(forestr::round_df(head(dados_hub), 2), "html") %>% 
  column_spec(1:ncol(dados_hub), width = "2cm")

dados_vol_secao <- dados_hub %>%
  mutate(
    AS_CC = (di_cc^2 * pi) / 40000,  # area seccional com casca
    VCC   = AS_CC * comp_secao ) # volume com casca

#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(head(dados_vol_secao), "html",digits=c(1,1,2,2,2,2,1,1,4,4)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")

dados_vol_secao <- dados_hub %>% # df
  mutate( # funcao para adicionar novas variaveis
    di_sc = di_cc - 2 * (e_casca / 10), # diametro sem casca 
    AS_CC = (di_cc^2 * pi) / 40000, # AS com casca
    AS_SC = (di_sc^2 * pi) / 40000,  # AS sem casca
    VCC   = AS_CC * comp_secao, # volume com casca
    VSC   = AS_SC * comp_secao) # volume sem casca

#+results="hide"
head(dados_vol_secao)

#+echo=FALSE
kable(head(dados_vol_secao),"html",digits=c(1,1,2,2,2,2,1,1,2,4,4,4,4)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")


## # Cálculo do volume por árvore ####

dados_vol_secao <-  read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_vol_secao_sma.csv")
#+results="hide"
head(dados_vol_secao)
#+echo=FALSE
kable(head(dados_vol_secao), "html",digits=c(1,2,1,1,2,1,4,4,2,4,4)) %>% 
  column_spec(1:ncol(dados_vol_secao), width = "2cm")

## $$ V_{arvore} = V_{secao1} + V_{secao2} + ... V_n $$

dados_vol_arv <- dados_vol_secao  %>% # define df utilizado
  group_by(ARVORE)                %>% # define grupo
  summarize(                          # totaliza os dados
    DAP   = mean(DAP),                # Media de DAP
    HT    = mean(HT) ,                # media de HT
    VCC   = sum(VCC) ,                # Soma de volume com casca
    VSC   = sum(VSC))                 # Soma de volume sem casca      
#+results="hide"
head(dados_vol_arv)
#+echo=FALSE
kable(head(dados_vol_arv), "html",digits=c(0,2,2,4,4)) %>% 
  column_spec(1:ncol(dados_vol_arv), width = "2cm")


dados_vol_arv <- dados_vol_secao  %>% # define df utilizado
  group_by(ARVORE)                %>% # define grupo
  summarize(                          # totaliza os dados
    DAP   = mean(DAP)             ,   # Media de DAP
    HT    = mean(HT)              ,   # media de HT
    VCC   = sum(VCC, na.rm = TRUE),   # Soma de volume com casca
    VSC   = sum(VSC, na.rm = TRUE))   # Soma de volume sem casca      a

    #+results="hide"
head(dados_vol_arv)
#+echo=FALSE
kable(head(dados_vol_arv), "html",digits=c(0,2,2,4,4,1)) %>% 
  column_spec(1:ncol(dados_vol_arv), width = "2cm")

## ## Fator de forma médio e ajuste de modelos voumétricos ####
dados_vol_arv <-  read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_vol_arv.csv")
#+results="hide"
head(dados_vol_arv)
#+echo=FALSE
kable(head(dados_vol_arv), "html",digits=c(0,2,2,4,4,1)) %>% 
  column_spec(1:ncol(dados_vol_arv), width = "2cm")

## ### Fator de forma ####
ff_med <- dados_vol_arv %>% 
  summarise(
    FFCC = mean(VCC/(pi * DAP^2 / 40000 * HT)),  
    FFSC = mean(VSC/(pi * DAP^2 / 40000 * HT)) )

#+results="hide"
ff_med
#+echo=FALSE
kable(ff_med, "html",digits=c(4,4)) %>% 
  column_spec(1:ncol(ff_med), width = "2cm")


## ### Modelos volumétricos ####

##$$ Ln(Vcc) = \beta_0 + \beta_1*Ln(dap) + \beta_2*Ln(Ht) + \varepsilon_i $$ 


summary(lm(log(VCC) ~  log(DAP) + log(HT), data =dados_vol_arv))

tabcoef_vcc <- dados_vol_arv %>% # selecao do df
  do(Reg = lm(log(VCC) ~  log(DAP) + log(HT), data =.)) 

#+results="hide"
tabcoef_vcc 
#+echo=FALSE
kable( tabcoef_vcc %>% mutate(Reg="<s3: lm>"),"html" )

#+echo=TRUE
tabcoef_vcc <- dados_vol_arv %>% # selecao do df
  do(Reg = lm(log(VCC) ~  log(DAP) + log(HT), data =.)) %>% 
  rowwise %>% 
  mutate(b0=coef(Reg)[1], # a variavel mod
         b1=coef(Reg)[2], # possui os coefs na ordem, 
         b2=coef(Reg)[3]) # por isso eles sao extraidos com []
#+results="hide"
tabcoef_vcc 
#+echo=FALSE
kable( tabcoef_vcc %>% mutate(Reg="<s3: lm>"), "html", digits=c(NA,6,6,6) )%>% 
  column_spec(1:ncol(tabcoef_vcc), width = "2cm")

#+echo=TRUE
tabcoef_vcc <- dados_vol_arv %>% 
  do(Reg = lm(log(VCC) ~  log(DAP) + log(HT), data =.)) %>% 
  rowwise %>% 
  mutate(b0=coef(Reg)[1], 
         b1=coef(Reg)[2], 
         b2=coef(Reg)[3],
         Rsqr=summary(Reg)[[8]],
         Rsqr_adj=summary(Reg)[[9]], 
         Std.Error=summary(Reg)[[6]]) %>% 
  select(-Reg) 

#+results="hide"
tabcoef_vcc 
#+echo=FALSE
kable( tabcoef_vcc, "html", digits=c(6,6,6,3,3,3) )%>% 
  column_spec(1:ncol(tabcoef_vcc), width = "2cm")


#+echo=TRUE
tabcoef_vsc <- dados_vol_arv %>% 
  do(Reg = lm(log(VSC) ~  log(DAP) + log(HT), data =.)) %>% 
  rowwise %>% 
  mutate(b0=coef(Reg)[1], 
         b1=coef(Reg)[2], 
         b2=coef(Reg)[3],
         Rsqr=summary(Reg)[[8]],
         Rsqr_adj=summary(Reg)[[9]], 
         Std.Error=summary(Reg)[[6]]) %>% 
  select(-Reg) 

#+results="hide"
tabcoef_vsc 
#+echo=FALSE
kable( tabcoef_vsc, "html", digits=c(6,6,6,3,3,3) )%>% 
  column_spec(1:ncol(tabcoef_vcc), width = "2cm")





## ### Determinação do melhor estimador ####


ff_med <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/ff_med.csv")
#+results="hide"
ff_med
#+echo=FALSE
kable(ff_med, "html",digits=c(4,4)) %>% 
  column_spec(1:ncol(ff_med), width = "2cm")

tabcoef_vcc <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/tabcoef_vcc.csv")
#+results="hide"
tabcoef_vcc 
#+echo=FALSE
kable( tabcoef_vcc, "html", digits=c(6,6,6,3,3,3) )%>% 
  column_spec(1:ncol(tabcoef_vcc), width = "2cm")

dados_vol_arv <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_vol_arv.csv")
#+results="hide"
head(dados_vol_arv)
#+echo=FALSE
kable(head(dados_vol_arv), "html",digits=c(0,2,2,4,4)) %>% 
  column_spec(1:ncol(dados_vol_arv), width = "2cm")

dados_vol_arv_est <- dados_vol_arv %>% 
  cbind(ff_med) %>% 
  mutate(VCC_FF = FFCC * pi*DAP^2/40000 * HT ) %>% 
  select(-matches("^FF"))

#+results="hide"
head(dados_vol_arv_est)
#+echo=FALSE
kable(head(dados_vol_arv_est), "html",digits=c(0,2,2,4,4,4)) %>% 
  column_spec(1:ncol(dados_vol_arv_est), width = "2cm")


dados_vol_arv_est  <- dados_vol_arv_est  %>% 
  cbind(tabcoef_vcc) %>% 
  mutate(VCC_MOD = exp(b0 + b1*log(DAP) + b2*log(HT)) ) %>% 
  select(-matches("b|Rsqr|Std"))

#+results="hide"
head(dados_vol_arv_est)
#+echo=FALSE
kable(head(dados_vol_arv_est), "html",digits=c(0,2,2,4,4,4,4)) %>% 
  column_spec(1:ncol(dados_vol_arv_est), width = "2cm")

## $$ Resíduo (\%) = \frac{V_{i}est - V_{i}obs}{V_{i}obs} . 100 $$

dados_vol_arv_est  <- dados_vol_arv_est  %>% 
  mutate(
    RES_FF  = ((VCC_FF  - VCC)/VCC)*100,
    RES_MOD = ((VCC_MOD - VCC)/VCC)*100 )

#+results="hide"
head(dados_vol_arv_est)
#+echo=FALSE
kable(head(dados_vol_arv_est), "html",digits=c(0,2,2,4,4,4,4,2,2)) %>% 
  column_spec(1:ncol(dados_vol_arv_est), width = "2cm")

dados_vol_arv_est <- dados_vol_arv %>% 
  cbind(ff_med,tabcoef_vcc) %>% 
  mutate(
    VCC_FF = FFCC * pi*DAP^2/40000 * HT,
    VCC_MOD = exp(b0 + b1*log(DAP) + b2*log(HT)),
    RES_FF  = ((VCC_FF  - VCC)/VCC)*100,
    RES_MOD = ((VCC_MOD - VCC)/VCC)*100  ) %>% 
  select(-matches("^FF|b|Rsqr|Std"))

#+results="hide"
head(dados_vol_arv_est)
#+echo=FALSE
kable(head(dados_vol_arv_est), "html",digits=c(0,2,2,4,4,4,4,2,2)) %>% 
  column_spec(1:ncol(dados_vol_arv_est), width = "2cm")

#+echo=TRUE
library(ggplot2)

ggplot(dados_vol_arv_est, aes(VCC, RES_FF)) + 
  geom_point() +
  scale_y_continuous(limits = c(-40,40))

ggplot(dados_vol_arv_est, aes(VCC, RES_MOD)) + 
  geom_point() +
  scale_y_continuous(limits = c(-40,40) )


## ## ####




