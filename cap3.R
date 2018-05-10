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

tabcoef_ht <- dados_invt_hd %>% 
  mutate( I_DAP = 1/DAP ) %>% 
  group_by(TALHAO) %>% 
  do(Reg = lm(log(HT) ~ I_DAP + log(HD), data =.)) %>%
  mutate(b0=coef(Reg)[1], 
         b1=coef(Reg)[2],
         b2=coef(Reg)[3],
         Rsqr=summary(Reg)[[9]],
         Std.Error=summary(Reg)[[6]]   ) %>%
  select(-Reg)

#+results="hide"
tabcoef_ht

#+echo=FALSE
kable(tabcoef_ht, "html",digits=c(1,6,6,6,3,3)) %>%
column_spec(1:ncol(tabcoef_ht), width = "2cm")

#+echo=TRUE








## ## Estimativa de altura ####

dados_invt_hd <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_invt_hd.csv")

#+results="hide"
head(dados_invt_hd)

#+echo=FALSE
kable(head(dados_invt_hd), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0)) %>%
  column_spec(1:ncol(head(dados_invt_hd)), width = "2cm")

tabcoef_ht <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/tabcoef_ht.csv")

#+results="hide"
tabcoef_ht

#+echo=FALSE
kable(tabcoef_ht, "html",digits=c(1,2)) %>%
column_spec(1:ncol(tabcoef_ht), width = "2cm")

#+echo=TRUE

dados_invt_htest <- left_join(dados_invt_hd, tabcoef_ht, by="TALHAO")

#+results="hide"
head(dados_invt_htest)

#+echo=FALSE
kable(head(dados_invt_htest), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,6,6,6,3,3)) %>%
column_spec(1:ncol(head(dados_invt_htest)), width = "2cm")

#+echo=TRUE

dados_invt_htest <- dados_invt_htest %>% 
  mutate(
    HT_EST = ifelse(
      is.na(HT),
      exp(b0 + b1 * (1/DAP) + b2 * log(HD) ),
      HT) ) %>% 
  select(-matches("b|RSqr|Std"))

#+results="hide"
head(dados_invt_htest)

#+echo=FALSE
kable(head(dados_invt_htest), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,2)) %>%
column_spec(1:ncol(dados_invt_htest), width = "2cm")

#+echo=TRUE

dados_invt_htest <- dados_invt_htest %>% 
  left_join(tabcoef_ht, by="TALHAO") %>% 
  mutate(
    HT_EST = ifelse(
      is.na(HT),
      exp(b0 + b1 * (1/DAP) + b2 * log(HD) ),
      HT) ) %>% 
  select(-matches("b|RSqr|Std"))

#+results="hide"
head(dados_invt_htest)

#+echo=FALSE
kable(head(dados_invt_htest), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,2)) %>%
  column_spec(1:ncol(dados_invt_htest), width = "2cm")



## # Estimativa de volume ####

dados_invt_htest <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_invt_htest.csv")

#+results="hide"
head(dados_invt_htest)

#+echo=FALSE
kable(head(dados_invt_htest), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,2)) %>%
  column_spec(1:ncol(dados_invt_htest), width = "2cm")

tabcoef_vcc <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/tabcoef_vcc.csv")
#+results="hide"
tabcoef_vcc 
#+echo=FALSE
kable( tabcoef_vcc, "html", digits=c(6,6,6,3,3,3) )%>% 
  column_spec(1:ncol(tabcoef_vcc), width = "2cm")

tabcoef_vsc <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/tabcoef_vsc.csv")
#+results="hide"
tabcoef_vsc 
#+echo=FALSE
kable( tabcoef_vsc, "html", digits=c(6,6,6,3,3,3) )%>% 
  column_spec(1:ncol(tabcoef_vcc), width = "2cm")


dados_invt_v_est  <- dados_invt_htest  %>% 
  cbind(tabcoef_vcc) %>% 
  mutate(VCC = exp(b0 + b1*log(DAP) + b2*log(HT_EST)) ) %>% 
  select(-matches("b|Rsqr|Std")) %>% 
  cbind(tabcoef_vsc) %>% 
  mutate(VSC = exp(b0 + b1*log(DAP) + b2*log(HT_EST)) ) %>% 
  select(-matches("b|Rsqr|Std"))
#+results="hide"
head(dados_invt_v_est)

#+echo=FALSE
kable(head(dados_invt_v_est), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,4,4)) %>%
column_spec(1:ncol(head(dados_invt_v_est)), width = "2cm")

#+echo=TRUE   

dados_invt_v_est <- dados_invt_v_est %>% 
  mutate(
    AS = pi*DAP^2/40000,
    I  = as.numeric((as.Date(DATA_MD) - as.Date(DATA_PL))/30)
  )
#+results="hide"
head(dados_invt_v_est)

#+echo=FALSE
kable(head(dados_invt_v_est), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,4,4,4,1)) %>%
  column_spec(1:ncol(head(dados_invt_v_est)), width = "2cm")


dados_invt_v_est  <- dados_invt_htest  %>% 
  cbind(tabcoef_vcc) %>% 
  mutate(VCC = exp(b0 + b1*log(DAP) + b2*log(HT_EST)) ) %>% 
  select(-matches("b|Rsqr|Std")) %>% 
  cbind(tabcoef_vsc) %>% 
  mutate(
    VSC = exp(b0 + b1*log(DAP) + b2*log(HT_EST)),
    AS  = pi*DAP^2/40000,
    I   = as.numeric((as.Date(DATA_MD) - as.Date(DATA_PL))/30) ) %>% 
  select(-matches("b|Rsqr|Std"))
#+results="hide"
head(dados_invt_v_est)

#+echo=FALSE
kable(head(dados_invt_v_est), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,4,4,4,1)) %>%
  column_spec(1:ncol(head(dados_invt_v_est)), width = "2cm")


## # Quantificação de parcelas ####
dados_invt_htest <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_invt_v_est.csv")
#+results="hide"
head(dados_invt_v_est)

#+echo=FALSE
kable(head(dados_invt_v_est), "html",digits=c(1,1,0,0,1,0,1,1,1,1,0,2,4,4,4,1)) %>%
  column_spec(1:ncol(head(dados_invt_v_est)), width = "2cm")



tab_invt <- dados_invt_v_est %>% 
  group_by(TALHAO, PARCELA) %>%
  summarise(
    INDV_HA = n()*10000/mean(AREA_PC,na.rm=T), 
    IDADE   = mean(I ,na.rm=T),
    AREA_TL = mean(AREA_TL,na.rm=T),  
    AREA_PC = mean(AREA_PC,na.rm=T), 
    DAP     = mean(DAP,na.rm=T) ,
    q       = sqrt(mean(AS,na.rm=T)*40000/pi),
    HT      = mean(HT_EST,na.rm=T), 
    HD      = mean(HD,na.rm=T), 
    G_HA    = sum(AS,na.rm=T)*10000/AREA_PC , 
    VCC_HA  = sum(VCC,na.rm=T)*10000/AREA_PC, 
    VSC_HA  = sum(VSC,na.rm=T)*10000/AREA_PC ) 

#+results="hide"
tab_invt

#+echo=FALSE
kable(tab_invt, "html",digits=c(1,1,1,1,1,1,1,1,1,1,1,1,1)) %>%
column_spec(1:ncol(tab_invt), width = "2cm")

#+echo=TRUE



## #####



