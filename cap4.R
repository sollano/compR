## ---

## title: CompR - Inventário Florestal
## subtitle: Capítulo 4 - Estatísticas do inventário florestal

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

tab_invt <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/tab_invt.csv")

#+results="hide"
tab_invt

#+echo=FALSE
kable(tab_invt, "html",digits=c(1,1,1,1,1,1,1,1,1,1,1,1,1)) %>%
  column_spec(1:ncol(tab_invt), width = "2cm")


## # Amostragem casual simples ####

alpha <- 0.05 
Erro_des <- 10

tab_acs <- tab_invt %>%
  group_by(TALHAO) %>%
  summarise(
    IDADE        = mean(IDADE),
    n            = n(),
    N            = mean(AREA_TL) / (mean(AREA_PC)/10000), 
    CV           = sd(VCC_HA) / mean(VCC_HA) * 100,
    t            = qt(alpha/2,n-1, lower.tail = F),
    t_rec        = qt(alpha/2,ceiling(
      t^2 * CV^2 / ( Erro_des^2 +(t^2 * CV^2 / N) ) ) - 1,
      lower.tail = F), 
    
    n_recalc     = ceiling(
      t_rec ^2 * CV^2 / ( Erro_des^2 +(t_rec^2 * CV^2 / N) ) ), 
    
    Y            = mean(VCC_HA, na.rm=T),
    Sy           = sqrt( var(VCC_HA)/n  * (1 - (n/N)) ),
    Erroabs      = Sy * t , 
    Erroperc     = Erroabs / Y * 100 , 
    Yhat         = Y * N, 
    Erro_Total   = Erroabs * N, 
    IC_ha_Inf    = Y - Erroabs,
    IC_ha_Sup    = Y + Erroabs,
    IC_Total_inf = Yhat - Erro_Total,
    IC_Total_Sup = Yhat + Erro_Total) %>% 
  as.data.frame

#+results="hide"
tab_acs

#+echo=FALSE
kable(tab_acs, "html",digits=c(1,1,1,1,2,2,1,1,2,2,2,1,1,1,1,1,1)) %>%
  column_spec(1:ncol(tab_acs), width = "2cm")


alpha <- 0.05 
Erro_des <- 10

tab_acs <- tab_invt %>%
  group_by(TALHAO) %>%
  summarise(
    IDADE        = mean(IDADE),
    n            = n(),
    N            = mean(AREA_TL) / ( mean(AREA_PC)/10000), 
    CV           = sd(VCC_HA) / mean(VCC_HA) * 100,
    t            = qt(alpha/2,n-1, lower.tail = F),
    
    t_rec        = qt(alpha/2,ceiling( 
      t^2 * CV^2 / Erro_des^2) - 1,lower.tail=F), 
    
    n_recalc     = ceiling( t_rec ^2 * CV^2 / Erro_des^2),  
    Y            = mean(VCC_HA, na.rm=T),
    Sy           = sqrt( var(VCC_HA)/n ),
    Erroabs      = Sy * t , 
    Erroperc     = Erroabs / Y * 100, 
    Yhat         = Y * N, 
    Erro_Total   = Erroabs * N, 
    IC_ha_Inf    = Y - Erroabs,
    IC_ha_Sup    = Y + Erroabs,
    IC_Total_inf = Yhat - Erro_Total,
    IC_Total_Sup = Yhat + Erro_Total) %>% 
  as.data.frame

#+results="hide"
tab_acs

#+echo=FALSE
kable(tab_acs, "html",digits=c(1,1,1,1,2,2,1,1,2,2,2,1,1,1,1,1,1)) %>%
column_spec(1:ncol(tab_acs), width = "2cm")


cbind(tab_acs[,1:4],)

#+echo=TRUE


## # Amostragem casual estratificada ####









## # Amostragem sistemática ####








## #####



