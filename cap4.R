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
    IDADE        = mean(IDADE,na.rm=T),
    n            = n(),
    N            = mean(AREA_TL,na.rm=T) / (mean(AREA_PC,na.rm=T)/10000), 
    CV           = sd(VCC_HA,na.rm=T) / mean(VCC_HA,na.rm=T) * 100,
    t            = qt(alpha/2,n-1, lower.tail = F),
    t_rec        = qt(alpha/2,ceiling(
      t^2 * CV^2 / ( Erro_des^2 +(t^2 * CV^2 / N) ) ) - 1,
      lower.tail = F), 
    
    n_recalc     = ceiling(
      t_rec ^2 * CV^2 / ( Erro_des^2 +(t_rec^2 * CV^2 / N) ) ), 
    
    Y            = mean(VCC_HA, na.rm=T),
    Sy           = sqrt( var(VCC_HA,na.rm=T)/n  * (1 - (n/N)) ),
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
    IDADE        = mean(IDADE,na.rm=T),
    n            = n(),
    N            = mean(AREA_TL,na.rm=T) / ( mean(AREA_PC,na.rm=T)/10000), 
    CV           = sd(VCC_HA,na.rm=T) / mean(VCC_HA,na.rm=T)*100,
    t            = qt(alpha/2,n-1, lower.tail = F),
    
    t_rec        = qt(alpha/2,ceiling( 
      t^2 * CV^2 / Erro_des^2) - 1,lower.tail=F), 
    
    n_recalc     = ceiling( t_rec ^2 * CV^2 / Erro_des^2),  
    Y            = mean(VCC_HA, na.rm=T),
    Sy           = sqrt( var(VCC_HA,na.rm=T)/n ),
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



#+echo=TRUE


## # Amostragem casual estratificada ####

alpha <- 0.05 
Erro_des <- 10

tab_ace_1 <- tab_invt %>% 
  group_by(TALHAO) %>% 
  summarise(Nj = mean(AREA_TL,na.rm=T) / (mean(AREA_PC,na.rm=T)/10000) ) %>% 
  summarise(N  = sum(Nj,na.rm=T) ) %>% 
  select(N) %>% 
  cbind(tab_invt,.) %>% 
  mutate(Nj = AREA_TL / (AREA_PC/10000) ) %>% 
  group_by(TALHAO) %>% 
  summarise(
    IDADE  = mean(IDADE,na.rm=T),
    nj     = n() , 
    Nj     = mean(Nj,na.rm=T),
    N      = mean(N,na.rm=T),
    Pj     = Nj/mean(N,na.rm=T), 
    Eyj    = sum(VCC_HA,na.rm=T), 
    Eyj2   = sum(VCC_HA^2,na.rm=T),
    Yj     = mean(VCC_HA,na.rm=T), 
    Pj_Sj2 = Pj * var(VCC_HA,na.rm=T),
    Pj_Sj  = Pj * sd(VCC_HA,na.rm=T),
    Pj_Yj  = Pj * Yj) %>%
  ungroup %>%
  mutate( 
    EPj_Sj2  = sum(Pj_Sj2), 
    EPj_Sj   = sum(Pj_Sj), 
    Y        = sum(Pj_Yj),     
    CV       = EPj_Sj / Y * 100,
    t        = qt(alpha/2, df = sum(nj)-1, lower.tail = FALSE),     
    t_rec    = qt(
        alpha/2, df = ceiling(
        t^2 * EPj_Sj^2 / ( (Erro_des*Y/100)^2 + (t^2 * EPj_Sj2 / N )  ) )-1, 
        lower.tail = FALSE),
    n_recalc = ceiling( 
        t_rec^2 * EPj_Sj^2 / ( (Erro_des*Y/100)^2 + (t_rec^2 * EPj_Sj2 / N )  ) ), 
    nj_otimo = ceiling(n_recalc*Pj_Sj/EPj_Sj), 
    n_otimo  = sum(nj_otimo), 
    Yhatj    = Nj * Yj ) %>% 
  as.data.frame
tab_ace_1  

## Tabela final:

tab_ace_2 <- tab_ace_1 %>%
  summarise(
    t            = mean(t),
    Sy           = sqrt(
      sum(Pj_Sj) ^2 / sum(nj) - (mean(EPj_Sj2) / mean(N) )  ), 
    Y            = sum(Pj_Yj), 
    Erroabs      = Sy * t,
    Erroperc     = Erroabs / Y * 100, 
    Yhat         = sum(Nj) * Y, 
    Erro_Total   = Erroabs * sum(Nj),
    IC_ha_Inf    = Y - Erroabs, 
    IC_ha_Sup    = Y + Erroabs, 
    IC_Total_inf = Yhat - Erro_Total, 
    IC_Total_Sup = Yhat + Erro_Total ) %>% 
  as.data.frame 

tab_ace_2

## # Amostragem sistemática ####

alpha <- 0.05 
Erro_des <- 10

tab_as <- tab_invt %>%
  group_by(TALHAO) %>%
  summarise(
    IDADE        = mean(IDADE,na.rm=T),
    n            = n(),
    N            = mean(AREA_TL,na.rm=T) / ( mean(AREA_PC,na.rm=T)/10000), 
    CV           = sd(VCC_HA,na.rm=T) / mean(VCC_HA,na.rm=T)*100,
    t            = qt(alpha/2,n-1, lower.tail = F),
    
    t_rec        = qt(alpha/2,ceiling( 
      t^2 * CV^2 / Erro_des^2) - 1,lower.tail=F), 
    
    n_recalc     = ceiling( t_rec ^2 * CV^2 / Erro_des^2),  
    Y            = mean(VCC_HA, na.rm=T),
    Sy           = sqrt((sum(diff(VCC_HA)^2) / (2 * n * (n-1) ) ) * ((N-n)/N)),
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
tab_as

#+echo=FALSE
kable(tab_as, "html",digits=c(1,1,1,1,2,2,1,1,2,2,2,1,1,1,1,1,1)) %>%
  column_spec(1:ncol(tab_acs), width = "2cm")











## #####



