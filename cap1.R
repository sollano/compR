#install.packages("dplyr", dependencies = T)

library(dplyr)

dados_sma <- read.csv2("https://raw.githubusercontent.com/sollano/compR/master/dados_sma.csv")

head(dados_sma)
