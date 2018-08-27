library(tidyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(plyr)

prd_diametro <- read_excel("dados_trabalho_rev01.xlsx",1)
prd_elmts <- read_excel("dados_trabalho_rev01.xlsx",2)

prd_diametro
prd_elmts

prd_diametro$peca <- replace(prd_diametro$peca, 
                             indice, 
                             prd_diametro$peca[indice-1]) # Remove apenas o primeiro NA
prd_diametro$peca <- replace(prd_diametro$peca,
                             indice, 
                             prd_diametro$peca[indice-1]) # Remove o segundo NA

which(is.na(prd_diametro$peca))

indice_remove <- ((1:46)*3)+1
indice_remove <- union(c(1),indice_remove)

prd_diametro <- prd_diametro[-indice_remove,] # Removendo as linhas com as médias.
prd_diametro$peca <- as.factor(prd_diametro$peca)
prd_d_mod1 <- prd_diametro %>% 
  select(peca, processo,
         'C57P Ø 392,5 +0,01 +0,047',
         'C62P Ø 424,1 +0,01 +0,087',
         'C76P Ø 367,0 +0,01 +0,047',
         'C77P Ø 260,2 +0,01 +0,071') %>% 
  rename(c('C57P Ø 392,5 +0,01 +0,047' = 'diametro_A', 
           'C62P Ø 424,1 +0,01 +0,087' = "diametro_B",
           'C76P Ø 367,0 +0,01 +0,047' = 'diametro_C',
           'C77P Ø 260,2 +0,01 +0,071' = 'diametro_D')) 



prd_d_mod1 <- prd_d_mod1 %>% group_by(peca,processo)
plot(density(x=prd_d_mod1$diametro_A,y=prd_d_mod1$diametro_B))


#############################################################
#      
#                     ELEMENTOS
#
#############################################################


prd_elmts_mod1 <- prd_elmts %>% select(-Comentário,-Ca)
prd_elmts_mod2 <- prd_elmts_mod1 %>% select(-Produto,
                                            -Processo,
                                            -Classe,
                                            -Espectro)
plot(prd_elmts_mod2)

prd_elmts_mod3 <- prd_elmts_mod2 %>% gather()
