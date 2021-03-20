# VACINA-JÁ
library(tidyverse)
setwd("~/Documentos/jfemdados")
vacinometro <- rio::import("vacina/mapa_vacina.xlsx")%>%
  filter(Nome_da_meso  == "Zona da Mata",
         !Eficiencia > 100)%>%
  arrange(desc(Eficiencia))%>%
  filter(Nome_do_municipio == "JUIZ DE FORA" | Nome_do_municipio == "ALÉM PARAÍBA" | Nome_do_municipio == "UBÁ" |
           Nome_do_municipio == "MURIAÉ" | Nome_do_municipio == "CATAGUASES" | Nome_do_municipio == "SANTOS DUMONT"|
           Nome_do_municipio == "VIÇOSA" | Nome_do_municipio == "LEOPOLDINA" | Nome_do_municipio == "CARANGOLA")

export(vacinometro, "vacina/flourish-classificacaovacina.csv")


