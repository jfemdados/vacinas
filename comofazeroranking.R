# VACINA-JÁ
library(tidyverse)
setwd("~/Documentos/jfemdados")
vacinometro <- rio::import("vacina/mapa_vacina.xlsx")%>%
  filter(Nome_da_meso  == "Zona da Mata",
         !Eficiencia > 100)%>%
  arrange(desc(Eficiencia))%>%
  filter(Nome_do_municipio %in% c("JUIZ DE FORA", "ALÉM PARAÍBA" , "UBÁ" , "MURIAÉ",
                                  "CATAGUASES", "SANTOS DUMONT", "VIÇOSA", "LEOPOLDINA", "CARANGOLA"))

export(vacinometro, "vacina/flourish-classificacaovacina.csv")


