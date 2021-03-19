library(tidyverse)

#Vacinação Open Data Sus
library(readr)
vacinacao <- read_csv2("dados/vacinação.csv")%>%
  

###  Doses Disponibilizadas - SES

library(httr)
library(rvest)

doses_por_municipio<- GET(url = "https://vacinaminas.mg.gov.br/doses-por-municipios/")%>%
  read_html()%>%
  html_table()

doses_disponibilizadas_total<- doses_por_municipio[[1]]%>%
    mutate(Quantidade= str_remove_all(Quantidade, "\\."))%>%
  mutate(Quantidade= as.numeric(Quantidade),
        Frascos= as.numeric(str_extract(Material, "\\d{2,}\\s")),
        QuantidadeReal= Quantidade*Frascos)%>%
  group_by(Macro, URS, Município)%>%
  tally(Quantidade, name="DosesDisponibilizadas")%>%
  mutate(Município= str_to_title(abjutils::rm_accent(Município)))%>%
  ungroup(Macro, URS, Município)%>%
  select(Município, DosesDisponibilizadas)

#doses_disponibilizadas_por_fabricante<- doses_por_municipio[[1]]%>%
 # mutate(Quantidade= str_remove_all(Quantidade, "\\."))%>%
  #mutate(Quantidade= as.numeric(Quantidade),
   #      Frascos= as.numeric(str_extract(Material, "\\d{2,}\\s")),
    #     QuantidadeReal= Quantidade*Frascos)%>%
  #group_by(Macro, URS, Município, Fabricante)%>%
  #tally(Quantidade, name="DosesDisponibilizadas")

#Vacinação  - Vacionmetro SES
library(readxl)
doses_aplicadas_raw <- read_excel("dados/xlsx_vacinometro.xlsx")


#doses_aplicadas_por_dose<- doses_aplicadas_raw%>%
#  group_by(MUNICIPIO_RESIDENCIA, URS, MICRO, MACRO, CodigoIBGE, Dose)%>%
 # tally(`Doses Aplicadas`, name= "DosesAplicadas")

doses_aplicadas_total<- doses_aplicadas_raw%>%
  group_by(MUNICIPIO_RESIDENCIA, URS, MICRO, MACRO, CodigoIBGE)%>%
  tally(`Doses Aplicadas`, name= "DosesAplicadas")%>%
  rename("Município" = MUNICIPIO_RESIDENCIA)%>%
  mutate(Município= str_to_title(abjutils::rm_accent(Município)))%>%
  ungroup(Município, URS, MICRO, MACRO)
  

#Juntar bases Disiponibilziadas e Efetivadas

doses_totais_completo <- full_join(doses_aplicadas_total, doses_disponibilizadas_total, by= "Município")%>%
  mutate(Eficiencia = DosesAplicadas/DosesDisponibilizadas*100)%>%
  mutate(EficienciaGroups= cut(Eficiencia, c(0,20,40,60,80,100,200)))




#    MAPAS


### CRIANDO GEOMS_MG - POLIGONOS DO MAPA DE MG         ####
library(readxl)
Base_informacoes_setores2010_sinopse_MG <- read_excel("~/R2/eleicoes_jf/base_ibge/Base_informacoes_setores2010_sinopse_MG.xls")

base_ibge_mg <-  Base_informacoes_setores2010_sinopse_MG%>%
  select(Cod_setor:Nome_do_bairro)%>%
  filter(Nome_da_UF== "Minas Gerais")%>%
  group_by(Nome_da_meso, Cod_meso, Nome_da_micro, Cod_micro, Cod_municipio)%>%
  count(Nome_do_municipio)%>%
  rename("code_muni"= Cod_municipio)%>%
  mutate(code_muni= as.numeric(code_muni))

#Fazendo Polígonos pelo Geobr::
library(ggplot2)
library(geobr)
geoms_mg<- geobr::read_municipality(code_muni = "MG")%>%
  inner_join(base_ibge_mg, by= "code_muni")%>%
  sf::st_simplify(dTolerance = 0.005)%>%
  mutate(name_muni= str_to_title(abjutils::rm_accent(name_muni)))
  

ggplot() + geom_sf(data=geoms_mg , size= .15, 
                   show.legend = F) + geom_sf(data=geoms_mg, aes(fill= Nome_da_meso))

##### ######
#Mapa Vacinação
library(ggplot2)
library(readr)


mapa_vacina<- geoms_mg%>%
  full_join(select(doses_totais_completo, DosesAplicadas, DosesDisponibilizadas, Eficiencia, Município, EficienciaGroups),
             by= c("name_muni" = "Município"))%>%
  tidyr::drop_na(code_muni)




  mapa_vacina%>%
  filter(Nome_da_meso== "Zona da Mata")%>%
  ggplot() + geom_sf(aes( fill= EficienciaGroups), colour= "gray20", size= .15,  show.legend = TRUE) +
    theme_void()+
  #scale_fill_brewer()
  scale_fill_manual( name= "Proporção Vacinas:\nAplicadas/Disponibilizadas",
                     label= c("Menos de 20%", "20% a 40%","40% a 60%","60% a 80%","80% a 100%", "Acima de 100%\nErros nos Dados"),
                     values= c("red2","red4", "yellow", "limegreen", "green4", "gray30"))+
  #labs(title = "Eficiência na Vacinação: Os Municípios estão Aplicando as Vacinas que Recebem?", 
   #     subtitle= "Nº de Vacinas enviadas aos Municípios dividido Pelas nº de Vacinas Aplicadas")





saveRDS(mapa_vacina,"dados/mapa_vacina.r" )
writexl::write_xlsx(mapa_vacina, "dados/mapa_vacina.xlsx")

###  GRAFICOS ##############
library(ggplot2)
library(readr)


vacinajfpordia <- read_csv("dados/vacinajfpordia.csv")

vacinajfpordia%>%
  ggplot(aes(x=Data, y=doses_totais)) + geom_line() + theme_minimal()+
  labs(title="Numero de Vacinas entregue a Juiz de Fora", subtitle= "Por Dia")

doses_por_municipio2%>%
  filter(Município %in% c("BELO HORIZONTE", "UBERLANDIA", "JUIZ DE FORA", "CONTAGEM","BETIM"))%>%
  ggplot(aes(x= Município, y= Disponibilizadas)) +geom_col()
