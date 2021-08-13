#Objetivo: o intuito desse script é criar análises sobre a vacinação em 
#Juiz de Fora, Minas Gerais

#Autor: Matheus Valentim

#Importacao de dados -------------------

basedosdados::set_billing_id('double-voice-305816')

base_completa<- basedosdados::read_sql("SELECT id_municipio_estabelecimento, data_aplicacao_vacina,
nome_fabricante_vacina,
nome_fantasia_estabelecimento, 
dose_vacina, lote_vacina, categoria_vacina,
sexo_paciente, raca_cor_paciente
FROM `basedosdados.br_ms_vacinacao_covid19.microdados`
WHERE id_municipio_estabelecimento = '3136702'", 'double-voice-305816')


#Tratamento e criação de bases desejadas ------------------------
library(tidyverse)

#1) Qual fabricante vem sendo aplicada?

fabricantes <- base_completa %>% 
  select(data_aplicacao_vacina, nome_fabricante_vacina, dose_vacina) %>%
  #havia mais de um nome para uma mesma marca de vacina
  mutate(nome_fabricante_vacina = 
           case_when(nome_fabricante_vacina == "SINOVAC LIFE SCIENCE CO LTD" ~ "FUNDACAO BUTANTAN",
                     nome_fabricante_vacina == "MINISTERIO DA SAUDE" ~ "Sem identificação",
                     nome_fabricante_vacina == "SERUM INSTITUTE OF INDIA LTD" ~ "Astrazeneca",
                     nome_fabricante_vacina == "FUNDACAO OSWALDO CRUZ" ~ "Astrazeneca",
                     TRUE ~ nome_fabricante_vacina),
         mes = str_sub(data_aplicacao_vacina,1,7)) %>%
  #agrupando por data e nome do fabricante
  group_by(mes, nome_fabricante_vacina) %>% 
  count() %>%
  #pivotando para ajudar o flourish
  pivot_wider(id_cols = c('mes', 'nome_fabricante_vacina'),
              names_from = nome_fabricante_vacina,
              values_from = n) %>% 
  arrange(mes)

#exportando base de fabricantes por dia
rio::export(fabricantes, 'fabricantes.csv')

#link flourish: https://app.flourish.studio/visualisation/6911886/edit


#2) Onde são aplicadas as vacinas

local <- base_completa %>% 
  select(data_aplicacao_vacina,nome_fantasia_estabelecimento) %>%
  mutate(mes = str_sub(data_aplicacao_vacina,1,7)) %>% 
  group_by(nome_fantasia_estabelecimento) %>% 
  count() %>% 
  arrange(desc(n)) 

rio::export(local, 'local-vacina.csv')

#3) Primeira, segunda e dose única

dose <- base_completa %>% 
  select(data_aplicacao_vacina, dose_vacina) %>% 
  mutate(mes = str_sub(data_aplicacao_vacina,1,7)) %>% 
  group_by(mes, dose_vacina) %>%
  count() %>% 
  pivot_wider(id_cols = c("mes", "dose_vacina"),
              names_from = dose_vacina,
              values_from = n) %>% 
  arrange(mes)
rio::export(dose, "dose-vacina.csv")  

#4) Tá faltando a segunda dose ein

deficit<- dose %>% 
  set_names(c("mes","primeira", "segunda", "unica"))

deficit_final <- tibble(mes = deficit$mes, ac_primeira = cumsum(deficit$primeira),
       ac_segunda = cumsum(deficit$segunda)) %>% 
  mutate(deficit_segunda = ac_primeira - ac_segunda)

#exportando

rio::export(deficit_final, 'deficit2dose.csv')









