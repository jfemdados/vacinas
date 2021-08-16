### SCRIPT SÓ PARA OS ATRASOS DA ASTRAZENECA EM JF

#Objetivo: o intuito desse script é criar análises sobre a vacinação em 
#Juiz de Fora, Minas Gerais

#Autor: Matheus Valentim

#Importacao de dados -------------------

basedosdados::set_billing_id('double-voice-305816')

base_completa<- basedosdados::read_sql("SELECT id_paciente, idade_paciente,
referencia_fabricante_vacina, id_municipio_estabelecimento, data_aplicacao_vacina,
nome_fabricante_vacina,
nome_fantasia_estabelecimento, 
dose_vacina, lote_vacina, categoria_vacina,
sexo_paciente, raca_cor_paciente
FROM `basedosdados.br_ms_vacinacao_covid19.microdados`
WHERE id_municipio_estabelecimento = '3136702'", 'double-voice-305816')

#5) Vacinas astrazeneca atrasadas

astrazeneca <- base_completa %>% 
  select(id_paciente, data_aplicacao_vacina, nome_fabricante_vacina, dose_vacina,
         raca_cor_paciente, sexo_paciente, idade_paciente, referencia_fabricante_vacina) %>%
  #havia mais de um nome para uma mesma marca de vacina
  mutate(nome_fabricante_vacina_novo = 
           case_when(nome_fabricante_vacina == "SINOVAC LIFE SCIENCE CO LTD" ~ "FUNDACAO BUTANTAN",
                     nome_fabricante_vacina == "MINISTERIO DA SAUDE" ~ "Sem identificação",
                     nome_fabricante_vacina == "SERUM INSTITUTE OF INDIA LTD" ~ "Astrazeneca",
                     nome_fabricante_vacina == "FUNDACAO OSWALDO CRUZ" ~ "Astrazeneca",
                     TRUE ~ nome_fabricante_vacina)) %>%
  filter(nome_fabricante_vacina_novo == 'Astrazeneca') %>% 
  distinct(id_paciente, dose_vacina, .keep_all = TRUE) 

# Filtrando a base para criar uma base de primeira e uma base de segunda dose

primeira_dose_astrazeneca<-astrazeneca %>% 
  select(id_paciente, data_aplicacao_vacina, dose_vacina, sexo_paciente, raca_cor_paciente, 
         idade_paciente) %>% 
  filter(dose_vacina =="1ª\xa0Dose") %>% 
  mutate(primeira_dose = 1) %>%
  rename(data_primeira_dose = data_aplicacao_vacina) %>% 
  select(-dose_vacina)

segunda_dose_astrazeneca<-astrazeneca %>% 
  select(id_paciente, data_aplicacao_vacina,dose_vacina) %>% 
  filter(dose_vacina =="2ª\xa0Dose") %>% 
  mutate(segunda_dose = 1) %>%
  rename(data_segunda_dose = data_aplicacao_vacina) %>% 
  select(-dose_vacina)


nova_astra <- primeira_dose_astrazeneca %>% 
  left_join(segunda_dose_astrazeneca, by = 'id_paciente') %>% 
  mutate(data_primeira_dose = lubridate::ymd(data_primeira_dose),
         data_segunda_dose = lubridate::ymd(data_segunda_dose),
         data_adequada_segunda = data_primeira_dose + 90,
         atrasada = case_when((data_segunda_dose > data_adequada_segunda) |
                                (segunda_dose == is_null(segunda_dose) & 
                                   data_adequada_segunda < 2021-07-31)~ 1))
