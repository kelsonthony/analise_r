## Pacotes necessarios para rodar os códigos
install.packages("data.table") # instala o pacote no r
library("data.table") # Libera o pacote para uso 
install.packages("tidyr") 
library(tidyr)
install.packages("dplyr") 
library(dplyr)
install.packages("readxl") 
library(readxl)


### para trazer a planilha para o r
# o select coloque o nome das colunas que você deseja importar - apenas no caso não seja todas
Planilha<- read_excel("TABELAS_coleta_HRT_FINAL.xlsx")

# o sheet é para selecionar a planilha do ecxel quando o arquivo tiver mais de 1 - coloque o nome que está no excel
prenatal_adequado <- read_excel("prenatal_adequado - Copia.xlsx", sheet = "metodo_01_indicador")



### Para mudar o nome da coluna
# corrigir quando tem espaço, acentos etc.. - o novo nome fica depois do <- 
colnames(Planilha_tais_atualizada)[colnames(Planilha_tais_atualizada) == "INDIVIDUO N°"] <- "N_INDIVIDUO"



## Criando variavel diferença entre datas (função 'difftime')
# Transformar a colunas em variaveis tipo data (função 'as.Date')
Planilha_tais_atualizada$DATA_ATENDIMENTO <- as.Date(Planilha_tais_atualizada$DATA_ATENDIMENTO, format = "%Y-%m-%d")

## data do primeiro atendimento (semana 1)
Planilha_tais_atualizada <- Planilha_tais_atualizada %>%
  group_by(N_INDIVIDUO) %>%
  mutate(Data_inicio = min(DATA_ATENDIMENTO))

## Criando uma variavel com a diferença de dias entre o 1 e o ultimo dia de internação 
Planilha_Tais$Dif_dias_entrada <- as.numeric(difftime(Planilha_Tais$DATA_ATENDIMENTO, Planilha_Tais$Data_inicio, units = "days")) #diferença em dias


## classificar as semanas  
Planilha_Tais$semana_entrada <- ceiling(Planilha_Tais$Dif_dias_entrada / 7) 


## alterar de semana 0 para semana 1
Planilha_Tais[Planilha_Tais$semana_entrada == 0,semana_entrada:=1]

#faz a mesma coisa do código acima - pode rodar apenas 1 dos dois
Planilha_Tais <- Planilha_Tais %>%
  mutate(semana_entrada = ifelse(semana_entrada == 0, 1, semana_entrada)) 


# Criar uma lista para cada linha
fullbase[, semana := mapply(seq, semana_entrada, semana_saida, SIMPLIFY = FALSE)]

# expandir essa lista
expanded <- fullbase[, .(semana = unlist(semana)), by = c('id_sinasc', 'gestacao', 'peso', 'semagestac')]

# remover os duplicados
expanded_unique <- unique(expanded)

# setar o estado default H - hospitalized
expanded_unique$estado <- "H"

# criar a tabela dinamica com as semanas como coluna e preencher as semanas vazias como NH - non hospitalized
pivot_table <- dcast(expanded_unique, id_sinasc + gestacao + peso + semagestac ~ semana, value.var = "estado", fill="NH")

####################
# separar datas de obito
patient_death <- fullbase[morte==1, .(id_sinasc, semana_entrada, morte, dt_inter_consolidado)]

#sort na coluna id_sinasc e semana_entrada
setorder(fullbase, id_sinasc, semana_entrada)

# Removendo duplicatas com base na coluna_especifica
patient_death <- unique(patient_death, by = "id_sinasc")

# renomeia a coluna semana_entrada para semana_obito
setnames(patient_death, "semana_entrada", "semana_obito")

# Relacionar as duas tabelas
pivot_table_obito <- pivot_table[patient_death, on = .(id_sinasc), week_of_death := i.semana_obito]

# Obter os nomes das colunas que são semanas
week_cols <- names(pivot_table_obito)[grepl("^[0-9]+$", names(pivot_table_obito))]

# A função ifelse estava sendo aplicada de forma errada. Aqui está a correção:
for(col in week_cols) {
  # Converte o nome da coluna para numérico e compara com 'semana_obito'
  pivot_table_obito[as.numeric(col) >= week_of_death, (col) := "D"]
}

# filtrar apenas 
pivot_table_obito_prematuro <- pivot_table_obito[(semagestac>=23)&(semagestac<=36)]


rm(expanded, expanded_unique, patient_death, pivot_table, pivot_table_obito)

##Salvar tabela
pivot_table_obito_prematuro <- pivot_table_obito_prematuro[, 1:56]



#Salvar em excel apenas até a 52a semana
install.packages("openxlsx")
library(openxlsx)

write.xlsx(pivot_table_obito_prematuro[, 1:56, with = FALSE], "estrutura_final.xlsx")




