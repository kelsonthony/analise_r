install.packages("data.table") # Install data.table
install.packages("tidyr") # Install data.table
install.packages("openxlsx")

library(data.table)
library(tidyr)
library(dplyr)
library(readxl)
library(openxlsx)

dados <- read_excel("TABELAS_coleta_HRT_FINAL.xlsx", 
                    sheet = "RTrajet.", col_types = c("numeric", 
                                                      "date", "date", "text"))

###Variaveis
glimpse(dados)


#Mudando nome da coluna
colnames(dados)[colnames(dados) == "1.0"] <- "N_INDIVIDUO"


##Criando variavel diferença entre datas (função 'difftime')
#Transformar a colunas em variaveis tipo data (função 'as.Date')
dados$dt_entrada <- as.Date(dados$dt_entrada, format = "%Y-%m-%d")
dados$dt_saida <- as.Date(dados$dt_saida, format = "%Y-%m-%d")

##data do primeiro atendimento (semana 1)
dados <- dados %>%
  group_by(n_neonato) %>%
  mutate(data_inicio = min(dt_entrada))


dados$Dif_dias_entrada <- as.numeric(difftime(dados$dt_entrada, dados$data_inicio, units = "days")) #diferença em dias
dados$Dif_dias_saida <- as.numeric(difftime(dados$dt_saida, dados$data_inicio, units = "days")) #diferença em dias
dados$Dif_dias_int <- as.numeric(difftime(dados$dt_saida, dados$dt_entrada, units = "days"))

#classificar as semanas  -- fiz somente semana entrada
dados$semana_entrada <- ceiling(dados$Dif_dias_entrada / 7) 
dados$semana_saida <- ceiling(dados$Dif_dias_saida / 7)


#alterar de semana 0 para semana 1
dados <- dados %>%
  mutate(semana_entrada = ifelse(semana_entrada == 0, 1, semana_entrada))

dados <- dados %>%
  mutate(semana_saida = ifelse(semana_saida == 0, 1, semana_saida))


# Substituindo valores
table(dados$nivel_assistencia)  ## saber o que tem preechido na coluna

dados <- dados %>%
  mutate(nivel_assistencia = ifelse(nivel_assistencia == "UCIN (HRT)", "UCIN", nivel_assistencia))

dados <- dados %>%
  mutate(nivel_assistencia = case_when(
    nivel_assistencia %in% c("UTIN (HRT)", "UTIN (HMIB)") ~ "UTIN",
    TRUE ~ nivel_assistencia
  ))

## transformando o nivel de assistencia em variavel numerica
## REMOVER UCIN
## ADD OBITO
dados <- dados %>%
  mutate(nivel_assist_num = case_when(
    nivel_assistencia == "ALTA" ~ 1,
    nivel_assistencia == "ENF/ALCON" ~ 2,
    nivel_assistencia == "UCINCO" ~ 3,
    nivel_assistencia == "UCINCA" ~ 4,
    nivel_assistencia == "UCIN" ~ 5, 
    nivel_assistencia == "UTIN" ~ 6,
    TRUE ~ 7  # Valor padrão para todos os outros casos
  ))

table(dados$nivel_assist_num)

## nova variavel para mais de uma internacao na mesma semana (considera a de mais dias)
dados <- dados %>%
  group_by(n_neonato, semana_entrada, semana_saida) %>%
  mutate(
    novo_nivel = if (all(is.na(Dif_dias_int))) {  # Se todos os valores forem NA
      NA_integer_
    } else {
      nivel_assist_num[which.max(Dif_dias_int)]  # Pega o maior Dif_dias_int
    }
  ) %>%
  ungroup()
 

# Calcular o valor máximo de 'NIVEL_ATENÇÃO_SAÚDE' para cada 'N_INDIVIDUO'
dados <- dados %>%
  group_by(n_neonato, semana_entrada) %>%
  mutate(nivel_assist_max = max(nivel_assist_num))

## Salvar nova tabela
write.csv2(dados, "Trajetoria_neonatos.csv")

dados <- fread("Trajetoria_neonatos.csv",
                        select = c("n_neonato", "dt_entrada", "dt_saida", "nivel_assistencia", "data_inicio", "Dif_dias_entrada",
                                   "Dif_dias_saida","Dif_dias_int",  "semana_entrada", "semana_saida", "nivel_assist_num", "novo_nivel"))


## criar sequencia de internacao por semana
# 1. Criar sequência de semanas para cada internação
dados <- dados %>%
  mutate(semana = mapply(seq, semana_entrada, semana_saida, SIMPLIFY = FALSE))



# 2. Expandir as listas de semanas
expanded <- dados[, .(semana = unlist(semana)), by = .(n_neonato, novo_nivel)]

# 3. Remover duplicados (mantendo apenas o último nível para semanas com múltiplos registros)
expanded_unique <- expanded[order(n_neonato, semana, -novo_nivel)] %>%  # Ordena para pegar o maior nível
  distinct(n_neonato, semana, .keep_all = TRUE)  # Mantém apenas o primeiro (que será o maior nível)

# 4. Mapear os níveis para estados
## REMOVER UCIN
## ADD OBITO
nivel_to_estado <- c("1" = "ALTA", "2" = "ENF/ALCON", "3" = "UCINCO", 
                     "4" = "UCINCA", "5" = "UCIN", "6" = "UTIN")

expanded_unique[, estado := nivel_to_estado[as.character(novo_nivel)]]

# 5. Criar tabela dinâmica com preenchimento inteligente
pivot_table <- expanded_unique %>%
  arrange(n_neonato, semana) %>%  # Ordenar por neonato e semana
  group_by(n_neonato) %>%
  complete(semana = full_seq(semana, period = 1)) %>%  # Completar semanas faltantes
  fill(estado, .direction = "down") %>%  # Preencher para frente
  mutate(estado = if_else(is.na(estado), "ALTA", estado)) %>%  # Preencher NAs remanescentes
  pivot_wider(
    id_cols = n_neonato,
    names_from = semana,
    values_from = estado,
    values_fill = "ALTA"
  )

# Converter para data.table se necessário
pivot_table <- as.data.table(pivot_table)


## 1. Processamento dos dados de óbito
# Selecionar apenas registros de óbito (novo_nivel == 7)
patient_death <- dados[novo_nivel == 7, .(n_neonato, semana_entrada, novo_nivel)]

# Garantir apenas um registro por neonato (mantendo a semana mais recente)
patient_death <- patient_death[order(n_neonato, -semana_entrada)]
patient_death <- patient_death[, .SD[1], by = n_neonato]

# Renomear coluna
setnames(patient_death, "semana_entrada", "semana_obito")

## 2. Junção com a tabela pivot
# Fazer merge mantendo todos os registros da pivot_table
pivot_table_obito <- merge(pivot_table, patient_death[, .(n_neonato, semana_obito)], 
                           by = "n_neonato", all.x = TRUE)

## 3. Identificar colunas de semana
week_cols <- names(pivot_table_obito)[grepl("^[0-9]+$", names(pivot_table_obito))]

## 4. Marcar semanas pós-óbito com "D"
# Converter para data.table se não for
setDT(pivot_table_obito)

# Aplicar a marcação de óbito
for(col in week_cols) {
  pivot_table_obito[!is.na(semana_obito) & as.numeric(col) >= semana_obito, 
                    (col) := "ÓBITO"]
}

## 5. Limpeza final
# Remover coluna auxiliar se necessário
pivot_table_obito[, semana_obito := NULL]

## Salvar tabela
trajetoria <- pivot_table_obito[, 1:26]
write.xlsx(trajetoria, "trajetoria.xlsx")
