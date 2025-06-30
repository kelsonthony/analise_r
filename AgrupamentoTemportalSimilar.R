# Instalar pacotes se necessário:
# install.packages(c("readxl", "dplyr", "tidyr", "data.table", "ggplot2", "purrr", "TraMineR", "cluster", "tibble", "lubridate"))

library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(purrr)
library(TraMineR)
library(cluster)
library(tibble)
library(lubridate)

# 1. Leitura e preparação
df <- read_excel("TABELAS_coleta_HRT_formatado.xlsx")
names(df) <- trimws(names(df))

# 2. Selecionar colunas de interesse
colunas_data <- c("ID RN",
                  "UTIN 1 (INT)", "UTIN 2 (INT)", "UTIN 3 (INT)",
                  "UCINCO 1 (INT)", "UCINCO 2 (INT)", "UCINCO 3 (INT)",
                  "UCINCA (INT)", "UCINCA 2 (INT)", "UCINCA 3 (INT)")

df_long <- df %>%
  select(all_of(colunas_data)) %>%
  filter(!is.na(`ID RN`)) %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.Date(.)))) %>%
  pivot_longer(cols = -`ID RN`, names_to = "unidade", values_to = "data_inicio") %>%
  filter(!is.na(data_inicio)) %>%
  arrange(`ID RN`, data_inicio) %>%
  group_by(`ID RN`) %>%
  mutate(data_fim = lead(data_inicio)) %>%
  ungroup() %>%
  group_by(`ID RN`) %>%
  mutate(data_fim = if_else(is.na(data_fim), max(data_inicio), data_fim)) %>%
  ungroup()

# 3. Expandir por dias
df_expandido <- df_long %>%
  mutate(dia = map2(data_inicio, data_fim, ~ {
    if (.x < .y) {
      seq(.x, .y - 1, by = "day")
    } else {
      .x
    }
  })) %>%
  unnest(dia)

# 4. Manter apenas a última unidade por dia
df_diaria <- df_expandido %>%
  group_by(`ID RN`, dia) %>%
  slice_tail(n = 1) %>%
  ungroup()

# 5. Criar coluna de semana epidemiológica
df_diaria <- df_diaria %>%
  mutate(semana = paste0("S", isoweek(dia), "/", year(dia)))

# 6. Selecionar a última unidade registrada na semana
df_semanal <- df_diaria %>%
  group_by(`ID RN`, semana) %>%
  slice_tail(n = 1) %>%
  ungroup()

# 7. Gerar grade completa com OUT
semanas_completas <- sort(unique(df_semanal$semana))
todos_rns <- unique(df_semanal$`ID RN`)
grade_semana <- expand_grid(`ID RN` = todos_rns, semana = semanas_completas)

df_completo <- grade_semana %>%
  left_join(df_semanal %>% select(`ID RN`, semana, unidade), by = c("ID RN", "semana")) %>%
  mutate(unidade = ifelse(is.na(unidade), "OUT", unidade))

# 8. Formato largo (pivot_wider corrigido)
df_wide <- df_completo %>%
  pivot_wider(names_from = semana, values_from = unidade, values_fill = list(unidade = "OUT"))

# 9. Matriz de sequência
matriz_seq <- df_wide %>%
  filter(!duplicated(`ID RN`)) %>%
  column_to_rownames("ID RN")

# 10. Criar objeto de sequência
seq_obj <- seqdef(matriz_seq, xtstep = 4)

# 11. Distância OM
dist_mat <- seqdist(seq_obj, method = "OM", indel = 1, sm = "CONSTANT")

# 12. Agrupamento por Ward
agrup <- agnes(dist_mat, diss = TRUE, method = "ward")
clusters <- cutree(agrup, k = 4)

# 13. Gráfico de sequência por cluster
seqIplot(seq_obj,
         group = clusters,
         sortv = "from.start",
         border = NA,
         with.legend = "right",
         main = "Clusters de Trajetória Semanal nas Unidades",
         ylab = "Recém-nascidos")

         