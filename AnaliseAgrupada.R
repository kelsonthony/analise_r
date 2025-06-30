# Instalar pacotes se necessário
#install.packages(c("data.table", "readxl", "dplyr", "ggplot2"))

# Carregar bibliotecas
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)

# Ler dados
df <- read_excel("TABELAS_coleta_HRT_formatado.xlsx")
names(df) <- trimws(names(df))  # limpar espaços extras

# Selecionar colunas de interesse
colunas_data <- c("ID RN",
                  "UTIN 1 (INT)", "UTIN 2 (INT)", "UTIN 3 (INT)",
                  "UCINCO 1 (INT)", "UCINCO 2 (INT)", "UCINCO 3 (INT)",
                  "UCINCA (INT)", "UCINCA 2 (INT)", "UCINCA 3 (INT)")

# Transformar para formato longo
df_long <- df %>%
  select(all_of(colunas_data)) %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.Date(.)))) %>%
  tidyr::pivot_longer(cols = -`ID RN`, names_to = "unidade", values_to = "data_inicio") %>%
  filter(!is.na(data_inicio)) %>%
  arrange(`ID RN`, data_inicio) %>%
  group_by(`ID RN`) %>%
  mutate(data_fim = lead(data_inicio)) %>%
  ungroup() %>%
  group_by(`ID RN`) %>%
  mutate(data_fim = if_else(is.na(data_fim), max(data_inicio), data_fim)) %>%
  ungroup()

# Converter para data.table
fullbase <- as.data.table(df_long)
fullbase[, Data_inicio := min(data_inicio), by = `ID RN`]
fullbase[, Dif_dias_entrada := as.numeric(difftime(data_inicio, Data_inicio, units = "days"))]
fullbase[, Dif_dias_saida   := as.numeric(difftime(data_fim,     Data_inicio, units = "days"))]
fullbase[, semana_entrada := ceiling(Dif_dias_entrada / 7)]
fullbase[, semana_saida   := ceiling(Dif_dias_saida / 7)]
fullbase[semana_entrada == 0, semana_entrada := 1]
fullbase[, semana := mapply(seq, semana_entrada, semana_saida, SIMPLIFY = FALSE)]

# Expandir semanas por linha
expanded <- fullbase[, .(semana = unlist(semana)), by = .(`ID RN`)]
expanded_unique <- unique(expanded)
expanded_unique[, estado := "H"]

# Criar matriz semanal
pivot_table <- dcast(expanded_unique, `ID RN` ~ semana, value.var = "estado", fill = "NH")

# Remover linhas com ID NA para evitar inconsistência
pivot_table <- pivot_table[!is.na(pivot_table$`ID RN`), ]

# Converter H/NH para 1/0
pivot_numeric_matrix <- as.data.frame(pivot_table)
pivot_numeric_matrix <- pivot_numeric_matrix[!is.na(pivot_numeric_matrix$`ID RN`), ]
rownames(pivot_numeric_matrix) <- pivot_numeric_matrix$`ID RN`
pivot_numeric_matrix <- pivot_numeric_matrix[, -1]
pivot_numeric_matrix[] <- lapply(pivot_numeric_matrix, function(x) ifelse(x == "H", 1, 0))

# Aplicar K-means
set.seed(123)
kmeans_result <- kmeans(pivot_numeric_matrix, centers = 3, nstart = 25)

# Adicionar cluster ao pivot_table
pivot_table$cluster <- as.factor(kmeans_result$cluster)

# Voltar cluster para df_long
df_long_clustered <- df_long %>%
  mutate(`ID RN` = as.character(`ID RN`)) %>%
  left_join(pivot_table %>%
              mutate(`ID RN` = as.character(`ID RN`)) %>%
              select(`ID RN`, cluster),
            by = "ID RN")


# Gráfico elegante por cluster
grafico <- ggplot(df_long_clustered, aes(x = data_inicio, xend = data_fim,
                                         y = forcats::fct_reorder(`ID RN`, as.numeric(cluster)),
                                         yend = forcats::fct_reorder(`ID RN`, as.numeric(cluster)),
                                         color = cluster)) +
  geom_segment(linewidth = 1.5, alpha = 0.8) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months") +
  labs(
    title = "Trajetórias dos recém-nascidos agrupadas por padrão semanal",
    subtitle = "Clusters baseados na presença semanal de hospitalização (H/NH)",
    x = "Data de Internação",
    y = "ID do Recém-nascido",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Exibir gráfico
print(grafico)

# Salvar gráfico
ggsave("trajetoria_por_cluster.png", plot = grafico,
       width = 14, height = 10, dpi = 400)
