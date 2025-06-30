# Carregar pacotes
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)  # para formatar datas no eixo x

# Caminho do arquivo Excel
arquivo <- "TABELAS_coleta_HRT_formatado.xlsx"

# Ler e preparar os dados
df <- read_excel(arquivo)
names(df) <- str_trim(names(df))  # remover espaços nos nomes

# Selecionar colunas relevantes
colunas_data <- c("ID RN",
                  "UTIN 1 (INT)", "UTIN 2 (INT)", "UTIN 3 (INT)",
                  "UCINCO 1 (INT)", "UCINCO 2 (INT)", "UCINCO 3 (INT)",
                  "UCINCA (INT)", "UCINCA 2 (INT)", "UCINCA 3 (INT)")

df_filtrado <- df %>% select(all_of(colunas_data))

# Converter para data (caso venham como texto)
df_filtrado <- df_filtrado %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.Date(.))))

# Transformar em formato longo
df_long <- df_filtrado %>%
  pivot_longer(cols = -`ID RN`,
               names_to = "unidade",
               values_to = "data_inicio") %>%
  filter(!is.na(data_inicio)) %>%
  arrange(`ID RN`, data_inicio) %>%
  group_by(`ID RN`) %>%
  mutate(data_fim = lead(data_inicio)) %>%
  ungroup()

# Substituir NA em data_fim pela última data do RN
ultima_data <- df_long %>%
  group_by(`ID RN`) %>%
  summarise(ultima_data = max(data_inicio), .groups = "drop")

df_long <- df_long %>%
  left_join(ultima_data, by = "ID RN") %>%
  mutate(data_fim = if_else(is.na(data_fim), ultima_data, data_fim)) %>%
  select(-ultima_data)

# Reordenar RN com base na primeira data de internação
df_ordem <- df_long %>%
  group_by(`ID RN`) %>%
  summarise(primeira_data = min(data_inicio), .groups = "drop")

df_long <- df_long %>%
  left_join(df_ordem, by = "ID RN") %>%
  mutate(`ID RN` = fct_reorder(as.character(`ID RN`), primeira_data)) %>%
  select(-primeira_data)

# Criar gráfico elegante
grafico <- ggplot(df_long, aes(x = data_inicio, xend = data_fim,
                               y = `ID RN`, yend = `ID RN`,
                               color = unidade)) +
  geom_segment(linewidth = 1.8, alpha = 0.8) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months", expand = c(0.01, 0.01)) +
  labs(
    title = "Trajetória dos recém-nascidos nas unidades neonatais",
    subtitle = "Cada linha representa o tempo de permanência por unidade",
    x = "Data de Internação",
    y = "ID do Recém-nascido",
    color = "Unidade"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Mostrar na tela
print(grafico)

# Salvar imagem de alta qualidade
ggsave("trajetoria_recem_nascidos_elegante.png", plot = grafico,
       width = 14, height = 10, dpi = 400)
