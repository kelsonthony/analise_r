### =============================================
### ANÁLISE DESCRITIVA E GRÁFICOS DE TRAJETÓRIA
### =============================================

# Pacotes necessários
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("ggplot2")

library(data.table)
library(dplyr)
library(ggplot2)

# 1. Carregar a matriz de trajetória
traj <- fread("Trajetoria_neonatos.csv")

# 2. Transformar em formato longo
traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.numeric(as.character(traj_long$semana))

# 3. Resumo por semana e estado
resumo <- traj_long %>%
  group_by(semana, estado) %>%
  summarise(n = n(), .groups = "drop")

# ============================================
# 📈 GRÁFICO DE LINHAS – Evolução semanal
# ============================================

grafico_linhas <- ggplot(resumo, aes(x = semana, y = n, color = estado)) +
  geom_line(size = 1.2) +
  labs(
    title = "Evolução Semanal por Estado",
    x = "Semana",
    y = "Número de Pacientes",
    color = "Estado"
  ) +
  theme_minimal()

ggsave("grafico_linhas_trajetoria.png", grafico_linhas, width = 10, height = 6, dpi = 300)

# ============================================
# 📊 GRÁFICO DE BARRAS EMPILHADAS
# ============================================

grafico_barras <- ggplot(resumo, aes(x = semana, y = n, fill = estado)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribuição por Estado em Cada Semana",
    x = "Semana",
    y = "Número de Pacientes",
    fill = "Estado"
  ) +
  theme_minimal()

ggsave("grafico_barras_empilhadas_trajetoria.png", grafico_barras, width = 10, height = 6, dpi = 300)

# ============================================
# 🔥 HEATMAP – Estado vs Semana
# ============================================

grafico_heatmap <- ggplot(resumo, aes(x = semana, y = estado, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(
    title = "Heatmap: Estado x Semana",
    x = "Semana",
    y = "Estado",
    fill = "Nº Pacientes"
  ) +
  theme_minimal()

ggsave("heatmap_estado_semana_trajetoria.png", grafico_heatmap, width = 10, height = 6, dpi = 300)

# ============================================
# ✅ Final
# ============================================

message("Gráficos gerados com sucesso:")
message("- grafico_linhas_trajetoria.png")
message("- grafico_barras_empilhadas_trajetoria.png")
message("- heatmap_estado_semana_trajetoria.png")
