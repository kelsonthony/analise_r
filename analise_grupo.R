# ===============================
# ANÁLISE DE TRAJETÓRIAS COM TraMineR (com Silhouette)
# ===============================

# Carregar pacotes
library(TraMineR)
library(data.table)
library(cluster)
library(factoextra)
library(ggplot2)
library(reshape2)
library(dplyr)

# Criar pasta de saída
dir_out <- "saida-img"
if (!dir.exists(dir_out)) dir.create(dir_out)

# 1. Leitura
traj <- fread("Trajetoria_neonatos.csv")
week_cols <- as.character(1:30)
estados <- c("ALTA", "ENF/ALCON", "UCINCO", "UCINCA", "UTIN", "ÓBITO")
traj[, (week_cols) := lapply(.SD, factor, levels = estados), .SDcols = week_cols]

# 2. Objeto de sequência
seq_obj <- seqdef(traj[, ..week_cols], alphabet = estados, states = estados, right = "DEL")

# 3. Distribuição Geral
png(file.path(dir_out, "plot_distribuicao_geral.png"), width = 1000, height = 600)
seqfplot(seq_obj, with.legend = "right", main = "Distribuição de Estados por Semana")
dev.off()

# 4. Matriz de distância
dist_mat <- seqdist(seq_obj, method = "OM", indel = 1, sm = "CONSTANT")

# 5. Escolher número ideal de grupos (Silhouette)
png(file.path(dir_out, "escolha_numero_grupos_silhouette.png"), width = 1000, height = 600)
fviz_nbclust(as.matrix(dist_mat), FUN = hcut, method = "silhouette", k.max = 6) +
  labs(title = "Número Ótimo de Grupos - Método Silhouette")
dev.off()

# 6. Clustering (fixando k = 3 após visualização)
agnes_fit <- agnes(dist_mat, diss = TRUE, method = "ward")
traj$grupo <- cutree(agnes_fit, k = 5)

# 7. Dendrograma
png(file.path(dir_out, "dendrograma_trajetorias.png"), width = 1000, height = 600)
plot(agnes_fit, which.plots = 2, main = "Dendrograma das Trajetórias")
dev.off()

# 8. Gráfico por grupo
png(file.path(dir_out, "trajetorias_por_grupo.png"), width = 1000, height = 600)
seqdplot(seq_obj, group = traj$grupo, border = NA, main = "Trajetórias por Grupo")
dev.off()

# 9. Heatmap de estados por grupo
traj_melt <- melt(traj[, c("grupo", ..week_cols)], id.vars = "grupo", variable.name = "semana", value.name = "estado")
traj_summary <- traj_melt %>%
  group_by(grupo, estado) %>%
  summarise(prop = n() / nrow(traj), .groups = "drop")

png(file.path(dir_out, "heatmap_por_grupo.png"), width = 1000, height = 600)
ggplot(traj_summary, aes(x = grupo, y = estado, fill = prop)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Distribuição Média de Estados por Grupo", x = "Grupo", y = "Estado") +
  theme_minimal()
dev.off()

# 10. Curva acumulada dos estados
traj_long <- melt(traj[, c("grupo", ..week_cols)], id.vars = "grupo", variable.name = "semana", value.name = "estado")
traj_long$semana <- as.integer(as.character(traj_long$semana))

curva_estados <- traj_long %>%
  group_by(semana, estado) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(semana) %>%
  mutate(prop = freq / sum(freq))

png(file.path(dir_out, "curva_acumulada_estados.png"), width = 1000, height = 600)
ggplot(curva_estados, aes(x = semana, y = prop, fill = estado)) +
  geom_area(alpha = 0.7, color = "white") +
  labs(title = "Proporção Acumulada dos Estados por Semana", x = "Semana", y = "Proporção") +
  theme_minimal()
dev.off()

# 11. Salvar base com cluster
fwrite(traj, "Trajetoria_neonatos_cluster.csv")

# ✅ Final
message("✅ Script finalizado com sucesso.")
message("📁 Saída de imagens: ", dir_out)
