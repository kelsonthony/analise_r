# Pacotes
library(dplyr)
library(data.table)
library(cluster)
library(ggplot2)
library(tidyr)

# Carrega a matriz de trajetória (trajetoria.csv)
traj <- fread("Trajetoria_neonatos.csv")

# Derreter o formato largo para longo
traj_long <- traj %>%
  pivot_longer(-n_neonato, names_to = "semana", values_to = "estado") %>%
  mutate(semana = as.integer(semana))

# Contar tempo total de cada paciente em cada estado
tempo_por_estado <- traj_long %>%
  group_by(n_neonato, estado) %>%
  summarise(tempo = n(), .groups = "drop") %>%
  pivot_wider(names_from = estado, values_from = tempo, values_fill = 0)

# Substituir NAs por zero caso existam
tempo_por_estado[is.na(tempo_por_estado)] <- 0

# Remover coluna ID para clustering
matriz_cluster <- tempo_por_estado %>% select(-n_neonato)

# Clustering hierárquico
dist_tempo <- dist(matriz_cluster)
agrupamento <- agnes(dist_tempo, method = "ward")
grupo_tempo <- cutree(agrupamento, k = 3)  # ou outro valor

# Anexar grupo ao dataframe
tempo_por_estado$grupo <- as.factor(grupo_tempo)

# 📊 Visualizar estatísticas por grupo
summary_por_grupo <- tempo_por_estado %>%
  group_by(grupo) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

print(summary_por_grupo)

# 📈 Visualizar com gráfico de barras médias por grupo
library(tidyr)

summary_por_grupo_long <- summary_por_grupo %>%
  pivot_longer(-grupo, names_to = "estado", values_to = "tempo_medio")

ggplot(summary_por_grupo_long, aes(x = estado, y = tempo_medio, fill = grupo)) +
  geom_col(position = "dodge") +
  labs(title = "Tempo médio em cada estado por grupo de similaridade temporal",
       x = "Estado", y = "Tempo médio (semanas)") +
  theme_minimal()

# ✅ Salvar resultado
write.csv2(tempo_por_estado, "agrupamento_por_tempo.csv", row.names = FALSE)
