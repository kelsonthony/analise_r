
#install.packages("TraMineR")
#install.packages("data.table")
#install.packages("cluster")   # Para agrupamentos
#install.packages("factoextra") # Visualização de clusters

# Pacotes
library(TraMineR)
library(data.table)
library(cluster)
library(factoextra)

# Ler os dados de trajetória
traj <- fread("Trajetoria_neonatos.csv")

# Definir colunas de sequência (semanas)
week_cols <- as.character(1:30)  # Assumindo 30 semanas

# Converter para fator com os estados possíveis
estados <- c("ALTA", "ENF/ALCON", "UCINCO", "UCINCA", "UTIN", "ÓBITO")
traj[, (week_cols) := lapply(.SD, factor, levels = estados), .SDcols = week_cols]

# Criar objeto de sequência
seq_obj <- seqdef(traj[, ..week_cols], alphabet = estados, states = estados, right = "DEL")

# Visualização de frequências
seqfplot(seq_obj, with.legend = "right", main = "Distribuição de Estados por Semana")

# Frequência de sequências
seqtab(seq_obj, tlim = 10)  # Top 10 sequências mais comuns

# Índice de complexidade (opcional)
ic <- seqici(seq_obj)
summary(ic)

# Dissimilaridade e clustering (agrupamento)
dist_mat <- seqdist(seq_obj, method = "OM", indel = 1, sm = "CONSTANT")
cluster_fit <- agnes(dist_mat, diss = TRUE, method = "ward")

# Visualizar dendrograma
plot(cluster_fit, which.plots = 2, main = "Dendrograma das Trajetórias")

# Cortar em 3 grupos (exemplo) e adicionar ao dataframe
traj$grupo <- cutree(cluster_fit, k = 3)

# Salvar com grupos atribuídos
fwrite(traj, "Trajetoria_neonatos_cluster_novo.csv")

# Gráfico agrupado por cluster
seqdplot(seq_obj, group = traj$grupo, border = NA, main = "Trajetórias por Grupo")
