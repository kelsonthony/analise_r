### =============================================
### HEATMAP DE TRAJETÓRIA DE NEONATOS POR SEMANA
### =============================================

# Pacotes necessários:
# install.packages("data.table")    # Leitura e manipulação eficiente de dados
# install.packages("ggplot2")       # Visualização gráfica
# install.packages("RColorBrewer")  # Paletas de cores

library(data.table)
library(ggplot2)
library(RColorBrewer)

plotar_barras_estado_por_semana <- function(csv_path = "Trajetoria_neonatos.csv",
                                            salvar_png = TRUE,
                                            nome_arquivo = "estado_por_semana.png",
                                            largura = 10, altura = 6, dpi = 300) {
  traj <- fread(csv_path)
  traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
  traj_long$semana <- as.numeric(as.character(traj_long$semana))
  
  traj_resumo <- traj_long %>%
    group_by(semana, estado) %>%
    summarise(qtd = n(), .groups = "drop")
  
  gg <- ggplot(traj_resumo, aes(x = semana, y = qtd, fill = estado)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = c(
      "UTIN" = "firebrick", "UCIN" = "orange", "UCINCA" = "gold",
      "UCINCO" = "khaki", "ENF/ALCON" = "lightblue",
      "ALTA" = "darkgreen", "ÓBITO" = "black"
    )) +
    labs(x = "Semana", y = "Proporção", fill = "Estado") +
    theme_minimal()
  
  if (salvar_png) {
    ggsave(nome_arquivo, gg, width = largura, height = altura, dpi = dpi)
    message("✅ Gráfico salvo como: ", nome_arquivo)
  } else {
    print(gg)
  }
}

