# Requisitos:
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("dplyr")
#install.packages("reshape2")

library(data.table)
library(ggplot2)
library(dplyr)

gerar_graficos_excel_like <- function(csv_path = "Trajetoria_neonatos.csv") {
  
  # Ler dados
  traj <- fread(csv_path)
  
  # Transformar em long
  traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
  
  # Frequência total de cada estado
  freq_estado <- traj_long %>%
    group_by(estado) %>%
    summarise(qtd = n(), .groups = "drop")
  
  # Gráfico de barras (colunas)
  gg_barras <- ggplot(freq_estado, aes(x = reorder(estado, -qtd), y = qtd, fill = estado)) +
    geom_bar(stat = "identity") +
    labs(title = "Frequência por Estado de Internação", x = "Estado", y = "Total de Semanas") +
    theme_minimal()
  
  ggsave("grafico_barras_estado.png", gg_barras, width = 8, height = 6, dpi = 300)
  
  # Gráfico de pizza por desfecho final
  traj_final <- traj_long %>%
    group_by(n_neonato) %>%
    arrange(as.numeric(as.character(semana))) %>%
    summarise(estado_final = last(estado), .groups = "drop") %>%
    count(estado_final)
  
  gg_pizza <- ggplot(traj_final, aes(x = "", y = n, fill = estado_final)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Destino Final dos Neonatos", x = NULL, y = NULL, fill = "Desfecho") +
    theme_void()
  
  ggsave("grafico_pizza_destino.png", gg_pizza, width = 6, height = 6, dpi = 300)
  
  message("✅ Gráficos salvos: 'grafico_barras_estado.png' e 'grafico_pizza_destino.png'")
}


plotar_gantt_trajetoria <- function(csv_path = "Trajetoria_neonatos.csv",
                                    nome_arquivo = "grafico_gantt_trajetoria.png",
                                    largura = 12, altura = 10, dpi = 300) {
  traj <- fread(csv_path)
  
  # Converter para formato longo
  traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
  traj_long$semana <- as.numeric(as.character(traj_long$semana))
  
  # Definir cores fixas para os estados
  cores_estados <- c(
    "UTIN" = "#ff0000", "UCINCA" = "#ff8800", "UCINCO" = "#ffee00",
    "UCIN" = "#00ccff", "ENF/ALCON" = "#99ff99", "ALTA" = "#cccccc", "ÓBITO" = "#000000"
  )
  
  # Criar o gráfico Gantt
  p <- ggplot(traj_long, aes(x = semana, y = factor(n_neonato), fill = estado)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = cores_estados, na.value = "grey90") +
    labs(x = "Semana", y = "Neonato", fill = "Estado") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  # Salvar o gráfico
  ggsave(filename = nome_arquivo, plot = p, width = largura, height = altura, dpi = dpi)
  message("✅ Gráfico Gantt salvo como: ", nome_arquivo)
}

plotar_linhas_evolucao <- function(csv_path = "Trajetoria_neonatos.csv",
                                   nome_arquivo = "linha_estado_semana.png") {
  traj <- fread(csv_path)
  traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
  traj_long$semana <- as.numeric(as.character(traj_long$semana))
  
  resumo <- traj_long %>%
    group_by(semana, estado) %>%
    summarise(n = n()) %>%
    ungroup()
  
  ggplot(resumo, aes(x = semana, y = n, color = estado)) +
    geom_line(size = 1) +
    labs(x = "Semana", y = "Número de Pacientes", color = "Estado") +
    theme_minimal() +
    ggsave(nome_arquivo, width = 10, height = 6, dpi = 300)
}

plotar_heatmap_semana_estado <- function(csv_path = "Trajetoria_neonatos.csv",
                                         nome_arquivo = "heatmap_estado_semana.png") {
  traj <- fread(csv_path)
  traj_long <- melt(traj, id.vars = "n_neonato", variable.name = "semana", value.name = "estado")
  traj_long$semana <- as.numeric(as.character(traj_long$semana))
  
  resumo <- traj_long %>%
    group_by(semana, estado) %>%
    summarise(n = n()) %>%
    ungroup()
  
  p <- ggplot(resumo, aes(x = semana, y = estado, fill = n)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "darkblue") +
    labs(x = "Semana", y = "Estado", fill = "Nº Pacientes") +
    theme_minimal()
  
  ggsave(nome_arquivo, plot = p, width = 10, height = 6, dpi = 300)
  message("✅ Heatmap salvo como: ", nome_arquivo)
  
}
