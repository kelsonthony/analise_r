# Carregar pacotes necessários
library(dplyr)
install.packages("lubridate")
library(lubridate) # Para trabalhar com datas

# 1. Análise Descritiva Básica (sem agrupamento) --------------------------

# Supondo que seu dataframe se chame 'df'

# Ver estrutura do dataframe
str(df)
summary(df)

# Contagem de valores em uma variável categórica (substitua 'variavel_categorica' pelo nome da sua variável)
contagem <- table(df$variavel_categorica)
print(contagem)

# Proporções
proporcoes <- prop.table(contagem) * 100
print(proporcoes)

# # Para variáveis categóricas
table(df$variavel_categorica)
prop.table(table(df$variavel_categorica)) * 100

# Estatísticas descritivas para variáveis numéricas (substitua 'variavel_numerica')
estatisticas <- df %>%
  summarise(
    Media = mean(variavel_numerica, na.rm = TRUE),
    Mediana = median(variavel_numerica, na.rm = TRUE),
    Desvio_Padrao = sd(variavel_numerica, na.rm = TRUE),
    Minimo = min(variavel_numerica, na.rm = TRUE),
    Maximo = max(variavel_numerica, na.rm = TRUE),
    Qtd_Observacoes = n(),
    Qtd_NA = sum(is.na(variavel_numerica))
  )

print(estatisticas)

## Criar nova variável com diferença entre datas (substitua 'data1' e 'data2')
df <- df %>%
  mutate(diferenca_dias = as.numeric(difftime(data1, data2, units = "days")))

# Diferença entre datas (R base) - faz o mesmo do código acima
df$diferenca_dias <- as.numeric(difftime(df$data1, df$data2, units = "days"))

# 2. Análise Descritiva Agrupada ------------------------------------------

# Substitua 'variavel_agrupamento' pela variável que deseja agrupar
analise_agrupada <- df %>%
  group_by(variavel_agrupamento) %>%
  summarise(
    Media = mean(variavel_numerica, na.rm = TRUE),
    Mediana = median(variavel_numerica, na.rm = TRUE),
    Desvio_Padrao = sd(variavel_numerica, na.rm = TRUE),
    Minimo = min(variavel_numerica, na.rm = TRUE),
    Maximo = max(variavel_numerica, na.rm = TRUE),
    Qtd_Observacoes = n(),
    Qtd_NA = sum(is.na(variavel_numerica)),
    .groups = 'drop'
  )

print(analise_agrupada)

# Contagem agrupada com proporções
contagem_agrupada <- df %>%
  count(variavel_agrupamento, variavel_categorica) %>%
  group_by(variavel_agrupamento) %>%
  mutate(Proporcao = n / sum(n) * 100) %>%
  ungroup()
print(contagem_agrupada)
