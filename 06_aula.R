# Definindo parâmetros do modelo
s <- 0.4
alpha <- 0.5
delta <- 0.1
mi <- 0.04
A <- 1

n_linhas <- 100

# Criando o dataframe que será base para o gráfico
df_raw <- data.frame(
  t = 1:n_linhas,
  k = NA_real_,
  y = NA_real_,
  consumo = NA_real_,
  poupanca = NA_real_,
  delta_k = NA_real_
)

# Definindo o valor inicial do capital
df_raw$k[1] <- 3

# Definindo modelo
modelo <- function(k, alpha, s, delta, mi) {
  
  y <- k^alpha
  consumo <- y * (1 - s)
  poupanca <- y * s
  
  delta_k <- k - (delta + mi) * k
  
  return(list(y = y, consumo = consumo, poupanca = poupanca, delta_k = delta_k))
}

# Preenchendo a tabela
while (any(is.na(df_raw$y))) {
  # Update model variables
  df_raw <- df_raw |> 
    dplyr::mutate(
      dplyr::across(c(y, consumo, poupanca, delta_k), 
                    ~ modelo(k, alpha, s, delta, mi)[[dplyr::cur_column()]]))
  
  df_raw <- df_raw |> 
    dplyr::mutate(
      k = dplyr::if_else(is.na(dplyr::lag(delta_k)), k, dplyr::lag(delta_k))
    )
}

# plotando ----------------------------------------------------------------

library(ggplot2)

df_raw |> 
  ggplot(
    aes(
      x = t,
      y = k
    )) +
  geom_line() +
  ggthemes::theme_economist() +
  labs(x = "Período",
       y = "Capital de Equilíbrio",
       title = "Modelo de crescimento de Solow",
       caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099") +
  theme(plot.title = element_text(hjust = 0.5))