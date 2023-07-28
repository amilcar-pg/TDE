# Calculando o modelo de Solow ----

dados <- list(
  "t" = 1,
  "mi" = 0.03,
  "delta" = 0.15,
  "s" = 0.22,
  "A" = 1.2,
  "k" = 1.452,
  "alpha" = 0.5
)

calculo_Y <- function(k, A = dados[["A"]], mi = dados[["mi"]],
                      delta = dados[["delta"]], s = dados[["s"]], alpha = dados[["alpha"]]){
  
  Y <- A * (k^alpha)
  
  return(list(y = Y))
}

calculo_k <- function(k, A = dados[["A"]], mi = dados[["mi"]],
                      delta = dados[["delta"]], s = dados[["s"]], alpha = dados[["alpha"]]){
  
  y <- A * (k^alpha)
  
  k <- k + (s * y - ((delta + mi) * k))
  
  return(list(k = k))
}

## Atualizando o modelo até atingir o limiar desejado (A fixo)

df_solow_fixo <- tibble::tibble(
  k = dados[["k"]],
  A = dados[["A"]],
  y = calculo_Y(dados[["k"]])$y,
  delta_y = 0
)

threshold <- 0.0000001

while (TRUE) {
  new_k <- calculo_k(df_solow_fixo$k[length(df_solow_fixo$k)])$k
  new_y <- calculo_Y(new_k)$y
  delta_y <- new_y - df_solow_fixo$y[length(df_solow_fixo$y)]
  
  if (delta_y < threshold) {
    rm(new_k, new_y)
    break
  }
  
  df_temp <- tibble::tibble(
    k = new_k,
    A = dados[["A"]],
    y = new_y,
    delta_y = delta_y
  )
  
  df_solow_fixo <- dplyr::bind_rows(df_solow_fixo, df_temp)
  
  rm(new_k, new_y, delta_y, df_temp)
}; rm(threshold)

## Atualizando o modelo até atingir a mesma quantidade de linhas que o modelo anterior (A variável)

n_row <- nrow(df_solow_fixo)

df_solow_crescente <- tibble::tibble(
  k = numeric(n_row),
  A = numeric(n_row),
  y = numeric(n_row),
  delta_y = numeric(n_row)
)

for (i in 1:n_row) {
  
  if(i == 1){
    df_solow_crescente$A[i] <- dados[["A"]]
    df_solow_crescente$k[i] <- dados[["k"]]
    df_solow_crescente$delta_y[i] <- 0
    df_solow_crescente$y[i] <- calculo_Y(df_solow_crescente$k[[i]])$y
  } else {
    df_solow_crescente$A[i] <- df_solow_crescente$A[i-1]*1.02
    df_solow_crescente$k[i] <- calculo_k(df_solow_crescente$k[[i-1]])$k
    df_solow_crescente$y[i] <- calculo_Y(k = df_solow_crescente$k[[i]],
                                         A = df_solow_crescente$A[[i]])$y
    df_solow_crescente$delta_y[i] <- df_solow_crescente$y[[i]] - df_solow_crescente$y[[i-1]]
  }
  
}

# Plotando os resultados

library(ggplot2)

ggplot2::theme_set(ggthemes::theme_economist())

df_resultados <- data.frame(
  fixo = df_solow_fixo$y,
  crescente = df_solow_crescente$y,
  tempo = 1:nrow(df_solow_fixo)
) |> 
  tidyr::pivot_longer(-tempo)

df_resultados |> 
  dplyr::filter(tempo < 62) |> 
  ggplot(
    aes(x = tempo,
        y = value,
        color = name)
  ) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title=element_blank()) +
  labs(title = "Modelo de Solow",
       x = "Índice",
       y = "Y")

# Aplicando o choque de poupança em t = 61

for (i in 61:n_row) {
  
  # Atualizando modelo fixo
  df_solow_fixo$A[i] <- df_solow_fixo$A[i-1]
  df_solow_fixo$k[i] <- calculo_k(df_solow_fixo$k[[i-1]])$k
  df_solow_fixo$y[i] <- calculo_Y(k = df_solow_fixo$k[[i]])$y
  df_solow_fixo$delta_y[i] <- df_solow_fixo$y[[i]] - df_solow_fixo$y[[i-1]]
  
  # Atualizando modelo crescente
  df_solow_crescente$A[i] <- df_solow_crescente$A[i-1]*1.02
  df_solow_crescente$k[i] <- calculo_k(df_solow_crescente$k[[i-1]])$k
  df_solow_crescente$y[i] <- calculo_Y(k = df_solow_crescente$k[[i]],
                                       A = df_solow_crescente$A[[i]])$y
  df_solow_crescente$delta_y[i] <- df_solow_crescente$y[[i]] - df_solow_crescente$y[[i-1]]
  
}

df_resultados <- data.frame(
  fixo = df_solow_fixo$y,
  crescente = df_solow_crescente$y,
  tempo = 1:nrow(df_solow_fixo)
) |> 
  tidyr::pivot_longer(-tempo)

df_resultados |> 
  ggplot(
    aes(x = tempo,
        y = value,
        color = name)
  ) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title=element_blank()) +
  labs(title = "Modelo de Solow",
       x = "Índice",
       y = "Y")

plotly::ggplotly()

# FALTA FAZER O MODELO AK