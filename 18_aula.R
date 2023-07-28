# exercicio ---------------------------------------------------------------

custo_total <- function(q, w = 3, b = 8, la = 100){
  ct <- w*((q/b)+la)
  return(ct)
}

custo_medio <- function(q, w = 3, b = 8, la = 100){
  cm <- w*((1/b)+(la/q))
  return(cm)
}

df_raw <- data.frame(q = 1:100) |> 
  dplyr::mutate(
    ct = custo_total(q),
    cm = custo_medio(q)
  )

library(ggplot2)

df_raw |> 
  tidyr::pivot_longer(-q) |> 
  dplyr::mutate(
    name = dplyr::case_match(name,
                             "cm" ~ "Custo Médio",
                             "ct" ~ "Custo Total")
  ) |> 
  ggplot(
    aes(
      x = q,
      y = value,
      color = name
    )
  ) +
  geom_line(size = 1) +
  ggthemes::theme_economist() +
  labs(x = "Q",
       y = "Valor") +
  theme(legend.title = element_blank())

rm(list = ls())

# importando dados --------------------------------------------------------
# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data") |> 
  as.data.frame()
rownames(df_data) <- stringr::str_c(df_data$country, "_", df_data$year)

# exercicio ---------------------------------------------------------------

df_exercicio <- df_data |> 
  dplyr::filter(
    country %in% c("United States")
  )

df_exercicio |> 
  ggplot(
    aes(
      x = year, 
      y = log(rgdpo/emp)
    )
  ) +
  geom_line()