# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data") |> 
  as.data.frame()
rownames(df_data) <- stringr::str_c(df_data$country, "_", df_data$year)

library(ggplot2)
ggplot2::theme_set(ggthemes::theme_economist())

# aula --------------------------------------------------------------------

# Segunda prova vai ser teo ria do capital humano (L);
  # Educação enaquanto investimento;
# Alguns capítulos do Livro

## calculando alpha de 2019

mean_alpha <- mean(
1 - df_data["United States_2019", "labsh"],
1 - df_data["Brazil_2019", "labsh"])

df_data <- df_data |> 
  dplyr::mutate(
    gdp_por_trab = rgdpo/pop
  )

## Relação da remuneração do capital
(df_data["United States_2019", "gdp_por_trab"]/df_data["Brazil_2019", "gdp_por_trab"])^((mean_alpha-1)/mean_alpha)

rm(mean_alpha)

## Taxa de depreciação
df_data["United States_2019", "delta"]
