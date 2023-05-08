# importando dados --------------------------------------------------------
df_info <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Info")
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data")
ggplot2::theme_set(ggthemes::theme_economist())

# Cálculos ----------------------------------------------------------------

df_data <- df_data |> 
  dplyr::mutate(
    pib_hora = rgdpo/avh,
    pib_trabalhador = rgdpo/emp
  )

df_data |> 
  dplyr::filter(year == 2019,
                country %in% c("United States", "France")) |> 
  dplyr::select(rgdpo, avh, emp, pib_trabalhador, pib_hora)

# FAZER UM GRÁFICO:
## Eixo X é PIB per capita
## Eixo Y são Horas trabalhadas

library(ggplot2)

df_data |> 
  dplyr::filter(
    country %in% c("United States", "France", "Brazil"),
    year %in% c(1960:2019)
  ) |> 
  ggplot(
    aes(
      x = rgdpo/pop,
      y = avh,
      color = country
    )
  ) +
  geom_point() +
  labs(
    x = "PIB per Capita",
    y = "Horas trabalhadas",
    color = NULL,
    title = "Horas trabalhadas pelo PIB Per Capita",
    caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot2::ggsave(
  plot = last_plot(),
  filename = "output/aula_06/horas_ppc.png",
  width = 7200,
  height = 4050,
  dpi = 600,
  units = "px"
)

