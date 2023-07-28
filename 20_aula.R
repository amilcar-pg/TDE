# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data") |> 
  as.data.frame()
rownames(df_data) <- stringr::str_c(df_data$country, "_", df_data$year)

# exercicio ---------------------------------------------------------------

df_exercicio <- df_data |> 
  dplyr::filter(
    country %in% c(
      "United States", "Brazil", "Netherlands", "Uruguay", "Chile", "Republic of Korea", "China"),
    # year %in% c(2009:2019)
    year %in% c(2019)
  )

# Para casa
# df_exercicio <- df_exercicio |> 
#   dplyr::mutate(
#     pib_per_capita = ,
#     x_mais_m_sobre_pib = ,
#   )

# Em sala
df_exercicio <- df_exercicio |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(dplyr::across(dplyr::where(is.numeric), mean))

df_exercicio <- df_exercicio |> 
  dplyr::mutate(
    x_m_pib = (csh_x - csh_m) # não dividimos por PIB pois já são shares
  )

df_exercicio |>
  dplyr::select(country, csh_x, csh_m, rgdpo, x_m_pib)

library(ggplot2)

df_exercicio |> 
  ggplot(
    aes(
      y = forcats::fct_reorder(country, x_m_pib),
      x = x_m_pib,
      label = stringr::str_c(round(100*x_m_pib, 0), "%")
    )
  ) +
  geom_col(fill = "darkblue") +
  geom_text(hjust = -0.25) +
  ggthemes::theme_economist() +
  theme(axis.title = element_blank()) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.25), 
                     labels = scales::label_percent(),
                     limits = c(0,2))
