# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data")
ggplot2::theme_set(ggthemes::theme_economist())
library(ggplot2)

# exercicios --------------------------------------------------------------

df_exercicio <- df_data |> 
  dplyr::filter(
    country %in% c('Mexico', 'United States', 'Brazil'),
    year == 2019
  ) |> 
  t() |> 
  as.data.frame()

colnames(df_exercicio) <- as.character(df_exercicio[1,])

df_exercicio <- df_exercicio |> 
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

df_exercicio <- df_exercicio[complete.cases(df_exercicio),]

# calculando salário

df_exercicio["labsh", "BRA"] * df_exercicio["rgdpo", "BRA"]/df_exercicio["emp", "BRA"]
df_exercicio["labsh", "MEX"] * df_exercicio["rgdpo", "MEX"]/df_exercicio["emp", "MEX"]
df_exercicio["labsh", "USA"] * df_exercicio["rgdpo", "USA"]/df_exercicio["emp", "USA"]

# calculando remuneração da renda

(1-df_exercicio["labsh", "BRA"]) * df_exercicio["rgdpo", "BRA"]/df_exercicio["rnna", "BRA"] # não se comportou como o esperado
(1-df_exercicio["labsh", "MEX"]) * df_exercicio["rgdpo", "MEX"]/df_exercicio["rnna", "MEX"]
(1-df_exercicio["labsh", "USA"]) * df_exercicio["rgdpo", "USA"]/df_exercicio["rnna", "USA"]

# exercicio_2 -------------------------------------------------------------

df_exercicio2 <- df_data |> 
  dplyr::filter(
    country %in% c('United Kingdom', 'Poland'),
    year == 2019
  ) |> 
  t() |> 
  as.data.frame()

colnames(df_exercicio2) <- as.character(df_exercicio2[1,])

df_exercicio2 <- df_exercicio2 |> 
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

df_exercicio2 <- df_exercicio2[complete.cases(df_exercicio2),]

# calculando salário

df_exercicio2["labsh", "POL"] * df_exercicio2["rgdpo", "POL"]/df_exercicio2["emp", "POL"]
df_exercicio2["labsh", "GBR"] * df_exercicio2["rgdpo", "GBR"]/df_exercicio2["emp", "GBR"]

# calculando remuneração da renda

(1-df_exercicio2["labsh", "POL"]) * df_exercicio2["rgdpo", "POL"]/df_exercicio2["rnna", "POL"]
(1-df_exercicio2["labsh", "GBR"]) * df_exercicio2["rgdpo", "GBR"]/df_exercicio2["rnna", "GBR"]


# exercicio 3 -------------------------------------------------------------

df_exercicio3 <- df_data |> 
  dplyr::filter(
    country %in% c('Portugal', 'Netherlands'),
    year > 1995
  ) |> 
  dplyr::mutate(
    wage = labsh*rgdpo/emp
  )

# gráfico do salário
df_exercicio3 |> 
  ggplot(
    aes(
      x = year, 
      y = wage,
      color = country
    )
  ) +
  geom_line()


# gráfico do hiato
df_exercicio3 |> 
  dplyr::select(year, country, wage) |> 
  tidyr::pivot_wider(names_from = country, values_from = wage) |> 
  dplyr::mutate(
    delta = Netherlands - Portugal
  ) |> 
  ggplot(
    aes(
      x = year,
      y = delta
    )
  ) +
  geom_line()


# importando dados --------------------------------------------------------

df_mdb <- readxl::read_xlsx("data/mpd2020.xlsx",
                            sheet = "Regional data",
                            skip= 1)

df_mdb <- df_mdb[2:nrow(df_mdb),]

colnames(df_mdb)[1] <- 'year'

df_mdb |> 
  ggplot(
    aes(x = year,
        y = log(as.numeric(`Western Europe...2`)),
        group = 1)
  ) +
  geom_line()

df_mdb |> 
  ggplot(
    aes(x = year,
        y = log(as.numeric(`Latin America...5`)),
        group = 1)
  ) +
  geom_line()

df_mdb <- df_mdb |> 
  tidyr::pivot_longer(cols = -c('year'))

df_mdb |> 
  dplyr::filter(
    name %in% c("Sub-Sahara Africa...17", "Western Europe...2"),
    year > 1940
  ) |> 
  ggplot(aes(
    x = year,
    y = value, 
    color = name,
    group = name
  )) +
    geom_line()
