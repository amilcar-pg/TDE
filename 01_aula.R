# importando dados --------------------------------------------------------
df_info <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Info")
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data")

# manipulando -------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Para calcular o crescimento médio, seguimos a seguinte lógica:

rgdpo_corea_2019 <- df_data |>
  filter(year == 2019, country == "Republic of Korea") |> 
  select(rgdpo)

rgdpo_corea_1959 <- df_data |>
  filter(year == 1959, country == "Republic of Korea") |> 
  select(rgdpo)

rgdpo_br_2019 <- df_data |>
  filter(year == 2019, country == "Brazil") |> 
  select(rgdpo)

rgdpo_br_1959 <- df_data |>
  filter(year == 1959, country == "Brazil") |> 
  select(rgdpo)

anos <- 2019-1959

crescimento_brazil <- ((rgdpo_br_2019[[1]]/rgdpo_br_1959[[1]]))^(1/anos)
crescimento_corea <- ((rgdpo_corea_2019[[1]]/rgdpo_corea_1959[[1]]))^(1/anos)

## Limpando o ambiente
rm(rgdpo_corea_2019, rgdpo_corea_1959,
   rgdpo_br_2019, rgdpo_br_1959, anos)

# Gráfico da evolução do PIB
df_data |> 
  filter(country %in% c("Brazil", "Republic of Korea")) |> 
  ggplot(
    aes(x = year,
        y = rgdpo,
        color = country)) +
  geom_line() +
  theme_minimal()

# Gráfico da evolução do PIB per capita
df_data |> 
  filter(country %in% c("Brazil", "Republic of Korea")) |> 
  ggplot(
    aes(x = year,
        y = rgdpo/pop, # pib per capita
        color = country)) +
  geom_line() +
  theme_minimal()

# Calculando a taxa de participação do Brasil

df_data <- df_data |> 
  mutate(tx_participacao = emp/pop)

df_data |> 
  filter(country %in% c("Brazil", "Switzerland", "D.R. of the Congo"), year %in% c(1960, 2019)) |> 
  select(country, year, tx_participacao)

## O aumento pode ser dado:
### Participação feminina;
### Queda na taxa de natalidade (transição demográfica);

# Para fazer em casa: 
## Separar os países em grupos: c("pobre", "classe_media", "rico")
## Fazer um gráfico:
### aes(x = tx_participacao, y = rgdpo/pop) + geom_point()

# criando variável de grupo de países

df_data <- df_data |> 
  filter(!is.na(country)) |> 
  mutate(
    grupo_pais = case_when(
      country %in% c(
        "Angola", "Benin", "Burkina Faso", "Burundi", "Central African Republic", "Chad",
        "Comoros", "Democratic Republic of the Congo", "Djibouti", "Eritrea", "Ethiopia",
        "Gambia", "Guinea", "Guinea-Bissau", "Lesotho", "Liberia", "Madagascar", "Malawi",
        "Mali", "Mauritania", "Mozambique", "Niger", "Rwanda", "Sao Tome and Principe",
        "Senegal", "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Togo", "Uganda",
        "United Republic of Tanzania and Zambia") ~ "Menos desenvolvidos",
      country %in% c("Algeria", "Egypt", "Libya", "Mauritania", "Morocco",
                     "Sudan", "Tunisia", "Cameroon", "Central African Republic",
                     "Chad", "Congo", "Equatorial Guinea", "Gabon", 
                     "Sao Tome and Prinicipe", "Burundi", "Comoros", 
                     "Democratic Republic of the Congo", "Djibouti", "Eritrea",
                     "Ethiopia", "Kenya", "Madagascar", "Rwanda", "Somalia", 
                     "Uganda", "United Republic of Tanzania", "Angola", 
                     "Botswana", "Lesotho", "Malawi", "Mauritius", "Mozambique",
                     "Namibia", "South Africa", "Zambia", "Zimbabwe", "Benin",
                     "Burkina Faso", "Cabo Verde", "Côte d’Ivoire", "Gambia", 
                     "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", 
                     "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo", 
                     "Brunei Darussalam", "China", "Hong Kong SARc", "Indonesia",
                     "Malaysia", "Myanmar", "Papua New Guinea", "Philippines", 
                     "Republic of Korea", "Singapore", "Taiwan Province of China",
                     "Thailand", "Viet Nam", "Bangladesh", "India", 
                     "Iran (Islamic Republic of)", "Nepal", "Pakistan", 
                     "Sri Lanka", "Bahrain", "Iraq", "Israel", "Jordan", 
                     "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia",
                     "Syrian Arab Repuplic", "Turkey", "United Ara", "Barbados",
                     "Cuba", "Dominican Republic", "Guyana", "Haiti", "Jamaica",
                     "Trinidad and Tobago", "Costa Rica", "El Salvador",
                     "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama",
                     "Argentina", "Bolivia (Plurinational State of)", "Brazil",
                     "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", 
                     "Venezuela (Bolivarian Repúblic of)") ~ "Em desenvolvimento",
      country %in% c(
        "Austria","Belgium","Denmark","Finland","France","Germany","Greece",
        "Ireland","Italy","Luxembourg","Netherlands","Portugal","Spain",
        "Sweden","United Kingdom,","Bulgaria","Croatia","Cyprus","Czech Republic",
        "Estonia","Hungary","Latvia","Lithuania","Malta","Poland","Romania",
        "Slovakia","Slovenia","Iceland","Norway","Switzerland","Australia",
        "Canada","Japan","New Zealand","United States","Canada","Japan","France",
        "Germany","Italy","United Kingdom","United States") ~ "Mais desenvolvidos",
      TRUE ~ "Não classificado"))

df_data$grupo_pais <- factor(df_data$grupo_pais, levels = c("Menos desenvolvidos", "Em desenvolvimento", 
                                      "Mais desenvolvidos", "Não classificado"))

df_data |> 
  filter(grupo_pais != "Não classificado") |> 
  filter(year == 2019) |> 
  ggplot(aes(x = tx_participacao,
             y = rgdpo/pop,
             color = grupo_pais,
             label = country))  + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel() +
  ggthemes::theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Relação entre a taxa de participação e o PIB per capita",
    x = "Taxa de participação",
    y = "PIB per capita",
    colour = "Classificação de desenvolvimento") +
  labs(colour = NULL)

ggsave(plot = last_plot(), dpi = "retina",
       width = 20, height = 10, units = "cm",
       filename = "output/agrupado_nomeado.jpg")

df_data |> 
  filter(grupo_pais == "Em desenvolvimento") |> 
  filter(year == 2019) |> 
  ggplot(aes(x = tx_participacao,
             y = rgdpo/pop,
             label = country))  + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel() +
  ggthemes::theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(
    title = "Relação entre a taxa de participação e o PIB per capita",
    subtitle = "Em desenvolvimento", # MUDAR AQUI TAMBÉM!
    x = "Taxa de participação",
    y = "PIB per capita")

ggsave(plot = last_plot(), dpi = "retina",
       width = 20, height = 10, units = "cm",
       filename = "output/em_desenvolvimento_nomeado.jpg")

df_data |> 
  filter(grupo_pais != "Não classificado") |> 
  filter(year == 2019) |> 
  ggplot(aes(x = tx_participacao,
             y = rgdpo/pop,
             color = grupo_pais,
             label = country))  + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel() +
  facet_wrap(~grupo_pais, ncol = 1) + 
  ggthemes::theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(
    title = "Relação entre a taxa de participação e o PIB per capita",
    x = "Taxa de participação",
    y = "PIB per capita",
    colour = "Classificação de desenvolvimento") +
  labs(colour = NULL)

ggsave(plot = last_plot(), dpi = "retina",
       width = 20, height = 30, units = "cm",
       filename = "output/empilhado_nomeado.jpg")

# Calculando a produtividade

df_data <- df_data |> 
  mutate(produtividade = rgdpo/emp)

df_data |> 
  filter(
    country %in% c("Brazil", "D.R. of the Congo", "Switzerland"),
    year %in% c(1960, 2019)
  ) |> 
  select(country, year, produtividade)

# Evolucao da produtividade dos EUA

df_data |> 
  filter(country == "United States") |> 
  ggplot( aes(x = year,
              y = produtividade)) +
    geom_line() +
  theme_minimal()

# Encontrando a variação da produtividade

df_data <- df_data |> 
  mutate(delta_produtividade = produtividade/lag(produtividade))

df_data |> 
  filter(country == "United States", year > 1959) |> 
  ggplot( aes(x = year,
              y = delta_produtividade)) +
  geom_line() +
  theme_minimal() +
  geom_smooth() +
  geom_smooth(method = "lm", se = FALSE, color = "red")