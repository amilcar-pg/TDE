# importando dados --------------------------------------------------------
df_info <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Info")
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data")

# manipulando -------------------------------------------------------------
library(dplyr)
library(ggplot2)
ggplot2::theme_set(ggthemes::theme_economist())

# PIB per capita BR vs USA

df_data <- df_data |> 
  mutate(
    gdp_per_capita = rgdpo/pop
  )

df_grafico <- df_data |> 
  filter(country %in% c("Brazil", "United States")) |> 
  select(year, country, gdp_per_capita) |> 
  tidyr::pivot_wider(names_from = country, values_from = gdp_per_capita) |> 
  mutate(gdp_final = Brazil/`United States`)

df_grafico |> 
  ggplot(aes(x = year, y = gdp_final)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "GDP Brazil / GDP USA",
       x = "Ano",
       y = "Razão do GDP") +
  scale_y_continuous(breaks = seq(0, 0.75, by = 0.25), limits = c(0,0.75))

ggsave(plot = last_plot(), filename = "output/aula_02/gdpbr_gdpus.png",
       dpi = "retina", height = 10, width = 20, units = "cm")

# EXERCÍCIO: FAZER A MESMA COISA PARA CHILE E COREIA ----

# PIB per capita Corea vs USA

df_grafico <- df_data |> 
  filter(country %in% c("Republic of Korea", "United States")) |> 
  select(year, country, gdp_per_capita) |> 
  tidyr::pivot_wider(names_from = country, values_from = gdp_per_capita) |> 
  mutate(gdp_final = `Republic of Korea`/`United States`)

df_grafico |> 
  ggplot(aes(x = year, y = gdp_final)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "GDP Coreia / GDP USA",
       x = "Ano",
       y = "Razão do GDP") +
  scale_y_continuous(breaks = seq(0, 0.75, by = 0.25), limits = c(0,0.75))

ggsave(plot = last_plot(), filename = "output/aula_02/gdpcoreia_gdpus.png",
       dpi = "retina", height = 10, width = 20, units = "cm")

# PIB per capita Corea vs USA

df_grafico <- df_data |> 
  filter(country %in% c("Chile", "United States")) |> 
  select(year, country, gdp_per_capita) |> 
  tidyr::pivot_wider(names_from = country, values_from = gdp_per_capita) |> 
  mutate(gdp_final = `Chile`/`United States`)

df_grafico |> 
  ggplot(aes(x = year, y = gdp_final)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "GDP Chile / GDP USA",
       x = "Ano",
       y = "Razão do GDP") +
  scale_y_continuous(breaks = seq(0, 0.75, by = 0.25), limits = c(0,0.75))

ggsave(plot = last_plot(), filename = "output/aula_02/gdpchile_gdpus.png",
       dpi = "retina", height = 10, width = 20, units = "cm")

# Fazendo o mesmo exercício, mas com produtividade ----

df_data <- df_data |> 
  mutate(
    produtividade = rgdpo/emp
  )

df_grafico <- df_data |> 
  filter(country %in% c("Brazil", "United States")) |> 
  select(year, country, produtividade) |> 
  tidyr::pivot_wider(names_from = country, values_from = produtividade) |> 
  mutate(produtividade = Brazil/`United States`)

df_grafico |> 
  ggplot(aes(x = year, y = produtividade)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Produtividade Brazil / Produtividade USA",
       x = "Ano",
       y = "Razão da Produtividade")

# Corea e EUA

df_grafico <- df_data |> 
  filter(country %in% c("Brazil", "United States", "Republic of Korea")) |> 
  select(year, country, gdp_per_capita) |> 
  tidyr::pivot_wider(names_from = country, values_from = gdp_per_capita) |> 
  mutate(gdp_final_br = Brazil/`United States`,
         gdp_final_corea = `Republic of Korea`/`United States`) |> 
  select(year, starts_with("gdp")) |> 
  tidyr::pivot_longer(-year)

df_grafico |> 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "GDP Pais / GDP USA",
       x = "Ano",
       y = "Razão do GDP")


# Situacao problema -------------------------------------------------------

df_situacao <- df_data |> 
  filter(country %in% c("Brazil", "Portugal")) |> 
  filter(year == 2019) |> 
  select(country, gdp_per_capita)

ppc_2019 <- setNames(df_situacao$gdp_per_capita, df_situacao$country)


# Tx de crescimento discreta
tx_discreta <- function(i){ppc_2019["Brazil"] * ((1 + i)^20) - (ppc_2019["Portugal"]*((1+0.02)**20))}
uniroot(tx_discreta,interval = c(0,1))$root

# Tx de crescimento contínua

tx_continua <- function(i){ppc_2019["Brazil"] * exp((20*i)) - (ppc_2019["Portugal"] * exp(20*0.02))}
uniroot(tx_continua, interval = c(0,1))$root
