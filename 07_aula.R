# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data")
ggplot2::theme_set(ggthemes::theme_economist())
library(ggplot2)

# aula --------------------------------------------------------------------

# y = A*k^alpha
# yss = [AS/delta + mi]^(alpha/(1-alpha))

brasil_2019 <- df_data |> 
  dplyr::filter(country == "Brazil", year == 2019) |> 
  dplyr::select(dplyr::where(is.numeric))
brasil_2019 <- setNames(as.numeric(brasil_2019[1,]), colnames(brasil_2019))

y <- brasil_2019["rgdpo"]/brasil_2019["emp"]
delta <- brasil_2019["delta"]
s <- brasil_2019["csh_i"]
alpha <- 1 - brasil_2019["labsh"]
mi <- "descobrir"
A <- brasil_2019["ctfp"] # Não da certo pois A está normalizado pelos EUA

rm(brasil_2019, y, delta, s, alpha, A, mi)

## exercício ----

df_exercicio <- df_data |> 
  dplyr::filter(
    country %in% c("Brazil", "United States"),
    year %in% c(2009:2019)
  ) |> 
  dplyr::select(year, country, rgdpo, emp, delta, csh_i, labsh, ctfp) |> 
  dplyr::mutate(
    y = rgdpo/emp,
    alpha = 1-labsh
  ) |> 
  dplyr::rename(
    s = csh_i,
    A = ctfp
  )

# Calculando a taxa de crescimento media anual 

df_exercicio <- df_exercicio |> 
  dplyr::group_by(country) |> 
  dplyr::mutate(
    emp_final = emp[which.max(year)],
    emp_inicial = emp[which.min(year)],
    tx_cresc = (emp_final/emp_inicial)^(1/(dplyr::n()-1)) -1
  ) |> 
  dplyr::select(-c('emp_final', 'emp_inicial'))

# Recuperando as taxas de crescimento
df_exercicio |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(tx_cresc = mean(tx_cresc))

df_exercicio <- df_exercicio |> 
  dplyr::group_by(country) |> 
  dplyr::mutate(
    delta_mean = mean(delta),
    s_mean = mean(s),
    labsh_mean = mean(labsh),
    A_mean = mean(A)
  )

df_exercicio <- df_exercicio |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(dplyr::across(dplyr::everything(), mean)) |> 
  dplyr::select(country, tx_cresc, delta_mean, s_mean, labsh_mean, A_mean)

df_exercicio <- df_exercicio |> 
  reshape2::melt() |> 
  tidyr::pivot_wider(names_from = country,
                     values_from = value) |> 
  tibble::column_to_rownames(var = 'variable')

# Calcular os ss do BR e dos EUA e depois ver a razão

df_exercicio["Yss", 'Brazil'] <- ((
  df_exercicio["A_mean", 'Brazil'])^(1/df_exercicio["labsh_mean", 'Brazil'])) * (
  df_exercicio["s_mean", 'Brazil']/
    (df_exercicio["delta_mean", 'Brazil'] + df_exercicio["tx_cresc", 'Brazil']))^(
      (1-df_exercicio["labsh_mean", 'Brazil'])/df_exercicio["labsh_mean", 'Brazil'])


df_exercicio["Yss", 'United States'] <- ((
  df_exercicio["A_mean", 'United States'])^(1/df_exercicio["labsh_mean", 'United States'])) * (
    df_exercicio["s_mean", 'United States']/
      (df_exercicio["delta_mean", 'United States'] + df_exercicio["tx_cresc", 'United States']))^(
        (1-df_exercicio["labsh_mean", 'United States'])/df_exercicio["labsh_mean", 'United States'])

## Razão buscada
df_exercicio["Yss", 'Brazil']/df_exercicio["Yss", 'United States']

# Recuperando agora a razão do PIB por trabalhador de cada país

df_ppt <- df_data |> 
  dplyr::filter(
    country %in% c("Brazil", "United States"),
    year == 2019
  ) |> 
  dplyr::mutate(ppt = rgdpo/emp) |> 
  dplyr::select(country, ppt) |> 
  tibble::column_to_rownames(var = 'country')

## Razão buscada
df_ppt["Brazil", "ppt"]/df_ppt["United States", "ppt"]
