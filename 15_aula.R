# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data") |> 
  as.data.frame()
rownames(df_data) <- stringr::str_c(df_data$country, "_", df_data$year)

# exercicios --------------------------------------------------------------

df_exercicio <- df_data |> 
  dplyr::filter(country %in% c("United States", "Brazil", "Portugal", "Mozambique", "China"))

max(df_exercicio$year)

# calculando o PIB por trabalhador
df_exercicio <- df_exercicio |> 
  dplyr::mutate(
    pib_l = rgdpo/emp
  ) |> 
  dplyr::mutate(
    # pib_l_eua = pib_l/130106.742
    pib_l_eua = pib_l/pib_l[country == "United States"]
  )

df_exercicio |> 
  dplyr::filter(year == 2019) |> 
  dplyr::select(country, year, pib_l, pib_l_eua)

# calculando o modelo indicado

df_exercicio <- df_exercicio |> 
  dplyr::filter(year %in% c(2000:2019)) |> 
  dplyr::group_by(country) |> 
  dplyr::mutate(
    mi_pop = (pop[which.max(year)]/pop[which.min(year)])^(1/(max(year) - min(year) + 1)) - 1,
    mi_lab = (emp[which.max(year)]/emp[which.min(year)])^(1/(max(year) - min(year) + 1)) - 1,
    s = mean(csh_i),
    g = 0.02, # definido pelo professor
    delta_mean = mean(delta),
    h = mean(hc),
    A = mean(ctfp),
    alpha = mean(1-labsh)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    yss_pop = ((s/(mi_pop+g+delta_mean))^(alpha/(1-alpha)))*h*A,
    yss_pop_relativo = yss_pop/mean(yss_pop[country == "United States"]),
    yss_lab = ((s/(mi_lab+g+delta_mean))^(alpha/(1-alpha)))*h*A,
    yss_lab_relativo = yss_lab/mean(yss_lab[country == "United States"])
  )

# Resposta
df_exercicio |> 
  dplyr::distinct(country, mi_pop, mi_lab, s, g, delta_mean, h, A, alpha,
                  yss_pop, yss_pop_relativo, yss_lab, yss_lab_relativo)

# Definição de externalidade
# Custo ou benefício não valorado pelo mercado