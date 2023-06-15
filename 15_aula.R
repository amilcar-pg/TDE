# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data") |> 
  as.data.frame()
rownames(df_data) <- stringr::str_c(df_data$country, "_", df_data$year)

# exercicio ---------------------------------------------------------------

# Y = k"alpha [LhA]^(1-alpha)

# Y/L = hA[K/L]^(alpha/(1 - alpha))

df_exercicio <- df_data |> 
  dplyr::filter(country %in% c("United States", "Brazil",
                               "Portugal", "Mozambique", "China",
                               "Republic of Korea", "Japan"),
                year == 2019) |> 
  tibble::remove_rownames()

df_exercicio <- df_exercicio |> 
  dplyr::mutate(
    y_l = rgdpo/emp,
    k_y_alpha = (ck/rgdpo)^((1-labsh)/labsh),
    h = hc,
    A = ctfp
  ) |> 
  dplyr::select(
    country, y_l, k_y_alpha, h, A
  )

df_exercicio <- df_exercicio |> 
  dplyr::mutate(resultado = "absoluto") |> 
  dplyr::bind_rows(
    df_exercicio |> 
      dplyr::mutate(
        y_l = round(100*y_l/y_l[which(country == "United States")], 2),
        k_y_alpha = round(100*k_y_alpha/k_y_alpha[which(country == "United States")], 2),
        h = round(100*h/h[which(country == "United States")], 2),
        A = round(100*A/A[which(country == "United States")], 2),
        resultado = "relativo"))

# Consultando resultados

## Absoluto
df_exercicio[df_exercicio$resultado == "absoluto", -ncol(df_exercicio)]

## Relativo
df_exercicio[df_exercicio$resultado == "relativo", -ncol(df_exercicio)]

# Para copiar é preciso ter capital humano