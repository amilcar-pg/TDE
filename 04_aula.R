# importando dados --------------------------------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data")

# importando pacotes e setando tema ---------------------------------------

library(ggplot2)
ggplot2::theme_set(ggthemes::theme_economist())

## template de salvar gráfico
ggplot2::ggsave(
  plot = last_plot(),
  filename = "output/aula_07/{nome_grafico}.png",
  width = 7200,
  height = 4050,
  dpi = 600,
  units = "px"
)

# aula --------------------------------------------------------------------

df_grafico <- df_data |> 
  dplyr::filter(
    country %in% c("China", "United States")
  )

df_grafico |> 
  ggplot(
    aes(
      x = year,
      y = rgdpo,
      color = country
    )
  ) +
  geom_point()

df_grafico |> 
  dplyr::filter(year == 2019) |> 
  dplyr::mutate(produtividade = rgdpo/emp) |> 
  dplyr::select(country, rgdpo, pop, emp, produtividade)


# CALCULAR O A DA FÓRMULA Q = A*(K^ALPHA)*L^(1-ALPHA)

## Gravando o nome das variáveis
alpha <- "csh_i"
K <- "cn"
L <- "emp"
Q <- 'rgdpo'

## Criando vetor nomeado para os EUA
eua_2019 <- df_data |> 
  dplyr::filter(country == "United States", year == 2019) |> 
  dplyr::select(dplyr::where(is.numeric))
eua_2019 <- setNames(as.numeric(eua_2019[1,]), colnames(eua_2019))

## Criando vetor nomeado para a China
china_2019 <- df_data |> 
  dplyr::filter(country == "China", year == 2019) |> 
  dplyr::select(dplyr::where(is.numeric))
china_2019 <- setNames(as.numeric(china_2019[1,]), colnames(china_2019))

## Calculando os valores de A
A_eua <-  eua_2019[Q]/((eua_2019[K]^eua_2019[alpha])*(eua_2019[L]^(1-eua_2019[alpha])))
A_china <-  china_2019[Q]/((china_2019[K]^china_2019[alpha])*(china_2019[L]^(1-china_2019[alpha])))

rm(K, L, Q, alpha, china_2019, eua_2019, br_2019)

# CALULAR HIATO BRASIL EUA E CHINA