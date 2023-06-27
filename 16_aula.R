# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data") |> 
  as.data.frame()
rownames(df_data) <- stringr::str_c(df_data$country, "_", df_data$year)

# exercicio ---------------------------------------------------------------

df_exercicio <- df_data |> 
  dplyr::filter(
    country %in% c(
      "Chile", "Dominican Republic", "Argentina", "Colombia", 
      "Peru", "Mexico", "Brazil"),
    year == 2019
  )

df_exercicio <- df_exercicio |> 
  dplyr::mutate(
    pib_pop = rgdpo/pop,
    w = (labsh*rgdpo)/emp,
    unemp = emp/pop,
    migracao = dplyr::case_match(country, # dados coletados do e-mail da Fernanda
               "Chile" ~ 0.861,
               "Dominican Republic" ~ 0.557,
               "Argentina" ~ 0.505,
               "Colombia" ~ 0.375,
               "Peru" ~ 0.371,
               "Mexico" ~ 0.093,
               "Brazil" ~ 0.051))

df_exercicio |> 
  dplyr::select(country, pib_pop, w, unemp, migracao) |> 
  tibble::remove_rownames() |> 
  dplyr::arrange(dplyr::desc(migracao))

# limpando o ambiente

rm(list = ls())

# PNAD --------------------------------------------------------------------

library(PNADcIBGE)

# Carregar os dados que já foram baixados anteriormente
survey_pnadc_2022_04 <- PNADcIBGE::read_pnadc(microdata = "data/pnad/PNADC_042022.txt" ,
                                              input_txt = "data/pnad/input_PNADC_trimestral.txt",
                                              vars = c(
                                                # Informações sociodemográficas
                                                "V2009", # Idade do morador na data de referência
                                                "V2010", # Cor ou raça
                                                "V2007", # Sexo
                                                
                                                # Informações escolares
                                                "V3001", # Sabe ler e escrever?
                                                "VD3004", # Nível de instrução mais elevado alcançado
                                                "VD3005", # Anos de estudo padronizado para o ensino fundamental
                                                
                                                # Informações do trabalho
                                                "VD4017", # Rendimento mensal efetivo do trabalho principal
                                                "VD4001", # Condição em relação à força de trabalho
                                                "VD4002" # Condição de ocupação na semana de referência para pessoas de 14 ou mais anos de idade
                                              ))

survey_pnadc_2022_04 <- pnadc_labeller(data_pnadc = survey_pnadc_2022_04,
                                       dictionary.file = "data/pnad/dicionario_PNADC_microdados_trimestral.xls")

survey_pnadc_2022_04 <- pnadc_design(data_pnadc = survey_pnadc_2022_04)

detach("package:PNADcIBGE", unload = TRUE)

library(survey) # como não possuo costume em realizar operações com este pacote, não vou usar a sintaxe ::
library(srvyr) # usar a sintaxe do dplyr para manipular objetos survey

# Transformando o objeto svyrep.design em survey.design para usar o srvyr

survey_pnadc_2022_04 <- as_survey_rep(survey_pnadc_2022_04)

# exercicio

salario_uf <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 66) |> # Pessoas de 18 a 65 anos
  dplyr::group_by(UF) |> 
  dplyr::summarise(salario_medio = survey_mean(VD4017, na.rm = TRUE))

salario_uf_alfab <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 66) |> # Pessoas de 18 a 65 anos
  dplyr::group_by(UF, V3001) |> 
  dplyr::summarise(salario_medio = survey_mean(VD4017, na.rm = TRUE))

salario_uf_alfab |> 
  dplyr::select(-salario_medio_se) |> 
  dplyr::filter(UF %in% c("Maranhão", "São Paulo", "Distrito Federal")) |> 
  tidyr::pivot_wider(names_from = V3001, values_from = salario_medio)

tx_desemprego <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 66) |> # Pessoas de 18 a 65 anos
  dplyr::group_by(UF) |> 
  dplyr::summarise(txdesocup = survey_ratio(numerator = VD4002 == "Pessoas desocupadas",
                                            denominator = VD4001 == "Pessoas na força de trabalho",
                                            na.rm = TRUE))

tx_desemprego |> 
  dplyr::filter(UF %in% c("Maranhão", "São Paulo", "Distrito Federal"))
