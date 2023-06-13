# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data") |> 
  as.data.frame()
rownames(df_data) <- stringr::str_c(df_data$country, "_", df_data$year)

library(ggplot2)
ggplot2::theme_set(ggthemes::theme_economist())

## Importando dados da PNAD
library(PNADcIBGE)

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
                                                "VD4002")) # Condição de ocupação na semana de referência para pessoas de 14 ou mais anos de idade

survey_pnadc_2022_04 <- pnadc_labeller(data_pnadc = survey_pnadc_2022_04,
                                       dictionary.file = "data/pnad/dicionario_PNADC_microdados_trimestral.xls")

survey_pnadc_2022_04 <- pnadc_design(data_pnadc = survey_pnadc_2022_04)

detach("package:PNADcIBGE", unload = TRUE) # Por algum motivo não consigo chamar a função com os ::, então optei por carregá-la e depois retirá-la para economizar espaço no ambiente.

# aula --------------------------------------------------------------------

# PIRLS -> Pegar essa base de dados e fazer os seguintes gráficos:
# Eixo y -> Not PIRL
# Eixo x -> Renda per capita