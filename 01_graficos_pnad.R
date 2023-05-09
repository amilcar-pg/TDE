# importando dados pnad ---------------------------------------------------

library(PNADcIBGE)

# Baixar os dados caso tenha conexão com a internet
# survey_pnadc_2022_04 <- get_pnadc(year = 2022,
#                                   quarter = 4, 
#                                   vars = c(
#                                     # Informações sociodemográficas
#                                     "V2009", # Idade do morador na data de referência
#                                     "V2010", # Cor ou raça
#                                     "V2007", # Sexo
#                                     
#                                     # Informações escolares
#                                     "V3001", # Sabe ler e escrever?
#                                     "VD3004", # Nível de instrução mais elevado alcançado
#                                     "VD3005", # Anos de estudo padronizado para o ensino fundamental
#                                     
#                                     # Informações do trabalho
#                                     "VD4017", # Rendimento mensal efetivo do trabalho principal
#                                     "VD4001", # Condição em relação à força de trabalho
#                                     "VD4002" # Condição de ocupação na semana de referência para pessoas de 14 ou mais anos de idade
#                                   ), savedir = "data/pnad/")

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

# ESTA PARTE DO CÓDIGO PRECISA SER RODADA APENAS SE DECIDIRMOS FAZER COMPARAÇÃO TEMPORAL
# survey_pnadc_2022_04 <- pnadc_deflator(data_pnadc = survey_pnadc_2022_04,
#                                        deflator.file = "data/pnad/deflator_PNADC_2022_trimestral_101112.xls")

survey_pnadc_2022_04 <- pnadc_design(data_pnadc = survey_pnadc_2022_04)

detach("package:PNADcIBGE", unload = TRUE) # Por algum motivo não consigo chamar a função com os ::, então optei por carregá-la e depois retirá-la para economizar espaço no ambiente.

# importando pacotes ------------------------------------------------------

library(ggplot2) # para os gráficos que queremos fazer
ggplot2::theme_set(ggthemes::theme_economist())

library(survey) # como não possuo costume em realizar operações com este pacote, não vou usar a sintaxe ::
library(srvyr) # usar a sintaxe do dplyr para manipular objetos survey

# Transformando o objeto svyrep.design em survey.design para usar o srvyr

survey_pnadc_2022_04 <- as_survey_rep(survey_pnadc_2022_04)

# salario x idade ---------------------------------------------------------

## Salario x idade geral ----
salario_idade <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 66) |> # Pessoas de 18 a 65 anos
  dplyr::group_by(V2009) |> 
  dplyr::summarise(salario_medio = survey_mean(VD4017, na.rm = TRUE))

# Plotando o resultado
salario_idade |> 
  ggplot(aes(x = V2009,
             y = salario_medio)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Idade",
       y = "Rendimento mensal efetivo do trabalho principal",
       caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099",
       title = "Salário x Idade - Geral") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -5))

# Salvando o resultado
ggplot2::ggsave(
  plot = last_plot(),
  filename = "output/pnad/salario_idade_geral.png",
  width = 7200,
  height = 4050,
  dpi = 600,
  units = "px")

# Removendo variáveis que não serão mais utilizadas
rm(salario_idade)

## Salario x (idade + Nível de instrução) ----

# Criando a variável que faremos o cruzamento
survey_pnadc_2022_04 <- survey_pnadc_2022_04 |> 
  dplyr::mutate(
    educacao_modelado = dplyr::case_when(
      V3001 == "Não" ~ "Analfabeto",
      TRUE ~ "Superior completo"))

# Gerando o cruzamento
salario_idade_estratificado <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 66, # Pessoas de 18 a 65 anos
                !is.na(VD4017), # NA de salário
                !is.na(educacao_modelado)) |>  # NA da variável criada
  dplyr::group_by(V2009, educacao_modelado) |> 
  dplyr::summarise(salario_medio = survey_mean(VD4017, na.rm = TRUE))

# Plotando o resultado
salario_idade_estratificado |> 
  ggplot(aes(x = V2009,
             y = salario_medio,
             color = educacao_modelado
             )) +
  geom_line() +
  geom_smooth(se = FALSE) + 
  labs(x = "Idade",
       y = "Rendimento mensal efetivo do trabalho principal",
       color = NULL,
       caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099",
       title = "Salário x Idade por nível de instrução") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -5)) 

# Salvando o resultado
ggplot2::ggsave(
  plot = last_plot(),
  filename = "output/pnad/salario_idade_nivel_educacional.png",
  width = 7200,
  height = 4050,
  dpi = 600,
  units = "px")

# Removendo variáveis que não serão mais utilizadas
rm(salario_idade_estratificado)

# idade x ocupacao --------------------------------------------------------

## Gerando variável PPI
survey_pnadc_2022_04 <- survey_pnadc_2022_04 |> 
  dplyr::mutate(
    PPI = dplyr::case_when(
      V2010 %in% c("Preta", "Parda", "Indígena") ~ TRUE,
      TRUE ~ FALSE))

## Tx de desemprego no Y - Idade no X
desempregados_idade <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 65,
                V2010 != "Ignorado") |> 
  dplyr::group_by(V2009, PPI) |> 
  dplyr::summarise(
    txdesocup = survey_ratio(numerator = VD4002 == "Pessoas desocupadas",
                             denominator = VD4001 == "Pessoas na força de trabalho",
                             na.rm = TRUE))

desempregados_idade |> 
  dplyr::mutate(
    PPI = dplyr::case_match(PPI,
                            TRUE ~ "PPI",
                            FALSE ~ "Não PPI")) |> 
  ggplot(aes(x = V2009,
             y = txdesocup,
             color = PPI)) +
  geom_line() +
  geom_smooth(se = FALSE) + 
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.25)) +
  labs(x = "Idade",
       y = "Taxa de Desemprego",
       color = NULL,
       caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099",
       title = "Taxa de desemprego x Idade por Raça") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -5))

# Salvando o resultado
ggplot2::ggsave(
  plot = last_plot(),
  filename = "output/pnad/desemprego_idade.png",
  width = 7200,
  height = 4050,
  dpi = 600,
  units = "px")

## Tx de desemprego no Y - Anos de estudo no X

desempregados_anos_estudo <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 65,
                V2010 != "Ignorado") |> 
  dplyr::group_by(VD3005, PPI) |> 
  dplyr::summarise(
    txdesocup = survey_ratio(numerator = VD4002 == "Pessoas desocupadas",
                             denominator = VD4001 == "Pessoas na força de trabalho",
                             na.rm = TRUE))

desempregados_anos_estudo |> 
  dplyr::mutate(
    PPI = dplyr::case_match(PPI,
                            TRUE ~ "PPI",
                            FALSE ~ "Não PPI"),
    VD3005 = as.numeric(VD3005) - 1) |> 
  ggplot(aes(x = VD3005,
             y = txdesocup,
             color = PPI,
             group = PPI)) +
  geom_line() +
  geom_smooth(se = FALSE) + 
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.15)) +
  scale_x_continuous(breaks = 0:16, labels = c(0:15, "16+")) +
  labs(x = "Anos de estudo",
       y = "Taxa de Desemprego",
       color = NULL,
       caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099",
       title = "Taxa de desemprego x Anos de Estudo por por Raça") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -5))

# Salvando o resultado
ggplot2::ggsave(
  plot = last_plot(),
  filename = "output/pnad/desemprego_anos_estudo.png",
  width = 7200,
  height = 4050,
  dpi = 600,
  units = "px")

desempregados_anos_estudo <- survey_pnadc_2022_04 |> 
  dplyr::filter(V2009 > 17, V2009 < 65,
                V2010 != "Ignorado") |> 
  dplyr::group_by(VD3005) |> 
  dplyr::summarise(
    txdesocup = survey_ratio(numerator = VD4002 == "Pessoas desocupadas",
                             denominator = VD4001 == "Pessoas na força de trabalho",
                             na.rm = TRUE))

desempregados_anos_estudo |> 
  dplyr::mutate(VD3005 = as.numeric(VD3005) - 1) |> 
  ggplot(aes(x = VD3005,
             y = txdesocup)) +
  geom_line() +
  geom_smooth(se = FALSE) + 
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.15)) +
  scale_x_continuous(breaks = 0:16, labels = c(0:15, "16+")) +
  labs(x = "Anos de estudo",
       y = "Taxa de Desemprego",
       color = NULL,
       caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099",
       title = "Taxa de desemprego x Anos de Estudo") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -5))

# Salvando o resultado
ggplot2::ggsave(
  plot = last_plot(),
  filename = "output/pnad/desemprego_anos_estudo_geral.png",
  width = 7200,
  height = 4050,
  dpi = 600,
  units = "px")
