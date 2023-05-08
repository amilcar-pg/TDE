library(PNADcIBGE)

df_raw <- get_pnadc(year = 2022, quarter  = 4,
                               vars = c("VD3005", # anos de estudo
                                        "VD4017"),
                    design = FALSE) # remuneração

library(ggplot2)

df_raw |> 
  ggplot(
    aes(
      x = as.numeric(VD3005),
      y = VD4017
    )
  ) +
  geom_point() +
  ggthemes::theme_economist() +
  labs(x = "Anos de estudo",
        y = "Remuneração do trabalho principal",
        caption = "Amilcar L. do Prado G. Gramacho \n 21/2008099")


