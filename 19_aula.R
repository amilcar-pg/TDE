# Modelo de Romer ---------------------------------------------------------

df_target <- data.frame(
  t = c(1:30),
  l_barra = numeric(30),
  a_chapeu = numeric(30),
  a = numeric(30),
  y = numeric(30),
  y_chapeu = numeric(30)
)

dados_iniciais = c("l_barra" = 100, "z_barra" = 1/500, "a" = 100, "la" = 0.1)

.a_chapeu <- function(z = dados_iniciais[["z_barra"]], 
                      la = ados_iniciais[["la"]],
                      L = dados_iniciais[["l_barra"]]){
  
  a_chapeu <- z*la*L
  
  return(a_chapeu)
}

.y <- function(t,
               z = dados_iniciais[["z_barra"]], 
               la = ados_iniciais[["la"]],
               L = dados_iniciais[["l_barra"]],
               a0 = dados_iniciais[["a"]]){
  
  y <- a0*(1-la)*((1+z*la*L)^t)
  
  return(y)
}

.a <- function(t,
               la = ados_iniciais[["la"]],
               L = dados_iniciais[["l_barra"]],
               a0 = dados_iniciais[["a"]]){
  a <- ao*((1+(z*la*L))^t)
  return(a)
}

df_target <- df_target |> 
  dplyr::mutate(
    a = .a(t = t),
    a_chapeu = .a_chapeu()
  )