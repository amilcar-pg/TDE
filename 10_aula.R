# importando dados e definindo parametros ---------------------------------
df_legend <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Legend")
df_data <- readxl::read_xlsx("data/pwt1001.xlsx", sheet = "Data")
ggplot2::theme_set(ggthemes::theme_economist())
library(ggplot2)

# exercicio ---------------------------------------------------------------

# eixo x = tx media de variacao do capital humano;
# eixo y = tx media de variacao do pi per capita
