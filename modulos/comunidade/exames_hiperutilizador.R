library(dplyr)

# Simulação de dados
set.seed(123)
dados <- data.frame(
  id_usuario = 1:100,
  faz_exame_X = sample(c(TRUE, FALSE), 100, replace = TRUE),
  qtde_exames = rpois(100, lambda = 5)
)

# Definição de hiperutilizador baseado no terceiro quartil de quem não faz o exame X
limite_hiperutilizador <- quantile(dados$qtde_exames[dados$faz_exame_X == FALSE], 0.75)

# Identificação de hiperutilizadores entre quem faz o exame X
dados <- dados |>
  mutate(hiperutilizador = faz_exame_X & qtde_exames > limite_hiperutilizador)

# Comparação das medianas de quantidade de exames entre os grupos com e sem o exame X
wilcox.test(qtde_exames ~ faz_exame_X, data = dados, paired = FALSE)

# Visualização dos hiperutilizadores
dados |> filter(hiperutilizador == TRUE)
