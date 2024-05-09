
##
# 2 AMOSTRAS
# Exemplo prático: Avaliando o impacto de uma nova ferramenta de marketing
# Gerando dados simulados para vendas antes e depois da implementação

# Dados de exemplo
vendas_antes <- c(100, 102, 98, 97, 105, 103, 100, 95, 102, 98)
vendas_depois <- c(110, 108, 115, 120, 130, 125, 130, 112, 120, 125)

# Teste T para duas amostras independentes
# Hipóteses:
# H0: As médias são iguais
# H1: A média das vendas depois é maior que a média das vendas antes
teste_t <- t.test(vendas_antes, vendas_depois, alternative = "less", paired = TRUE)
print(teste_t)

# Teste de Wilcoxon
# Mesmas hipóteses, mas sem supor normalidade
teste_wilcoxon <- wilcox.test(vendas_antes, vendas_depois, alternative = "less", paired = TRUE)
print(teste_wilcoxon)

##
# MAIS DE 2 GRUPOS / AMOSTRAS
# Exemplo prático: Avaliando a eficácia de diferentes tipos de fertilizantes na produtividade das plantas
# Gerando dados simulados para três grupos com tratamentos diferentes
library(tibble)

# Criando os dados
grupo1 <- c(45, 55, 50, 52, 40, 48, 53, 42, 51, 49)  # Dados para o primeiro grupo
grupo2 <- c(44, 57, 49, 52, 40, 48, 53, 42, 51, 49)  # Dados para o segundo grupo
grupo3 <- c(65, 66, 68, 70, 67, 69, 72, 64, 71, 73)  # Dados para o terceiro grupo

# Preparando os dados para ANOVA
dados <- tibble(
  valores = c(grupo1, grupo2, grupo3),
  grupo = factor(rep(c("Grupo1", "Grupo2", "Grupo3"), each = 10))
)

# Teste de normalidade para verificar se as amostras seguem uma distribuição normal
shapiro.test(c(grupo1, grupo2, grupo3))  # Teste de Shapiro-Wilk

# Realizando a ANOVA
resultado_anova <- aov(valores ~ grupo, data = dados)
summary(resultado_anova)  # Exibindo o resultado da ANOVA

# Teste de Kruskal-Wallis para comparar as medianas dos três grupos quando a normalidade é questionada
kruskal_teste <- kruskal.test(list(grupo1, grupo2, grupo3))
print(kruskal_teste)  # Exibe o resultado do teste de Kruskal-Wallis

# Teste de Dunn para análises pós-hoc após rejeição da hipótese nula pelo Kruskal-Wallis
library(dunn.test)
dunn_resultado <- dunn.test(list(grupo1, grupo2, grupo3))
print(dunn_resultado)  # Exibe o resultado detalhado do teste de Dunn

