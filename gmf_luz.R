# Pacotes
library(readxl)
library(dplyr)
library(ggplot2)

# Importar dados
df <- read_excel("/Users/gmf/Library/CloudStorage/OneDrive-Personal/UNIFECAF/2025/2025-1/Projeto/Resultados/Estatistica/Results.xlsx")

# -----------------------------
# Criar variáveis auxiliares
# -----------------------------

# Variável binária para doença crônica
df$Doenca_cronica <- ifelse(is.na(df$`Você possui alguma doença crônica?`) |
                              df$`Você possui alguma doença crônica?` %in% c("Nenhum","Não"),
                            "Não","Sim")

# Variável de risco de depressão a partir do PHQ-9
df$Risco <- cut(df$`Pontuação (PHQ-9)`,
                breaks = c(-1,4,9,14,19,27),
                labels = c("Mínimo","Leve","Moderado","Moderadamente Grave","Grave"))

# -----------------------------
# 1. PHQ-9 por gênero
# -----------------------------
# Teste de normalidade
by(df$`Pontuação (PHQ-9)`, df$`Como você se identifica em termos de gênero?`, shapiro.test)

# Teste t de Student
t.test(`Pontuação (PHQ-9)` ~ `Como você se identifica em termos de gênero?`, data = df)

# Mann-Whitney (alternativa não paramétrica)
wilcox.test(`Pontuação (PHQ-9)` ~ `Como você se identifica em termos de gênero?`, data = df)

# Gráfico
ggplot(df, aes(x=`Como você se identifica em termos de gênero?`, 
               y=`Pontuação (PHQ-9)`, fill=`Como você se identifica em termos de gênero?`)) +
  geom_boxplot() +
  labs(title="PHQ-9 por gênero", x="Gênero", y="Pontuação PHQ-9")

# -----------------------------
# 2. PHQ-9 por faixa etária
# -----------------------------
# ANOVA
anova_model <- aov(`Pontuação (PHQ-9)` ~ `Faixa etária`, data=df)
summary(anova_model)

# Kruskal-Wallis
kruskal.test(`Pontuação (PHQ-9)` ~ `Faixa etária`, data=df)

# Gráfico
ggplot(df, aes(x=`Faixa etária`, y=`Pontuação (PHQ-9)`, fill=`Faixa etária`)) +
  geom_boxplot() +
  labs(title="PHQ-9 por faixa etária", x="Faixa etária", y="Pontuação PHQ-9")

# -----------------------------
# 3. PHQ-9 e doença crônica
# -----------------------------
t.test(`Pontuação (PHQ-9)` ~ Doenca_cronica, data=df)
wilcox.test(`Pontuação (PHQ-9)` ~ Doenca_cronica, data=df)

# Gráfico
ggplot(df, aes(x=Doenca_cronica, y=`Pontuação (PHQ-9)`, fill=Doenca_cronica)) +
  geom_boxplot() +
  labs(title="PHQ-9 e presença de doença crônica", x="Doença crônica", y="Pontuação PHQ-9")

# -----------------------------
# 4. PHQ-9 e uso de medicamento (categorias de risco)
# -----------------------------
# Tabela de contingência
tab <- table(df$`Uso de medicamento psiquiátrico?`, df$Risco)
chisq.test(tab)

# Gráfico de barras
ggplot(df, aes(x=`Uso de medicamento psiquiátrico?`, fill=Risco)) +
  geom_bar(position="dodge") +
  labs(title="Risco de depressão por uso de medicamento", x="Uso de medicamento", y="Frequência")
