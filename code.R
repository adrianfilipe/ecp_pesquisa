# Instalar e carregar o pacote
if(!require(msm)) install.packages("msm")
library(msm)
library(dplyr)


df <- read.csv2("C:/Users/b46817/Desktop/Pesquisa/dados_processados_para_modelo.csv", dec=",")

# Converter datas e garantir que estão ordenadas por atleta e tempo
df$Dia <- as.Date(df$Dia)
df <- df[order(df$Atleta, df$Dia), ]

# Criar uma coluna de tempo numérico (dias desde o início para cada atleta)
df <- df %>%
  group_by(Atleta) %>%
  mutate(Tempo = as.numeric(Dia - min(Dia))) %>%
  ungroup()

# Garantir que o estado de Afastamento comece em 1 (o msm exige estados 1, 2, 3...)
# Se o seu 'Afastamento' for 0, 1, 2 -> somamos 1 para virar 1, 2, 3
df$Estado <- df$Afastamento + 1


# Matriz 3x3 (para os estados 1, 2 e 3 de afastamento)
# 1 indica transição permitida, 0 indica impossível
q_inicial <- matrix(c(0, 1, 1,
                      1, 0, 1,
                      1, 1, 0), 
                    nrow = 3, byrow = TRUE)


# Ajustando o modelo
modelo_msm <- msm(Estado ~ Tempo, 
                  subject = Atleta, 
                  data = df, 
                  qmatrix = q_inicial, 
                  gen.inits = TRUE) # gera estimativas iniciais automaticamente

# 1. Visualizar a Matriz de Intensidade (Q)
print(modelo_msm)

# 2. Obter a Matriz de Probabilidade de Transição para daqui a 7 dias
# (Pode alterar o t para o intervalo que desejar)
P_7dias <- pmatrix.msm(modelo_msm, t = 7)
print("Probabilidades de transição após 7 dias:")
print(P_7dias)

# 3. Calcular o Tempo Esperado de Permanência (Sojourn Times)
# Quanto tempo, em média, o atleta fica em cada estado antes de mudar
tempos_esperados <- sojourn.msm(modelo_msm)
print("Tempo médio de permanência em cada estado (em dias):")
print(tempos_esperados)



# Modelo onde a PSE influencia as taxas de transição
modelo_covariada <- msm(Estado ~ Tempo, 
                        subject = Atleta, 
                        data = df, 
                        qmatrix = q_inicial, 
                        covariates = ~ PSE + Vigor)

# Ver os Hazard Ratios (Risco relativo)
# Se o HR da PSE for > 1, significa que maior PSE acelera a transição (lesão)
hazard.msm(modelo_covariada)




