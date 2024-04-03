# Carregar bibliotecas
library(seewave)
library(dygraphs)
library(dplyr)

# Definir diretório de trabalho
setwd("C:/Users/lucas/OneDrive/Documentos/Faculdade/7 Semestre/Proj/PRJBiomedica")

# Função para calcular características
calcular_caracteristicas <- function(dados, tempo, nome) {
  # Plotar o gráfico
  plot(tempo, dados, 
       type = "l", col = "purple", 
       xlab = "Tempo", ylab = nome, 
       main = paste("Dados", nome))
  
  # Transforma para o domínio da frequência
  FreqAmostragem <- 1 / (tempo[2] - tempo[1])
  delta <- FreqAmostragem / length(tempo)
  VetorFreq <- seq(0, (length(tempo) - 1) * delta, delta)
  
  # Cálculo da FFT do sinal
  FFT_Sinal <- Mod(fft(dados))
  
  # Plotando o espectro
  dygraph(data.frame(freq = VetorFreq[1:(length(VetorFreq) / 2)],
                     mag = FFT_Sinal[1:(length(FFT_Sinal) / 2)]),
          main = paste("Espectro de Frequência -", nome),
          xlab = "Frequência (Hz)",
          ylab = "Magnitude") %>%
    dyRangeSelector()
  
  # Cálculos de características
  MAVSD <- mean(abs(diff(diff(dados, differences = 2))))
  
  # F50
  df_F50 <- data.frame(Frequencia = VetorFreq, Amplitude = FFT_Sinal)
  EnergiaTotal <- sum(df_F50$Amplitude)
  Limiar_F50 <- 0.5 * EnergiaTotal
  Soma_Cumulativa <- 0
  F50 <- NULL
  
  for (i in 1:nrow(df_F50)) {
    Soma_Cumulativa <- Soma_Cumulativa + df_F50$Amplitude[i]
    if (Soma_Cumulativa >= Limiar_F50) {
      F50 <- df_F50$Frequencia[i]
      break
    }
  }
  
  # Curtose
  dadosKurtosis <- c(tempo, dados)
  Kurtosis <- kurtosis(dadosKurtosis)
  
  return(list(MAVSD = MAVSD, F50 = F50, Kurtosis = Kurtosis))
}

# Carregar dados do grupo controle
GrupoControleTotal <- read.table("Grupo2 Giroscopio Controle.txt", header = TRUE, sep = "\t", dec = ',')

# Nomes das colunas
nomes_colunas_controle <- c("C1", "C2", "C3", "C4", "C5")

# Loop sobre cada sinal de controle
resultados_controle <- lapply(nomes_colunas_controle, function(nome_coluna) {
  dados <- GrupoControleTotal[[nome_coluna]]
  tempo <- GrupoControleTotal$Tempo
  calcular_caracteristicas(dados, tempo, nome_coluna)
})

# Carregar dados do grupo problema motor
GrupoProblemaMotorTotal <- read.table("Grupo2 Giroscopio Problema Motor.txt", header = TRUE, sep = "\t", dec = ',')

# Nomes das colunas
nomes_colunas_problema_motor <- c("DP1", "DP2", "DP3", "DP4", "DP5")

# Loop sobre cada sinal de problema motor
resultados_problema_motor <- lapply(nomes_colunas_problema_motor, function(nome_coluna) {
  dados <- GrupoProblemaMotorTotal[[nome_coluna]]
  tempo <- GrupoProblemaMotorTotal$Tempo
  calcular_caracteristicas(dados, tempo, nome_coluna)
})

# Extrair os resultados para criar a tabela
MAVSD_CONTROLE <- sapply(resultados_controle, function(x) x$MAVSD)
F50_CONTROLE <- sapply(resultados_controle, function(x) x$F50)
KURTOSIS_CONTROLE <- sapply(resultados_controle, function(x) x$Kurtosis)

MAVSD_PROBLEMAMOTOR <- sapply(resultados_problema_motor, function(x) x$MAVSD)
F50_PROBLEMAMOTOR <- sapply(resultados_problema_motor, function(x) x$F50)
KURTOSIS_PROBLEMAMOTOR <- sapply(resultados_problema_motor, function(x) x$Kurtosis)

# Criar a tabela
Tabela_Características <- data.frame(
  MAVSD = c(MAVSD_CONTROLE, MAVSD_PROBLEMAMOTOR),
  F50 = c(F50_CONTROLE, F50_PROBLEMAMOTOR),
  KURTOSIS = c(KURTOSIS_CONTROLE, KURTOSIS_PROBLEMAMOTOR)
)

# Adicionar nomes das linhas
rownames(Tabela_Características) <- c(paste0("C", 1:5), paste0("DP", 1:5))

# Mostrar a tabela
Tabela_Características

# //Teste de Normalidade\\ ------------------------------------------------------------

# Normalidade MAVSD ---------------

Normalidade_ControleMAVSD <- c(MAVSD_CONTROLE_1.1,MAVSD_CONTROLE_1.2,MAVSD_CONTROLE_1.3,MAVSD_CONTROLE_1.4,MAVSD_CONTROLE_1.5)
Normalidade_ProblemaMotorMAVSD <- c(MAVSD_PROBLEMAMOTOR_2.1,MAVSD_PROBLEMAMOTOR_2.2,MAVSD_PROBLEMAMOTOR_2.3,MAVSD_PROBLEMAMOTOR_2.4,MAVSD_PROBLEMAMOTOR_2.5)

TesteNormalidade_Controle_MAVSD <- shapiro.test(Normalidade_ControleMAVSD)
TesteNormalidade_ProblemaMotor_MAVSD <- shapiro.test(Normalidade_ProblemaMotorMAVSD)

TesteNormalidade_Controle_MAVSD
TesteNormalidade_ProblemaMotor_MAVSD

# Hipótese MAVSD -------------

Teste_Tstudent_MAVSD <- t.test(Normalidade_ControleMAVSD, Normalidade_ProblemaMotorMAVSD)
Teste_Tstudent_MAVSD


# Normalidade F50 -------------
Normalidade_ControleF50 <- c(F50_Controle1.1,F50_Controle1.2,F50_Controle1.3,F50_Controle1.4,F50_Controle1.5)
Normalidade_ProblemaMotorF50 <- c(F50_ProblemaMotor2.1,F50_ProblemaMotor2.2,F50_ProblemaMotor2.3,F50_ProblemaMotor2.4,F50_ProblemaMotor2.5)

TesteNormalidade_Controle_F50 <- shapiro.test(Normalidade_ControleF50)
TesteNormalidade_ProblemaMotor_F50 <- shapiro.test(Normalidade_ProblemaMotorF50) 

TesteNormalidade_Controle_F50
TesteNormalidade_ProblemaMotor_F50

# Hipótese F50 ------------

Teste_Hipótese_F50 <- t.test(Normalidade_ControleF50, Normalidade_ProblemaMotorF50)

Teste_Hipótese_F50

# Normalidade Kurtosis ------------

Normalidade_Controle_Kurtosis <- c(Kurtosis1.1, Kurtosis1.2, Kurtosis1.3, Kurtosis1.4, Kurtosis1.5)
Normalidade_ProblemaMotor_Kurtosis <- c(Kurtosis2.1, Kurtosis2.2, Kurtosis2.3, Kurtosis2.4, Kurtosis2.5)

TesteNormalidade_Controle_Kurtosis <- shapiro.test(Normalidade_Controle_Kurtosis)
TesteNormalidade_ProblemaMotor_Kurtosis <- shapiro.test(Normalidade_ProblemaMotor_Kurtosis)

TesteNormalidade_Controle_Kurtosis
TesteNormalidade_ProblemaMotor_Kurtosis

# Hipótese Kurtosis ------------

Teste_Hipótese_Kurtosis <- pairwise.wilcox.test(Normalidade_Controle_Kurtosis, Normalidade_ProblemaMotor_Kurtosis)
Teste_Hipótese_Kurtosis
