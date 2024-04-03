# Carregar bibliotecas
library(seewave)
library(signal)
library(dygraphs)
library(e1071)
library(dplyr)
library(tidyverse)
library(readxl)
library(tibble)
library(nortest)

# Definir diretório de trabalho
setwd("C:/Users/lucas/OneDrive/Documentos/Faculdade/7 Semestre/Proj/PRJBiomedica/Dados")

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
  plot(VetorFreq[1:(length(VetorFreq) / 2)], 
       FFT_Sinal[1:(length(FFT_Sinal) / 2)], 
       type = "l", 
       main = paste("Espectro de Frequência -", nome),
       xlab = "Frequência (Hz)", 
       ylab = "Magnitude")
  
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
Normalidade_ControleMAVSD <- c(MAVSD_CONTROLE[1],MAVSD_CONTROLE[2],MAVSD_CONTROLE[3],MAVSD_CONTROLE[4],MAVSD_CONTROLE[5])
Normalidade_ProblemaMotorMAVSD <- c(MAVSD_PROBLEMAMOTOR[1],MAVSD_PROBLEMAMOTOR[2],MAVSD_PROBLEMAMOTOR[3],MAVSD_PROBLEMAMOTOR[4],MAVSD_PROBLEMAMOTOR[5])

TesteNormalidade_Controle_MAVSD <- shapiro.test(Normalidade_ControleMAVSD)
TesteNormalidade_ProblemaMotor_MAVSD <- shapiro.test(Normalidade_ProblemaMotorMAVSD)

TesteNormalidade_Controle_MAVSD
TesteNormalidade_ProblemaMotor_MAVSD

# Hipótese MAVSD -------------

Teste_Tstudent_MAVSD <- t.test(Normalidade_ControleMAVSD, Normalidade_ProblemaMotorMAVSD)
Teste_Tstudent_MAVSD


# Normalidade F50 -------------
Normalidade_ControleF50 <- c(F50_CONTROLE[1],F50_CONTROLE[2],F50_CONTROLE[3],F50_CONTROLE[4],F50_CONTROLE[5])
Normalidade_ProblemaMotorF50 <- c(F50_PROBLEMAMOTOR[1],F50_PROBLEMAMOTOR[2],F50_PROBLEMAMOTOR[3],F50_PROBLEMAMOTOR[4],F50_PROBLEMAMOTOR[5])

TesteNormalidade_Controle_F50 <- shapiro.test(Normalidade_ControleF50)
TesteNormalidade_ProblemaMotor_F50 <- shapiro.test(Normalidade_ProblemaMotorF50) 

TesteNormalidade_Controle_F50
TesteNormalidade_ProblemaMotor_F50

# Hipótese F50 ------------

Teste_Hipótese_F50 <- t.test(Normalidade_ControleF50, Normalidade_ProblemaMotorF50)

Teste_Hipótese_F50

# Normalidade Kurtosis ------------

Normalidade_Controle_Kurtosis <- c(KURTOSIS_CONTROLE[1], KURTOSIS_CONTROLE[2], KURTOSIS_CONTROLE[3], KURTOSIS_CONTROLE[4], KURTOSIS_CONTROLE[5])
Normalidade_ProblemaMotor_Kurtosis <- c(KURTOSIS_PROBLEMAMOTOR[1], KURTOSIS_PROBLEMAMOTOR[2], KURTOSIS_PROBLEMAMOTOR[3], KURTOSIS_PROBLEMAMOTOR[4], KURTOSIS_PROBLEMAMOTOR[5])

TesteNormalidade_Controle_Kurtosis <- shapiro.test(Normalidade_Controle_Kurtosis)
TesteNormalidade_ProblemaMotor_Kurtosis <- shapiro.test(Normalidade_ProblemaMotor_Kurtosis)

TesteNormalidade_Controle_Kurtosis
TesteNormalidade_ProblemaMotor_Kurtosis


# Módulo 3 - FILTRO -------------------------------------------------------
# Utilizar filtro passa faixa Butterworth de 4a ordem, 1 a 16Hz 
# Filtro Butterworth

# Função para calcular características com filtro
calcular_caracteristicas_filtrado <- function(dados, tempo, nome) {
  # Plotar o gráfico
  plot(tempo, dados, 
       type = "l", col = "green", 
       xlab = "Tempo", ylab = nome, 
       main = paste("Dados Filtrados", nome))
  
  # Aplicar filtro Butterworth
  n <- 4 # Ordem do filtro
  fs <- 50 # Frequência de amostragem em Hz
  fpass <- c(1, 16) # Frequência de corte em Hz
  butterworth <- butter(n, c(fpass/(fs*0.5)), type = "pass")
  
  # Filtrar os dados
  dados_filtrados <- filtfilt(butterworth, dados)
  
  # Plotar sinal filtrado no domínio do tempo
  lines(tempo, dados_filtrados, col = "blue")
  
  # Transforma para o domínio da frequência
  FreqAmostragem <- 1 / (tempo[2] - tempo[1])
  delta <- FreqAmostragem / length(tempo)
  VetorFreq <- seq(0, (length(tempo) - 1) * delta, delta)
  
  # Cálculo da FFT do sinal
  FFT_Sinal <- Mod(fft(dados_filtrados))
  
  # Plotando o espectro de frequência do sinal filtrado
  plot(VetorFreq[1:(length(VetorFreq) / 2)], FFT_Sinal[1:(length(FFT_Sinal) / 2)],
       type = "l", col = "red",
       xlab = "Frequência (Hz)", ylab = "Magnitude",
       main = paste("Espectro de Frequência Filtrado -", nome))
  
  # Calculando F50 para o sinal filtrado
  df_F50 <- data.frame(Frequencia = VetorFreq, Amplitude = FFT_Sinal)
  EnergiaTotal <- sum(df_F50$Amplitude)
  Limiar_F50 <- 0.5 * EnergiaTotal
  Soma_Cumulativa <- 0
  F50_Filtrado <- NULL
  
  for (i in 1:nrow(df_F50)) {
    Soma_Cumulativa <- Soma_Cumulativa + df_F50$Amplitude[i]
    if (Soma_Cumulativa >= Limiar_F50) {
      F50_Filtrado <- df_F50$Frequencia[i]
      break
    }
  }
  
  return(list(MAVSD_Filtrado = mean(abs(diff(diff(dados_filtrados, differences = 2)))),
              F50_Filtrado = F50_Filtrado,
              Kurtosis_Filtrado = kurtosis(c(tempo, dados_filtrados))))
}

# Loop sobre cada sinal de controle
resultados_controle_filtrado <- lapply(nomes_colunas_controle, function(nome_coluna) {
  dados <- GrupoControleTotal[[nome_coluna]]
  tempo <- GrupoControleTotal$Tempo
  calcular_caracteristicas_filtrado(dados, tempo, nome_coluna)
})

# Loop sobre cada sinal de problema motor
resultados_problema_motor_filtrado <- lapply(nomes_colunas_problema_motor, function(nome_coluna) {
  dados <- GrupoProblemaMotorTotal[[nome_coluna]]
  tempo <- GrupoProblemaMotorTotal$Tempo
  calcular_caracteristicas_filtrado(dados, tempo, nome_coluna)
})

# Extrair os resultados para criar a tabela
MAVSD_CONTROLE_Filtrado <- sapply(resultados_controle_filtrado, function(x) x$MAVSD_Filtrado)
F50_CONTROLE_Filtrado <- sapply(resultados_controle_filtrado, function(x) x$F50_Filtrado)
KURTOSIS_CONTROLE_Filtrado <- sapply(resultados_controle_filtrado, function(x) x$Kurtosis_Filtrado)

MAVSD_PROBLEMAMOTOR_Filtrado <- sapply(resultados_problema_motor_filtrado, function(x) x$MAVSD_Filtrado)
F50_PROBLEMAMOTOR_Filtrado <- sapply(resultados_problema_motor_filtrado, function(x) x$F50_Filtrado)
KURTOSIS_PROBLEMAMOTOR_Filtrado <- sapply(resultados_problema_motor_filtrado, function(x) x$Kurtosis_Filtrado)

# Criar a tabela com dados filtrados
Tabela_Características_Filtradas <- data.frame(
  MAVSD_F = c(MAVSD_CONTROLE_Filtrado, MAVSD_PROBLEMAMOTOR_Filtrado),
  F50_F = c(F50_CONTROLE_Filtrado, F50_PROBLEMAMOTOR_Filtrado),
  KURTOSIS_F = c(KURTOSIS_CONTROLE_Filtrado, KURTOSIS_PROBLEMAMOTOR_Filtrado)
)

# Adicionar nomes das linhas
rownames(Tabela_Características_Filtradas) <- c(paste0("C", 1:5), paste0("DP", 1:5))

# Mostrar a tabela com dados filtrados
Tabela_Características_Filtradas

Tabela_Características

