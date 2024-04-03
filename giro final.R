# Grupo Sensor Inercial (Giroscópio) | PROJ - Engenharia Biomédica

# // BIBLIOTECAS \\ 

library(seewave)
library(signal)
library(dygraphs)
library(e1071)
library(dplyr)
library(tidyverse)
library(readxl)
library(tibble)
library(nortest)
#INÍCIO---------------------------------------------------------

setwd("C:/Users/lucas/OneDrive/Documentos/Faculdade/7 Semestre/Proj/PRJBiomedica")

#Dados Grupo 1 ------------------------------------------------------

GrupoControleTotal <- read.table("Grupo2 Giroscopio Controle.txt", header = TRUE, sep = "\t", dec = ',')

#Sinal 1.1 ----------------------------------------------------------

DadosControle1 <- data.frame(
  Tempo = GrupoControleTotal$Tempo,
  Controle = GrupoControleTotal$C1
)

# Plotar o gráfico
plot(DadosControle1$Tempo, DadosControle1$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Dados Controle C1")

# Transforma para o domínio da frequência
FreqAmostragem1.1 <- 1/(DadosControle1$Tempo[2]-DadosControle1$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta1.1 <- FreqAmostragem1.1/(length(DadosControle1$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes1.1 <- (length(DadosControle1$Tempo)-1)*delta1.1

#Criação do Vetor de frequência
VetorFreq1.1 <- seq(0,FreqRes1.1,delta1.1)

#Cálculo da FFT do sinal
FFT_Sinal1.1 <- Mod(fft(DadosControle1$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq1.1[1:(length(VetorFreq1.1)/2)],
        mag = FFT_Sinal1.1[1:(length(FFT_Sinal1.1)/2)]),
        main = "Espectro de Frequência - Controle 1",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ------------------------------------------------------------

# MAVSD -------

MAVSD_CONTROLE_1.1 <- mean(abs(diff(diff(DadosControle1$Controle, differences = 2))))

# F50 -------

#  Data frame com frequências e amplitudes
df_F50_CONTROLE1.1 <- data.frame(Frequencia = VetorFreq1.1, Amplitude = FFT_Sinal1.1)

# Calcule a F50
EnergiaTotal_1.1 <- sum(df_F50_CONTROLE1.1$Amplitude)
Limiar_F50_1.1 <- 0.5 * EnergiaTotal_1.1

F50_Controle1.1 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_CONTROLE1.1)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_CONTROLE1.1$Amplitude[i]
  if (Soma_Cumulativa > Limiar_F50_1.1) {
    F50_Controle1.1 <- df_F50_CONTROLE1.1$Frequencia[i]
    break
  }
}

F50_Controle1.1

# Kurtosis -------

dadosKurtosis1.1 <- c(DadosControle1$Tempo, DadosControle1$Controle)
Kurtosis1.1 <- kurtosis(dadosKurtosis1.1)




# /Sinal 1.2\ ----------------------------------------------------------

DadosControle2 <- data.frame(
  Tempo = GrupoControleTotal$Tempo,
  Controle = GrupoControleTotal$C2
)

# Plotar o gráfico
plot(DadosControle2$Tempo, DadosControle2$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Dados Controle C2")

# Transforma para o domínio da frequência
FreqAmostragem1.2 <- 1/(DadosControle2$Tempo[2]-DadosControle2$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta1.2 <- FreqAmostragem1.2/(length(DadosControle2$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes1.2 <- (length(DadosControle2$Tempo)-1)*delta1.2

#Criação do Vetor de frequência
VetorFreq1.2 <- seq(0,FreqRes1.2,delta1.2)

#Cálculo da FFT do sinal
FFT_Sinal1.2 <- Mod(fft(DadosControle2$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq1.2[1:(length(VetorFreq1.2)/2)],
        mag = FFT_Sinal1.2[1:(length(FFT_Sinal1.2)/2)]),
        main = "Espectro de Frequência - Controle 2",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
       dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ----------------------------------------------------------------------------

# MAVSD -------

MAVSD_CONTROLE_1.2 <- mean(abs(diff(diff(DadosControle2$Controle, differences = 2))))

# F50 -------
#  Data frame com frequências e amplitudes
df_F50_CONTROLE1.2 <- data.frame(Frequencia = VetorFreq1.2, Amplitude = FFT_Sinal1.2)

# Calcule a F50
EnergiaTotal_1.2 <- sum(df_F50_CONTROLE1.2$Amplitude)
Limiar_F50_1.2 <- 0.5 * EnergiaTotal_1.2

F50_Controle1.2 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_CONTROLE1.2)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_CONTROLE1.2$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_1.2) {
    F50_Controle1.2 <- df_F50_CONTROLE1.2$Frequencia[i]
    break
  }
}

F50_Controle1.2
# Kurtosis -------

dadosKurtosis1.2 <- c(DadosControle2$Tempo, DadosControle2$Controle)
Kurtosis1.2 <- kurtosis(dadosKurtosis1.2)




# /Sinal 1.3\ ----------------------------------------------------------

DadosControle3 <- data.frame(
  Tempo = GrupoControleTotal$Tempo,
  Controle = GrupoControleTotal$C3
)

# Plotar o gráfico
plot(DadosControle3$Tempo, DadosControle3$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Dados Controle C3")

# Transforma para o domínio da frequência
FreqAmostragem1.3 <- 1/(DadosControle3$Tempo[2]-DadosControle3$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta1.3 <- FreqAmostragem1.3/(length(DadosControle3$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes1.3 <- (length(DadosControle3$Tempo)-1)*delta1.3

#Criação do Vetor de frequência
VetorFreq1.3 <- seq(0,FreqRes1.3,delta1.3)

#Cálculo da FFT do sinal
FFT_Sinal1.3 <- Mod(fft(DadosControle3$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq1.3[1:(length(VetorFreq1.3)/2)],
        mag = FFT_Sinal1.3[1:(length(FFT_Sinal1.3)/2)]),
        main = "Espectro de Frequência - Controle 3",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ----------------------------------------------------------------------------

# MAVSD -------

MAVSD_CONTROLE_1.3 <- mean(abs(diff(diff(DadosControle3$Controle, differences = 2))))

# F50 -------

#  Data frame com frequências e amplitudes
df_F50_CONTROLE1.3 <- data.frame(Frequencia = VetorFreq1.3, Amplitude = FFT_Sinal1.3)

# Calcule a F50
EnergiaTotal_1.3 <- sum(df_F50_CONTROLE1.3$Amplitude)
Limiar_F50_1.3 <- 0.5 * EnergiaTotal_1.3

F50_Controle1.3 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_CONTROLE1.1)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_CONTROLE1.3$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_1.3) {
    F50_Controle1.3 <- df_F50_CONTROLE1.3$Frequencia[i]
    break
  }
}

F50_Controle1.3

# Kurtosis -------

dadosKurtosis1.3 <- c(DadosControle3$Tempo, DadosControle3$Controle)
Kurtosis1.3 <- kurtosis(dadosKurtosis1.3)




# /Sinal 1.4\ ----------------------------------------------------------

DadosControle4 <- data.frame(
  Tempo = GrupoControleTotal$Tempo,
  Controle = GrupoControleTotal$C4
)

# Plotar o gráfico
plot(DadosControle4$Tempo, DadosControle4$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Dados Controle C4")

# Transforma para o domínio da frequência
FreqAmostragem1.4 <- 1/(DadosControle4$Tempo[2]-DadosControle4$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta1.4 <- FreqAmostragem1.4/(length(DadosControle4$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes1.4 <- (length(DadosControle4$Tempo)-1)*delta1.4

#Criação do Vetor de frequência
VetorFreq1.4 <- seq(0,FreqRes1.4,delta1.4)

#Cálculo da FFT do sinal
FFT_Sinal1.4 <- Mod(fft(DadosControle4$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq1.4[1:(length(VetorFreq1.4)/2)],
        mag = FFT_Sinal1.4[1:(length(FFT_Sinal1.4)/2)]),
        main = "Espectro de Frequência - Controle 4",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ----------------------------------------------------------------------------

# MAVSD -------

MAVSD_CONTROLE_1.4 <- mean(abs(diff(diff(DadosControle4$Controle, differences = 2))))

# F50 -------

#  Data frame com frequências e amplitudes
df_F50_CONTROLE1.4 <- data.frame(Frequencia = VetorFreq1.4, Amplitude = FFT_Sinal1.4)

# Calcule a F50
EnergiaTotal_1.4 <- sum(df_F50_CONTROLE1.4$Amplitude)
Limiar_F50_1.4 <- 0.5 * EnergiaTotal_1.4

F50_Controle1.4 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_CONTROLE1.1)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_CONTROLE1.4$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_1.4) {
    F50_Controle1.4 <- df_F50_CONTROLE1.4$Frequencia[i]
    break
  }
}

F50_Controle1.4

# Kurtosis -------

dadosKurtosis1.4 <- c(DadosControle4$Tempo, DadosControle4$Controle)
Kurtosis1.4 <- kurtosis(dadosKurtosis1.4)




# /Sinal 1.5\ ----------------------------------------------------------

DadosControle5 <- data.frame(
  Tempo = GrupoControleTotal$Tempo,
  Controle = GrupoControleTotal$C5
)

# Plotar o gráfico
plot(DadosControle5$Tempo, DadosControle5$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Dados Controle C5")

# Transforma para o domínio da frequência
FreqAmostragem1.5 <- 1/(DadosControle5$Tempo[2]-DadosControle5$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta1.5 <- FreqAmostragem1.5/(length(DadosControle5$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes1.5 <- (length(DadosControle5$Tempo)-1)*delta1.5

#Criação do Vetor de frequência
VetorFreq1.5 <- seq(0,FreqRes1.5,delta1.5)

#Cálculo da FFT do sinal
FFT_Sinal1.5 <- Mod(fft(DadosControle5$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq1.5[1:(length(VetorFreq1.5)/2)],
        mag = FFT_Sinal1.5[1:(length(FFT_Sinal1.5)/2)]),
        main = "Espectro de Frequência - Controle 5",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ----------------------------------------------------------------------------

# MAVSD -------------------------------------------------------------------------------

MAVSD_CONTROLE_1.5 <- mean(abs(diff(diff(DadosControle5$Controle, differences = 2))))

# F50 ---------------------------------------------------------------------------------

#  Data frame com frequências e amplitudes
df_F50_CONTROLE1.5 <- data.frame(Frequencia = VetorFreq1.5, Amplitude = FFT_Sinal1.5)

# Calcule a F50
EnergiaTotal_1.5 <- sum(df_F50_CONTROLE1.5$Amplitude)
Limiar_F50_1.5 <- 0.5 * EnergiaTotal_1.5

F50_Controle1.5 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_CONTROLE1.5)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_CONTROLE1.5$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_1.5) {
    F50_Controle1.5 <- df_F50_CONTROLE1.5$Frequencia[i]
    break
  }
}

F50_Controle1.5

# Kurtosis ---------------------------------------------------------------------------

dadosKurtosis1.5 <- c(DadosControle5$Tempo, DadosControle5$Controle)
Kurtosis1.5 <- kurtosis(dadosKurtosis1.5)














# //Dados Grupo 2\\ ------------------------------------------------------

GrupoProblemaMotorTotal <- read.table("Grupo2 Giroscopio Problema Motor.txt", header = TRUE, sep = "\t", dec = ',')

# /Sinal 2.1\ ----------------------------------------------------------

DadosProblemaMotor1 <- data.frame(
  Tempo = GrupoProblemaMotorTotal$Tempo,
  Controle = GrupoProblemaMotorTotal$DP1
)

# Plotar o gráfico
plot(DadosProblemaMotor1$Tempo, DadosProblemaMotor1$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Problema Motor", 
     main = "Dados Problema Motor DP1")

# Transforma para o domínio da frequência
FreqAmostragem2.1 <- 1/(DadosProblemaMotor1$Tempo[2]-DadosProblemaMotor1$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta2.1 <- FreqAmostragem2.1/(length(DadosProblemaMotor1$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes2.1 <- (length(DadosProblemaMotor1$Tempo)-1)*delta2.1

#Criação do Vetor de frequência
VetorFreq2.1 <- seq(0,FreqRes2.1,delta2.1)

#Cálculo da FFT do sinal
FFT_Sinal2.1 <- Mod(fft(DadosProblemaMotor1$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq2.1[1:(length(VetorFreq2.1)/2)],
        mag = FFT_Sinal2.1[1:(length(FFT_Sinal2.1)/2)]),
        main = "Espectro de Frequência - Problema Motor 1",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ------------------------------------------------------------

# MAVSD -------

MAVSD_PROBLEMAMOTOR_2.1 <- mean(abs(diff(diff(DadosProblemaMotor1$Controle, differences = 2))))

# F50 -------
#  Data frame com frequências e amplitudes
df_F50_PROBLEMAMOTOR_2.1 <- data.frame(Frequencia = VetorFreq2.1, Amplitude = FFT_Sinal2.1)

# Calcule a F50
EnergiaTotal_2.1 <- sum(df_F50_PROBLEMAMOTOR_2.1$Amplitude)
Limiar_F50_2.1 <- 0.5 * EnergiaTotal_2.1

F50_ProblemaMotor2.1 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_PROBLEMAMOTOR_2.1)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_PROBLEMAMOTOR_2.1$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_2.1) {
    F50_ProblemaMotor2.1 <- df_F50_PROBLEMAMOTOR_2.1$Frequencia[i]
    break
  }
}


F50_ProblemaMotor2.1

# Kurtosis -------

dadosKurtosis2.1 <- c(DadosProblemaMotor1$Tempo, DadosProblemaMotor1$Controle)
Kurtosis2.1 <- kurtosis(dadosKurtosis2.1)




# /Sinal 2.2\ ----------------------------------------------------------

DadosProblemaMotor2 <- data.frame(
  Tempo = GrupoProblemaMotorTotal$Tempo,
  Controle = GrupoProblemaMotorTotal$DP2
)

# Plotar o gráfico
plot(DadosProblemaMotor2$Tempo, DadosProblemaMotor2$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Problema Motor", 
     main = "Dados Problema Motor DP2")

# Transforma para o domínio da frequência
FreqAmostragem2.2 <- 1/(DadosProblemaMotor2$Tempo[2]-DadosProblemaMotor2$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta2.2 <- FreqAmostragem2.2/(length(DadosProblemaMotor2$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes2.2 <- (length(DadosProblemaMotor2$Tempo)-1)*delta2.2

#Criação do Vetor de frequência
VetorFreq2.2 <- seq(0,FreqRes2.2,delta2.2)

#Cálculo da FFT do sinal
FFT_Sinal2.2 <- Mod(fft(DadosProblemaMotor2$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq2.2[1:(length(VetorFreq2.2)/2)],
        mag = FFT_Sinal2.2[1:(length(FFT_Sinal2.2)/2)]),
        main = "Espectro de Frequência - Controle 2",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ------------------------------------------------------------

# MAVSD -------

MAVSD_PROBLEMAMOTOR_2.2 <- mean(abs(diff(diff(DadosProblemaMotor2$Controle, differences = 2))))

# F50 -------

df_F50_PROBLEMAMOTOR_2.2 <- data.frame(Frequencia = VetorFreq2.2, Amplitude = FFT_Sinal2.2)

# Calcule a F50
EnergiaTotal_2.2 <- sum(df_F50_PROBLEMAMOTOR_2.2$Amplitude)
Limiar_F50_2.2 <- 0.5 * EnergiaTotal_2.2

F50_ProblemaMotor2.2 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_PROBLEMAMOTOR_2.2)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_PROBLEMAMOTOR_2.2$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_2.2) {
    F50_ProblemaMotor2.2 <- df_F50_PROBLEMAMOTOR_2.2$Frequencia[i]
    break
  }
}


F50_ProblemaMotor2.2

# Kurtosis -------

dadosKurtosis2.2 <- c(DadosProblemaMotor1$Tempo, DadosProblemaMotor2$Controle)
Kurtosis2.2 <- kurtosis(dadosKurtosis2.2)




# /Sinal 2.3\ ----------------------------------------------------------

DadosProblemaMotor3 <- data.frame(
  Tempo = GrupoProblemaMotorTotal$Tempo,
  Controle = GrupoProblemaMotorTotal$DP3
)

# Plotar o gráfico
plot(DadosProblemaMotor3$Tempo, DadosProblemaMotor3$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Problema Motor", 
     main = "Dados Problema Motor DP1")

# Transforma para o domínio da frequência
FreqAmostragem2.3 <- 1/(DadosProblemaMotor3$Tempo[2]-DadosProblemaMotor3$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta2.3 <- FreqAmostragem2.3/(length(DadosProblemaMotor3$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes2.3 <- (length(DadosProblemaMotor3$Tempo)-1)*delta2.3

#Criação do Vetor de frequência
VetorFreq2.3 <- seq(0,FreqRes2.3,delta2.3)

#Cálculo da FFT do sinal
FFT_Sinal2.3 <- Mod(fft(DadosProblemaMotor3$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq2.3[1:(length(VetorFreq2.3)/2)],
        mag = FFT_Sinal2.3[1:(length(FFT_Sinal2.3)/2)]),
        main = "Espectro de Frequência - Controle 3",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ------------------------------------------------------------

# MAVSD -------

MAVSD_PROBLEMAMOTOR_2.3 <- mean(abs(diff(diff(DadosProblemaMotor3$Controle, differences = 2))))

# F50 -------

df_F50_PROBLEMAMOTOR_2.3 <- data.frame(Frequencia = VetorFreq2.3, Amplitude = FFT_Sinal2.3)

# Calcule a F50
EnergiaTotal_2.3 <- sum(df_F50_PROBLEMAMOTOR_2.3$Amplitude)
Limiar_F50_2.3 <- 0.5 * EnergiaTotal_2.3

F50_ProblemaMotor2.3 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_PROBLEMAMOTOR_2.3)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_PROBLEMAMOTOR_2.3$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_2.3) {
    F50_ProblemaMotor2.3 <- df_F50_PROBLEMAMOTOR_2.3$Frequencia[i]
    break
  }
}


F50_ProblemaMotor2.3

# Kurtosis -------

dadosKurtosis2.3 <- c(DadosProblemaMotor3$Tempo, DadosProblemaMotor3$Controle)
Kurtosis2.3 <- kurtosis(dadosKurtosis2.3)




# /Sinal 2.4\ ----------------------------------------------------------

DadosProblemaMotor4 <- data.frame(
  Tempo = GrupoProblemaMotorTotal$Tempo,
  Controle = GrupoProblemaMotorTotal$DP4
)

# Plotar o gráfico
plot(DadosProblemaMotor4$Tempo, DadosProblemaMotor4$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Problema Motor", 
     main = "Dados Problema Motor DP4")

# Transforma para o domínio da frequência
FreqAmostragem2.4 <- 1/(DadosProblemaMotor4$Tempo[2]-DadosProblemaMotor4$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta2.4 <- FreqAmostragem2.4/(length(DadosProblemaMotor4$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes2.4 <- (length(DadosProblemaMotor4$Tempo)-1)*delta2.4

#Criação do Vetor de frequência
VetorFreq2.4 <- seq(0,FreqRes2.4,delta2.4)

#Cálculo da FFT do sinal
FFT_Sinal2.4 <- Mod(fft(DadosProblemaMotor4$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq2.4[1:(length(VetorFreq2.4)/2)],
        mag = FFT_Sinal2.4[1:(length(FFT_Sinal2.4)/2)]),
        main = "Espectro de Frequência - Controle 4",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ------------------------------------------------------------

# MAVSD -------

MAVSD_PROBLEMAMOTOR_2.4 <- mean(abs(diff(diff(DadosProblemaMotor1$Controle, differences = 2))))

# F50 -------


df_F50_PROBLEMAMOTOR_2.4 <- data.frame(Frequencia = VetorFreq2.4, Amplitude = FFT_Sinal2.4)

# Calcule a F50
EnergiaTotal_2.4 <- sum(df_F50_PROBLEMAMOTOR_2.4$Amplitude)
Limiar_F50_2.4 <- 0.5 * EnergiaTotal_2.4

F50_ProblemaMotor2.4 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_PROBLEMAMOTOR_2.4)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_PROBLEMAMOTOR_2.4$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_2.4) {
    F50_ProblemaMotor2.4 <- df_F50_PROBLEMAMOTOR_2.4$Frequencia[i]
    break
  }
}


F50_ProblemaMotor2.4

# Kurtosis -------

dadosKurtosis2.4 <- c(DadosProblemaMotor1$Tempo, DadosProblemaMotor4$Controle)
Kurtosis2.4 <- kurtosis(dadosKurtosis2.4)




# /Sinal 2.5\ ----------------------------------------------------------

DadosProblemaMotor5 <- data.frame(
  Tempo = GrupoProblemaMotorTotal$Tempo,
  Controle = GrupoProblemaMotorTotal$DP5
)

# Plotar o gráfico
plot(DadosProblemaMotor5$Tempo, DadosProblemaMotor5$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Problema Motor", 
     main = "Dados Problema Motor DP5")

# Transforma para o domínio da frequência
FreqAmostragem2.5 <- 1/(DadosProblemaMotor5$Tempo[2]-DadosProblemaMotor5$Tempo[1])

# Cálculo do incremento do vetor de frequência
delta2.5 <- FreqAmostragem2.5/(length(DadosProblemaMotor5$Tempo))

# Cálculo do n-ésimo valor do vetor de frequência
FreqRes2.5 <- (length(DadosProblemaMotor5$Tempo)-1)*delta2.5

#Criação do Vetor de frequência
VetorFreq2.5 <- seq(0,FreqRes2.5,delta2.5)

#Cálculo da FFT do sinal
FFT_Sinal2.5 <- Mod(fft(DadosProblemaMotor5$Controle))

#Plotando o espectro

dygraph(data.frame(freq = VetorFreq2.5[1:(length(VetorFreq2.5)/2)],
        mag = FFT_Sinal2.5[1:(length(FFT_Sinal2.5)/2)]),
        main = "Espectro de Frequência - Controle 5",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
        dyRangeSelector()


# CÁLCULOS CARACTERÍSTICAS ------------------------------------------------------------

# MAVSD -------

MAVSD_PROBLEMAMOTOR_2.5 <- mean(abs(diff(diff(DadosProblemaMotor5$Controle, differences = 2))))

# F50 -------

df_F50_PROBLEMAMOTOR_2.5 <- data.frame(Frequencia = VetorFreq2.5, Amplitude = FFT_Sinal2.5)

# Calcule a F50
EnergiaTotal_2.5 <- sum(df_F50_PROBLEMAMOTOR_2.5$Amplitude)
Limiar_F50_2.5 <- 0.5 * EnergiaTotal_2.5

F50_ProblemaMotor2.5 <- NULL
Soma_Cumulativa <- 0

for (i in 1:nrow(df_F50_PROBLEMAMOTOR_2.5)) {
  Soma_Cumulativa <- Soma_Cumulativa + df_F50_PROBLEMAMOTOR_2.5$Amplitude[i]
  if (Soma_Cumulativa >= Limiar_F50_2.5) {
    F50_ProblemaMotor2.5 <- df_F50_PROBLEMAMOTOR_2.5$Frequencia[i]
    break
  }
}


F50_ProblemaMotor2.5

# Kurtosis -------

dadosKurtosis2.5 <- c(DadosProblemaMotor5$Tempo, DadosProblemaMotor5$Controle)
Kurtosis2.5 <- kurtosis(dadosKurtosis2.5)

# //Tabela final\\ ----------------------------

Tabela_Características <- data.frame(
  
  MAVSD = c(MAVSD_CONTROLE_1.1,
          MAVSD_CONTROLE_1.2,
          MAVSD_CONTROLE_1.3,
          MAVSD_CONTROLE_1.4,
          MAVSD_CONTROLE_1.5,
          MAVSD_PROBLEMAMOTOR_2.1,
          MAVSD_PROBLEMAMOTOR_2.2,
          MAVSD_PROBLEMAMOTOR_2.3,
          MAVSD_PROBLEMAMOTOR_2.4,
          MAVSD_PROBLEMAMOTOR_2.5),

  
  F50 = c(F50_Controle1.1,
          F50_Controle1.2,
          F50_Controle1.3,
          F50_Controle1.4,
          F50_Controle1.5,
          F50_ProblemaMotor2.1,
          F50_ProblemaMotor2.2,
          F50_ProblemaMotor2.3,
          F50_ProblemaMotor2.4,
          F50_ProblemaMotor2.5),
  
  KURTOSIS = c(Kurtosis1.1,
               Kurtosis1.2,
               Kurtosis1.3,
               Kurtosis1.4,
               Kurtosis1.5,
               Kurtosis2.1,
               Kurtosis2.2,
               Kurtosis2.3,
               Kurtosis2.4,
               Kurtosis2.5)
)

rownames(Tabela_Características) <- c("C1","C2","C3","C4","C5","DP1","DP2","DP3","DP4","DP5")

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



# Defina a disposição dos gráficos na janela de plotagem (2 linhas por 3 colunas)
par(mfrow = c(2, 3))


# //Plotagem dos dados de controle\\ ------------------------------------------------------------

# Plotando cada gráfico individualmente
plot(DadosControle1$Tempo, DadosControle1$Controle, type = "l", col = "purple", xlab = "Tempo", ylab = "Controle", main = "Dados Controle C1")
plot(DadosControle2$Tempo, DadosControle2$Controle, type = "l", col = "blue", xlab = "Tempo", ylab = "Controle", main = "Dados Controle C2")
plot(DadosControle3$Tempo, DadosControle3$Controle, type = "l", col = "green", xlab = "Tempo", ylab = "Controle", main = "Dados Controle C3")
plot(DadosControle4$Tempo, DadosControle4$Controle, type = "l", col = "red", xlab = "Tempo", ylab = "Controle", main = "Dados Controle C4")
plot(DadosControle5$Tempo, DadosControle5$Controle, type = "l", col = "orange", xlab = "Tempo", ylab = "Controle", main = "Dados Controle C5")
par(mfrow = c(1, 1))

# Plotando um gráfico em cima de outro
plot(DadosControle1$Tempo, DadosControle1$Controle, type = "l", col = "purple", xlab = "Tempo", ylab = "Controle", main = "Dados Controle C1")
lines(DadosControle2$Tempo, DadosControle2$Controle, col = "blue")
lines(DadosControle3$Tempo, DadosControle3$Controle, col = "green")
lines(DadosControle4$Tempo, DadosControle4$Controle, col = "red")
lines(DadosControle5$Tempo, DadosControle5$Controle, col = "orange")

# //Plotagem dos dados de Problema motor\\ ------------------------------------------------------------

# Plotando os gráficos dos problemas do motor


plot(DadosProblemaMotor1$Tempo, DadosProblemaMotor1$Controle, type = "l", col = "purple", xlab = "Tempo", ylab = "Problema Motor", main = "Dados Problema Motor DP1")
plot(DadosProblemaMotor2$Tempo, DadosProblemaMotor2$Controle, type = "l", col = "blue", xlab = "Tempo", ylab = "Problema Motor", main = "Dados Problema Motor DP2")
plot(DadosProblemaMotor3$Tempo, DadosProblemaMotor3$Controle, type = "l", col = "green", xlab = "Tempo", ylab = "Problema Motor", main = "Dados Problema Motor DP3")
plot(DadosProblemaMotor4$Tempo, DadosProblemaMotor4$Controle, type = "l", col = "red", xlab = "Tempo", ylab = "Problema Motor", main = "Dados Problema Motor DP4")
plot(DadosProblemaMotor5$Tempo, DadosProblemaMotor5$Controle, type = "l", col = "orange", xlab = "Tempo", ylab = "Problema Motor", main = "Dados Problema Motor DP5")

