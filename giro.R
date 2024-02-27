# Recebendo os dados ------------------------------------------------------
setwd("C:/Users/lucas/OneDrive/Documentos/Faculdade/7 Semestre/Proj/PRJBiomedica")

# Ler os dados
dados <- read.table("giro.txt", header = TRUE, sep = "\t", dec = ",")


# Plotar o gráfico
plot(dados$Tempo, dados$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Giroscópio")

lines(dados$Tempo, dados$Problema.Motor, col = "blue")
legend("topright", legend = c("Controle", "Problema Motor"), col = c("purple", "blue"), lty = 1)

dados_controle <- data.frame(
  Tempo = dados$Tempo,
  Controle = dados$Controle
)

dados_Problema <- data.frame(
  Tempo = dados$Tempo,
  Problema = dados$Problema.Motor
)

plot(dados_controle$Tempo, dados_controle$Controle, 
     type = "l", col = "green", 
     xlab ="tempo", ylab ="controle", 
     main = "TempoXcontrole")
plot(dados_Problema$Tempo, dados_Problema$Problema, 
     type = "l", col = "red", 
     xlab ="tempo", ylab ="problema", 
     main = "TempoXProblema")


# Características ---------------------------------------------------------

# MAVSD CONTROLE ----------------------------------------------------------
mavsd_controle <- mean(abs(diff(dados_controle$Controle)))
print(mavsd_controle)

# MAVSD PROBLEMA -------------------------------------------------------------------
media <- mean(dados_controle$Controle)
mavsd_Problema <- mean(abs(diff(dados_Problema$Problema)))
print(mavsd_Problema)


# F50 ---------------------------------------------------------------------

#F50 Dados Controle
Controle_ordenado <- sort(dados_controle$Controle)
freq_acumulada <- cumsum(table(Controle_ordenado))
F50_controle <- names(freq_acumulada)[which.max(freq_acumulada >= 0.5)]
F50_controle

#F50 Dados problema
Problema_ordenado <- sort(dados_Problema$Problema)
freq_acumulada2 <- cumsum(table(Problema_ordenado))
F50_Problema <- names(freq_acumulada2)[which.max(freq_acumulada >= 0.5)]
F50_Problema


# KURTOSIS ----------------------------------------------------------------
# KURTOSIS Dados controle

library(e1071)
dadoskur <- c(dados$Tempo, dados_controle$Controle)
curtose <- kurtosis(dadoskur)
print(curtose)

# KURTOSIS Dados Problema
dadoskur2 <- c(dados$Tempo, dados_Problema$Problema)
curtose1 <- kurtosis(dadoskur2)
print(curtose1)

# Fazer FFT ---------------------------------------------------------------

# FFT Dados Controle
AmplitudeFFTControle <- c(dados$Controle)

FFTControle <- fft(AmplitudeFFTControle)

df.dadosFFTControle <- data.frame(
  Tempo = dados$Tempo,
  AmplitudeFFT.Controle = FFTControle
)

plot(dados$Tempo, dados$Controle, # Sinal Original
     type = "l", col = "red",
     xlab ="Tempo", ylab ="Amplitude", 
     main = "Original Signal") 

plot(df.dadosFFTControle$Tempo, df.dadosFFTControle$AmplitudeFFT.Controle, 
     type = "l", col = "purple",
     xlab ="Tempo", ylab ="Amplitude", 
     main = "Sinal FFT - Controle")


# FFT Dados Problema
AmplitudeFFTProblema <- c(dados$Problema.Motor)

FFTProblema <- fft(AmplitudeFFTProblema)

df.dadosFFTProblema <- data.frame(
  Tempo = dados$Tempo,
  AmplitudeFFT.Problema = FFTProblema
)

plot(dados$Tempo, dados$Problema.Motor, # Sinal Original
     type = "l", col = "red",
     xlab ="Tempo", ylab ="Amplitude", 
     main = "Original Signal") 

plot(df.dadosFFTProblema$Tempo, df.dadosFFTProblema$AmplitudeFFT.Problema, 
     type = "l", col = "purple", 
     xlab ="Tempo", ylab ="Amplitude", 
     main = "Sinal FFT - Problema")


