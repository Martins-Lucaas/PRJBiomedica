# Recebendo os dados ------------------------------------------------------
setwd("E:/GitHub/PRJBiomedica/Dados")

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
plot(dados$Tempo, dados$Controle, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Dados Controle - Sinal Módulo 1")

FreqAmostragem <- 1/(dados$Tempo[2]-dados$Tempo[1])
delta <- FreqAmostragem/(length(dados$Tempo))
FreqRes <- (length(dados$Tempo)-1)*delta
VetorFreq <- seq(0,FreqRes,delta)
FFT_Sinal <- Mod(fft(dados$Controle))

dygraph(data.frame(freq = VetorFreq[1:(length(VetorFreq)/2)],
        mag = FFT_Sinal[1:(length(FFT_Sinal)/2)]),
        main = "Espectro de Frequência - Sinal Controle Módulo 1",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
  dyRangeSelector()

# FFT Dados Problema -------------------------------

plot(dados$Tempo, dados$Problema, 
     type = "l", col = "purple", 
     xlab = "Tempo", ylab = "Controle", 
     main = "Dados Controle - Sinal Módulo 1")

FreqAmostragem2 <- 1/(dados$Tempo[2]-dados$Tempo[1])

delta2 <- FreqAmostragem2/(length(dados$Tempo))
FreqRes2 <- (length(dados$Tempo)-1)*delta2
VetorFreq2 <- seq(0,FreqRes2,delta2)
FFT_Sinal2 <- Mod(fft(dados$Problema))

dygraph(data.frame(freq = VetorFreq2[1:(length(VetorFreq2)/2)],
                   mag = FFT_Sinal2[1:(length(FFT_Sinal2)/2)]),
        main = "Espectro de Frequência - Sinal Problema Motor Módulo 1",
        xlab = "Frequência (Hz)",
        ylab = "Magnitude") %>%
  dyRangeSelector()
