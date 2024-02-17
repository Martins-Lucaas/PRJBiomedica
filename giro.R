# Recebendo os dados ------------------------------------------------------
setwd("C:/Users/lucas/OneDrive/Documentos/Faculdade/7 Semestre/Proj/PRJBiomedica")


# Ler os dados
dados <- read.table("giro.txt", header = TRUE, sep = "\t", dec = ",")

# Plotar o gráfico
plot(dados$Tempo, dados$Controle, type = "l", col = "purple", xlab = "Tempo", ylab = "Controle", main = "Giroscópio")
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

plot(dados_controle$Tempo, dados_controle$Controle, type = "l", col = "green", xlab ="tempo", ylab ="controle", main = "TempoXcontrole")
plot(dados_Problema$Tempo, dados_Problema$Problema, type = "l", col = "red", xlab ="tempo", ylab ="problema", main = "TempoXProblema")

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
library(e1071)
dados <- c(dados$Tempo, dados$Controle, dados$Problema.Motor)
curtose <- kurtosis(dados)
print(curtose)



# Fazer FFT ---------------------------------------------------------------



