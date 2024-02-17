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

#MAVSD TempoXControle

#MAVSD TexmpoXProblemaMotor
mavsdasa <- mean(abs(diff(dados_controle)))
print(mavsd)


#F50

#Kurtosis
library(e1071)
dados <- c(dados$Tempo, dados$Controle, dados$Problema.Motor)
curtose <- kurtosis(dados)
print(curtose)
