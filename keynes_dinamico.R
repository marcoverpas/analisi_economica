# Modello keynesiano dinamico elementare
# Prepara l'ambiente ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")              # Cancella tutto
nPeriods   <- 60         # Numero di periodi
nScenarios <- 2          # 1 = scenario base; 2 = shock all'investimento
# Crea funzione che definisce le variabili come matrici ####
mat <- function(z) matrix(data = z, nrow = nScenarios, ncol = nPeriods)
# Definisci i coefficienti del modello ####
c0 <- 10                 # Consumo autonomo
c1 <- 0.8                # Propensione marginale al consumo (0 < c1 < 1)
I0 <- mat(10)            # Investimento autonomo
Y  <- mat((c0 + 10) / (1 - c1))   # Valore di stato stazionario del reddito (moltiplicatore)
C  <- mat(0)             # Consumo totale
I  <- mat(0)             # Investimento totale
# Lancia il modello ####
for (j in 1:nScenarios) {
  
  for (i in 2:nPeriods) {
    
    if (i >= 10 && j == 2) I0[j, i] <- 30      # Shock: investimento 10 -> 30
    
    I[j, i] <- I0[j, i]
    C[j, i] <- c0 + c1 * Y[j, i - 1]
    Y[j, i] <- C[j, i] + I0[j, i]
    
  }
}
# Visualizza i risultati ####
plot(Y[2, ], type = "l", lwd = 2, col = 4, ylim = range(min(Y[,]),max(Y[,])),
     main = "Modello keynesiano dinamico: shock all'investimento",
     xlab = "Tempo", ylab = "Reddito Y", font.main = 1, cex.main = 1, cex.axis=1, cex.lab=1)
#grid()
lines(Y[1, ], lwd = 2, lty = 3, col = 2)
legend("right", c("Scenario alternativo (shock)", "Scenario base"),
       lty = c(1, 3), lwd = c(2, 2), col = c(4, 2), bty = "n", cex=1)