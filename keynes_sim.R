# Modello keynesiano SFC (SIM di Godley e Lavoie, 2007)
# Prepara l'ambiente ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")              # Cancella tutto
nPeriods   <- 180        # Numero di periodi
nScenarios <- 2          # 1 = scenario base; 2 = shock di spesa pubblica
# Crea funzione che definisce le variabili come matrici ####
mat <- function(z) matrix(data = z, nrow = nScenarios, ncol = nPeriods)
# Definisci i coefficienti del modello ####
alpha1 <- 0.6            # Propensione al consumo dal reddito disponibile
alpha2 <- 0.2            # Propensione al consumo dalla ricchezza (moneta)
theta  <- 0.2            # Aliquota fiscale
# Definisci le variabili del modello ####
G  <- mat(20)            # Spesa pubblica (esogena)
Y  <- mat(0)             # Reddito
Tax <- mat(0)            # Tasse
YD <- mat(0)             # Reddito disponibile
C <- mat(0)              # Consumo
H <- mat(0)              # Stock di ricchezza
# Lancia il modello ####
for (j in 1:nScenarios) {
  
  for (i in 2:nPeriods) {
    
    if (i >= 120 && j == 2) G[j, i] <- 25          # Shock: spesa pubblica 20 -> 25
    
    for (iter in 1:60) {                          # Iterazioni per convergenza alla soluzione simultanea
      
      Y[j, i]  <- C[j, i] + G[j, i]
      Tax[j, i] <- theta * Y[j, i]
      YD[j, i] <- Y[j, i] - Tax[j, i]
      C[j, i]  <- alpha1 * YD[j, i] + alpha2 * H[j, i - 1]
      
    }
    
    H[j, i] <- H[j, i - 1] + (YD[j, i] - C[j, i])
    
  }
}
# Visualizza i risultati ####
plot(Y[2,100:nPeriods], type = "l", lwd = 2, col = 4, ylim = range(min(Y[,100:nPeriods]),max(H[,100:nPeriods])),
     main = "Modello keynesiano SFC (SIM): shock di spesa pubblica",
     xlab = "Tempo", ylab = "Livello", font.main = 1, cex.main = 1, cex.axis=1, cex.lab=1)
#grid()
lines(H[2,100:nPeriods], lwd = 2, lty = 1, col = 2)
legend("right", c("Reddito", "Stock di moneta"),
       lty = c(1, 1), lwd = c(2, 2), col = c(4, 2), bty = "n", cex=1)