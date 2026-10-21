# Modello di Harrod-Domar con ASPETTATIVE ESTRAPOLATIVE (il "filo del rasoio")
# Corso "Analisi economica" - Parte II (sezione 2.2)
#
#   ge = ge(-1) + lam*(g(-1) - ge(-1)) + eps   crescita attesa (adattiva)
#   dYe = ge * Y(-1)                           variazione attesa del reddito
#   I = a * dYe                                acceleratore
#   C = c1 * Y                                 consumo
#   Y = C + I                                  equilibrio (=> S = I)
#   S = Y - C                                  risparmio
#   g = Y/Y(-1) - 1                            crescita effettiva
#
# Saggio garantito: gw = s/(a - s), con s = 1 - c1

# Prepara l'ambiente
rm(list = ls(all = TRUE))

# Parametri
nPeriods = 30      # Numero di periodi
nScenarios = 3     # 1 = base, 2 = ottimismo, 3 = pessimismo
nIter = 100        # Iterazioni per risolvere il sistema in ogni periodo

c1 = 0.8           # Propensione al consumo
s = 1 - c1         # Propensione al risparmio
a = 4              # Rapporto capitale/prodotto (acceleratore)
lam = 0.01         # Velocita' di revisione delle aspettative
gw = s / (a - s)   # Saggio di crescita garantito
shockAt = 11       # Periodo dello shock
eps0 = 0.0005      # Ampiezza dello shock sulla crescita attesa (0,05 punti %)

# Matrici
Y = matrix(0, nrow = nScenarios, ncol = nPeriods)    # Reddito
C = matrix(0, nrow = nScenarios, ncol = nPeriods)    # Consumo
I = matrix(0, nrow = nScenarios, ncol = nPeriods)    # Investimento
S = matrix(0, nrow = nScenarios, ncol = nPeriods)    # Risparmio
ge = matrix(0, nrow = nScenarios, ncol = nPeriods)   # Crescita attesa
g = matrix(0, nrow = nScenarios, ncol = nPeriods)    # Crescita effettiva
dYe = matrix(0, nrow = nScenarios, ncol = nPeriods)  # Variazione attesa di Y
eps = matrix(0, nrow = nScenarios, ncol = nPeriods)  # Shock

# Valori iniziali: l'economia parte sul sentiero garantito
Y[, 1] = 100
C[, 1] = c1 * Y[, 1]
I[, 1] = s * Y[, 1]
S[, 1] = s * Y[, 1]
ge[, 1] = gw
g[, 1] = gw

# Shock una tantum sulle aspettative
eps[2, shockAt] = eps0     # Ottimismo
eps[3, shockAt] = -eps0    # Pessimismo

# Modello
for (i in 2:nPeriods){
  
  for (j in 1:nScenarios){
    
    for (iter in 1:nIter){
      
      ge[j,i] = ge[j,i-1] + lam*(g[j,i-1] - ge[j,i-1]) + eps[j,i]
      
      dYe[j,i] = ge[j,i]*Y[j,i-1]
      
      I[j,i] = a*dYe[j,i]
      
      C[j,i] = c1*Y[j,i]
      
      Y[j,i] = C[j,i] + I[j,i]
      
      S[j,i] = Y[j,i] - C[j,i]
      
      g[j,i] = Y[j,i]/Y[j,i-1] - 1
      
    }
  }
}

# Controlli
cat("Saggio garantito gw =", round(100*gw, 2), "%\n")
cat("Fattore di instabilita' =", round(1 + lam*(a/s - 1), 3), "\n")
cat("Scarto massimo S - I:", max(abs(S - I)), "\n")

# Grafici
par(mfrow = c(1, 3))

# 1) Scenario base: le variabili crescono tutte al saggio garantito
plot(Y[1,], type = "l", col = 2, lwd = 2, ylab = "£", xlab = "Periodi",
     ylim = range(Y[1,], C[1,], I[1,]), main = "Scenario base")
lines(C[1,], col = 3, lwd = 2)
lines(I[1,], col = 4, lwd = 2)
lines(S[1,], col = 5, lty = 2, lwd = 2)
legend("topleft", c("Y", "C", "I", "S"), col = c(2, 3, 4, 5),
       lty = c(1, 1, 1, 2), lwd = 2, bty = "n")

# 2) Tasso di crescita effettivo nei tre scenari
plot(100*g[1,], type = "l", col = 1, lwd = 2, ylab = "%", xlab = "Periodi",
     ylim = range(100*g), main = "Crescita effettiva")
lines(100*g[2,], col = 2, lwd = 2, lty = 2)
lines(100*g[3,], col = 4, lwd = 2, lty = 2)
abline(v = shockAt, lty = 3, col = "grey")
legend("topleft", c("Base (garantito)", "Ottimismo", "Pessimismo"),
       col = c(1, 2, 4), lty = c(1, 2, 2), lwd = 2, bty = "n")

# 3) Reddito nei tre scenari
plot(Y[1,], type = "l", col = 1, lwd = 2, ylab = "£", xlab = "Periodi",
     ylim = range(Y), main = "Reddito")
lines(Y[2,], col = 2, lwd = 2, lty = 2)
lines(Y[3,], col = 4, lwd = 2, lty = 2)
abline(v = shockAt, lty = 3, col = "grey")
legend("topleft", c("Base", "Ottimismo", "Pessimismo"),
       col = c(1, 2, 4), lty = c(1, 2, 2), lwd = 2, bty = "n")
