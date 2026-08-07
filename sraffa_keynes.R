# Modello dinamico Keynes + Sraffa (senza moneta, NON ancora SFC)

# Cancella tutto ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

# Parametri di sistema ####
nPeriods <- 180                                             # Periodi
nScenarios <- 2                                             # Numero di scenari
nIndustries <- 3                                            # Numero di industrie

# Parametri del modello ####
c0 <- 10                                                    # Consumo autonomo       
c1 <- 0.8                                                   # Propensione marginale al consumo sul reddito
Gexog <- 20                                                 # Valore iniziale della spesa pubblica
I0base <- 10                                                # Valore iniziale dell'investimento
wage <- 0.4                                                 # Salario unitario
pr <- c(3.5, 5, 2.2)                                        # Prodotto per unità di lavoro
mu <- 0.875                                                 # Mark-up o saggio di profitto
A <- matrix(c(0.11, 0.12, 0.10,
              0.21, 0.22, 0.20,
              0.15, 0.18, 0.10),
              nrow = nIndustries,
              byrow = TRUE)                                 # Matrice dei coefficienti tecnici
Leontief <- solve(diag(nIndustries) - A)                    # Inversa di Leontief (I-A)^-1
betaC <- c(0.15, 0.35, 0.50)                                # Quote di consumo per prodotto (industria)
betaI <- c(0.20, 0.50, 0.30)                                # Quote di investimento per prodotto (industria)
betaG <- c(0.10, 0.30, 0.60)                                # Quote di spesa pubblica per prodotto (industria)

# Parametri dello shock ####
I0shock <- 30                                               # Nuovo livello dell'investimento
shockAt <- 120                                              # Periodo dello shock

# Prezzi di produzione (cost-plus alla Sraffa), COSTANTI ####
p <- rep(1, nIndustries)
for (iter in 1:500) for (z in 1:nIndustries)
  p[z] <- wage / pr[z] + (1 + mu) * sum(p * A[, z])
p_c <- sum(p * betaC)                                       # Indice dei prezzi al consumo

# Variabili ####
I0 <- matrix(I0base, nScenarios, nPeriods)                  # Investimento
yr <- matrix(0, nScenarios, nPeriods)                       # Prodotto netto reale
C <- matrix(0, nScenarios, nPeriods)                        # Consumo
Y  <- matrix(0, nScenarios, nPeriods)                       # Reddito netto nominale
d  <- array(0, dim = c(nScenarios, nPeriods, nIndustries))  # Domanda reale
x  <- array(0, dim = c(nScenarios, nPeriods, nIndustries))  # Prodotto lordo reale

# Lancia il modello ####
for (j in 1:nScenarios) {
  
  for (i in 2:nPeriods) {
    
    if (i >= shockAt && j == 2) I0[j, i] <- I0shock          # Shock all'investimento
    
    C[j, i]  <- c0 + c1 * yr[j, i - 1]                       # Consumo reale (Keynes)
    d[j, i, ] <- betaC * C[j, i] + betaI * I0[j, i] + betaG * Gexog
    Y[j, i]  <- sum(p * d[j, i, ])                           # Reddito netto nominale
    yr[j, i] <- Y[j, i] / p_c                                # Reddito netto reale
    x[j, i, ] <- Leontief %*% d[j, i, ]                      # Produzione lorda (Leontief)
  
  }
}

# Grafici ####
indCol <- c("springgreen4", "orangered", "dodgerblue3")
indLab <- c("Agricoltura", "Manifattura", "Servizi")
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# a) Reddito reale
plot(yr[2,100:nPeriods], type = "l", lwd = 2, col = "purple", ylim = range(180,320),
     main = "a) Reddito reale (moltiplicatore)", xlab = "Tempo", ylab = "Reddito reale",
     font.main = 1, cex.main = 0.95)
lines(yr[1,100:nPeriods], lwd = 2, lty = 3, col = "gold3")
legend("right", c("Shock", "Baseline"), lty = c(1, 3), lwd = 2, col = c("purple", "gold3"), bty = "n")

# a) Consumo reale
plot(C[2,100:nPeriods]/p_c, type = "l", lwd = 2, col = "purple", ylim = range(150,270),
     main = "b) Consumo reale", xlab = "Tempo", ylab = "Consumo reale",
     font.main = 1, cex.main = 0.95)
lines(C[1,100:nPeriods]/p_c, lwd = 2, lty = 3, col = "gold3")
legend("right", c("Shock", "Baseline"), lty = c(1, 3), lwd = 2, col = c("purple", "gold3"), bty = "n")

# c) Produzione lorda per industria
plot((x[2,100:nPeriods,1]), type = "l", lty = 1, lwd = 2, col = "springgreen4", ylim = range(x[2,100:nPeriods,]),
     main = "c) Produzione lorda per industria (shock)", xlab = "Tempo",
     ylab = "Produzione lorda (reale)", font.main = 1, cex.main = 0.95)
lines((x[2,100:nPeriods,2]), lty = 1, lwd = 2, col = "orangered")
lines((x[2,100:nPeriods,3]), lty = 1, lwd = 2, col = "dodgerblue3")
legend("right", indLab, col = indCol, lwd = 2, bty = "n")

# c) Prezzi
barplot(p, names.arg = indLab, col = indCol, ylim = c(0, 1.2),
        main = "d) Prezzi di produzione (cost-plus)", ylab = "Prezzo unitario",
        font.main = 1, cex.main = 0.95)
