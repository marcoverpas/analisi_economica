# Modello di crescita di Harrod-Domar: il "filo del rasoio"
# Prepara l'ambiente ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")              # Cancella tutto
nPeriods <- 60           # Numero di periodi
s <- 0.2                 # Propensione al risparmio
v <- 4                   # Rapporto capitale/prodotto (incrementale)
gw <- s / v              # Saggio di crescita garantito
# Visualizza l'informazione
cat("Saggio di crescita garantito g_w = ", gw, "\n")
# Tre scenari: investimento che cresce sopra, uguale, sotto g_w ####
gI <- c(0.06, 0.05, 0.04)
# Definisci il grado di utilizzo degli impianti ####
u  <- matrix(0, nrow = length(gI), ncol = nPeriods)
# Nota:
# Saggio di crescita garantito:  g_w = s / v.
# s = propensione al risparmio; v = rapporto capitale/prodotto.
# Domanda:  Y_dom = I / s   (moltiplicatore)
# Capacità: Y_cap = K / v ; K_t = K_{t-1} + I_{t-1}
# Grado di utilizzo: u = Y_dom / Y_cap. Solo se l'investimento cresce a g_w
# la capacita' resta pienamente utilizzata. Ogni scostamento si autoalimenta.
# Lancia il modello nei tre scenari ####
for (j in 1:length(gI)) {
  
  K <- numeric(nPeriods); I <- numeric(nPeriods)  # Definisci lo stock di capitale e l'investimento
  K[1] <- 100                                     # Assegna valore iniziale allo stock di capitale
  I[1] <- (s / v) * K[1]                          # Definisci l'investimento iniziale
  
  # Definisci il "loop" temporale
  for (t in 2:nPeriods) {
    
    I[t] <- I[1] * (1 + gI[j])^(t - 1)          # Investimento
    
    K[t] <- K[t - 1] + I[t - 1]                 # Stock di capitale
    
    u[j, t] <- (I[t] / s) / (K[t] / v)          # Grado di utilizzo
    
  }
}
# Visualizza i risultati ####
plot(u[1, 2:nPeriods], type = "l", lwd = 2, col = 2, ylim = range(0.80, 1.20),
     main = "Harrod-Domar: il filo del rasoio",
     xlab = "Tempo", ylab = "Grado di utilizzo", font.main = 1, cex.main = 1, cex.axis=1, cex.lab=1)
#grid()
lines(u[2, 2:nPeriods ], lwd = 2, lty = 1, col = 1)
lines(u[3, 2:nPeriods], lwd = 2, lty = 1, col = 4)
abline(h = 1, lty = 3, col = "grey")
legend("topleft", c("g > g_w (0.06)", "g = g_w (0.05)", "g < g_w (0.04)"),
       lty = 1, lwd = 2, col = c(2, 1, 4), bty = "n", cex=1)