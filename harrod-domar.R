# Modello di Harrod-Domar con ASPETTATIVE ESTRAPOLATIVE (il "filo del rasoio")
# Corso "Analisi economica" - Parte II (sezione 2.2)
#
# Variante del modello acceleratore + moltiplicatore in cui la crescita ATTESA
# è estrapolata (in modo adattivo) da quella osservata. Il TASSO di crescita
# effettivo DIVERGE dal garantito: uno scostamento non si riassorbe, si autoalimenta.
#
#   Equazioni:
#   g_e_t = g_e_{t-1} + lam*(g_{t-1} - g_e_{t-1}) + eps_t   aspettative adattive
#   INV_t = a * g_e_t * Y_{t-1}                              acceleratore
#   S = I  con  SAV_t = s*Y_t   =>   Y_t = INV_t/s = (a/s)*g_e_t*Y_{t-1}
#   g_t   = Y_t/Y_{t-1} - 1
#
# Saggio garantito: g_w = s/(a-s)  (~ s/a con questa temporizzazione).
# Instabilità ("filo del rasoio"): rho = 1 + lam*(a/s - 1) > 1, quindi ogni
# scostamento di g da g_w si amplifica di rho a ogni periodo. L'amplificazione
# moltiplicatore-acceleratore a/s e' enorme (=20): lam piccolo la rende lenta
# e visualizzabile; lam = 1 (estrapolazione piena) darebbe l'esplosione "violenta".

# Prepara l'ambiente ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

# Parametri ####
nPeriods <- 40            # Numero di periodi
s   <- 0.2                # Propensione al risparmio
a   <- 4                  # Rapporto capitale/prodotto (acceleratore, a > 1)
lam <- 0.008              # Velocità di revisione delle aspettative (0 < lam <= 1)
gw  <- s / (a - s)        # Saggio di crescita garantito
Y0  <- 100                # Reddito iniziale
shockAt <- 20             # Periodo dello shock di aspettativa
eps0 <- 0.0011            # Ampiezza dello shock (una tantum, sulla crescita attesa)

# Tre scenari: nessuno shock; ottimismo (+eps); pessimismo (-eps) ####
segno <- c(0, +1, -1); nS <- length(segno)
Y  <- matrix(0, nS, nPeriods)     # Reddito
ge <- matrix(0, nS, nPeriods)     # Crescita attesa
g  <- matrix(0, nS, nPeriods)     # Crescita effettiva

# Visualizza informazioni
cat("Saggio garantito g_w = s/(a-s) =", round(gw, 5),
    " ; fattore di instabilita' rho =", round(1 + lam * (a / s - 1), 3), "\n")

# Lancia il modello
for (j in 1:nS) {
  Y[j, 1] <- Y0; ge[j, 1] <- gw; g[j, 1] <- gw
  for (t in 2:nPeriods) {
    eps <- if (t == shockAt) segno[j] * eps0 else 0
    ge[j, t] <- ge[j, t - 1] + lam * (g[j, t - 1] - ge[j, t - 1]) + eps   # Aspettative adattive
    Y[j, t]  <- (a / s) * ge[j, t] * Y[j, t - 1]                          # Equilibrio S=I
    g[j, t]  <- Y[j, t] / Y[j, t - 1] - 1                                 # Crescita effettiva
  }
}
cat("Senza shock il tasso resta sul garantito:", round(g[1, nPeriods], 5), "\n")

# Grafici ####
cW <- "orange"; cH <- "black"; cL <- "blue"
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# a) Tasso di crescita effettivo (diverge da g_w dopo lo shock)
plot(NA, xlim = c(1, nPeriods), ylim = c(-25, 35), font.main = 1, cex.main = 0.95,
     main = "a) Tasso di crescita effettivo g(t)", xlab = "Tempo", ylab = "g (%)")
abline(h = 100 * gw, lwd = 2.5, col = cW)
lines(100 * g[2, ], lwd = 2, lty = 2, col = cH)
lines(100 * g[3, ], lwd = 2, lty = 2, col = cL)
abline(v = shockAt, lty = 3, col = "grey70")
legend("topleft", c("Garantito (g = G)", "Ottimismo (boom)", "Pessimismo (collasso)"),
       col = c(cW, cH, cL), lwd = 2, lty = c(1, 2, 2), bty = "n", cex = 0.8)

# b) Reddito in livello (scala logaritmica)
plot(Y[1, ], type = "l", lwd = 2.5, col = cW, log = "y", ylim = range(Y),
     main = "b) Reddito (scala logaritmica)", xlab = "Tempo", ylab = "Y (log)",
     font.main = 1, cex.main = 0.95)
lines(Y[2, ], lwd = 2, lty = 2, col = cH)
lines(Y[3, ], lwd = 2, lty = 2, col = cL)
abline(v = shockAt, lty = 3, col = "grey70")
legend("topleft", c("Garantito", "Boom", "Collasso"),
       col = c(cW, cH, cL), lwd = 2, lty = c(1, 2, 2), bty = "n", cex = 0.8)
