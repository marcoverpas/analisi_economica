# Modello di crescita di Harrod-Domar (acceleratore + moltiplicatore)
# Corso "Analisi economica" - Parte II (sezione 2.2)
# Estende il modello keynesiano dinamico elementare aggiungendo una funzione
# di investimento "acceleratore". 
#   (1) Y_t    = CONS_t + INV_t
#   (2) SAV_t  = s * Y_{t-1}
#   (3) INV_t  = a * (Ye_t - Y_{t-1})     Acceleratore (a = rapporto capitale/prodotto)
#   (4) CONS_t = Y_t - SAV_t
#   (5) Ye_t   = Y_t + eps_t              Domanda attesa (eps = errore di aspettativa)
# Da (1) e (4) segue INV_t = SAV_t (equilibrio S=I). Sostituendo (2),(3),(5):
#   a (Y_t + eps_t - Y_{t-1}) = s Y_{t-1}   =>   Y_t = (1 + s/a) Y_{t-1} - eps_t
# Saggio di crescita garantito: G = s/a (con eps=0 e aspettative confermate).

# Prepara l'ambiente ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

# Parametri ####
nPeriods <- 50
s  <- 0.2                 # Propensione al risparmio
a  <- 4                   # Rapporto capitale/prodotto (acceleratore, a > 1)
G  <- s / a               # Saggio di crescita garantito
Y0 <- 100                 # Reddito iniziale
shockAt <- 20             # Periodo dello shock di aspettativa
eps0 <- 0.08 * Y0 * (1 + G)^(shockAt - 2)   # ampiezza dello shock (una tantum)
cat("Saggio di crescita garantito  G = s/a =", G, "\n")

# Tre scenari: (1) aspettative confermate; (2) domanda effettiva > attesa; (3) < attesa
# eps < 0  ->  Ye < Y  ->  effettiva > attesa  ->  traiettoria piu' alta
# eps > 0  ->  Ye > Y  ->  effettiva < attesa  ->  traiettoria piu' bassa
segno <- c(0, -1, +1)
nS <- length(segno)

Y    <- matrix(0,  nS, nPeriods)
CONS <- matrix(0,  nS, nPeriods)
INV  <- matrix(0,  nS, nPeriods)
SAV  <- matrix(0,  nS, nPeriods)
g    <- matrix(NA, nS, nPeriods)      # tasso di crescita effettivo

# Lancia il modello ####
for (j in 1:nS) {
  eps <- rep(0, nPeriods); eps[shockAt] <- segno[j] * eps0
  Y[j, 1] <- Y0
  for (t in 2:nPeriods) {
    Y[j, t]    <- (1 + G) * Y[j, t - 1] - eps[t]     # Forma ridotta (equilibrio S=I)
    SAV[j, t]  <- s * Y[j, t - 1]                    # Eq. (2)
    INV[j, t]  <- SAV[j, t]                          # Equilibrio S=I
    CONS[j, t] <- Y[j, t] - SAV[j, t]                # Eq. (4)
    g[j, t]    <- Y[j, t] / Y[j, t - 1] - 1          # Tasso di crescita effettivo
  }
}
cat("Tasso di crescita sul sentiero garantito:", round(g[1, nPeriods], 4), "\n")

# Grafici ####
cW <- "orange"; cH <- "black"; cL <- "blue"
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# a) Livello del reddito
plot(Y[1, ], type = "l", lwd = 2.5, col = cW, ylim = range(Y),
     main = "a) Reddito (livello)", xlab = "Tempo", ylab = "Y",
     font.main = 1, cex.main = 0.95)
lines(Y[2, ], lwd = 2, lty = 2, col = cH)
lines(Y[3, ], lwd = 2, lty = 2, col = cL)
abline(v = shockAt, lty = 3, col = "grey70")
legend("topleft", c("Crescita garantita (g = G)",
                    "Domanda effettiva > attesa", "Domanda effettiva < attesa"),
       col = c(cW, cH, cL), lwd = 2, lty = c(1, 2, 2), bty = "n", cex = 0.85)

# b) Scarto dal sentiero garantito
gapH <- Y[2, ] - Y[1, ]; gapL <- Y[3, ] - Y[1, ]
plot(NA, xlim = c(1, nPeriods), ylim = range(gapH, gapL),
     main = "b) Scarto dal sentiero garantito", xlab = "Tempo",
     ylab = "Y - Y garantito", font.main = 1, cex.main = 0.95)
abline(h = 0, lwd = 2.5, col = cW)
lines(gapH, lwd = 2, lty = 2, col = cH)
lines(gapL, lwd = 2, lty = 2, col = cL)
abline(v = shockAt, lty = 3, col = "grey70")
