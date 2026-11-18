# Il ritorno delle tecniche (reswitching)

# Esempio numerico di "reswitching" in un'economia a due settori:
# la tecnica B è conveniente ai saggi di profitto bassi e alti,
# mentre la tecnica A è conveniente per valori intermedi di r.

# Cancella tutto ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

# Nota: a) due beni: consumo (c) e macchine (m); le macchine servono a produrre entrambi i beni
#       b) numerario = il bene di consumo (pc = 1)
#       c) prezzi: pc = (1+r)*pm*amc + w*lc  e  pm = (1+r)*pm*amm + w*lm
#       d) nella tecnica B i due settori hanno la stessa composizione (amm/lm = amc/lc), nella A no

# Scrivi il modello ####
lcA <- 1.9                                                  # Lavoro per unità di bene di consumo, tecnica A
amcA <- 0.14                                                # Macchine per unità di bene di consumo, tecnica A
lmA <- 1                                                    # Lavoro per macchina, tecnica A
ammA <- 0.6                                                 # Macchine per macchina, tecnica A
lcB <- 1                                                    # Lavoro per unità di bene di consumo, tecnica B
amcB <- 0.5                                                 # Macchine per unità di bene di consumo, tecnica B
lmB <- 1                                                    # Lavoro per macchina, tecnica B
ammB <- 0.5                                                 # Macchine per macchina, tecnica B
r <- seq(0, 0.65, length.out = 651)                         # Saggio di profitto (vettore)
x <- 1 + r                                                  # Coefficiente di ricarico (vettore)
dA <- lcA * ammA - lmA * amcA                               # Coefficiente di (1+r) al denominatore della frontiera A
dB <- lcB * ammB - lmB * amcB                               # Coefficiente di (1+r) al denominatore della frontiera B (= 0)
wA <- (1 - ammA * x) / (lcA - dA * x)                       # Frontiera salario-profitto della tecnica A (vettore)
wB <- (1 - ammB * x) / (lcB - dB * x)                       # Frontiera salario-profitto della tecnica B (vettore)
rsw <- sort(Re(polyroot(c(lcB - lcA,
                          dA + ammB * lcA - dB - ammA * lcB,
                          ammA * dB - ammB * dA)))) - 1     # Saggi di profitto ai quali le due tecniche si equivalgono
  # Nota: polyroot() = calcola le radici di un polinomio (ottenuto uguagliando le due frontiere, in x = 1+r)
  #       Re = estrae la parte reale ed elimina la parte immaginaria
  #       sort = ordina le soluzioni in ordine crescente
wenv <- pmax(wA, wB)                                        # Frontiera salario-profitto (inviluppo delle due tecniche) (vettore)
yA <- wA[1]                                                 # Prodotto netto per addetto della tecnica A (= salario con r = 0)
yB <- wB[1]                                                 # Prodotto netto per addetto della tecnica B (= salario con r = 0)
rk <- r[-1]                                                 # Saggio di profitto meno il primo valore nullo
kA <- (yA - wA[-1]) / rk                                    # Capitale per addetto (in valore) della tecnica A (vettore)
kB <- (yB - wB[-1]) / rk                                    # Capitale per addetto (in valore) della tecnica B (vettore)
kenv <- ifelse(wA[-1] > wB[-1], kA, kB)                     # Capitale per addetto della tecnica scelta (vettore)

# Mostra informazioni
cat("Saggi di profitto ai quali le due tecniche si equivalgono r =", round(rsw, 4), "\n")
cat("Prodotto netto per addetto: yA =", round(yA, 4), " yB =", round(yB, 4), "\n")

# Impostazioni grafiche
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))

# Pannello 1: inviluppo colorato per tecnica scelta ####
plot(r, wB, type = "l", lwd = 1, col = "red", ylim = c(0, max(wA, wB)),
     main = "Tecnica scelta (inviluppo): B -> A -> B",
     xlab = "Saggio di profitto r", ylab = "Salario w", font.main = 1, cex.main = 0.95)
lines(r, wA, lwd = 1, col = "blue")
mB1 <- r <= rsw[1]; mA <- r > rsw[1] & r < rsw[2]; mB2 <- r >= rsw[2]
lines(r[mB1], wenv[mB1], lwd = 3.5, col = "red")
lines(r[mA],  wenv[mA],  lwd = 3.5, col = "blue")
lines(r[mB2], wenv[mB2], lwd = 3.5, col = "red")
abline(v = rsw, lty = 3, col = "grey")
text(0.06, 0.50, "B", col = "red"); text(0.35, 0.37, "A", col = "blue")
text(0.60, 0.25, "B (ritorna)", col = "red"); grid()

# Pannello 2: capitale per addetto della tecnica scelta ####
kA1 <- (yA - (1 - ammA * (1 + rsw)) / (lcA - dA * (1 + rsw))) / rsw   # k della tecnica A ai due saggi di switch
kB1 <- (yB - (1 - ammB * (1 + rsw)) / (lcB - dB * (1 + rsw))) / rsw   # k della tecnica B ai due saggi di switch
plot(rk, kB, type = "l", lwd = 1, lty = 2, col = "red", ylim = c(0, 0.6),
     main = "Capitale per addetto: scende e poi risale",
     xlab = "Saggio di profitto r", ylab = "k = (y - w) / r",
     font.main = 1, cex.main = 0.95)
lines(rk, kA, lwd = 1, lty = 2, col = "blue")
nB1 <- rk <= rsw[1]; nA <- rk >= rsw[1] & rk <= rsw[2]; nB2 <- rk >= rsw[2]
lines(rk[nB1], kB[nB1], lwd = 3.5, col = "red")
lines(rk[nA],  kA[nA],  lwd = 3.5, col = "blue")
lines(rk[nB2], kB[nB2], lwd = 3.5, col = "red")
segments(rsw, kB1, rsw, kA1, lwd = 2)                       # Salti del capitale per addetto nei punti di switch
abline(v = rsw, lty = 3, col = "grey")
text(rsw[1] + 0.07, 0.36, "scende", cex = 0.85); text(rsw[2] - 0.07, 0.45, "sale", cex = 0.85)
legend("bottomright", c("k tecnica A", "k tecnica B", "tecnica scelta (tratto spesso)"),
       lty = c(2, 2, 1), lwd = c(1, 1, 3.5), col = c("blue", "red", "black"), bty = "n"); grid()
