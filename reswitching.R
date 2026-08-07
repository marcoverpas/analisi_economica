# Il ritorno delle tecniche (reswitching)

# Esempio numerico di "reswitching":
# la tecnica B è conveniente ai saggi di profitto bassi e alti,
# mentre la tecnica A è conveniente per valori intermedi di r.

# Cancella tutto ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

# Nota: a) due tecniche per ottenere 1 unità del bene
#       b) numerario = il bene stesso
#       c) costo unitario = w * Σ l_t (1+r)^t.
#       d) la tecnica A impiega lavoro nei periodi 0 e 2; la tecnica B nel periodo 1

# Scrivi il modello ####
lA0 <- 1.3125                                               # Lavoro anticipato di 0 periodi nella tecnica A
lA2 <- 1                                                    # Lavoro anticipato di 2 periodi nella tecnica A
lB1 <- 2.3                                                  # Lavoro anticipato di 1 periodo nella tecnica B
r <- seq(0, 0.45, length.out = 600)                         # Saggio di profitto (vettore)
x <- 1 + r                                                  # Coefficiente di ricarico (vettore)
costA <- lA0 + lA2 * x^2                                    # Costo unitario della tecnica A (vettore)
costB <- lB1 * x                                            # Costo unitario della tecnica B (vettore)
wA <- 1 / costA                                             # Salario nel settore A (vettore)
wB <- 1 / costB                                             # Salario nel settore B (vettore)
rsw <- sort(Re(polyroot(c(lA0, -lB1, lA2)))) - 1            # Saggi di profitto ai quali le due tecniche hanno lo stesso costo
  # Nota: polyroot() = calcola le radici di un polinomio (ottenuto uguagliando i costi unitari delle due tecniche)
  #       Re = estrae la parte reale ed elimina la parte immaginaria
  #       sort = ordina le soluzioni in ordine crescente
wenv <- pmax(wA, wB)                                        # Frontiera salario-profitto (inviluppo delle due tecniche) (vettore)
rk <- r[-1]                                                 # Saggio di profitto meno il primo valore nullo
kA <- -diff(wA)/diff(r)                                     # Intensità capitalistica implicita della tecnica A (= -dw/dr) (vettore)   
kB <- -diff(wB)/diff(r)                                     # Intensità capitalistica implicita della tecnica B (= -dw/dr) (vettore)

# Mostra informazioni
cat("Saggi di profitto ai quali le due tecniche hanno lo stesso costo r =", round(rsw, 4), "\n")

# Impostazioni grafiche
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))

# Pannello 1: inviluppo colorato per tecnica scelta ####
plot(r, wA, type = "l", lwd = 1, col = "blue", ylim = range(wA, wB),
     main = "Tecnica scelta (inviluppo): B -> A -> B",
     xlab = "Saggio di profitto r", ylab = "Salario w", font.main = 1, cex.main = 0.95)
lines(r, wB, lwd = 1, col = "red")
mB1 <- r <= rsw[1]; mA <- r > rsw[1] & r < rsw[2]; mB2 <- r >= rsw[2]
lines(r[mB1], wenv[mB1], lwd = 3.5, col = "red")
lines(r[mA],  wenv[mA],  lwd = 3.5, col = "blue")
lines(r[mB2], wenv[mB2], lwd = 3.5, col = "red")
abline(v = rsw, lty = 3, col = "grey")
text(0.02, max(wB), "B", col = "red"); text(0.15, 0.40, "A", col = "blue")
text(0.33, 0.35, "B (ritorna)", col = "red"); grid()

# Pannello 2: intensita' capitalistica delle due tecniche ####
plot(rk, kA, type = "l", lwd = 2, col = "blue", ylim = range(kA, kB),
     main = "Intensità capitalistica: l'ordinamento si inverte",
     xlab = "Saggio di profitto r", ylab = "k = valore capitale / lavoro",
     font.main = 1, cex.main = 0.95)
lines(rk, kB, lwd = 2, col = "red")
abline(v = rsw, lty = 3, col = "grey")
legend("topright", c("k tecnica A", "k tecnica B"),
       lty = 1, lwd = 2, col = c("blue", "red"), bty = "n"); grid()