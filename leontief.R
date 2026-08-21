# Modello input-output: inversa di Leontief e moltiplicatori di produzione
# Corso "Analisi economica" - Parte III (sezione 3.1)
# Tre settori: agricoltura, manifattura e servizi. Utilizziamo la stessa matrice
# A dei coefficienti tecnici del modello Keynes + Sraffa (sezione 2.3.5).
# x = A * x + d   =>   x = (I - A)^-1 * d   (produzione lorda)
# Il moltiplicatore di produzione del settore j è la somma della colonna j
# dell'inversa di Leontief: misura la produzione complessiva attivata da
# un'unità aggiuntiva di domanda finale rivolta al settore j.

# Prepara l'ambiente ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

# Definisci i dati ####
ind <- c("Agr.", "Man.", "Ser.")
A <- matrix(c(0.11, 0.12, 0.10,        # a_ij = quantità del bene i necessaria per
              0.21, 0.22, 0.20,        #        produrre un'unità del bene j
              0.15, 0.18, 0.10),       #        (colonna = settore che utilizza l'input)
            nrow = 3, byrow = TRUE)
d <- c(50, 80, 120)                    # Domanda finale (consumi + investimenti + spesa)

# Soluzione di Leontief ####
n <- nrow(A)
L <- solve(diag(n) - A)                # Inversa di Leontief: (I-A)^-1
x <- as.numeric(L %*% d)               # Produzione lorda necessaria a soddisfare d
inter <- x - d                         # Domanda intermedia: A * x
mult <- colSums(L)                     # Moltiplicatori di produzione: somme delle colonne di L
lam <- max(abs(eigen(A)$values))       # Raggio spettrale di A: deve essere < 1 per garantire
                                       # la convergenza della serie di Neumann e l'esistenza di L

# Visualizza informazioni ####
cat("Raggio spettrale di A:", round(lam, 4), " (sistema produttivo se < 1)\n")
cat("Produzione lorda x   :", round(x, 2), "\n")
cat("Moltiplicatori       :", round(mult, 3), "\n")

# Convergenza della serie di Neumann ####
# La soluzione di Leontief può essere scritta come:
# x = (I + A + A^2 + ...) * d
# Ogni potenza di A rappresenta un ulteriore giro di domanda intermedia.
nGiri <- 10                            # Numero di giri di domanda intermedia considerati
tot <- numeric(nGiri + 1)              # Produzione totale cumulata dopo ciascun giro
term <- d                              # Domanda finale, ossia il termine di ordine zero (il primo, quello che non contiene A)
tot[1] <- sum(d)                       # Produzione associata alla domanda finale
for (k in 1:nGiri){                    # Calcola il successivo giro di domanda intermedia
  term <- A %*% term                        
  tot[k + 1] <- tot[k] + sum(term) }  

# Grafici (2 x 2) ####
indCol <- c("springgreen4", "orangered", "dodgerblue3")
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# a) Domanda finale e produzione lorda
# La differenza tra produzione lorda e domanda finale è la domanda intermedia.
barplot(rbind(d, x), beside = TRUE, names.arg = ind, col = c("grey75", "orange"),
        ylim = c(0, max(x) * 1.15), font.main = 1, cex.main = 0.95, cex.names = 0.85,
        main = "a) Domanda finale e produzione lorda", ylab = "Livello (reale)")
legend("topleft", c("Domanda finale d", "Produzione lorda x"),
       fill = c("grey75", "orange"), bty = "n", cex = 0.8)

# b) Moltiplicatori di produzione
# Ogni moltiplicatore è la somma della corrispondente colonna dell'inversa di Leontief.
bp <- barplot(mult, names.arg = ind, col = indCol, ylim = c(0, max(mult) * 1.15),
              font.main = 1, cex.main = 0.95, cex.names = 0.85,
              main = "b) Moltiplicatori di produzione", ylab = "Somma di colonna di (I-A)^-1")
text(bp, mult, round(mult, 2), pos = 3, cex = 0.85)

# c) Convergenza della serie di Neumann
# La produzione totale converge a sum(x) quando aumenta il numero di giri
# di domanda intermedia.
plot(0:nGiri, tot, type = "b", pch = 19, lwd = 2, col = "purple",
     font.main = 1, cex.main = 0.95, main = "c) Convergenza (serie di Neumann)",
     xlab = "Numero di giri (k)", ylab = "Produzione lorda totale")
abline(h = sum(x), lty = 3, col = "grey40")
text(nGiri * 0.55, sum(x) * 0.95, "Totale = somma di x", cex = 0.8, col = "grey40")

# d) Inversa di Leontief come mappa di calore
# I valori riportati nelle celle mostrano quanto la produzione del settore i
# aumenta in risposta a un'unità aggiuntiva di domanda finale del settore j.
oranges <- colorRampPalette(c("#fff5eb", "#fd8d3c", "#7f2704"))(24)
Lp <- t(L)[, n:1]                      # Orienta la matrice per visualizzarla con la prima riga in alto
image(1:n, 1:n, Lp, col = oranges, axes = FALSE, xlab = "", ylab = "",
      font.main = 1, cex.main = 0.95, main = "d) Inversa di Leontief (I-A)^-1")
axis(1, at = 1:n, labels = ind, tick = FALSE, cex.axis = 0.85)
axis(2, at = 1:n, labels = rev(ind), tick = FALSE, las = 1, cex.axis = 0.85)
box()
for (i in 1:n) for (j in 1:n)
  text(j, n - i + 1, sprintf("%.2f", L[i, j]), cex = 0.9,
       col = if (L[i, j] > 0.9) "white" else "black")
mtext("Domanda finale del settore", side = 1, line = 2.4, cex = 0.7)
mtext("Produzione del settore",     side = 2, line = 2.6, cex = 0.7)