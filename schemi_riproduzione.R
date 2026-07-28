# Schemi di riproduzione di Marx - versione didattica semplificata
# Corso "Analisi economica" - Parte I (sezione 1.4.5)
# Basato su: M. Veronese Passarella, "Marx's Reproduction Schemes"
# (versione originale 30/03/2016)
#
# Rispetto alla versione completa si tralascia il blocco ricchezza/portafoglio
# (consumo dei capitalisti da ricchezza, propensioni sigma, stock h). Restano il
# cuore a due settori, i saggi di accumulazione e l'aggiustamento graduale della
# propensione all'accumulo del settore 2. Le equazioni sono ricorsive (nessuna
# simultaneità entro il periodo): il ciclo iterativo di soluzione alla
# Gauss-Seidel, tipico dei modelli SFC, verrà reintrodotto nella Parte III.
#
# Notazione: v = capitale variabile (V), k = capitale costante (C),
# s = plusvalore (S). Indici: 1 = settore dei beni capitali,
# 2 = settore dei beni di consumo.

#~~~~~~~~~~~~~~~~
# Passo 1: Prepara l'ambiente ####
rm(list = ls(all = TRUE))
if (!is.null(dev.list())) dev.off()
cat("\014")

nPeriods   <- 100     # Numero di periodi
nScenarios <- 2       # 1 = baseline ; 2 = shock alla propensione all'accumulo del settore 1

#~~~~~~~~~~~~~~~~
# Passo 2: Crea variabili e parametri ####
mat <- function(z) matrix(data = z, nrow = nScenarios, ncol = nPeriods)

# Parametri (costanti nel tempo)
e_1 <- mat(1)        # Saggio di sfruttamento, settore 1
e_2 <- mat(1)        # Saggio di sfruttamento, settore 2
q_1 <- mat(4)        # Composizione organica, settore 1 (alta)
q_2 <- mat(2)        # Composizione organica, settore 2 (bassa)
theta0_1 <- mat(0.5) # Propensione all'accumulo (retention rate) del settore 1 - esogena
adj <- 0.3           # Velocita' di aggiustamento della propensione del settore 2

# Nota: 0 < adj <= 1, e adj = 1 riproduce l'aggiustamento istantaneo.

# Variabili di stato: capitale variabile iniziale SUL sentiero bilanciato
v_1 <- mat(1100)     # Capitale variabile anticipato, settore 1
v_2 <- mat(800)      # Capitale variabile anticipato, settore 2

# Nota: la proporzione v_1/v_2 = 1.375 soddisfa la condizione di riproduzione.

# Variabili derivate (inizializzate al valore di regime)
k_1 <- v_1 * q_1     # Capitale costante, settore 1
k_2 <- v_2 * q_2     # Capitale costante, settore 2
s_1 <- v_1 * e_1     # Plusvalore, settore 1
s_2 <- v_2 * e_2     # Plusvalore, settore 2
y_1 <- k_1 + v_1 + s_1 # Valore del prodotto, settore 1
y_2 <- k_2 + v_2 + s_2 # Valore del prodotto, settore 2
theta_1 <- mat(0.5)  # Propensione all'accumulo del settore 1
theta_2 <- mat(0.3)  # Propensione all'accumulo del settore 2 (endogena, aggiustamento graduale)
g_1 <- mat(0.1)      # Saggio di accumulazione, settore 1
g_2 <- mat(0.1)      # Saggio di accumulazione, settore 2
r   <- mat(0)        # Saggio generale del profitto
omega <- mat(0)      # Quota salari (sul reddito netto)
pri   <- mat(0)      # Quota profitti  

#~~~~~~~~~~~~~~~~
# Passo 3: Lancia il modello ####
for (j in 1:nScenarios) {
  
  for (i in 2:nPeriods) {
    
    # Scenario 2: Shock permanente alla propensione all'accumulo del settore 1 dal periodo 20
    if (i >= 20 && j == 2) {
      theta0_1[2, i] <- 0.25
    }
    
    #(1) Propensione all'accumulo del settore 1 (esogena)
    theta_1[j, i] <- theta0_1[j, i]
    
    #(2) Capitale variabile del settore 1 (accumula la quota trattenuta del plusvalore)
    v_1[j, i] <- v_1[j, i - 1] + theta_1[j, i - 1] * s_1[j, i - 1] / (1 + q_1[j, i - 1])
    
    #(3) Capitale variabile del settore 2
    v_2[j, i] <- v_2[j, i - 1] + theta_2[j, i - 1] * s_2[j, i - 1] / (1 + q_2[j, i - 1])
    
    #(4)-(5) Capitale costante (capitale circolante; ammortamento = 100%)
    k_1[j, i] <- v_1[j, i] * q_1[j, i]
    k_2[j, i] <- v_2[j, i] * q_2[j, i]
    
    #(6)-(7) Plusvalore
    s_1[j, i] <- v_1[j, i] * e_1[j, i]
    s_2[j, i] <- v_2[j, i] * e_2[j, i]
    
    #(8)-(9) Valore del prodotto per settore
    y_1[j, i] <- k_1[j, i] + v_1[j, i] + s_1[j, i]
    y_2[j, i] <- k_2[j, i] + v_2[j, i] + s_2[j, i]
    
    #(10) Saggio di accumulazione del settore 1:  g_1 = e_1 * theta_1 / (1 + q_1)
    g_1[j, i] <- e_1[j, i] * theta_1[j, i] / (1 + q_1[j, i])
    
    #(11) Propensione all'accumulo "di regime" del settore 2, coerente con la
    #     crescita del settore 1 (valore-obiettivo verso cui il settore 2 si adegua)
    theta_2_star <- g_1[j, i] * (1 + q_2[j, i]) / e_2[j, i]
    
    #(12) Aggiustamento GRADUALE della propensione all'accumulo del settore 2
    theta_2[j, i] <- theta_2[j, i - 1] + adj * (theta_2_star - theta_2[j, i - 1])
    
    #(13) Saggio di accumulazione del settore 2 (dalla propensione effettiva)
    g_2[j, i] <- e_2[j, i] * theta_2[j, i] / (1 + q_2[j, i])
    
    #(14) Saggio generale del profitto (plusvalore totale / capitale anticipato totale)
    r[j, i] <- (s_1[j, i] + s_2[j, i]) /
      (k_1[j, i] + k_2[j, i] + v_1[j, i] + v_2[j, i])
    
    #(15)-(16) Quote distributive (sul reddito netto = valore aggiunto)
    omega[j, i] <- (v_1[j, i] + v_2[j, i]) /
      (y_1[j, i] + y_2[j, i] - k_1[j, i] - k_2[j, i])
    pri[j, i] <- 1 - omega[j, i]
  }
}

#~~~~~~~~~~~~~~~~
# Passo 4: Produci i grafici ####
tt <- 15:50   # Finestra temporale (lo shock avviene al periodo 20)

layout(matrix(c(1, 2), 1, 2, byrow = TRUE))

# Fig. 1 - Saggi di accumulazione (scenario 2)
plot(tt, g_1[2, tt], type = "l", lty = 1, lwd = 2, col = 4, font.main = 1,
     cex.main = 0.75, ylim = range(0.045, 0.105),
     main = "Fig. 1 - Shock alla propensione all'accumulo del settore 1: \n saggi di accumulazione",
     ylab = "Saggi di accumulazione", xlab = "Tempo", cex.axis = 0.75, cex.lab = 0.8)
#grid()
lines(tt, g_2[2, tt], type = "l", lty = 3, lwd = 2, col = 2)
legend("topright", c("Settore 1", "Settore 2"), bty = "n", cex = 0.8,
       lty = c(1, 3), lwd = c(2, 2), col = c(4, 2))

# Fig. 2 - Propensioni all'accumulo (scenario 2)
plot(tt, theta_1[2, tt], type = "l", lty = 1, lwd = 2, col = 4, font.main = 1,
     cex.main = 0.75, ylim = range(0.13, 0.52),
     main = "Fig. 2 - Shock alla propensione all'accumulo del settore 1: \n propensioni all'accumulo",
     ylab = "Propensioni all'accumulo", xlab = "Tempo", cex.axis = 0.75, cex.lab = 0.8)
#grid()
lines(tt, theta_2[2, tt], type = "l", lty = 3, lwd = 2, col = 2)
legend("topright", c("Settore 1", "Settore 2"), bty = "n", cex = 0.8,
       lty = c(1, 3), lwd = c(2, 2), col = c(4, 2))
