# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Un modello di Lotka-Volterra malthusiano: salario reale contro popolazione
# Adattato dal LVMODEL di Marco Veronese Passarella (31 maggio 2019)
#
# Reinterpretazione del classico sistema preda-predatore:
#   * PREDA        -> SALARIO REALE      (w)
#   * PREDATORI    -> POPOLAZIONE        (P)
#
# Lettura economica (Malthus / Ricardo):
#   - un salario reale elevato fa crescere la popolazione (i predatori si nutrono delle prede);
#   - una popolazione più numerosa deprime il salario reale (le prede vengono consumate).
# Il salario reale continua quindi a oscillare intorno al proprio livello di sussistenza:
# w* = gamma_pop / beta_pop.
#
# Lo script produce due grafici:
# a) diagramma di fase (salario, popolazione)
# b) salario e popolazione nel tempo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PASSO 1: prepara l'ambiente ####
rm(list = ls(all = TRUE)) # Cancella tutto
nPeriods <- 3000          # Orizzonte temporale 

# PASSO 2: coefficienti ####
gamma_wage <- 0.01     # Crescita autonoma del salario quando la popolazione è nulla
beta_wage  <- 1e-6     # Freno esercitato dalla popolazione sul salario
gamma_pop  <- 0.01     # Declino autonomo della popolazione quando il salario è nullo
beta_pop   <- 1e-6     # Spinta data dal salario alla crescita della popolazione

# Punti di riposo di lungo periodo (qui tutti pari a 10000)
wage_star <- gamma_pop  / beta_pop      # Salario reale di sussistenza
pop_star  <- gamma_wage / beta_wage     # Popolazione di equilibrio

# PASSO 3: variabili di stato ####
wage <- numeric(nPeriods)
pop  <- numeric(nPeriods)

# Condizione iniziale #### 
wage[1] <- 10500
pop[1]  <- 10000

# Nota: salario fissato al 5% sopra la sussistenza, popolazione a riposo.

# PASSO 4: esecuzione del modello ####
for (i in 2:nPeriods) {
  g_wage  <- gamma_wage - beta_wage * pop[i - 1]
  wage[i] <- wage[i - 1] * (1 + g_wage)
  g_pop   <- -gamma_pop + beta_pop * wage[i]
  pop[i]  <- pop[i - 1] * (1 + g_pop)
}

# Nota: Aggiornamento di Eulero semi-implicito (simplettico): il salario viene aggiornato per primo e
# il nuovo salario viene usato per aggiornare la popolazione. Ciò mantiene l'orbita limitata,
# cosicché il salario oscilla stabilmente intorno alla sussistenza senza allontanarsene.

# PASSO 5: impostazioni delle figure ####
col_wage <- "#2c6fbb"   # salario reale
col_pop  <- "#c85a3f"   # popolazione
col_orb  <- "#3b7a57"   # traiettoria del diagramma di fase
xlim_ph <- c(9300, 10700)
ylim_ph <- c(9300, 10700)
ylim_ts <- c(9300, 10700)

# Figura (a): diagramma di fase ####
plot(wage[1:nPeriods]/100, pop[1:nPeriods]/100, type = "l", col = col_orb, lwd = 1.8,
     xlim = xlim_ph/100, ylim = ylim_ph/100,
     xlab = "Salario reale", ylab = "Popolazione",
     main = "a) Diagramma di fase")
abline(v = wage_star/100, col = "grey60", lty = 3)
abline(h = pop_star/100,  col = "grey60", lty = 3)
points(wage[nPeriods]/100, pop[nPeriods]/100, pch = 19, col = "#c0392b", cex = 1.3)

# Figura (b) dinamica nel tempo ####
tt <- 1:nPeriods
plot(tt, wage[1:nPeriods]/100, type = "l", col = col_wage, lwd = 1.8,
     xlim = c(0, nPeriods), ylim = ylim_ts/100,
     xlab = "Tempo", ylab = "",
     main = "b) Salario reale e popolazione nel tempo")
lines(tt, pop[1:nPeriods]/100, col = col_pop, lwd = 1.8)
abline(h = wage_star/100, col = "grey60", lty = 2)
text(nPeriods, wage_star/100, "Sussistenza", col = col_wage,
     cex = 1, adj = c(1, -0.4))
legend("topright", c("Salario reale", "Popolazione"),
       col = c(col_wage, col_pop), lwd = 2, bty = "n", cex = 1)