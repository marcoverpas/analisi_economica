# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Semplice modello preda-predatore di Lotka-Volterra in tempo discreto
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Corso:       Analisi Economica
# Autore:      Marco Veronese Passarella
# Data:        1 Ottobre 2026
# Versione:    1.0
# Licenza:     vedi il file LICENSE nel repository
#
# Descrizione:
# Il modello simula la dinamica di due popolazioni, prede (x) e
# predatori (y), a partire da una situazione di equilibrio perturbata
# da un piccolo shock sul numero di prede. Le equazioni sono risolte
# con uno schema semi-implicito, che genera oscillazioni cicliche
# stabili nel tempo.
#
# Output:
# 1. Andamento nel tempo di prede e predatori
# 2. Diagramma delle fasi con il punto di equilibrio
#
# Riferimenti:
# Lotka, A. J. (1925). Elements of Physical Biology.
# Volterra, V. (1926). Variazioni e fluttuazioni del numero
# d'individui in specie animali conviventi. Memorie della R.
# Accademia dei Lincei, 2, 31-113.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Definisci l'arco temporale ####
period <- 800

# Definisci i parametri del modello
gamma1 <- 0.02   #Tasso di crescita delle prede in assenza di predatori
gamma2 <- 0.02   #Tasso di mortalità dei predatori in assenza di prede
omega1 <- 0.001  #Tasso di riduzione delle prede dovuto a ciascun predatore
omega2 <- 0.0009 #Tasso di aumento dei predatori dovuto a ciascuna preda

# Calcola i valori di equilibrio delle due variabili ####
xstar <- gamma2/omega2 #Numero di prede di equilibrio 
ystar <- gamma1/omega1 #Numero dei predatori di equilibrio

# Definisci le variabili ####
x <- matrix(xstar,nrow=1,ncol=period)  #Numero di prede
y <- matrix(ystar,nrow=1,ncol=period)  #Numero di predatori
gx <- matrix(0,nrow=1,ncol=period)  #Tasso effettivo di crescita delle prede
gy <- matrix(0,nrow=1,ncol=period)  #Tasso effettivo di (de)crescita dei predatori

# Genera un piccolo shock sul numero di prede ####
x[1,1] <- x[1,1]+1

# Lancia il modello nell'arco di tempo di riferimento ####

for (i in 2:period){
  
    gx[1,i] = gamma1 - omega1*y[1,i-1]
    x[1,i] = x[1,i-1]*(1+gx[1,i])
    gy[1,i] = -gamma2 + omega2*x[1,i]
    # Nota: si usa x[1,i] (prede del periodo corrente) e non x[1,i-1].
    # Questo mantiene cicli chiusi. Per contro, con x[1,i-1]
    # le oscillazioni si amplierebbero progressivamente.
    y[1,i] = y[1,i-1]*(1+gy[1,i])
    
}

# Mostra l'andamento nel tempo di prede e predatori ####
plot(x[1,], type = "l", ylim = c(min(x, y), max(x, y) * 1.05),
     xlab = "Tempo", ylab = "Popolazione")
lines(y[1,], col = 2)
legend("topright", legend = c("Prede", "Predatori"), col = 1:2, lty = 1, bty = "n")

# Mostra il diagramma delle fasi di prede e predatori ####
plot(x[1,], y[1,], type = "l", col = 3,
     xlab = "Prede", ylab = "Predatori")
points(xstar, ystar, pch = 19)   
abline(h=ystar,lty=2,lwd=1)
abline(v=xstar,lty=2,lwd=1)

