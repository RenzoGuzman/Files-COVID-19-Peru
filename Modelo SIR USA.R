#Modelo epidemiológico del COVID-19 en Perú

#Serie de infectados 
Infectados <- c(1, 1, 2, 2, 5, 5, 5, 5, 5, 7, 8, 8, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 15, 15, 51, 51, 57, 58, 60, 68, 74, 98, 118, 176, 223, 341, 417, 584, 778, 1053, 1315, 1922, 2450, 3173, 4019, 5723, 7730, 11719, 17033, 23197, 31879, 42152, 51954, 63928, 80735)
Tests <- c(8, 8, 16, 16, 41, 41, 41, 41, 41, 57, 66, 66, 90, 90, 90, 90, 90, 90, 90, 90, 99, 99, 107, 107, 107, 107, 107, 107, 107, 107, 123, 123, 419, 419, 468, 476, 493, 558, 608, 805, 969, 1326, 2252, 2752, 3099, 4264, 5054, 7686, 10029, 16665, 20788, 27963, 41814, 55014, 76493, 103863, 138516, 182583, 228184, 294044, 359145, 472767, 579589)
Día <- 1:(length(Infectados))

#Población de Perú (Fuente: Censo de Vivienda y Población 2017)
N <- 327200000

#Gráfico de infectados
old <- par(mfrow = c(1, 2))
plot(Día, Infectados, type ="b")
plot(Día, Infectados, log = "y")
abline(lm(log10(Infectados) ~ Día))
title("Casos confirmados COVID-19 en Perú", outer = TRUE, line = -2)

#Modelo SIR
#Sistema de eq. diferenciales
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dT <- rate/N * T * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR, dT))
  })
}

#Usando package deSolve para resolver sistema de eq. diferenciales
install.packages("deSolve")
library(deSolve)
init <- c(S = N-Infectados[1], I = Infectados[1], R = 0, T = Tests[1])
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma", "rate")
  out <- ode(y = init, times = Día, func = SIR, parms = parameters)
  fit <- out[ , 4]
  sum((Infectados - fit)^2)
}

Opt <- optim(c(0.59, 0.41, 0.0018), RSS, method = "L-BFGS-B", lower = c(0, 0, 0), upper = c(1, 1, 1)) 
Opt$message
 
#Estimación de beta y gamma
Opt_par <- setNames(Opt$par, c("beta", "gamma", "rate"))
Opt_par

#Periodo de evaluación: 60 días
t <- 1:250
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 

#Evolución de Susceptibles, Infectados y Recuperados
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Día", ylab = "Número de casos", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Día", ylab = "Número de casos", lwd = 2, lty = 1, col = col, log = "y")
points(Día, Infectados)
title("Modelo SIR COVID-19 Perú", outer = TRUE, line = -2)
par(old)

#Estimación del R0
R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
R0

#Total de infectados
fit[fit$I == max(fit$I), "I", drop = FALSE] 

#Tasa de mortalidad del COVID-19
max(fit$I) * 0.0436 

