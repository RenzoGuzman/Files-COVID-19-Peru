#Modelo epidemiológico del COVID-19 en Perú

#Serie de infectados (Fuente: MINSA)
#Infected <- c(1, 6, 7, 9, 11, 15, 22, 38, 43, 71, 86, 117, 145, 234, 263, 318, 363, 395, 416, 480, 580, 635, 671, 852, 950, 1065, 1353, 1414, 1595, 1746, 2281, 2561, 2954, 4342, 5256, 5897, 6848, 7519, 9784, 10303, 11475, 12491, 13489)
Infected <- c(1, 6, 7, 9, 11, 15, 22, 38, 43, 71, 86, 117, 145, 234, 263, 318, 363, 395, 416, 480, 580, 635, 671, 852, 950, 1065, 1353, 1454, 1595, 1746, 2281, 2561, 2954, 4342, 5256, 5897, 6848, 7519, 9784, 10303, 11475, 12491, 13489, 14420, 15628)
Day <- 1:(length(Infected))

#Población de Perú (Fuente: Censo de Vivienda y Población 2017)
N <- 32170000 

#Gráfico de infectados
old <- par(mfrow = c(1, 2))
plot(Day, Infected, type ="b")
plot(Day, Infected, log = "y")
abline(lm(log10(Infected) ~ Day))

#Modelo SIR
#Sistema de eq. diferenciales
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

#Usando package deSolve para resolver sistema de eq. diferenciales
library(deSolve)
init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), hessian = TRUE) 
Opt$message

#Estimación de beta y gamma
Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

#Periodo de evaluación: 60 días
t <- 1:100
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 

#Evolución de Susceptibles, Infectados y Recuperados
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of cases", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of cases", lwd = 2, lty = 1, col = col, log = "y")
points(Day, Infected)
par(old)

#Estimación del R0
R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
R0

#Total de infectados
fit[fit$I == max(fit$I), "I", drop = FALSE] 

#Tasa de mortalidad del COVID-19
max(fit$I) * 0.0436 