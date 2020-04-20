library(httr)
library(tidyverse)
library(lubridate)
library(knitr)

##Introduction

###Importing Data 

COVID <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
head(COVID, 10)
summary(COVID)

#CASES
#cleaning removing province, longititude and latitude 
COVID.clean <- COVID[ ,-c(1,3,4)]
#subset peru cumulative cases
COVID.peru <- COVID.clean %>% filter(Country.Region == "Peru")
#creating cumulative cases table for  peru 
start_date <- mdy("3-6-2020")
end_date <- mdy("4-18-2020")
intervals <- seq(from = start_date, to=end_date, by = "day")
cases <- as.numeric(COVID.peru[1, -c(1, 1:45)])
peru.cases <- as.data.frame(cbind(Date = intervals, Cases = cases))
peru.cases$Date <- as.Date(peru.cases$Date, origin ="1970-01-01")
#plot.cummulative.cases
plot(peru.cases$Date, type = "l", lwd = "3", col = "navy", peru.cases$Cases, main = "Cumulative Cases in Peru", xlab = "Date", ylab = "Cases")

###Model Validation: Estimating the number of cases based on different R0 values

#assumption: random mixing, person to person transmission, no difference in individual immunity, no deaths
# Parameter values
#initial variables per year
N_peru <- 32170000 #population of peru
I_peru <- 5 #imported cases from peru
E_peru <- 100 * I_peru #number potentially exposed
R_peru <- 0 
S_peru <- N_peru-E_peru-I_peru-R_peru
alpha <- 1/4.5 #incubation period
gamma <- 0.38 #recovery period
duration <- 1/0.38 #days 
# R0 and beta for different phases based on the impact of intervention to minimize transmission
beta_1_a <- 1.4/(N_peru*duration)
beta_2_a <- 1.5/(N_peru*duration)
#time specification
dt <- 1
start <- 0
end_peru <- 43
time_peru <- seq(start, end_peru, by = dt)
niter_peru <- (end_peru - start) / dt + 1
s_date_peru <- mdy("3-6-2020") #first cases reported in Rome
e_date_peru <- mdy("4-18-2020") #last up to date results
axis_peru <- seq(from = s_date_peru, to = e_date_peru, by = "day")
#create table
l <- 1 #index for beta_1: R0: 3.1
m <- 2 #index for beta_2: R0: 3.5
X_peru <- array(data = 0, dim = c(niter_peru, 5, 2)) #created an array to catch the results
colnames(X_peru) <- c("S", "E", "I","R", "New Case")
#2.3
X_peru[1, 1, l] <- S_peru #initial susuceptible population R0: 2.3
X_peru[1, 2, l] <- E_peru #initial latent population R0: 2.3
X_peru[1, 3, l] <- I_peru #initial infectious population R0: 2.3
X_peru[1, 4, l] <- R_peru #initial recovered population R0: 2.3
X_peru[1, 5, l] <- I_peru #initial infectious population R0: 2.3: using this row to count the number of new cases for cumulative incidence calculation.
#2.5
X_peru[1, 1, m] <- S_peru #initial susuceptible population R0: 2.5
X_peru[1, 2, m] <- E_peru #initial latent population R0: 2.5
X_peru[1, 3, m] <- I_peru #initial infectious population R0: 2.5
X_peru[1, 4, m] <- R_peru #initial recovered population R0: 2.5
X_peru[1, 5, m] <- I_peru #initial infectious population R0: 2.5: using this row to count the number of new cases for cumulative incidence calculation.
for (i in 2:niter_peru) { 
  #3.1
  X_peru[i,1, l] <- X_peru[i-1, 1, l] + dt * (-beta_1_a * X_peru[i-1,1,l] * (X_peru[i-1,3,l]) )  #S
  X_peru[i,2, l]<- X_peru[i-1,2, l] + dt * (beta_1_a * X_peru[i-1,1,l] *  (X_peru[i-1,3,l]) - alpha * X_peru[i-1,2,l]) #E
  X_peru[i,3, l] <- X_peru[i-1,3, l] + dt * (alpha * X_peru[i-1,2, l] - gamma * X_peru[i-1,3, l])  #I 
  X_peru[i,4, l] <- X_peru[i-1,4, l] + dt * (gamma * X_peru[i-1,3, l]) #R
  X_peru[i,5, l] <- dt * (alpha * X_peru[i-1,2, l]) #new cases added 
  
  #3.5
  X_peru[i,1, m] <- X_peru[i-1, 1, m] + dt * (-beta_2_a * X_peru[i-1,1,m] * (X_peru[i-1,3,m])  ) #S
  X_peru[i,2, m]<- X_peru[i-1,2, m] + dt * (beta_2_a * X_peru[i-1,1,m] * (X_peru[i-1,3,m]) - alpha * X_peru[i-1,2,m]) #E
  X_peru[i,3, m] <- X_peru[i-1,3, m] + dt * (alpha * X_peru[i-1,2, m] - gamma * X_peru[i-1,3, m])  #I 
  X_peru[i,4, m] <- X_peru[i-1,4, m] + dt * (gamma * X_peru[i-1,3, m]) #R    
  X_peru[i,5, m] <- dt * (alpha * X_peru[i-1,2, m]) #new cases added
  
}
first_beta_peru <- as.data.frame(X_peru[ , ,l])
second_beta_peru <- as.data.frame(X_peru[ , ,m])
first_beta_peru$Date <- axis_peru
second_beta_peru$Date <- axis_peru
#"Comparing Modeled Cumulative Cases in peru with varying R0 values to current cases",
plot(peru.cases[c(1:44), 1], peru.cases[c(1:44), 2], type = 'l', xlab = "Date", ylab = "Infected", col = "navy", lwd = 3, main = "Model Validation of \n varying R0 values ")
lines(cumsum(first_beta_peru$`New Case`) ~ axis_peru, col = "green", lwd = 3)
lines(cumsum(second_beta_peru$`New Case`) ~ axis_peru, col = 'red', lwd = 3)
legend("topleft", legend = c("Actual cases \n in Peru", "Model R0: 1.8","Model R0: 1.6"), fill = c("navy", "green",  "red"), col = c("navy", "green",  "red"), cex = 0.85, box.lty = 0)

###Phase Adjustment

####Current Dates

##############################Intervention###########################################
# Assumption: random mixing, person to person transmission, no difference in individual immunity, no deaths
# Parameter values
N_peru <- 32170000 #population of peru
I_peru <- 5 #imported cases from peru
E_peru <- 15 * I_peru #number potentially exposed
R_peru <- 0 
S_peru <- N_peru-E_peru-I_peru-R_peru
alpha <- 1/4.5 #incubation period
gamma <- 0.38 #recovery period
duration <- 1/0.38 #days 

# R0 and beta for different phases based on the impact of intervention to minimize transmission #using infromation from more recent papers. Since R0 = ce/D, these interventions are targeting the ce (contact rate) to reduce transmision. 
R0_1 <- 2.83 #No restriction (1-10)
R0_2 <- 1.77 #Initial Lockdown (11-28)
R0_3 <- 1.68 #Restriction I (29-36)
R0_4 <- 1.61 #Restriction II (37-52)
R0_5 <- 0.5 #Restriction III (53-150)
beta_1 <- R0_1/(N_peru * duration)
beta_2 <- R0_2/(N_peru * duration)
beta_3 <- R0_3/(N_peru * duration)
beta_4 <- R0_4/(N_peru * duration)
beta_5 <- R0_5/(N_peru * duration)
#time specification
dt <- 1
start <- 0
end_peru_p <- 100 #days
time_peru_p <- seq(start, end_peru_p, by = dt)
niter_peru_p <- (end_peru_p - start) / dt + 1
s_date_peru_p <- mdy("3-6-2020")
e_date_peru_p <- mdy("6-14-2020")
axis_peru_p <- seq(from = s_date_peru_p, to = e_date_peru_p, by = "day")
X_peru_p <- array(data = 0, dim = c(niter_peru_p, 5)) #created an array to catch the results
colnames(X_peru_p) <- c("S", "E", "I","R", "New Cases")
X_peru_p[1, 1] <- S_peru #initial susuceptible population 
X_peru_p[1, 2] <- E_peru #initial latent population 
X_peru_p[1, 3] <- I_peru #initial infectious population 
X_peru_p[1, 4] <- R_peru #initial recovered population 
X_peru_p[1, 5] <- I_peru #new cases
for (i in 2:niter_peru_p) { 
  if(i < 11) {
    #Phase 1 transmission with an R0 of 3.1 (January 31 - February 23rd)
    X_peru_p[i,1] <- X_peru_p[i-1, 1] + dt * (-beta_1 * X_peru_p[i-1,1] * X_peru_p[i-1,3] )  #S
    X_peru_p[i,2]<- X_peru_p[i-1,2] + dt * ((beta_1 * X_peru_p[i-1,1] * X_peru_p[i-1,3]) - alpha * X_peru_p[i-1,2]) #E
    X_peru_p[i,3] <- X_peru_p[i-1,3] + dt * (alpha * X_peru_p[i-1,2] - gamma * X_peru_p[i-1,3])  #I 
    X_peru_p[i,4] <- X_peru_p[i-1,4] + dt * (gamma * X_peru_p[i-1,3]) #R
    X_peru_p[i,5] <- dt * (alpha * X_peru_p[i-1,2])
  }
  else {
    if ( i >= 11 & i < 29) { 
      #Phase 2 transmission with an R0 of 1.9 (February 24 -  March 8) blocking the north
      X_peru_p[i,1] <- X_peru_p[i-1, 1] + dt * (-beta_2 * X_peru_p[i-1,1] * X_peru_p[i-1,3] )  #S
      X_peru_p[i,2]<- X_peru_p[i-1,2] + dt * ((beta_2 * X_peru_p[i-1,1] * X_peru_p[i-1,3]) - alpha * X_peru_p[i-1,2]) #E
      X_peru_p[i,3] <- X_peru_p[i-1,3] + dt * (alpha * X_peru_p[i-1,2] - gamma * X_peru_p[i-1,3])  #I 
      X_peru_p[i,4] <- X_peru_p[i-1,4] + dt * (gamma * X_peru_p[i-1,3]) #R
      X_peru_p[i,5] <- dt * (alpha * X_peru_p[i-1,2])
    }
    else {
      if ( i >= 29 & i < 37) { 
        #Phase 2 transmission with an R0 of 1.9 (February 24 -  March 8) blocking the north
        X_peru_p[i,1] <- X_peru_p[i-1, 1] + dt * (-beta_3 * X_peru_p[i-1,1] * X_peru_p[i-1,3] )  #S
        X_peru_p[i,2]<- X_peru_p[i-1,2] + dt * ((beta_3 * X_peru_p[i-1,1] * X_peru_p[i-1,3]) - alpha * X_peru_p[i-1,2]) #E
        X_peru_p[i,3] <- X_peru_p[i-1,3] + dt * (alpha * X_peru_p[i-1,2] - gamma * X_peru_p[i-1,3])  #I 
        X_peru_p[i,4] <- X_peru_p[i-1,4] + dt * (gamma * X_peru_p[i-1,3]) #R
        X_peru_p[i,5] <- dt * (alpha * X_peru_p[i-1,2])
      }
      else{
        if( i >= 37 & i < 53){
          #Phase 3 transmission with an R0 of 0.65 (March 8 - March 10) North total Lockdown
          X_peru_p[i,1] <- X_peru_p[i-1, 1] + dt * (-beta_4 * X_peru_p[i-1,1] * X_peru_p[i-1,3] )  #S
          X_peru_p[i,2]<- X_peru_p[i-1,2] + dt * ((beta_4 * X_peru_p[i-1,1] * X_peru_p[i-1,3]) - alpha * X_peru_p[i-1,2]) #E
          X_peru_p[i,3] <- X_peru_p[i-1,3] + dt * (alpha * X_peru_p[i-1,2] - gamma * X_peru_p[i-1,3])  #I 
          X_peru_p[i,4] <- X_peru_p[i-1,4] + dt * (gamma * X_peru_p[i-1,3]) #R
          X_peru_p[i,5] <- dt * (alpha * X_peru_p[i-1,2])  
        }
        else{
          X_peru_p[i,1] <- X_peru_p[i-1, 1] + dt * (-beta_5 * X_peru_p[i-1,1] * X_peru_p[i-1,3])  #S
          X_peru_p[i,2]<- X_peru_p[i-1,2] + dt * ((beta_5 * X_peru_p[i-1,1] * X_peru_p[i-1,3]) - alpha * X_peru_p[i-1,2]) #E
          X_peru_p[i,3] <- X_peru_p[i-1,3] + dt * (alpha * X_peru_p[i-1,2] - gamma * X_peru_p[i-1,3])  #I 
          X_peru_p[i,4] <- X_peru_p[i-1,4] + dt * (gamma * X_peru_p[i-1,3]) #R
          X_peru_p[i,5] <- dt * (alpha * X_peru_p[i-1,2])  
        }
      }
    }
  }
}
X_peru_p <- as.data.frame(X_peru_p)
cumsum(X_peru_p$`New Cases`)
X_peru_p$date <- axis_peru_p

##############################Comparison with no iuntervention###########################################
#No intervention using high transmission rate 
# Parameter values
N_peru_n <- 32170000 #population of peru
I_peru_n <- 5 #imported cases from peru
E_peru_n <- 15 * I_peru #number potentially exposed
R_peru_n <- 0 
S_peru_n <- N_peru-E_peru-I_peru-R_peru
alpha <- 1/4.5 #incubation period
gamma <- 0.38 #recovery period
duration <- 1/0.38 #days 

R0_n <- 2.3 #high end (the CI from JHU and authors)
beta_n <- R0_n/(N_peru * duration)
#time specification
dt <- 1
start <- 0
end_peru_p_n <- 100 #days
time_peru_p_n <- seq(start, end_peru_p_n, by = dt)
niter_peru_p_n <- (end_peru_p_n - start) / dt + 1
s_date_peru_p_n <- mdy("3-6-2020")
e_date_peru_p_n <- mdy("6-18-2020")
axis_peru_p_n <- seq(from = s_date_peru_p_n, to = e_date_peru_p_n, by = "day")
X_peru_p_n <- array(data = 0, dim = c(niter_peru_p_n, 5)) #created an array to catch the results
colnames(X_peru_p_n) <- c("S", "E", "I","R", "New Cases")
X_peru_p_n[1, 1] <- S_peru_n #initial susuceptible population 
X_peru_p_n[1, 2] <- E_peru_n #initial latent population 
X_peru_p_n[1, 3] <- I_peru_n #initial infectious population 
X_peru_p_n[1, 4] <- R_peru_n #initial recovered population 
X_peru_p_n[1, 5] <- I_peru_n #new cases
for(i in 2:niter_peru_p_n) {
  X_peru_p_n [i,1] <- X_peru_p_n [i-1, 1] + dt * (-beta_n * X_peru_p_n [i-1,1] * X_peru_p_n[i-1,3] )  #S
  X_peru_p_n [i,2]<- X_peru_p_n [i-1,2] + dt * ((beta_n * X_peru_p_n [i-1,1] * X_peru_p_n[i-1,3]) - alpha * X_peru_p_n[i-1,2]) #E
  X_peru_p_n [i,3] <- X_peru_p_n [i-1,3] + dt * (alpha * X_peru_p_n [i-1,2] - gamma * X_peru_p_n [i-1,3])  #I 
  X_peru_p_n [i,4] <- X_peru_p_n [i-1,4] + dt * (gamma * X_peru_p_n [i-1,3]) #R
  X_peru_p_n [i,5] <- dt * (alpha * X_peru_p_n [i-1,2]) #new cases
}
X_peru_p_n <- as.data.frame(X_peru_p_n)
cumsum(X_peru_p_n$`New Cases`)
X_peru_p_n$date <- axis_peru_p


###########plots#################
#plot: number of new cases main = "Impact of Intervention on the Number of Infectious People Each Day",
plot(X_peru_p[c(1:10), 3] ~ axis_peru_p[c(1:10)] ,  col = 'steelblue4',  lwd = 3,  type = 'l', ylab = "Infected", xlab = "Date",  xlim = as.Date(c("2020-03-06", "2020-06-18")), ylim = c(0,6000), main = "Number of People Infectious \n with Interventions")
lines(X_peru_p[c(10:28), 3] ~ axis_peru_p[c(10:28)] , col = 'violetred4', lwd = 3)
lines(X_peru_p[c(28:36), 3] ~ axis_peru_p[c(28:36)], col = 'green', lwd = 3)
lines(X_peru_p[c(36:53), 3] ~ axis_peru_p[c(36:53)], col = 'gold', lwd = 3)
lines(X_peru_p[c(53:100), 3] ~ axis_peru_p[c(53:100)], col = 'red', lwd = 3)
lines(X_peru_p_n[c(1:100), 3] ~ axis_peru_p[c(1:100)] , col = 'navy', lwd = 3) #if we are assuming that transmission continued at a high rate
legend("topright", legend = c("R0=2.83 (06-Mar - 15-Mar)", "R0=1.77 (16-Mar - 02-Apr)", "R0=1.68 (03-Apr - 10-Apr)", "R0=1.61 (10-Apr - 26-Apr)", "R0=0.90 (27-Apr - 02-Agu)", "No Intervention"), fill = c("steelblue4", "violetred4", "green", "gold", "red", "navy"), col = c("steelblue4", "violetred4", "green", "gold", "red", "navy"), cex = 0.75, box.lty = 0)
#cumulative cases comparison
#without intervention
plot(cumsum(X_peru_p_n[c(1:100), 5]) ~ axis_peru_p[c(1:100)],  col = 'navy',  lwd = 3,  type = 'l', ylab = 'Infected', xlab = "Date",  xlim = as.Date(c("2020-03-06", "2020-06-14")), ylim = c(0,45000), main = "Number of Cumulative Cases \n with Interventions") 
#current situation
lines(peru.cases[c(1:44), 2] ~ axis_peru_p[c(1:44)], col = 'chartreuse4', lwd = 3) 
#with intervetnion
lines(cumsum(X_peru_p[c(1:100), 5]) ~ axis_peru_p[c(1:100)],  col = 'red', lwd = 3) 
legend("topright", legend = c("Without Interventions", "Current Situation", "With Interventions"), fill = c("navy", "chartreuse4", "red"), col = c("navy", "chartreuse4", "red"), cex = 0.75, box.lty = 0)


