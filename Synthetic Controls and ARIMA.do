

ssc install grstyle
grstyle init
grstyle set ci burd, select(11 13) opacity(60)

twoway  (line R0 num, lwidth(medthick)), ///
xlabel(1 "10-mar" 10 "19-mar" 20 "29-mar" 30 "08-apr" 41 "19-apr") /// 
ytitle("Basic reproduction number (R0)") xtitle("Day")  /// 
graphregion(color(white)) ylab( ,nogrid)

twoway  (line beta num, lwidth(medthick)), ///
xlabel(1 "10-mar" 10 "19-mar" 20 "29-mar" 30 "08-apr" 41 "19-apr") /// 
ytitle("Transmission rate (beta)") xtitle("Day") /// 
graphregion(color(white)) ylab( ,nogrid)

twoway  (line gamma num, lwidth(medthick)), ///
xlabel(1 "10-mar" 10 "19-mar" 20 "29-mar" 30 "08-apr" 41 "19-apr") /// 
ytitle("Recovery rate (gamma)") xtitle("Day") /// 
graphregion(color(white)) ylab( ,nogrid)

**Series

twoway  (line infected num, lwidth(medthick)), ///
xlabel(1 "06-mar" 11 "16-mar" 21 "26-mar" 31 "05-apr" 45 "19-apr") /// 
ytitle("Number of infected") xtitle("Date")  /// 
graphregion(color(white)) ylab( ,nogrid)

twoway  (line recovered num, lwidth(medthick)), ///
xlabel(1 "06-mar" 11 "16-mar" 21 "26-mar" 31 "05-apr" 45 "19-apr") /// 
ytitle("Number of recovered") xtitle("Date")  /// 
graphregion(color(white)) ylab( ,nogrid)

twoway  (line deaths num, lwidth(medthick)), ///
xlabel(1 "06-mar" 11 "16-mar" 21 "26-mar" 31 "05-apr" 45 "19-apr") /// 
ytitle("Number of deaths") xtitle("Date")  /// 
graphregion(color(white)) ylab( ,nogrid)

twoway  (line death_rate num, lwidth(medthick)), ///
xlabel(1 "06-mar" 11 "16-mar" 21 "26-mar" 31 "05-apr" 45 "19-apr") /// 
ytitle("Death rate") xtitle("Date")  /// 
graphregion(color(white)) ylab( ,nogrid)


*ARIMA

arima D.infected, ar(1) ma(1)
predict arima_1_1, y

estimates store arima_1_1

arima D.infected, ar(1 2) ma(1)
predict arima_2_1, y

estimates store arima_2_1

arima D.infected, ar(1) ma(1 2)
predict arima_1_2, y

estimates store arima_1_2

arima D.infected, ar(1 2) ma(1 2)
predict arima_2_2, y

estimates store arima_2_2

arima D.infected, ar(2) ma(2)
predict arima_2_2_only, y

estimates store arima_2_2_only

twoway  (line infected num) (line arima_1_1 num, lpattern(dash_dot) lcolor(green)) /// 
(line arima_2_1 num, lpattern(dash_dot) lcolor(red)) (line arima_1_2 num, lpattern(dash_dot) lcolor(grey)) /// 
(line arima_2_2 num, lpattern(dash_dot) lcolor(orange)), ///
xsize (3) ysize (2.5) ///
legend(order(1 "Actual values" 2 "ARIMA(1,1)" 3 "ARIMA(2,1)" 4 "ARIMA(1,2)" 5 "ARIMA(2,2)")) ///
xlabel(1 "06-mar" 11 "16-mar" 21 "26-mar" 31 "05-apr" 44 "19-apr") /// 
ytitle("Infected") xtitle("Date")  /// 
graphregion(color(white)) ylab( ,nogrid)

esttab arima_1_1 arima_2_1 arima_1_2 arima_2_2 arima_2_2_only, b(%10.4f) se scalars(N r2 F ll) star(* .05 ** .01 *** .001)
 
esttab arima_1_1 arima_2_1 arima_1_2 arima_2_2 using arima.tex


twoway  (line infected num) (line SVM num, lpattern(dash_dot) lcolor(green)) /// 
(line random_forest num, lpattern(dash_dot) lcolor(red)) /// 
(line RNN num, lpattern(dash_dot) lcolor(gold)) /// 
(line LSTM num, lpattern(dash_dot) lcolor(orange)), ///
xsize (3) ysize (2.5) ///
legend(order(1 "Actual values" 2 "SVM" 3 "Random Forest" 4 "RNN" 5 "LSTM")) ///
xlabel(1 "06-mar" 11 "16-mar" 21 "26-mar" 31 "05-apr" 44 "19-apr") /// 
ytitle("Infected") xtitle("Date")  /// 
graphregion(color(white)) ylab( ,nogrid)


twoway  (line R0 num) , ///
xlabel(1 "06-mar" 20 "25-mar" 40 "14-apr" 60 "04-may" 80 "24-may" 100 "13-jun") /// 
ytitle("Infected") xtitle("Date")  /// 
graphregion(color(white)) ylab( ,nogrid)


ssc install grstyle
grstyle init
grstyle set ci burd, select(11 13) opacity(60)

twoway (line infected_ Day, lcolor (black)) (line peru_synth Day,  lpattern(dash)  lcolor (blue)), /// 
legend(ring(0) pos(5) col(1) order(1 "Peru" 2 "Synthetic Peru")) /// 
xline(43, lpattern(dash_dot) lcolor(grey)) /// 
xlabel(1 "06-mar" 20 "25-mar" 40 "16-apr" 60 "04-may" 80 "24-may") /// 
xtitle("Day") /// 
ytitle("Infected") /// 
graphregion(color(white)) ylab( ,nogrid)

twoway (line infected_ Day, lcolor (blue)) (line peru_synth Day,  lpattern(dash)  lcolor (red)), /// 
legend(ring(0) pos(5) col(1) order(1 "Actual values" 2 "Synthetic Peru")) /// 
xline(43, lpattern(dash_dot) lcolor(grey)) /// 
xlabel(1 "06-mar" 20 "25-mar" 40 "14-apr" 60 "04-may" 85 "29-may") /// 
xtitle("Date") /// 
ytitle("Infected") /// 
graphregion(color(white)) ylab( ,nogrid)

*MAE

gen mae_arima_1_1=abs(infected-arima_1_1)
gen mae_arima_2_1=abs(infected-arima_2_1)
gen mae_arima_1_2=abs(infected-arima_1_2)
gen mae_arima_2_2=abs(infected-arima_2_2)
gen mae_LSTM=abs(infected-LSTM)
gen mae_SVM=abs(infected-SVM)
gen mae_random_forest=abs(infected-random_forest)
gen mae_RNN=abs(infected-RNN)
gen mae_SIR=abs(infected-SIR)
gen mae_SEIR=abs(infected-SEIR)
gen mae_SEIR_tv=abs(infected-SEIR_tv)
gen mae_synthetic=abs(infected-synthetic)

*MSE

gen mse_arima_1_1=(infected-arima_1_1)^2
gen mse_arima_2_1=(infected-arima_2_1)^2
gen mse_arima_1_2=(infected-arima_1_2)^2
gen mse_arima_2_2=(infected-arima_2_2)^2
gen mse_LSTM=(infected-LSTM)^2
gen mse_SVM=(infected-SVM)^2
gen mse_random_forest=(infected-random_forest)^2
gen mse_RNN=(infected-RNN)^2
gen mse_SIR=(infected-SIR)^2
gen mse_SEIR=(infected-SEIR)^2
gen mse_SEIR_tv=(infected-SEIR_tv)^2
gen mse_synthetic=(infected-synthetic)^2

collapse (mean) mae_arima_1_1 mae_arima_2_1 mae_arima_1_2 mae_arima_2_2 /// 
mae_LSTM mae_SVM mae_random_forest mae_RNN mae_SIR mae_SEIR mae_SEIR_tv /// 
mae_synthetic mse_arima_1_1 mse_arima_2_1 mse_arima_1_2 mse_arima_2_2 mse_LSTM /// 
mse_SVM mse_random_forest mse_RNN mse_SIR mse_SEIR mse_SEIR_tv mse_synthetic 


*Synthetic Controls


synth infected_ infected_1, trunit(31) trperiod(34) figure

gen dum=.
replace dum=1 if id==1
replace dum=1 if id==4
replace dum=1 if id==5
replace dum=1 if id==6
replace dum=1 if id==13
replace dum=1 if id==17
replace dum=1 if id==23
replace dum=1 if id==24
replace dum=1 if id==31
replace dum=1 if id==32
replace dum=1 if id==33
replace dum=1 if id==34
replace dum=1 if id==35
replace dum=1 if id==36
replace dum=1 if id==37
replace dum=1 if id==38
replace dum=1 if id==39

gen dum2=.
replace dum2=1 if id==6
replace dum2=1 if id==17
replace dum2=1 if id==31

keep if dum2==1

drop if Day>70

drop if id==4
drop if id==5
drop if id==6
drop if id==33
drop if id==34
drop if id==36
drop if id==37

drop if id==24


replace infected_=4500 if id==31 & Day>34

gen infected_1=L.infected_

gen peru_synth=.

keep if id==31
