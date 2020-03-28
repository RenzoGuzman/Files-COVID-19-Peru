
*Dofile COVID-19

import excel "/Users/renzomauricioguzmananaya/Downloads/Principales cultivos.xlsx", sheet("Hoja 3") firstrow clear

collapse (sum) day1 day2 day3 day4 day5 day6 day7 day8 day9 day10 day11 day12 day13 day14 day15 day16 day17 day18 day19 day20 day21 day22 day23 day24 day25 day26 day27 day28 day29 day30 day31 day33 day34 day35 day36 day37 day38 day39 day40 day41 day42 day43 day44 day45 day46 day47 day48 day49 day50 day51 day52 day53 day54 day55 day56 day57 day58 day59 day60 day61, by(CountryRegion)

gen dum=.
replace dum=1 if day61>500
replace dum=1 if CountryRegion=="Peru"
keep if dum==1
drop dum

export excel using "/Users/renzomauricioguzmananaya/Desktop/COVID-19 R/data_countries.xlsx", replace

import excel "/Users/renzomauricioguzmananaya/Downloads/Principales cultivos-2.xlsx", sheet("Hoja 5") firstrow clear

save "/Users/renzomauricioguzmananaya/Desktop/COVID-19 R/diary_data_countries.dta", replace
