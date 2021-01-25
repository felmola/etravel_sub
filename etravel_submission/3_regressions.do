/*############################################################################*/
* 0. Set WD
/*############################################################################*/

clear all
set more off
cd "C:\Users\Felipe M\Dropbox\2_CID\4 diversificación Open Evidence (1)\2 Online tourism\paper\efficency_analysis_etravel"

use etravel

/*############################################################################*/
* 0. Basic regressions + RI + Bootstrapping
/*############################################################################*/
*============================ will_buy

eststo clear
eststo reg1: reg will_buy i.treatment, rob

eststo reg2: reg will_buy i.treatment round gender age education country, rob
matrix pvalues=r(p) //save the p-values from ritest

eststo reg3: reg will_buy i.treatment round gender age education country, cluster(round)

eststo reg4: reg will_buy i.treatment round gender age education country, cluster(sessioncode)

eststo reg5: ritest treatment _b[1.treatment] _b[2.treatment] _b[3.treatment] _b[4.treatment], reps(1000) seed(123): reg will_buy i.treatment round gender age education country, rob
matrix pvalues=r(p) //save the p-values from ritest
mat colnames pvalues = 1 2 3 4 //name p-values so that esttab knows to which coefficient they belong
matrix list pvalues
est restore reg5
*for pvalues below table
estadd scalar t1 = pvalues[1,1]
estadd scalar t2 = pvalues[1,2]
estadd scalar t3 = pvalues[1,3]
estadd scalar t4 = pvalues[1,4]

eststo reg6: ritest treatment _b[1.treatment] _b[2.treatment] _b[3.treatment] _b[4.treatment], reps(1000) seed(123) cluster(sessioncode): reg will_buy i.treatment round gender age education country
matrix pvalues=r(p) //save the p-values from ritest
mat colnames pvalues = 1 2 3 4 //name p-values so that esttab knows to which coefficient they belong
matrix list pvalues
est restore reg6
*for pvalues below table
estadd scalar t1 = pvalues[1,1]
estadd scalar t2 = pvalues[1,2]
estadd scalar t3 = pvalues[1,3]
estadd scalar t4 = pvalues[1,4]

eststo reg7: ritest treatment _b[1.treatment] _b[2.treatment] _b[3.treatment] _b[4.treatment], reps(1000) seed(123) cluster(sessioncode round): reg will_buy i.treatment round gender age education country
matrix pvalues=r(p) //save the p-values from ritest
mat colnames pvalues = 1 2 3 4 //name p-values so that esttab knows to which coefficient they belong
matrix list pvalues
est restore reg7
*for pvalues below table
estadd scalar t1 = pvalues[1,1]
estadd scalar t2 = pvalues[1,2]
estadd scalar t3 = pvalues[1,3]
estadd scalar t4 = pvalues[1,4]

eststo reg8: bootstrap, reps (1000) seed(123) cluster(sessioncode): reg will_buy i.treatment round gender age education country

esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 using will_buy.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels drop(round gender age education country) stats(t1 t2 t3 t4) mtitles( Robust Controls Cluster(round) Cluster(Session) RI RI(session) RI(round) Bootstrapping ) replace

*============================ ask_price_fin

eststo clear

eststo reg1: reg ask_price_fin i.treatment, rob

eststo reg2: reg ask_price_fin i.treatment round gender age education country, rob
matrix pvalues=r(p) //save the p-values from ritest

eststo reg3: reg ask_price_fin i.treatment round gender age education country, cluster(round)

eststo reg4: reg ask_price_fin i.treatment round gender age education country, cluster(sessioncode)

eststo reg5: ritest treatment _b[1.treatment] _b[2.treatment] _b[3.treatment] _b[4.treatment], reps(1000) seed(123): reg ask_price_fin i.treatment round gender age education country, rob
matrix pvalues=r(p) //save the p-values from ritest
mat colnames pvalues = 1 2 3 4 //name p-values so that esttab knows to which coefficient they belong
matrix list pvalues
est restore reg5
*for pvalues below table
estadd scalar t1 = pvalues[1,1]
estadd scalar t2 = pvalues[1,2]
estadd scalar t3 = pvalues[1,3]
estadd scalar t4 = pvalues[1,4]

eststo reg6: ritest treatment _b[1.treatment] _b[2.treatment] _b[3.treatment] _b[4.treatment], reps(1000) seed(123) cluster(sessioncode): reg ask_price_fin i.treatment round gender age education country
matrix pvalues=r(p) //save the p-values from ritest
mat colnames pvalues = 1 2 3 4 //name p-values so that esttab knows to which coefficient they belong
matrix list pvalues
est restore reg6
*for pvalues below table
estadd scalar t1 = pvalues[1,1]
estadd scalar t2 = pvalues[1,2]
estadd scalar t3 = pvalues[1,3]
estadd scalar t4 = pvalues[1,4]

eststo reg7: ritest treatment _b[1.treatment] _b[2.treatment] _b[3.treatment] _b[4.treatment], reps(1000) seed(123) cluster(sessioncode round): reg ask_price_fin i.treatment round gender age education country
matrix pvalues=r(p) //save the p-values from ritest
mat colnames pvalues = 1 2 3 4 //name p-values so that esttab knows to which coefficient they belong
matrix list pvalues
est restore reg7
*for pvalues below table
estadd scalar t1 = pvalues[1,1]
estadd scalar t2 = pvalues[1,2]
estadd scalar t3 = pvalues[1,3]
estadd scalar t4 = pvalues[1,4]

eststo reg8: bootstrap, reps (1000) seed(123) cluster(sessioncode): reg ask_price_fin i.treatment round gender age education country

esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 using ask_price_fin.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels drop(round gender age education country) stats(t1 t2 t3 t4) mtitles( Robust Controls Cluster(round) Cluster(session) RI RI(session) RI(round) Bootstrapping ) replace

/*############################################################################*/
* 0. Additional regressions
/*############################################################################*/
clear all
use etravel
eststo clear

*============================ will_buy
*---------------------------- controlando por precio promedio por sesión
eststo reg1: reg will_buy i.treatment mean_s_askpricefin round gender age education country, rob

eststo reg2: reg will_buy i.treatment mean_s_askpricefin round gender age education country, cluster(round)

eststo reg3: reg will_buy i.treatment mean_s_askpricefin round gender age education country, cluster(sessioncode)

*---------------------------- controlando por buyer valuations
eststo reg4: reg will_buy i.treatment mean_s_askpricefin buyer_valuation_pac1 buyer_valuation_pac2 buyer_valuation_pac3 buyer_valuation_pac4 buyer_valuation_pac5 round gender age education country, rob

eststo reg5: reg will_buy i.treatment mean_s_askpricefin buyer_valuation_pac1 buyer_valuation_pac2 buyer_valuation_pac3 buyer_valuation_pac4 buyer_valuation_pac5 round gender age education country, cluster(round)

eststo reg6: reg will_buy i.treatment mean_s_askpricefin buyer_valuation_pac1 buyer_valuation_pac2 buyer_valuation_pac3 buyer_valuation_pac4 buyer_valuation_pac5 round gender age education country, cluster(sessioncode)

*---------------------------- Corran la regresión (2) agrupada por mercado (ronda), instrumentando el precio promedio por el promedio de los costos y agregando los treatments
bysort sessioncode round:  gen dup = cond(_N==1,0,_n)
drop if dup > 1

eststo reg7: ivregress 2sls mean_sr_willbuy i.treatment round gender age education country (mean_sr_askpricefin = mean_sr_sellervaluation)

eststo reg8: ivregress 2sls mean_sr_willbuy i.treatment round gender age education country (mean_sr_askpricefin = mean_sr_sellervaluation), cluster(round)

eststo reg9: ivregress 2sls mean_sr_willbuy i.treatment round gender age education country (mean_sr_askpricefin = mean_sr_sellervaluation), cluster(sessioncode)
bysort treatment: summarize mean_sr_willbuy

esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 using will_buy_additional.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels keep(1.treatment 2.treatment 3.treatment 4.treatment) mtitles( mean_sesion_price (1)+Cluster(round) (1)+Cluster(sessioncode) buyer_val (4)+Cluster(round) (4)+Cluster(sessioncode) IV (7)+Cluster(round) (7)+Cluster(sessioncode)) replace

*============================ ask_price_fin
*---------------------------- controlando carta costo de producción
clear all
use etravel
eststo clear

eststo reg1: reg ask_price_fin i.treatment seller_valuation round gender age education country, rob

eststo reg2: reg ask_price_fin i.treatment seller_valuation round gender age education country, cluster(round)

eststo reg3: reg ask_price_fin i.treatment seller_valuation round gender age education country, cluster(sessioncode)

esttab reg1 reg2 reg3 using askpricefin_additional.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels keep(1.treatment 2.treatment 3.treatment 4.treatment) mtitles( seller_valuation (1)+Cluster(round) (1)+Cluster(sessioncode)) replace

/*############################################################################*/
* 0. Additional regressions (2020-01-08)
/*############################################################################*/
clear all
use etravel

*============================ regresion de see_list
tab role see_list
bysort treatment: summarize see_list /*to get the averages by treatment */

eststo clear

eststo reg1: reg see_list i.treatment round gender age education country, rob

eststo reg2: reg see_list i.treatment round gender age education country, cluster(round)

eststo reg3: reg see_list i.treatment round gender age education country, cluster(sessioncode)

esttab reg1 reg2 reg3 using see_list.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels keep(1.treatment 2.treatment 3.treatment 4.treatment) mtitles( see_list (1)+Cluster(round) (1)+Cluster(sessioncode)) replace

*============================ regresion de used_com_practice
tab role used_com_practice
tab treatment used_com_practice

eststo clear

eststo reg1: reg used_com_practice i.treatment round gender age education country, rob

eststo reg2: reg used_com_practice i.treatment round gender age education country, cluster(round)

eststo reg3: reg used_com_practice i.treatment round gender age education country, cluster(sessioncode)

esttab reg1 reg2 reg3 using used_com_practice.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels keep( 2.treatment 3.treatment 4.treatment) mtitles( used_com_practice (1)+Cluster(round) (1)+Cluster(sessioncode)) replace

*============================ regresion de badpractice

tab treatment badpractice
tab com_practice badpractice

eststo clear

eststo reg1: reg badpractice i.treatment round gender age education country, rob

eststo reg2: reg badpractice i.treatment round gender age education country, cluster(round)

eststo reg3: reg badpractice i.treatment round gender age education country, cluster(sessioncode)

esttab reg1 reg2 reg3 using badpractice.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels keep( 2.treatment 3.treatment 4.treatment) mtitles( badpractice (1)+Cluster(round) (1)+Cluster(sessioncode)) replace

*============================ regresion de las dummies de cada una de las commercial practices

tab treatment badpractice
tab com_practice badpractice

eststo clear

eststo reg1: reg dum_com_practice_1 i.treatment round gender age education country, rob

eststo reg2: reg dum_com_practice_1 i.treatment round gender age education country, cluster(round)

eststo reg3: reg dum_com_practice_1 i.treatment round gender age education country, cluster(sessioncode)

eststo reg4: reg dum_com_practice_2 i.treatment round gender age education country, rob

eststo reg5: reg dum_com_practice_2 i.treatment round gender age education country, cluster(round)

eststo reg6: reg dum_com_practice_2 i.treatment round gender age education country, cluster(sessioncode)

eststo reg7: reg dum_com_practice_3 i.treatment round gender age education country, rob

eststo reg8: reg dum_com_practice_3 i.treatment round gender age education country, cluster(round)

eststo reg9: reg dum_com_practice_3 i.treatment round gender age education country, cluster(sessioncode)

esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 using dum_com_practices.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels keep( 2.treatment 3.treatment 4.treatment) mtitles( cp_1 cp_1$ cp_1$$ cp_2 cp_2$ cp_2$$ cp_3 cp_3$ cp_3$$) replace

*============================ regresion de las dummies de cada una de las commercial practices condicionada a si hizo cheating

eststo clear

eststo reg1: reg dum_bad_cp1 i.treatment round gender age education country, rob

eststo reg2: reg dum_bad_cp1 i.treatment round gender age education country, cluster(round)

eststo reg3: reg dum_bad_cp1 i.treatment round gender age education country, cluster(sessioncode)

eststo reg4: reg dum_bad_cp2 i.treatment round gender age education country, rob

eststo reg5: reg dum_bad_cp2 i.treatment round gender age education country, cluster(round)

eststo reg6: reg dum_bad_cp2 i.treatment round gender age education country, cluster(sessioncode)

eststo reg7: reg dum_bad_cp3 i.treatment round gender age education country, rob

eststo reg8: reg dum_bad_cp3 i.treatment round gender age education country, cluster(round)

eststo reg9: reg dum_bad_cp3 i.treatment round gender age education country, cluster(sessioncode)

esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 using dum_cp_cheating.rtf, star(* 0.1 ** 0.05 *** 0.001) se(%9.2f) b(%9.2f) label nobaselevels keep( 2.treatment 3.treatment 4.treatment) mtitles( cp_1 cp_1$ cp_1$$ cp_2 cp_2$ cp_2$$ cp_3 cp_3$ cp_3$$) replace

*************************** Cheating by commercial practice (as percent of total cheating)

bysort treatment: tab badpractice
bysort treatment: tab dum_com_practice_1 if badpractice == 1
bysort treatment: tab dum_com_practice_2 if badpractice == 1
bysort treatment: tab dum_com_practice_3 if badpractice == 1

*************************** 
/*
This change in the use of BGP deserves some further explanation. We notice 
that the price posted by those who use BPG are actually the highest on average. 
In fact, the average price in the control condition is ##, whereas for those who
 do not use practices is ##, for those who use RP is ##, and for those who use 
 DP is ##. The practice that is more likely to be sanctioned when audited is 
 ###. The BPG is the one most reported by buyers overall, with #% reporting rate.
*/

bysort treatment: summarize ask_price_fin
bysort com_practice: summarize ask_price_fin if treatment >= 1

bysort com_practice: tab badpractice audited
bysort com_practice: summarize reported

tabout treatment com_practice using lala.xls, replace

/*############################################################################*/
* 0. t-tests (2021-01-14)
/*############################################################################*/

clear all
use etravel
eststo clear

reg ask_price_fin i.treatment round gender age education country, cluster(sessioncode) /*(line 74)*/
test 1.treatment = 2.treatment
test 1.treatment = 3.treatment
test 1.treatment = 4.treatment

reg will_buy i.treatment round gender age education country, cluster(sessioncode)
test 1.treatment = 2.treatment
test 1.treatment = 3.treatment
test 1.treatment = 4.treatment

/*############################################################################*/
* 0. (2021-01-19)
/*############################################################################*/
clear all
use etravel

*============================ Cuanto ganan en promedio

summarize payoff
summarize payoff if role == "seller"
summarize payoff if role == "buyer"

*============================ cuantas veces multan en formal sanction, y cuantas veces multan los buenos y los cheaters en informal sanctions

tab audited role
tab audited badpractice

tab reported role
tab reported badpractice

/*############################################################################*/
* 0. Delta Method (2021-01-25)
/*############################################################################*/

* Elasticity (from ivregress of will_buy)

clear all
use etravel
eststo clear

* save mean session round askpricefin into local
summarize mean_sr_askpricefin
* = 61.868
*local mean_price = r(mean)
*display `mean_price'

* save mean session round willbuy into local
summarize mean_sr_willbuy
*local mean_wb = r(mean)
* = 0.908
*display `mean_wb'

*regression for having the estimator
bysort sessioncode round:  gen dup = cond(_N==1,0,_n)
drop if dup > 1
eststo reg9: ivregress 2sls mean_sr_willbuy i.treatment round gender age education country (mean_sr_askpricefin = mean_sr_sellervaluation), cluster(sessioncode)

* Delta method
di (61.868 / 0.908) * -0.0017624
*di (`mean_price' / `mean_wb') * -0.0017624
* = -0.12008388

* Delta method Standard Error: Francesco: Al parecer es solo P/Q multiplicado por el estandard error del beta estimado
di (61.868 / 0.908) * 0.0027468
*= 0.18715751
