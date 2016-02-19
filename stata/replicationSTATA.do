clear all
sysuse auto
set more off
// NOTE: This script requires the dd.1981 object that was created
// in ./r/buildModel/01_defineEconomicShockVariables.R
use "./dd.1981.dta", clear

// Label variables ----------------------------------------------
label variable ctryname "Country name"
label variable cowcode2 "DD modified COW Country code"
label variable reg_fail "Regime failure"
label variable ciri_physint2 "Reversed CIRI Physical Integrity Index"
label variable ciri_pf "Reversed Sum CIRI SPEECH, WORKER, ELECSD, ASSN"
label variable spellrn "Spell duration, linear"
label variable type "GWF Regime type"
label variable lparty2 "DD parties in legislature"
label variable pwt7_grgdpch_wght "PWT7 Weigthed growth experience"
label variable pwt7_rgdpch "PWT7 Real GDP per Capita"
// The suffix "_l1" marks the lagged version (k = 1).

// Drop redundant data ------------------------------------------
keep cowcode2 ctryname year reg_fail ciri_physint2 ciri_physint2_l1 ///
	ciri_pf ciri_pf_l1 spellrn type type_l1 lparty2 lparty2_l1 pwt7_grgdpch_wght ///
	pwt7_grgdpch_wght_l1 pwt7_rgdpch pwt7_rgdpch_l1
order cowcode2 ctryname year reg_fail ciri_physint2 ciri_physint2_l1 ///
	ciri_pf ciri_pf_l1 spellrn type type_l1 lparty2 lparty2_l1 pwt7_grgdpch_wght ///
	pwt7_grgdpch_wght_l1 pwt7_rgdpch pwt7_rgdpch_l1

// Minor preps for model stage ----------------------------------
// generate spell duration polynomials
gen spellrn_sq = spellrn^2
gen spellrn_cb = spellrn^3
gen spellrn_cb_scaled = spellrn_cb/1000

// genrate inverted growth to ease interpretation
gen negative_pwt7_grgdpch_wght_l1 = -1 * pwt7_grgdpch_wght_l1

// generate log gdp per capita
gen log_pwt7_rgdpch_l1 = log(pwt7_rgdpch_l1)

// label categorical variables
label define regimetype ///
	1 "PARTY" 2 "MILITARY" 3 "MONARCHY" 4 "PERSONAL", replace
label values type type_l1 regimetype

label define parliament ///
	1 "Legislature, Single party" 2 "Legislature, No party" ///
	3 "Legislature, Multiple parties"
label values lparty2 lparty2_l1

// Approximate published models ---------------------------------
// Using STATA 12 IC this is as close as I'll get. In R my
// results can be reproduced exactly.
#delimit ;
logit reg_fail 
	negative_pwt7_grgdpch_wght_l1 spellrn spellrn_sq spellrn_cb_scaled if
	pwt7_grgdpch_wght_l1 >= -27.72891 & pwt7_grgdpch_wght_l1 <= 82.69569 ;
	
logit reg_fail 
	negative_pwt7_grgdpch_wght_l1 spellrn spellrn_sq spellrn_cb_scaled 
	log_pwt7_rgdpch_l1 I.type_l1 if
	pwt7_grgdpch_wght_l1 >= -27.72891 & pwt7_grgdpch_wght_l1 <= 82.69569 ;

logit reg_fail 
	negative_pwt7_grgdpch_wght_l1 spellrn spellrn_sq spellrn_cb_scaled 
	I.type_l1
	ciri_physint2_l1 ciri_pf_l1 I.lparty2_l1 if
	pwt7_grgdpch_wght_l1 >= -27.72891 & pwt7_grgdpch_wght_l1 <= 82.69569 ;
#delimit cr
