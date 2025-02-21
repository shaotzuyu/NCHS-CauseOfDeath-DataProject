*************************************************
*************************************************
** Project: PARM
** Start date: 20 Feb, 2025
** Update date: 20 Feb, 2025
*************************************************
*************************************************


* ------------------------------------------- *
* ETL pipeline for 2003:2022
* ------------------------------------------- *

cd ""

forvalue i = 2003/2022 {
* Load
	import delimited "nvss_cod_mismatch_`i'.csv", stringcols(2 3 4 5) clear

	* Ordering
	order ids-mismatch_state icd10_chapter-multiple_cause_4plus


	* Sex
	encode sex, gen(SEX)
	tab SEX
	drop sex
	rename SEX sex
	numlabel, add
	tab sex

	* Marital status
	encode marital, gen(MARRY)
	tab MARRY
	drop marital
	rename MARRY marital
	numlabel, add
	tab marital
		label define marital 1"Divorced" 2"Married" 3"Single/Never" 4"Unknown" 5"Widowed"
		label value marital marital
		numlabel, add
		tab marital
		
	* Age
	gen agegroup = .
		replace agegroup = 1 if inlist(age_recode_52, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)   // <1 year
		replace agegroup = 2 if inlist(age_recode_52, 24, 25, 26)  // 1-4 years
		replace agegroup = 3 if age_recode_52 == 27  // 5-9 years
		replace agegroup = 4 if age_recode_52 == 28  // 10-14 years
		replace agegroup = 5 if age_recode_52 == 29  // 15-19 years
		replace agegroup = 6 if age_recode_52 == 30  // 20-24 years
		replace agegroup = 7 if age_recode_52 == 31  // 25-29 years
		replace agegroup = 8 if age_recode_52 == 32  // 30-34 years
		replace agegroup = 9 if age_recode_52 == 33  // 35-39 years
		replace agegroup = 10 if age_recode_52 == 34  // 40-44 years
		replace agegroup = 11 if age_recode_52 == 35  // 45-49 years
		replace agegroup = 12 if age_recode_52 == 36  // 50-54 years
		replace agegroup = 13 if age_recode_52 == 37  // 55-59 years
		replace agegroup = 14 if age_recode_52 == 38  // 60-64 years
		replace agegroup = 15 if age_recode_52 == 39  // 65-69 years
		replace agegroup = 16 if age_recode_52 == 40  // 70-74 years
		replace agegroup = 17 if age_recode_52 == 41  // 75-79 years
		replace agegroup = 18 if inlist(age_recode_52, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51)  // 80+ years
		replace agegroup = . if age_recode_52 == 52  // Age not stated

	label define agegroup 1 "<1" 2 "1-4" 3 "5-9" 4 "10-14" 5 "15-19" 6 "20-24" 7 "25-29" ///
						 8 "30-34" 9 "35-39" 10 "40-44" 11 "45-49" 12 "50-54" 13 "55-59" ///
						 14 "60-64" 15 "65-69" 16 "70-74" 17 "75-79" 18 "80+" 
						 
	label values agegroup agegroup
	numlabel, add
	tab agegroup


	* Mismatch variable
	tab mismatch_fips
	gen mis_fips = .
		replace mis_fips = 0 if mismatch_fips == 0 
		replace mis_fips = 1 if mismatch_fips == 1
	tab mis_fips

	encode mismatch_state, gen(mis_state)
	numlabel, add
	tab mis_state
		replace mis_state = . if mis_state ==3
		replace mis_state = 0 if mis_state ==1
		replace mis_state = 1 if mis_state ==2
		label define mis_state 0"0" 1"1", modify
		label value mis_state mis_state
	tab mis_state

	* Total condition
	replace total_condition = 10 if total_condition>=10 & total_condition!=.
	tab total_condition

	* Unique chapter
	replace unique_chapters = 8 if unique_chapters>=8 & unique_chapters!=.
	tab unique_chapters

	* Save
	save "nvss_cod_mismatch_`i'", replace
}

* ------------------------------------------- *
* log-file for 2010 and 2020 logistic
* ------------------------------------------- *

log using "", replace

* ------------------------------------------- *
* logistic regression 
* County change
* 2010 and 2020
* ------------------------------------------- *

forvalue i = 2010(10)2020 {
	
	use "nvss_cod_mismatch_`i'", clear
	* Total condition
	logit mis_fips i.sex ib3.marital i.agegroup i.total_conditions, or base robust
	* Unique condition
	logit mis_fips i.sex ib3.marital i.agegroup i.unique_chapters, or base robust
	* Binarized 1/0 on 2/more unique cause, 3/more, 4/more
	logit mis_fips i.sex ib3.marital i.agegroup i.multiple_cause, or base robust
	logit mis_fips i.sex ib3.marital i.agegroup i.multiple_cause_3plus, or base robust
	logit mis_fips i.sex ib3.marital i.agegroup i.multiple_cause_4plus, or base robust

	* ------------------------------------------- *
	* logistic regression for 2010 and 2020
	* State
	* ------------------------------------------- *

	* Total condition
	logit mis_state i.sex ib3.marital i.agegroup i.total_conditions, or base robust
	* Unique condition
	logit mis_state i.sex ib3.marital i.agegroup i.unique_chapters, or base robust
	* Binarized 1/0 on 2/more unique cause, 3/more, 4/more
	logit mis_state i.sex ib3.marital i.agegroup i.multiple_cause, or base robust
	logit mis_state i.sex ib3.marital i.agegroup i.multiple_cause_3plus, or base robust
	logit mis_state i.sex ib3.marital i.agegroup i.multiple_cause_4plus, or base robust

}

log close


























