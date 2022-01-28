#########################

#########################
####
####
####
#### Replication file for stata code Judge-Lord, Grimmer, and Powell
####
##########################
##########################


clear 

use /users/jgrimmer/Dropbox/Correspondence/Data/AgencyComm.dta

destring year, replace

tostring year , gen(str_year)
gen year_agency = agency  + str_year 


egen ia_fixed = group(icpsr_agency)
egen ya_fixed = group(year_agency)


gen lpyear = log(perYear + 1)

xtset ia_fixed ya_fixed

eststo clear 



eststo: reg perYear chair ranking_minority prestige majority presidents_party first second third fourth fifth sixth if year >2006 & year < 2019,  cluster(icpsr)
estadd local ia_fixed "", replace
estadd local year_fixed "", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace 
estadd local maj "\checkmark", replace 
estadd local DV "Count", replace 

eststo: xtreg perYear chair ranking_minority prestige majority presidents_party first second third fourth fifth sixth  if year >2006 & year < 2019,  fe cluster(icpsr)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace 
estadd local maj "\checkmark", replace 
estadd local DV "Count", replace 


eststo: xtreg perYear chair ranking_minority prestige majority presidents_party first second third fourth fifth sixth if year >2006 & year < 2019 & survive==1,  fe cluster(icpsr)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "", replace
estadd local survive "\checkmark", replace 
estadd local maj "\checkmark", replace 
estadd local DV "Count", replace 


eststo: xtreg lpyear chair ranking_minority prestige majority presidents_party first second third fourth fifth sixth if year >2006 & year < 2019 ,  fe cluster(icpsr) 
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace 
estadd local time "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local DV "Log(Count + 1)", replace 

esttab using /Users/jgrimmer/Dropbox/correspondence/tables/FinalTable_1.tex,  nostar nogap nomtitles booktabs replace drop(_cons majority presidents_party) s(maj ia_fixed year_fixed all_leg survive DV N, labels("Majority, President's Party" "Legislator $\times$ Agency Fixed Effects" "Year $\times$ Agency Fixed Effects" "All Legislators" "Serve At Least Second Term" "Dependent Variable" "Observations")) label varlabels(chair "Committee Chair" ranking_minority "Ranking Member" prestige "Prestige Committee" oversight_committee "Oversight Committee" first "First Year" second "Second Year" third "Third Year" fourth "Fourth Year" fifth "Fifth Year" sixth "Sixth Year") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at legislator level")


###ratio of constituent to policy letters


clear
use ~/Dropbox/correspondence/data/ProportionContact.dta
destring year, replace
egen icpsr_a = group(icpsr)
xtset icpsr_a year



eststo clear 	

eststo: reg ratio prestige chair ranking_minority majority presidents_party first second third fourth fifth sixth if year>2006 & year< 2019, cluster(icpsr_a)
estadd local leg "" , replace
estadd local year "", replace
estadd local maj "", replace 

eststo: xtreg ratio prestige chair ranking_minority majority presidents_party first second third fourth fifth sixth if year>2006 & year< 2019, fe cluster(icpsr_a)
estadd local leg "\checkmark" , replace
estadd local year "\checkmark", replace
estadd local maj "\checkmark", replace 


esttab using /Users/jgrimmer/Dropbox/correspondence/tables/TypeContact.tex,  nostar nogap nomtitles booktabs replace drop(majority presidents_party) s(maj leg year N, labels("Majority" "Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(first "First Year" second "Second Year" third "Third Year" fourth "Fourth Year" fifth "Fifth Year" sixth "Sixth Year" prestige "Prestige" chair "Chair" _cons "Intercept" ranking_minority "Ranking Minority") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")



##District Level Analysis


clear 

use /users/jgrimmer/Dropbox/correspondence/data/DistrictLevel.dta


destring year, replace

egen sd_fixed = group(state_dist)

egen c_fixed  = group(chamber)

xtset sd_fixed 

gen lpyear = log(perYear + 1)

eststo clear 


eststo: reg perYear new_member second_year third_year fourth_year fifth_year sixth_year if year < 2019 & year > 2006 & c_fixed!=2 , cluster(sd_fixed)
estadd local sd_fixed "", replace
estadd local year_fixed "", replace
estadd local all_leg "\checkmark", replace
estadd local house "", replace
estadd local senate "", replace 

eststo: xtreg perYear new_member second_year third_year fourth_year fifth_year sixth_year i.year   if year < 2019 & year>2006 & c_fixed!=2 , fe cluster(sd_fixed)
estadd local sd_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "\checkmark", replace
estadd local house "", replace
estadd local senate "", replace 

eststo: xtreg perYear new_member second_year third_year fourth_year fifth_year sixth_year i.year  if year < 2019 & year>2006 & c_fixed==1, fe cluster(sd_fixed)
estadd local sd_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "", replace
estadd local house "\checkmark", replace
estadd local senate "", replace 



eststo: xtreg perYear new_member second_year third_year fourth_year fifth_year sixth_year i.year if year < 2019 & year>2006 & c_fixed==3, fe cluster(sd_fixed)
estadd local sd_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "", replace
estadd local house "", replace
estadd local senate "\checkmark", replace 


esttab using /Users/jgrimmer/Dropbox/correspondence/tables/DistrictTableTenure.tex, nostar nogap nomtitles booktabs replace order(new_member second_year third_year fourth_year fifth_year sixth_year) drop(_cons *.year) s(sd_fixed year_fixed all_leg house senate N, labels("District Fixed Effects" "Year Fixed Effects" "All Districts" "House Only" "Senate Only" "Observations")) label varlabels(new_member "New Legislator" second_year "Legislator 2nd Year" third_year "Legislator 3rd Year" fourth_year "Legislator 4th Year" fifth_year "Legislator 5th Year" sixth_year "Legislator 6th Year") se sfmt(%9.0fc %5.2f) nonotes addnotes("Robust standard errors in parentheses, clustered at district level")



clear 

use /users/justingrimmer/Dropbox/correspondence/data/DistrictLevel.dta

destring year, replace

egen sd_fixed = group(state_dist)

egen c_fixed  = group(chamber)

xtset sd_fixed 

eststo clear 

eststo: xtreg perYear mean_new i.year if year<2019 & year>2006 &  new_member==0, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "", replace

eststo: xtreg perYear state_new i.year if year<2019 & year>2006 &  new_member==0, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "", replace

eststo: xtreg perYear mean_new i.year if year<2019 & year>2006 & c_fixed==2 & new_member==0, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "\checkmark", replace

eststo: xtreg perYear state_new i.year if year<2019 & year>2006 & c_fixed==2 & new_member==0, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "\checkmark", replace

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TableSpill.tex,  nostar nogap nomtitles booktabs replace drop(_cons *.year) s(ia_fixed year_fixed senate N, labels("District Fixed Effects" "Year Fixed Effects" "Senators Only" "Observations")) label varlabels(mean_new "Proportion New Legislators" state_new "At Least One New Legislator") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at district level")







