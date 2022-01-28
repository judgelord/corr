##this is the do file for the analysis of the tenure and the positions
clear 

use /users/justingrimmer/Dropbox/Correspondence/Data/AgencyCommOversightChair.dta

destring year, replace

egen ia_fixed = group(icpsr_agency)

gen lpyear  = log(perYear + 1) 
xtset ia_fixed year




##Regression Table, Tenure 
eststo clear 

eststo: reg perYear first second third fourth fifth sixth majority presidents_party if year > 2006 & year < 2018, cluster(ia_fixed)
estadd local ia_fixed "" , replace
estadd local year_fixed  "", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace
estadd local maj "\checkmark", replace
estadd local DV "Count", replace 

##first specification, clustering at the individual x agency level
eststo: xtreg perYear first second third fourth fifth sixth majority presidents_party if year >2006 & year< 2018,  fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark" , replace
estadd local year_fixed  "\checkmark", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace
estadd local maj "\checkmark", replace
estadd local DV "Count", replace 

##regression on just those individuals who make it past the first election 

eststo: xtreg perYear first second third fourth fifth sixth majority presidents_party if year >2006 & year< 2018 & max_year>1,  fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark" , replace
estadd local year_fixed  "\checkmark", replace
estadd local all_leg "", replace
estadd local survive "\checkmark", replace
estadd local maj "\checkmark", replace
estadd local DV "Count", replace 

eststo: xtreg lpyear first second third fourth fifth sixth majority presidents_party if year >2006 & year< 2018 ,  fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark" , replace
estadd local year_fixed  "\checkmark", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace
estadd local maj "\checkmark", replace
estadd local DV "Log(Count + 1)", replace 

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TableTenure.tex,  nostar nogap nomtitles booktabs replace order(first second third fourth fifth sixth) drop(_cons majority presidents_party) s(maj ia_fixed year_fixed all_leg survive DV N, labels("Majority, President's Party" "Legislator $\times$ Agency Fixed Effects" "Year Fixed Effects" "All Legislators" "Serve At Least Second Term" "Dependent Variable" "Observations")) label varlabels(first "First Year" second "Second Year" third "Third Year" fourth "Fourth Year" fifth "Fifth Year" sixth "Sixth Year") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at legislator x agency level")


##creating a second table at the district-level that shows how things change after a new legislator is elected.  


##Regression Table, Tenure at District Level
clear 

use /users/justingrimmer/Dropbox/correspondence/data/DistrictLevel.dta


destring year, replace

egen sd_fixed = group(state_dist)

egen c_fixed  = group(chamber)

xtset sd_fixed 

gen lpyear = log(perYear + 1)

eststo clear 


eststo: reg perYear new_member second_year third_year fourth_year fifth_year sixth_year if year < 2018 & year > 2006 , cluster(sd_fixed)
estadd local sd_fixed "", replace
estadd local year_fixed "", replace
estadd local all_leg "\checkmark", replace
estadd local house "", replace
estadd local senate "", replace 

eststo: xtreg perYear new_member second_year third_year fourth_year fifth_year sixth_year i.year   if year < 2018 & year>2006 , fe cluster(sd_fixed)
estadd local sd_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "\checkmark", replace
estadd local house "", replace
estadd local senate "", replace 

eststo: xtreg perYear new_member second_year third_year fourth_year fifth_year sixth_year i.year  if year < 2018 & year>2006 & c_fixed==1, fe cluster(sd_fixed)
estadd local sd_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "", replace
estadd local house "\checkmark", replace
estadd local senate "", replace 



eststo: xtreg perYear new_member second_year third_year fourth_year fifth_year sixth_year i.year if year < 2018 & year>2006 & c_fixed==2, fe cluster(sd_fixed)
estadd local sd_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "", replace
estadd local house "", replace
estadd local senate "\checkmark", replace 


esttab using /Users/justingrimmer/Dropbox/correspondence/tables/DistrictTableTenure.tex, nostar nogap nomtitles booktabs replace order(new_member second_year third_year fourth_year fifth_year sixth_year) drop(_cons *.year) s(sd_fixed year_fixed all_leg house senate N, labels("District Fixed Effects" "Year Fixed Effects" "All Districts" "House Only" "Senate Only" "Observations")) label varlabels(new_member "New Legislator" second_year "Legislator 2nd Year" third_year "Legislator 3rd Year" fourth_year "Legislator 4th Year" fifth_year "Legislator 5th Year" sixth_year "Legislator 6th Year") se sfmt(%9.0fc %5.2f) nonotes addnotes("Robust standard errors in parentheses, clustered at district level")




##regression of committee assignment positions

clear 

use /users/justingrimmer/Dropbox/correspondence/data/AgencyData.dta

destring year, replace

tostring year , gen(str_year)
gen year_agency = agency  + str_year 


egen ia_fixed = group(icpsr_agency)
egen ya_fixed = group(year_agency)


gen lpyear = log(perYear + 1)

xtset ia_fixed ya_fixed 

eststo clear 


eststo: reg perYear chair ranking_minority prestige oversight majority presidents_party i.tenure if year >2006 & year < 2018,  cluster(ia_fixed)
estadd local ia_fixed "", replace
estadd local year_fixed "", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace 
estadd local time "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local DV "Count", replace 

eststo: xtreg perYear chair ranking_minority prestige oversight majority presidents_party i.tenure  if year >2006 & year < 2018,  fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "\checkmark", replace
estadd local survive "", replace 
estadd local time "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local DV "Count", replace 


eststo: xtreg perYear chair ranking_minority prestige oversight majority presidents_party i.tenure if year >2006 & year < 2018 & max_year>1,  fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "", replace
estadd local survive "\checkmark", replace 
estadd local time "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local DV "Count", replace 


eststo: xtreg lpyear chair ranking_minority prestige oversight majority presidents_party i.tenure if year >2006 & year < 2018 ,  fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local all_leg "", replace
estadd local survive "", replace 
estadd local time "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local DV "Log(Count + 1)", replace 

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TablePrestige.tex,  nostar nogap nomtitles booktabs replace drop(_cons *.tenure majority presidents_party) s(time maj ia_fixed year_fixed all_leg survive DV N, labels("Tenure" "Majority, President's Party" "Legislator $\times$ Agency Fixed Effects" "Year Fixed Effects" "All Legislators" "Serve At Least Second Term" "Dependent Variable" "Observations")) label varlabels(chair "Committee Chair" ranking_minority "Ranking Member" prestige "Prestige Committee" oversight_committee "Oversight Committee" majority "Majority" presidents_party "President's Party") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at legislator x agency level")


###we can include those seperately as well, or cluster at the individual level.  

clear

use /users/justingrimmer/Dropbox/correspondence/data/AgencyData_ConService.dta

destring year, replace

egen ia_fixed = group(icpsr_agency)


xtset ia_fixed year


reg perYear first second third fourth fifth sixth if year <2019 & year>2006, cluster(ia_fixed)

xtreg perYear first second third fourth fifth sixth if year <2019 & year>2006, fe cluster(ia_fixed)


reg perYear prestige chair oversight_committee majority presidents_party i.tenure if year<2018 & year>2006, cluster(ia_fixed)
xtreg perYear prestige chair oversight_committee majority presidents_party i.tenure if year <2018 & year>2006, fe cluster(ia_fixed)



##finally , doing same thing for policy


clear

use /users/justingrimmer/Dropbox/correspondence/data/AgencyData_Policy.dta

destring year, replace

egen ia_fixed = group(icpsr_agency)


xtset ia_fixed year


reg perYear first second third fourth fifth sixth if year <2019 & year>2006, cluster(ia_fixed)

xtreg perYear first second third fourth fifth sixth if year <2019 & year>2006, fe cluster(ia_fixed)


reg perYear prestige chair oversight_committee majority presidents_party i.tenure if year<2019 & year>2006, cluster(ia_fixed)
xtreg perYear prestige chair oversight_committee majority presidents_party i.tenure if year <2019 & year>2006, fe cluster(ia_fixed)


##these are the spillover regressions.  the years are merely to focus on the years where we have more complete 
##data. 


clear 

use /users/jgrimmer/Dropbox/correspondence/data/DistrictLevel.dta


destring year, replace

egen sd_fixed = group(state_dist)

egen c_fixed  = group(chamber)

xtset sd_fixed 

eststo clear 

eststo: xtreg perYear mean_new i.year if year<2019 & year>2006 &  new_member==0 & c_fixed != 2, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "", replace

eststo: xtreg perYear state_new i.year if year<2019 & year>2006 &  new_member==0 & c_fixed!=2, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "", replace

eststo: xtreg perYear mean_new i.year if year<2019 & year>2006 & c_fixed==3 & new_member==0, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "\checkmark", replace

eststo: xtreg perYear state_new i.year if year<2019 & year>2006 & c_fixed==3 & new_member==0, fe cluster(sd_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local senate "\checkmark", replace

esttab using /Users/jgrimmer/Dropbox/correspondence/tables/TableSpill.tex,  nostar nogap nomtitles booktabs replace drop(_cons *.year) s(ia_fixed year_fixed senate N, labels("District Fixed Effects" "Year Fixed Effects" "Senators Only" "Observations")) label varlabels(mean_new "Proportion New Legislators" state_new "At Least One New Legislator") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at district level")





###alright, that provides a pretty clear picture of what legislators receive
###will add in the information about name recognition for legislators


##here is the name recognition plots

clear 
use /users/justingrimmer/Dropbox/correspondence/data/NameRec.dta

destring year, replace 

egen i_fixed = group(icpsr)


xtset i_fixed year 

eststo clear 

eststo: reg avg_rec first third fifth seventh, cluster(i_fixed)
estadd local legislator "", replace
estadd local year "", replace

eststo: xtreg avg_rec first third fifth seventh , fe cluster(i_fixed)
estadd local legislator "\checkmark", replace
estadd local year "\checkmark", replace

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TableNameRec_Tenure.tex,  nostar nogap nomtitles booktabs replace drop(_cons) s(legislator year N, labels("Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(first "Second Year" third "Fourth Year" fifth "Sixth Year" seventh "Eighth Year") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")


eststo clear 

eststo: reg avg_rec prestige chair ranking_minority oversight_committee majority presidents_party , cluster(i_fixed)
estadd local legislator "", replace
estadd local year "", replace

eststo: xtreg avg_rec prestige chair ranking_minority oversight_committee majority presidents_party, fe cluster(i_fixed)
estadd local legislator "\checkmark", replace
estadd local year "\checkmark", replace

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TableNameRec_Prestige.tex,  nostar nogap nomtitles booktabs replace drop(_cons) s(legislator year N, labels("Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(prestige "Prestige" chair "Chair" ranking_minority "Ranking Minority" oversight_committee "Oversight Committee" majority "Majority Party" presidents_party "President's Party") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")



eststo: reg avg_rec first third fifth seventh prestige chair oversight_committee majority presidents_party , cluster(i_fixed)
estadd local legislator "", replace
estadd local year "", replace

eststo: xtreg avg_rec first third fifth seventh prestige chair oversight_committee majority presidents_party, fe cluster(i_fixed)
estadd local legislator "\checkmark", replace
estadd local year "\checkmark", replace


esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TableNameRec.tex,  nostar nogap nomtitles booktabs replace drop(_cons) s(legislator year N, labels("Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(first "Second Year" third "Fourth Year" fifth "Sixth Year" seventh "Eighth Year" prestige "Prestige" chair "Chair" oversight_committee "Oversight Committee" majority "Majority Party" presidents_party "President's Party") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")


##finally we need to do a specific analysis for a place.  



##this is the specification for hte house and senate separately
clear
use ~/Dropbox/correspondence/data/HouseAgencyData.dta

destring year, replace 

egen ia_fixed = group(icpsr_agency)


xtset ia_fixed year 


xtreg perYear chair prestige oversight majority presidents_party i.tenure if year >2006 & year < 2018 , fe cluster(ia_fixed)

xtreg perYear first second third fourth fifth sixth  if year >2006 & year < 2018 , fe cluster(ia_fixed)



clear
use ~/Dropbox/correspondence/data/SenateAgencyData.dta

destring year, replace 

egen ia_fixed = group(icpsr_agency)


xtset ia_fixed year 


xtreg perYear chair prestige oversight majority presidents_party i.tenure if year >2006 & year < 2019 , fe cluster(ia_fixed)


xtreg perYear first second third fourth fifth sixth majority presidents_party if year >2006 & year < 2019 , fe cluster(ia_fixed)



##this is the table for the type of contacts

clear
use ~/Dropbox/correspondence/data/ProportionContact.dta
destring year, replace
egen icpsr_a = group(icpsr)
xtset icpsr_a year


eststo clear 	


eststo: reg ratio first second third fourth fifth sixth if year>2006 & year< 2018, cluster(icpsr_a)
estadd local leg "" , replace
estadd local year "", replace
estadd local maj "", replace 

eststo: xtreg ratio first second third fourth fifth sixth majority presidents_party if year>2006 & year< 2018, fe cluster(icpsr_a)
estadd local leg "\checkmark" , replace
estadd local year "\checkmark", replace
estadd local maj "\checkmark", replace 


esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TypeContact_tenure.tex,  nostar nogap nomtitles booktabs replace drop(majority presidents_party) s(maj leg year N, labels("Majority" "Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(first "First Year" second "Second Year" third "Third Year" fourth "Fourth Year" fifth "Fifth Year" sixth "Sixth Year" _cons "Intercept") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")

eststo clear 	


eststo: reg ratio prestige chair ranking_minority oversight_committee if year>2006 & year< 2018, cluster(icpsr_a)
estadd local leg "" , replace
estadd local year "", replace
estadd local maj "", replace 

eststo: xtreg ratio prestige chair ranking_minority oversight_committee majority presidents_party if year>2006 & year< 2018, fe cluster(icpsr_a)
estadd local leg "\checkmark" , replace
estadd local year "\checkmark", replace
estadd local maj "\checkmark", replace 

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TypeContact_prestige.tex,  nostar nogap nomtitles booktabs replace drop(majority presidents_party) s(maj leg year N, labels("Majority" "Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(prestige "Prestige" chair "Chair" ranking_minority "Ranking Minority" oversight_committee "Oversight Committee" _cons "Intercept") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")



esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TypeContact.tex,  nostar nogap nomtitles booktabs replace drop(majority presidents_party) s(maj leg year N, labels("Majority" "Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(first "First Year" second "Second Year" third "Third Year" fourth "Fourth Year" fifth "Fifth Year" sixth "Sixth Year" prestige "Prestige" chair "Chair" oversight_committee "Oversight Committee" _cons "Intercept") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")




##performing the analysis

clear 
use ~/Dropbox/correspondence/data/MergedVetVA.dta

gen prop_vet = veterans/total

eststo clear 

destring year, replace 

eststo: reg perYear prop_vet if year>2006 & year<2018, cluster(icpsr)



clear 
use ~/Dropbox/correspondence/data/MergedOldSSA.dta

gen prop_old = total_65_plus/total

destring year, replace

reg perYear prop_old if year>2006 & year<2018, cluster(icpsr)



##doing the per committee analysis

clear 

use ~/Dropbox/Correspondence/Data/AgencyWithComm.dta

destring year, replace

tostring year , gen(str_year)
gen year_agency = agency  + str_year 


egen ia_fixed = group(icpsr_agency)
egen ya_fixed = group(year_agency)


gen lpyear = log(perYear + 1)

xtset ia_fixed ya_fixed 

eststo clear 



##this is the list of standing committees.  

##do the descriptive differences and then the fixed effects

eststo: xtreg perYear agriculture first second third fourth fifth sixth majority presidents_party if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear appropriations first second third fourth fifth sixth majority presidents_party if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear armed_services first second third fourth fifth sixth majority presidents_party if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear budget first second third fourth fifth sixth majority presidents_party if year>2006 & year<2018, fe cluster(ia_fixed)


eststo: xtreg perYear education first second third fourth fifth sixth majority presidents_party if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear energy first second third fourth fifth sixth majority presidents_party if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear ethics first second third fourth fifth sixth majority presidents_party if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear financial_services first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear foreign first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear homeland_security first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)


esttab using /Users/justingrimmer/Dropbox/correspondence/tables/CommTables1.tex, nostar nogap nomtitles booktabs replace drop(majority presidents_party first second third fourth fifth sixth _cons) s(N, labels("Observations")) label se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator x agency level")

eststo clear 

eststo: xtreg perYear house_administration first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear judiciary first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear natural_resources first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear rules first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear science first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear small_business first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear transportation first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear veterans first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear ways first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear banking first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/CommTables2.tex, nostar nogap nomtitles booktabs replace drop(majority presidents_party first second third fourth fifth sixth _cons) s(N, labels("Observations")) label se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator x agency level")

eststo clear 

eststo: xtreg perYear commerce first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear environment first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear finance first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear foreign_relations first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

eststo: xtreg perYear health first second third fourth fifth sixth majority presidents_party  if year>2006 & year<2018, fe cluster(ia_fixed)

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/CommTables3.tex, nostar nogap nomtitles booktabs replace drop(majority presidents_party first second third fourth fifth sixth _cons) s(N, labels("Observations")) label se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator x agency level")




reg perYear energy veterans appropriations armed_services budget judiciary economic financial_services foreign oversight commerce homeland_security health finance education transportation intelligence rules library printing indian foreign_relations small_business taxation natural_resources ethics environment ways science standards_of_official_conduct house_administration resources international_relations national_security house_oversight


xtreg perYear energy veterans appropriations armed_services budget judiciary economic financial_services foreign oversight commerce homeland_security health finance education transportation intelligence rules library printing indian foreign_relations small_business taxation natural_resources ethics environment ways science standards_of_official_conduct house_administration resources international_relations national_security house_oversight majority presidents_party first second third fourth fifth sixth if year>2006 & year<2018, fe cluster(ia_fixed)


##the additional regression
chair ranking_minority prestige oversight_committee majority presidents_party first second third fourth fifth sixth


estto clear 

eststo: reg perYear chair_oversight chair_non_oversight ranking_minority prestige oversight_committee majority presidents_party first second third fourth fifth sixth if year >2006 & year < 2018,  cluster(ia_fixed)
estadd local ia_fixed "", replace
estadd local year_fixed "", replace
estadd local prestige "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local time "\checkmark", replace
estadd local DV "Count", replace 


eststo: xtreg perYear chair_oversight chair_non_oversight ranking_minority prestige oversight_committee majority presidents_party first second third fourth fifth sixth if year >2006 & year < 2018, fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local prestige "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local time "\checkmark", replace
estadd local DV "Count", replace 


eststo: reg lpyear chair_oversight chair_non_oversight ranking_minority prestige oversight_committee majority presidents_party first second third fourth fifth sixth if year >2006 & year < 2018,  cluster(ia_fixed)
estadd local ia_fixed "", replace
estadd local year_fixed "", replace
estadd local prestige "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local time "\checkmark", replace
estadd local DV "Log(Count + 1)", replace 


eststo: xtreg lpyear chair_oversight chair_non_oversight ranking_minority prestige oversight_committee majority presidents_party first second third fourth fifth sixth if year >2006 & year < 2018, fe cluster(ia_fixed)
estadd local ia_fixed "\checkmark", replace
estadd local year_fixed "\checkmark", replace
estadd local prestige "\checkmark", replace
estadd local maj "\checkmark", replace 
estadd local time "\checkmark", replace
estadd local DV "Log(Count + 1)", replace 

esttab using /Users/justingrimmer/Dropbox/correspondence/tables/DecompComm.tex,  nostar nogap nomtitles booktabs replace drop(_cons majority presidents_party ranking_minority prestige oversight_committee first second third fourth fifth sixth) s(ia_fixed year_fixed prestige maj time DV N, labels("Legislator $\times$ Agency Fixed Effects" "Year $\times$ Agency Fixed Effects" "Prestige" "Majority, President's Party" "Tenure" "Dependent Variable" "Observations")) label varlabels(chair_oversight "Committee Chair, Oversight Power" chair_non_oversight "Committee Chair, Non-Oversight Power") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at legislator x agency level")




##performing the ratio analysis


clear
use ~/Dropbox/correspondence/data/ProportionContact.dta
destring year, replace
egen icpsr_a = group(icpsr)
xtset icpsr_a year


eststo clear 	


eststo: reg ratio chair ranking_minority prestige oversight_committee first second third fourth fifth sixth majority presidents_party if year>2006 & year< 2018, cluster(icpsr_a)
estadd local leg "" , replace
estadd local year "", replace
estadd local maj "", replace 

eststo: xtreg ratio chair ranking_minority prestige oversight_committee first second third fourth fifth sixth majority presidents_party if year>2006 & year< 2018, fe cluster(icpsr_a)
estadd local leg "\checkmark" , replace
estadd local year "\checkmark", replace
estadd local maj "\checkmark", replace 


esttab using /Users/justingrimmer/Dropbox/correspondence/tables/TypeContact.tex,  nostar nogap nomtitles booktabs replace drop(majority presidents_party) s(maj leg year N, labels("Majority, President's Party" "Legislator Fixed Effects" "Year Fixed Effects" "Observations")) label varlabels(chair "Chair" ranking_minority "Ranking Minority" prestige "Prestige" oversight_committee "Oversight Committee" first "First Year" second "Second Year" third "Third Year" fourth "Fourth Year" fifth "Fifth Year" sixth "Sixth Year" _cons "Intercept") se sfmt(%9.0fc %5.2c) nonotes addnote ("Robust standard errors in parentheses, clustered at legislator level")







