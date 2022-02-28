library(dplyr)
library(ggplot2)
library(lfe)
library(readstata13)
library(foreign)
library(here)
#load('/Users/jgrimmer/Dropbox/correspondence/data/dcounts_min.Rdata')
load(here::here("data", "dcounts_min.Rdata"))

##creating the aggregate count variable

d<- dcounts_min %>% group_by(agency, icpsr, chamber, year) %>% summarise(perYear = sum(per_icpsr_chamber_year_agency_type))

d_sub<- dcounts_min %>% subset(TYPE %in% c(1, 2, 3)) %>%  group_by(agency, icpsr, chamber, year) %>% summarise(perYear_con = sum(per_icpsr_chamber_year_agency_type))
d_sub2<- dcounts_min %>% subset(TYPE %in% c(4, 5)) %>%  group_by(agency, icpsr, chamber, year) %>% summarise(perYear_pol = sum(per_icpsr_chamber_year_agency_type))

d$perYear_con<- d_sub$perYear_con
d$perYear_policy<- d_sub2$perYear_pol




##now loading the data on members 
#load('/Users/jgrimmer/Dropbox/correspondence/data/agency_vars.Rdata')
load(here::here("data", "agency_vars.Rdata"))

#load('/Users/jgrimmer/Dropbox/correspondence/data/members.Rdata')
load(here::here("data", "members.Rdata"))



congress_years<- function(congress){
		years<- c(congress*2 + 1787, congress*2 + 1788 )
		return(years)
}

year_congress<- function(year){
	return(floor((year - 1787)/2))
}


d$congress<- year_congress(d$year)

d<- left_join(d, members %>% select(congress, icpsr, party,party_code, state_abbrev, district_code, nominate.dim1, presidents_party, female, chair, ranking_minority, party_leader, party_whip, majority, prestige, prestige_chair, yearelected, state), by = c('congress', 'icpsr'))

#FIXME 
# nom<- read.delim('/Users/jgrimmer/Correspondence/data/nominate_data.csv', sep=',')
# nom<- read.delim(here::here("data", "nominate_data.csv"), sep=',')

load(here::here("data", "nom.Rdata"))

# nom <- members

nom2<- nom %>% group_by(icpsr) %>% summarise(first_cong = min(congress))

nom2$first_year <- 1787 + 2*nom2$first_cong

df<- left_join(d, nom2, by ='icpsr' )
df$tenure<- df$year - df$first_year
df$first<- ifelse(df$tenure==0, 1, 0)
df$second<- ifelse(df$tenure==1, 1, 0)
df$third<- ifelse(df$tenure==2, 1, 0)
df$fourth<- ifelse(df$tenure==3, 1, 0)
df$fifth<- ifelse(df$tenure==4, 1, 0)
df$sixth<- ifelse(df$tenure==5, 1, 0)

colnames(agency_vars)
df<- left_join(df, agency_vars %>% select(agency, icpsr, chamber, year, oversight_committee, oversight_committee_chair), by = c('agency', 'icpsr', 'year'))

df$icpsr_agency<- paste(df$agency, df$icpsr, sep='_')
df$agency_year<- paste(df$agency, df$year, sep='_')


df$tenure <- df$year - df$first_year


final_tenure<- df %>% group_by(icpsr) %>%  summarise(max_year = max(tenure))

df<- left_join(df, final_tenure , by = 'icpsr')
df$survive <- ifelse((df$chamber.x=='House' & df$max_year>1)| (df$chamber.x=='Senate' & df$max_year>5),1, 0 )
df$chamber <- df$chamber.x

##purging repeated values 
#FIXME Should we not just select one for each pair rather than dropping all
doubles<- df %>% group_by(icpsr, year, agency) %>% summarise(count = n())
doubles$remove<- ifelse(doubles$count>1, 1, 0)

doubles %>% filter(remove == 1) %>%
  distinct(icpsr, year)

df2<- left_join(df, doubles%>% select(icpsr, year, agency, remove) , by = c('icpsr', 'year', 'agency'))

df2<- df2 %>% subset(remove==0)

##purging repeated values 
doubles<- df2 %>% group_by(icpsr, year, agency) %>% summarise(count = n())
doubles$remove<- ifelse(doubles$count>1, 1, 0)

# Save level data for Stata
write.dta(df2, file='/users/jgrimmer/Dropbox/Correspondence/Data/AgencyComm.dta')
#FIXME 
#write.dta(df2, file= here::here('Data', 'AgencyComm.dta'))



##creating the ratio variable 

#FIXME create perYear_con and perYear_policy here

d_rat<- df %>% 
  group_by(year, icpsr, prestige, prestige_chair, chair, 
           ranking_minority, majority, presidents_party,
				first, second, third, fourth, fifth, sixth) %>% 
  summarise(perCon = sum(perYear_con), 
            perPol = sum(perYear_policy)) %>% 
  mutate(ratio = perCon/(perCon + perPol))

d_rat<- as.data.frame(d_rat)

doubles<- d_rat %>% group_by(year, icpsr) %>% summarise(count = n())
doubles$remove<- ifelse(doubles$count>1, 1, 0)

d_rat2<- left_join(d_rat, doubles, by = c('year','icpsr'))
d_rat2<-d_rat2 %>% subset(remove==0)

# Save ratio data for Stata
write.dta(d_rat2, file='/users/jgrimmer/Dropbox/Correspondence/Data/ProportionContact.dta')
#FIXME
#write.dta(d_rat2, file=here::here('data', 'ProportionContact.dta'))


####
decade<- case_when(df2$year < 2011 ~ '0', 
				   df2$year > 2010 ~ '1')

state_dist<- case_when(df2$chamber=='Senate'~ paste(df2$state,df2$district_code,  sep='_' ), 
						df2$chamber =='House'~ paste(paste(df2$state, df2$district_code, sep='_'), decade, sep='_'))




df2$state_dist<- state_dist

perDist<- df2 %>% group_by(year, state_dist, icpsr, chamber, tenure, state) %>% summarise(perYear = sum(perYear)) %>% distinct()

##alright, now we want to identify when there is a new member.  to do this, we look for changes in the icpsr number



perDist$new_member<- ifelse(perDist$tenure==0, 1, 0)

perDist$second_year<- ifelse(perDist$tenure==1, 1, 0)
perDist$third_year<- ifelse(perDist$tenure==2, 1, 0)
perDist$fourth_year<- ifelse(perDist$tenure==3, 1, 0)
perDist$fifth_year<- ifelse(perDist$tenure==4, 1, 0)
perDist$sixth_year<- ifelse(perDist$tenure==5, 1, 0)


state_level<- perDist %>% group_by(state, year) %>% summarise(numpers = n(), sumYear = sum(perYear), mean_new = mean(new_member)) 

state_new<- ifelse(state_level$mean_new>0, 1, 0)
state_level$state_new<- state_new

perDist<- left_join(perDist, select(state_level, state_new, mean_new, state, year), by = c('state', 'year'))

# Save per District per Year data for Stata
write.dta(perDist, '~/Dropbox/correspondence/data/DistrictLevel.dta')
# FIXME 
# write.dta(perDist, here::here('data', 'DistrictLevel.dta'))





