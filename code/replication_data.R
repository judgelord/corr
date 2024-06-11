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




# nom<- read.delim('/Users/jgrimmer/Correspondence/data/nominate_data.csv', sep=',')
load(here::here("data", "members.Rdata"))

nom2<- members %>% group_by(icpsr) %>% summarise(first_cong = min(congress))

nom2$first_year <- 1787 + 2*nom2$first_cong

df<- left_join(d, nom2, by ='icpsr' )# FIXME (THESE JOINS ARE DUPLICATING VARIABLES)

df$tenure<- df$year - df$first_year
df$first<- ifelse(df$tenure==0, 1, 0)
df$second<- ifelse(df$tenure==1, 1, 0)
df$third<- ifelse(df$tenure==2, 1, 0)
df$fourth<- ifelse(df$tenure==3, 1, 0)
df$fifth<- ifelse(df$tenure==4, 1, 0)
df$sixth<- ifelse(df$tenure==5, 1, 0)

colnames(agency_vars)

# FIXME (THESE JOINS ARE DUPLICATING VARIABLES)
df<- left_join(df, agency_vars %>% select(agency, icpsr, chamber, year, oversight_committee, oversight_committee_chair), by = c('agency', 'icpsr', 'year'))

df$icpsr_agency<- paste(df$agency, df$icpsr, sep='_')
df$agency_year<- paste(df$agency, df$year, sep='_')


df$tenure <- df$year - df$first_year


final_tenure<- df %>% group_by(icpsr) %>%  summarise(max_year = max(tenure))

df<- left_join(df, final_tenure , by = 'icpsr') # FIXME

df$survive <- ifelse((df$chamber.x=='House' & df$max_year>1)| (df$chamber.x=='Senate' & df$max_year>5),1, 0 )
df$chamber <- df$chamber.x

## count or repeated values (chamber and party switchers)
# note that in the full data, we use the dates of letters to attribute them to the proper party or chamber at the time
# but in yearly counts, we lose this level of detail, leading to undercounts for switchers
df %<>% 
  ungroup() %>% 
  distinct() %>% 
  add_count(icpsr, year, agency, name = "n") 

# inspect duplicates 
df %>% filter(n > 1) %>% select(icpsr, party, chamber, year, agency)
  
##purging repeated values 
df2<- df %>% filter(n == 1)


write.dta(df2, file= here::here('data/AgencyComm.dta'))

dcounts_tenure <- df2
save(dcounts_tenure, file = here::here("data", "dcounts_tenure.Rdata"))


##creating the ratio variable 
d_rat<- df %>% group_by(year, icpsr, prestige, prestige_chair, chair, ranking_minority, majority, presidents_party,
				first, second, third, fourth, fifth, sixth) %>% 
  summarise(perCon = sum(perYear_con), 
            perPol = sum(perYear_policy)) %>% 
  mutate(ratio = perCon/(perCon + perPol))

d_rat<- as.data.frame(d_rat)

doubles<- d_rat %>% group_by(year, icpsr) %>% summarise(count = n())
doubles$remove<- ifelse(doubles$count>1, 1, 0)

d_rat2<- left_join(d_rat, doubles, by = c('year','icpsr'))
d_rat2<-d_rat2 %>% subset(remove==0)
write.dta(d_rat2, here::here('data/ProportionContact.dta'))

dcounts_ratio <- d_rat2
save(dcounts_ratio, file = here::here("data", "dcounts_ratio.Rdata"))


####
decade<- case_when(
  df2$year < 2011 ~ '0', 
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


write.dta(perDist, here::here('data/DistrictLevel.dta'))


dcounts_per_district <- perDist
save(dcounts_per_district, file = here::here("data", "dcounts_per_district.Rdata"))




#### making the figures for the slides

library(readstata13)
d<- read.dta13(here::here('data/AgencyComm.dta'))
d2<- read.dta13(here::here('data/ProportionContact.dta'))

#FIXME replace names with these when it won't mess justin up
d <- dcounts_tenure %>% mutate(nominate_dim1 = nominate.dim1,
                               chamber_x = chamber)
d2 <- dcounts_ratio 

sub<- d %>% group_by(icpsr, year, nominate_dim1, party, 
                     chamber_x, 
                     chair, ranking_minority, prestige) %>% 
  summarise(avg_over = mean(perYear), 
																avg_con = mean(perYear_con), 
																avg_pol = mean(perYear_policy))

sub$party<- as.factor(sub$party)
sub2<- sub %>% subset(party!= '(I)' & chamber_x!= 'President')


sub2$color<- ifelse(sub2$party=='(D)', rgb(0,0,1, alpha = 0.05), rgb(1,0,0, alpha = 0.05))

g3<- sub2 %>% ggplot(aes(x = nominate_dim1, y = avg_con, col= party)) + 
	geom_point(col = sub2$color)+ geom_smooth(se = F) + 
		scale_color_manual(values=c('blue', 'red'))  + ylim(c(0, 1.75)) + xlab('First Dimension, Nominate') + 
		ylab('Average Number of Constituency Service Contacts Per Year') + facet_wrap(.~chamber_x)

# g3

ggsave(g3, file = here::here('figs/NominateEffort.pdf'), height = 6, width = 6)


store<- sub %>% group_by(chair, ranking_minority) %>% summarise(avg_over = mean(avg_over),
														avg_con = mean(avg_con), 
														avg_pol = mean(avg_pol)) %>% mutate(per_con = avg_con/(avg_pol + avg_con))
store2<- store[-nrow(store), ]
store2$label<- c('Members', 'Ranking Minority', 'Chair')
store3<- store2 %>% select(avg_over, label, per_con)
colnames(store3)<- c('Chair', 'Overall Number', 'Rank', 'Share Constituency')
store3<- store3[,c(3, 2, 4, 1)]
library(xtable)
xtable(store3[,-4]) ##




d$year_in<- d$tenure + 1 
tenure<- d %>% group_by(year_in) %>% summarise(avg_over = mean(perYear), 
									 avg_con = mean(perYear_con), 
									 avg_pol = mean(perYear_policy)) %>% mutate(per_con = avg_con/(avg_con + avg_pol))

g5<- tenure %>% ggplot(aes(x = year_in, y = avg_over)) + geom_line(lwd = 2) + 
	xlab('Years in Congress') + ylab('Average Number of Requests per Agency') + 
		xlim(c(1, 20 )) + ylim(c(0, 1.5)) + ggtitle("Average Requests by Year in Congress")

# g5

ggsave(g5, file='/Users/jgrimmer/Dropbox/correspondence/figs/TenureDifference.pdf', height = 6, width = 6)




tenure %>% ggplot(aes(x = year_in, y = per_con)) + geom_line() + 
	xlab('Tenure in Congress') + ylab('Percent Requests Constituency Service') + 
		xlim(c(1, 20 )) + ylim(c(0, 1))




d3<- left_join(d2, sub %>% select(year, icpsr, nominate_dim1, party, chamber_x), by = c('year', 'icpsr'))

d3_sub<- d3 %>% subset(party!= '(I)' & chamber_x != 'president')

d3_sub$color<- ifelse(d3_sub$party=='(D)', rgb(0,0,1, alpha = 0.05), rgb(1,0,0, alpha = 0.05))

d3_sub %>% ggplot(aes(x = nominate_dim1, y = ratio, col= party)) + facet_wrap(.~chamber_x) + 
	geom_point(col = d3_sub$color)+ geom_smooth(se = F) + scale_color_manual(values=c('blue', 'red')) 


use<- d %>% group_by(icpsr) %>% summarise(eventual_chair = max(chair))

d_test<- left_join(d, use, by = 'icpsr')

when_chair<- d_test %>%subset(eventual_chair==1) %>% mutate(diff = eventual_chair - chair)

icpsr_min_year<- when_chair %>% subset(diff==0 ) %>% group_by(icpsr) %>% summarise(min_year = min(year))


icpsr_min_year$placebo_year_1 <- icpsr_min_year$min_year - 1 
icpsr_min_year$placebo_year_2 <- icpsr_min_year$min_year - 2 
icpsr_min_year$placebo_year_3 <- icpsr_min_year$min_year - 3 



placebo_year3<- placebo_year2<- placebo_year<- rep(0, nrow(d))
for(z in 1:nrow(icpsr_min_year)){
	placebo_year[which(d$icpsr==icpsr_min_year$icpsr[z] & d$year ==icpsr_min_year$placebo_year_1[z])]<- 1
	placebo_year2[which(d$icpsr==icpsr_min_year$icpsr[z] & d$year ==icpsr_min_year$placebo_year_2[z])]<- 1
	placebo_year3[which(d$icpsr==icpsr_min_year$icpsr[z] & d$year ==icpsr_min_year$placebo_year_3[z])]<- 1}

d$placebo_year<- placebo_year
d$placebo_year2<- placebo_year2
d$placebo_year3<- placebo_year3


ert<- felm(perYear~chair + placebo_year + placebo_year2 + placebo_year3 + prestige + ranking_minority + presidents_party + ranking_minority |icpsr_agency + agency_year, data = d  )

ff<- d %>% group_by(icpsr, year )%>% summarise(total = sum(perYear)) %>% group_by(icpsr) %>% summarise(sds = sd(total))

mean(ff$sds)
ert<- d %>% group_by(icpsr, year) %>% summarise(total = sum(perYear)) 
sd(ert$total)





library(lfe)

temp<- felm(perYear~eventual_chair |year_agency, data = d_test %>% subset(chair==0) )

write.dta(d_test, here::here('data/PlaceboTest.dta'))

#TODO MAKE THIS DEPEND ON DATA 
share<- c(0.09,0.07, 0.65, 0.03, 0.16)
# /TODO 

type<- c('Constituent', 'Constituent', 'Constituent', 'Policy', 'Policy')
spec<- c('501c3/Gov.', 'Constituent\nCorporate', 'Individual', 'Corporate', 'General')

out<- cbind(share*100, type, spec)
out<- as.data.frame(out)
colnames(out)<- c('Share', 'Type', 'Helped')

out$Helped_use <- factor(out$Helped, levels =levels(out$Helped)[c(5, 1, 2, 4,3)] )

b1<- out %>% ggplot(aes(x = Helped_use, y = share,  fill = Type)) +
	 geom_bar(stat = 'identity') + ylab('Share of Correspondence') + xlab('') + 
	 scale_fill_manual(values = c('cornflowerblue', 'red')) + theme(text=element_text(size = 20))

b1

ggsave(b1, file = here::here('data/ContactType.pdf'), height = 6, width = 11)

