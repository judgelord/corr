###writing code for the paper analysis
library(dplyr)
library(ggplot2)


load("/Users/justingrimmer/Dropbox/Correspondence/data/all_contacts_paper.RData")
df <- all_contacts

###merging in the information with ICPSR numbers

nom<- read.delim('/Users/justingrimmer/Correspondence/data/nominate_data.csv', sep=',')


nom2<- nom %>% group_by(icpsr) %>% summarise(first_cong = min(congress))

nom2$first_year <- 1787 + 2*nom2$first_cong

df<- left_join(df, nom2, by ='icpsr' )


ert<- df %>% group_by(year, first_year, icpsr) %>% summarise(perYear = n() ) 

##

ert %>% group_by(first_year) %>% summarise(avg = mean(perYear)) %>% ggplot(aes(x = first_year, y = avg)) + geom_line()

##so what happens over the yeras?


df$tenure <- df$year - df$first_year


final_tenure<- df %>% group_by(icpsr) %>%  summarise(max_year = max(tenure))

df<- left_join(df, final_tenure , by = 'icpsr')


##creating the bar plot in the presentation by type


hist1<- df  %>% subset(is.na(Type)==F & Type!= 'To be coded') %>% ggplot(aes(Type)) + geom_bar() + xlab('Label') + ylab('Number') + theme(axis.text=element_text(size=14))
ggsave(hist1, file='~/Dropbox/correspondence/figs/TypeHistogram1.pdf', height = 6, width = 10)


hist2<- df  %>% subset(is.na(Type2)==F) %>% ggplot(aes(Type2)) + geom_bar() + xlab('Label') + ylab('Number')+ theme(axis.text=element_text(size=14))

ggsave(hist2, file='~/Dropbox/correspondence/figs/TypeHistogram2.pdf', height = 6, width = 10)



type = 'Corp. Constituent'
df  %>% subset(Type = type) %>% group_by(tenure, year, icpsr) %>% summarise(perYear = n()) %>% group_by(tenure) %>% summarise(avg = mean(perYear)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line()


##creating a per year data set
per_year<- df  %>% group_by(tenure, year, icpsr, agency) %>% summarise(perYear = n())


year_year<- lm(perYear~ as.factor(year) + as.factor(icpsr) + as.factor(agency), data= per_year)

per_year$resid<- year_year$residuals  

per_year %>% group_by(tenure) %>% summarise(avg = mean(resid)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line() + geom_point(pch =20) +  ylim(c(-3,3))


per_year %>%group_by(tenure) %>% summarise(avg = mean(resid)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line() + geom_point(pch =20) +  ylim(c(-3,3))



##building models looking at the tenure variable

ten_base<- lm(perYear~ as.factor(agency), data = per_year) 


per_year$agency_resid<- ten_base$residual

per_year %>% group_by(tenure) %>% summarise(avg = mean(agency_resid)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line() + geom_point() #+ xlim(c(0, 20)) + ylim(c(2, 6))


ten_icpsr<- lm(perYear~as.factor(tenure) + as.factor(icpsr), data = per_year)
ten_resid<- lm(perYear~as.factor(icpsr) , data = per_year) 

per_year$residual<- ten_resid$residual

per_year %>% group_by(tenure) %>% summarise(avg = mean(residual)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line() + geom_point(pch = 20)+ xlim(c(0, 20)) + ylim(c(-1, 1))



plot(start, type='b')

df$election_year<- ifelse(df$year %% 2== 0, 1, 0)
per_year$election_year<- ifelse(per_year$year %%2==0, 1, 0)



year_ten<- lm(perYear~as.factor(year) + as.factor(icpsr), data = per_year)


##looking at what happens to individuals who exit 
##to do this, we can examine the set of people whose final duration is that year (but not censored)


##find the max per year 


final_tenure<- per_year %>% group_by(icpsr) %>%  summarise(max_year = max(tenure))

per_year2<- left_join(per_year, final_tenure , by = 'icpsr')

per_year2$ten2<- ifelse(per_year2$max_year<2, 1, 0)
per_year2$ten4<- ifelse(per_year2$max_year<4, 1, 0)


lm(perYear~ten4, data = per_year2, subset=which(tenure>1 & tenure<4) )



per_year2 %>% subset(max_year ==tenure) %>% group_by(max_year) %>%  summarise(avg = mean(perYear)) %>% ggplot(aes(x = max_year, y = avg)) + geom_line() + geom_point()

per_year2 %>% subset(max_year !=tenure) %>% group_by(tenure) %>%  summarise(avg = mean(perYear)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line() + geom_point()



per_year2$elect<- ifelse(per_year2$year %% 2==0, 1, 0)

elect_year<- lm(perYear~year + elect + as.factor(agency) + as.factor(icpsr), data = per_year2)

##there is not much evidence of an election year/off year effect here once we take into account agency 
##types

##perhaps there is something by type of 

per_year<- df %>% group_by(icpsr, year, tenure, chamber) %>% summarise(perYear = n()) 

per_year %>% group_by(tenure) %>% summarise(avg = mean(perYear)) %>% ggplot(aes(x = tenure, y = avg))  + geom_line() + geom_point() + xlim(c(0, 20)) + ylim(c(0, 80))

per_year$year_0<- ifelse(per_year$tenure ==0, 1, 0)
per_year$year_1<- ifelse(per_year$tenure ==1, 1, 0)
per_year$year_2<- ifelse(per_year$tenure==2, 1, 0)
per_year$year_3<- ifelse(per_year$tenure==3, 1, 0)
per_year$year_4<- ifelse(per_year$tenure==4, 1, 0)
per_year$year_5<- ifelse(per_year$tenure==5, 1, 0)
per_year$year_6<- ifelse(per_year$tenure==6, 1, 0)
per_year$year_7<- ifelse(per_year$tenure==7, 1, 0)
per_year$year_8<- ifelse(per_year$tenure==8, 1, 0)
per_year$year_9<- ifelse(per_year$tenure==9, 1, 0)
per_year$year_10<- ifelse(per_year$tenure==10, 1, 0)
per_year$year_11<- ifelse(per_year$tenure==11, 1, 0)
per_year$year_12<- ifelse(per_year$tenure==12, 1, 0)
per_year$year_13<- ifelse(per_year$tenure ==13, 1, 0)
per_year$year_14<- ifelse(per_year$tenure ==14, 1, 0)
per_year$year_15<- ifelse(per_year$tenure==15, 1, 0)
per_year$year_16<- ifelse(per_year$tenure==16, 1, 0)
per_year$year_17<- ifelse(per_year$tenure==17, 1, 0)
per_year$year_18<- ifelse(per_year$tenure==18, 1, 0)
per_year$year_19<- ifelse(per_year$tenure==19, 1, 0)
per_year$year_20<- ifelse(per_year$tenure>=20, 1, 0)



resid_reg<- lm(perYear~as.factor(icpsr) + as.factor(year), data = per_year)

per_year$residual<- resid_reg$residual


resid_ten_plot<- per_year %>% group_by(tenure) %>% summarise(avg = mean(residual)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line() + 
    geom_point(pch=20) + xlim(c(0, 20)) + ylim(c(-20, 20)) + xlab('Tenure') + ylab("Residualized Number of Requests")
ggsave(resid_ten_plot, file='/Users/justingrimmer/Dropbox/Correspondence/figs/ResidTenPlot.pdf', height = 6, width = 6)


##alright, let's build up the simplest possible models
##we need to cluster the standard errors too


##so we want to know which years someone was around



##so for each year, we will grab unique agencies and unique icpsrs.  



df$un_name<- paste(paste(df$icpsr, df$year, sep='-'), df$agency, sep='-')
df$chair_non_oversight<- ifelse(df$oversight_committee_chair==0 & df$chair==1, 1, 0)


per_agency<- df %>% group_by(icpsr, year, agency, tenure,max_year,  un_name, female, chair, ranking_minority, party_leader, party_whip, prestige, prestige_chair, majority, presidents_party) %>% summarise(perYear = n()) 

##alright, creating the unique combinations of zeroes that we need to add to the data. 
##for each year, we identify the icpsr and the agencies.  we can then merge them.  

years<- seq(2000, 2019, by = 1)
store_years<- list()
a<- 0 
for(z in years){
  a<- a + 1
  subset<- which(df$year==z)
  icpsrs<- unique(df[subset,]$icpsr)
  agencies<- unique(df[subset,]$agency)
  temp_names<- c()
  combo<- len(icpsrs)*len(agencies)
  m<- 0 
  for(y in icpsrs){
    for(x in agencies){
      m<- m + 1
    temp_names[m]<- paste(paste(y, z, sep='-'), x, sep='-')
  }
}
  dd<- as.data.frame(temp_names)
  colnames(dd)<- 'un_name'
  store_years[[a]]<- left_join(dd, per_agency)  

}

new_agency<- matrix(NA, nrow = 1, ncol = ncol(per_agency))
colnames(new_agency)<- colnames(per_agency)

for(z in 1:len(store_years)){
  new_agency<- rbind(new_agency, store_years[[z]])


}


new_agency$perYear[which(is.na(new_agency$perYear))]<- 0 
new_agency2<- new_agency[-1,]

##alright now filling in theother information
parse<- function(x){
    ert<- strsplit(x, split='-')
}

new_names<- matrix(NA, nrow = nrow(new_agency2), ncol = 3)


for(z in 1:nrow(new_agency2)){
    ee<- parse(new_agency2$un_name[z])
    new_names[z,]<- c(ee[[1]])
    if(z%%1000==0){print(z)}
}


new_agency2[,1:3]<- new_names


new_agency2 %>% group_by(tenure) %>% summarise(means = mean(perYear)) %>% ggplot(aes(x = tenure, y = means)) + geom_point() + geom_line()

df$icpsr<- as.character(df$icpsr)
df$year<- as.character(df$year)


out<- df %>% group_by(icpsr, year) %>% summarize(chair = max(chair), ranking_minority = max(ranking_minority), 
        party_leader = max(party_leader), party_whip = max(party_whip), prestige = max(prestige), prestige_chair = max(prestige_chair), 
            majority = max(majority), presidents_party = max(presidents_party), max_year = max(max_year), tenure = mean(tenure))

ert<- left_join(new_agency2, out , by = c('icpsr', 'year'))

new_agency2[,4:5]<- cbind(ert$tenure.y, ert$max_year.y)
new_agency2[,8:15]<- ert[,17:24]
new_agency2$chair_non_oversight<- ert$chair_non_oversight
new_agency2$oversight_committee_chair<- ert$oversight_committee_chair


new_agency2$first<- ifelse(new_agency2$tenure==0, 1, 0)
new_agency2$second<- ifelse(new_agency2$tenure==1, 1, 0)
new_agency2$third<- ifelse(new_agency2$tenure==2, 1, 0)
new_agency2$fourth<- ifelse(new_agency2$tenure==3, 1, 0)
new_agency2$fifth<- ifelse(new_agency2$tenure==4, 1, 0)
new_agency2$sixth<- ifelse(new_agency2$tenure==5, 1, 0)
new_agency2$seven<- ifelse(new_agency2$tenure==6, 1, 0)
new_agency2$eight<- ifelse(new_agency2$tenure==7, 1, 0)
new_agency2$nine<- ifelse(new_agency2$tenure==8, 1, 0)
new_agency2$ten<- ifelse(new_agency2$tenure==9, 1, 0)
new_agency2$eleven<- ifelse(new_agency2$tenure==10, 1, 0)
new_agency2$twelve<- ifelse(new_agency2$tenure==11, 1, 0)
new_agency2$thirteen<- ifelse(new_agency2$tenure==12, 1, 0)
new_agency2$fourteen<- ifelse(new_agency2$tenure==13, 1, 0)
new_agency2$fifteen_plus<- ifelse(new_agency2$tenure>13, 1, 0)


new_agency2$icpsr_agency<- paste(new_agency2$icpsr, new_agency2$agency, sep='_')


new_agency2<- na.omit(new_agency2[,-7])
library(foreign)
write.dta(new_agency2, file = '~/Dropbox/correspondence/data/AgencyData.dta')





###now adding a second data set, restricting ourselves to the type of service


per_agency<- df %>%  subset(Type2=='Constituent Service') %>% group_by(icpsr, year, agency, tenure,max_year,  un_name, female, chair, ranking_minority, party_leader, party_whip, prestige, prestige_chair, oversight_committee, majority, presidents_party) %>% summarise(perYear = n()) 

##alright, creating the unique combinations of zeroes that we need to add to the data. 
##for each year, we identify the icpsr and the agencies.  we can then merge them.  

years<- seq(2000, 2019, by = 1)
store_years<- list()
a<- 0 
for(z in years){
  a<- a + 1
  subset<- which(df$year==z)
  icpsrs<- unique(df[subset,]$icpsr)
  agencies<- unique(df[subset,]$agency)
  temp_names<- c()
  combo<- len(icpsrs)*len(agencies)
  m<- 0 
  for(y in icpsrs){
    for(x in agencies){
      m<- m + 1
    temp_names[m]<- paste(paste(y, z, sep='-'), x, sep='-')
  }
}
  dd<- as.data.frame(temp_names)
  colnames(dd)<- 'un_name'
  store_years[[a]]<- left_join(dd, per_agency)  

}

new_agency<- matrix(NA, nrow = 1, ncol = ncol(per_agency))
colnames(new_agency)<- colnames(per_agency)

for(z in 1:len(store_years)){
  new_agency<- rbind(new_agency, store_years[[z]])


}


new_agency$perYear[which(is.na(new_agency$perYear))]<- 0 
new_agency2<- new_agency[-1,]

##alright now filling in theother information
parse<- function(x){
    ert<- strsplit(x, split='-')
}

new_names<- matrix(NA, nrow = nrow(new_agency2), ncol = 3)


for(z in 1:nrow(new_agency2)){
    ee<- parse(new_agency2$un_name[z])
    new_names[z,]<- c(ee[[1]])
    if(z%%1000==0){print(z)}
}


new_agency2[,1:3]<- new_names


new_agency2 %>% group_by(tenure) %>% summarise(means = mean(perYear)) %>% ggplot(aes(x = tenure, y = means)) + geom_point() + geom_line()

df$icpsr<- as.character(df$icpsr)
df$year<- as.character(df$year)

out<- df %>% group_by(icpsr, year) %>% summarize(chair = max(chair), ranking_minority = max(ranking_minority), 
        party_leader = max(party_leader), party_whip = max(party_whip), prestige = max(prestige), prestige_chair = max(prestige_chair), 
            oversight_committee = max(oversight_committee), majority = max(majority), presidents_party = max(presidents_party), max_year = max(max_year), tenure = mean(tenure))

ert<- left_join(new_agency2, out , by = c('icpsr', 'year'))

new_agency2[,4:5]<- cbind(ert$tenure.y, ert$max_year.y)
new_agency2[,8:16]<- ert[,18:26]



new_agency2$first<- ifelse(new_agency2$tenure==0, 1, 0)
new_agency2$second<- ifelse(new_agency2$tenure==1, 1, 0)
new_agency2$third<- ifelse(new_agency2$tenure==2, 1, 0)
new_agency2$fourth<- ifelse(new_agency2$tenure==3, 1, 0)
new_agency2$fifth<- ifelse(new_agency2$tenure==4, 1, 0)
new_agency2$sixth<- ifelse(new_agency2$tenure==5, 1, 0)
new_agency2$seven<- ifelse(new_agency2$tenure==6, 1, 0)
new_agency2$eight<- ifelse(new_agency2$tenure==7, 1, 0)
new_agency2$nine<- ifelse(new_agency2$tenure==8, 1, 0)
new_agency2$ten<- ifelse(new_agency2$tenure==9, 1, 0)
new_agency2$eleven<- ifelse(new_agency2$tenure==10, 1, 0)
new_agency2$twelve<- ifelse(new_agency2$tenure==11, 1, 0)
new_agency2$thirteen<- ifelse(new_agency2$tenure==12, 1, 0)
new_agency2$fourteen<- ifelse(new_agency2$tenure==13, 1, 0)
new_agency2$fifteen_plus<- ifelse(new_agency2$tenure>13, 1, 0)


new_agency2$icpsr_agency<- paste(new_agency2$icpsr, new_agency2$agency, sep='_')

library(foreign)
new_agency2<- na.omit(new_agency2[,-7])
write.dta(new_agency2, file = '~/Dropbox/correspondence/data/AgencyData_ConService.dta')

###now for just policy data

per_agency<- df %>%  subset(Type2=='Policy') %>% group_by(icpsr, year, agency, tenure,max_year,  un_name, female, chair, ranking_minority, party_leader, party_whip, prestige, prestige_chair, oversight_committee, majority, presidents_party) %>% summarise(perYear = n()) 

##alright, creating the unique combinations of zeroes that we need to add to the data. 
##for each year, we identify the icpsr and the agencies.  we can then merge them.  

years<- seq(2000, 2019, by = 1)
store_years<- list()
a<- 0 
for(z in years){
  a<- a + 1
  subset<- which(df$year==z)
  icpsrs<- unique(df[subset,]$icpsr)
  agencies<- unique(df[subset,]$agency)
  temp_names<- c()
  combo<- len(icpsrs)*len(agencies)
  m<- 0 
  for(y in icpsrs){
    for(x in agencies){
      m<- m + 1
    temp_names[m]<- paste(paste(y, z, sep='-'), x, sep='-')
  }
}
  dd<- as.data.frame(temp_names)
  colnames(dd)<- 'un_name'
  store_years[[a]]<- left_join(dd, per_agency)  

}

new_agency<- matrix(NA, nrow = 1, ncol = ncol(per_agency))
colnames(new_agency)<- colnames(per_agency)

for(z in 1:len(store_years)){
  new_agency<- rbind(new_agency, store_years[[z]])


}


new_agency$perYear[which(is.na(new_agency$perYear))]<- 0 
new_agency2<- new_agency[-1,]

##alright now filling in theother information
parse<- function(x){
    ert<- strsplit(x, split='-')
}

new_names<- matrix(NA, nrow = nrow(new_agency2), ncol = 3)


for(z in 1:nrow(new_agency2)){
    ee<- parse(new_agency2$un_name[z])
    new_names[z,]<- c(ee[[1]])
    if(z%%1000==0){print(z)}
}


new_agency2[,1:3]<- new_names


new_agency2 %>% group_by(tenure) %>% summarise(means = mean(perYear)) %>% ggplot(aes(x = tenure, y = means)) + geom_point() + geom_line()

df$icpsr<- as.character(df$icpsr)
df$year<- as.character(df$year)

out<- df %>% group_by(icpsr, year) %>% summarize(chair = max(chair), ranking_minority = max(ranking_minority), 
        party_leader = max(party_leader), party_whip = max(party_whip), prestige = max(prestige), prestige_chair = max(prestige_chair), 
            oversight_committee = max(oversight_committee), majority = max(majority), presidents_party = max(presidents_party), max_year = max(max_year), tenure = mean(tenure))

ert<- left_join(new_agency2, out , by = c('icpsr', 'year'))

new_agency2[,4:5]<- cbind(ert$tenure.y, ert$max_year.y)
new_agency2[,8:16]<- ert[,18:26]



new_agency2$first<- ifelse(new_agency2$tenure==0, 1, 0)
new_agency2$second<- ifelse(new_agency2$tenure==1, 1, 0)
new_agency2$third<- ifelse(new_agency2$tenure==2, 1, 0)
new_agency2$fourth<- ifelse(new_agency2$tenure==3, 1, 0)
new_agency2$fifth<- ifelse(new_agency2$tenure==4, 1, 0)
new_agency2$sixth<- ifelse(new_agency2$tenure==5, 1, 0)
new_agency2$seven<- ifelse(new_agency2$tenure==6, 1, 0)
new_agency2$eight<- ifelse(new_agency2$tenure==7, 1, 0)
new_agency2$nine<- ifelse(new_agency2$tenure==8, 1, 0)
new_agency2$ten<- ifelse(new_agency2$tenure==9, 1, 0)
new_agency2$eleven<- ifelse(new_agency2$tenure==10, 1, 0)
new_agency2$twelve<- ifelse(new_agency2$tenure==11, 1, 0)
new_agency2$thirteen<- ifelse(new_agency2$tenure==12, 1, 0)
new_agency2$fourteen<- ifelse(new_agency2$tenure==13, 1, 0)
new_agency2$fifteen_plus<- ifelse(new_agency2$tenure>13, 1, 0)


new_agency2$icpsr_agency<- paste(new_agency2$icpsr, new_agency2$agency, sep='_')

##focusing on complete cases
new_agency2<- na.omit(new_agency2[,-7])

library(foreign)
write.dta(new_agency2, file = '~/Dropbox/correspondence/data/AgencyData_Policy.dta')



##same thing with rolled up data as well.  


##alright, descriptive findings first.  


model1<- lm(perYear~first + second + third + fourth + fifth + six_ten + eleven_twenty + twenty_plus-1, data = new_agency2 )
##so that shows that individuals in their first year behave differently than other individuals. Of course, there is some selection


##so let's create the distribution by year
##insert here

##now building the model where we analyze the results with icpsr, year, and agency fixed effects.  excluding the long serving members as the refernece category


model2<- lm(perYear~first + second + third + fourth + fifth + six_ten + as.factor(icpsr) + as.factor(agency) + as.factor(year) , data = new_agency2 )

summary(model2)$coef[1:10,]

##now subsetting to people who have a max tenure greater than 2

model3<- lm(perYear~first + second + third + fourth + fifth + six_ten + as.factor(icpsr) + as.factor(agency) + as.factor(year) , data = new_agency2, subset=which(max_year>2) )

summary(model3)$coef[1:10,]

##we can also create icpsr x agency fixed effects 


model4<- lm(perYear~first + second + third + fourth + fifth + six_ten + eleven_twenty  as.factor(icpsr_agency) + as.factor(year) , data = new_agency2, subset=which(max_year>2) )




##switching to plm
library(plm)

##including first, with ICPSR and year fixed effects

OSRES



test<- pdata.frame(new_agency2, index = c('icpsr_agency', 'year'))

plm1<- plm(perYear~first + second + third + fourth + fifth  , model = 'within', data = test, subset=which(tenure<11))
plm1_full<- plm(perYear~first + second + third + fourth + fifth + six_ten + eleven_twenty  , model = 'within', data = test)

library(lmtest)
store<- coeftest(plm1, vcov = function(x) vcovHC(x, cluster = 'group', type = 'HC1'))
store_full<- coeftest(plm1_full, vcov = function(x) vcovHC(x, cluster = 'group', type = 'HC1'))

vcov_plm1<- vcovHC(plm1, vcov = function(x) vcovHC(x, cluster = 'group', type = 'HC1'))
##we can then do the comparison across the coefficients, which yields the differences of interest

plm2<- plm(perYear~first + second + third + fourth + fifth , model = 'within', data = test, subset=which(tenure<11 & max_year > 2))

library(lmtest)
store<- coeftest(plm2, vcov = function(x) vcovHC(x, cluster = 'group', type = 'HC1'))
vcov_plm2<- vcovHC(plm2)


###

plm_vanilla<- plm(perYear~first + second + third + fourth + fifth , model = 'pooling', data = test, subset=which(tenure<11 ))
store<- coeftest(plm_vanilla, vcov = function(x) vcovHC(x, cluster = 'group', type = 'HC1'))



##alright, now doing the same specification with tenure
##that there is a drop that is then corrected in the second year
plm_tenure<- plm(perYear ~ tenure + I(tenure^2)  + I(tenure^3)   , model = 'within', data = test, subset=which(tenure<11)) 
store<- coeftest(plm_tenure, vcov = function(x) vcovHC(x, cluster = 'group', type = 'HC1'))

##so we then want to be sure that this is not just people who are leae







ert<- lm(perYear~first + second + third + fourth  +fifth +six_ten + eleven_twenty+   as.factor(icpsr) + as.factor(agency) + as.factor(year), data = new_agency2)


ert2<- lm(perYear~tenure + I(tenure^2) + I(tenure^3) + as.factor(icpsr) + as.factor(agency) + as.factor(year), data = new_agency2)

library(mgcv)
ert3<- gam(perYear~s(tenure) + as.factor(icpsr) + as.factor(agency) + as.factor(year), data = new_agency2)



##that is increasing, but very model dependent 
base_model<- lm(perYear~tenure , data = per_year)
library(mgcv)
per_year$tenure<- as.numeric(per_year$tenure)
base_model2<- gam(perYear~s(tenure) + as.factor(icpsr) + as.factor(year), data = per_year)


##susetting to avoid the very large numbers later in the sample.  
base_model3<- gam(perYear~s(tenure) + as.factor(icpsr) + as.factor(year), data = per_year, subset=which(tenure<30))


##let's do this with first year, and

first_model<- lm(perYear~year_0, data = per_year)
first_second_model<- lm(perYear~year_0 + year_1, data = per_year)
fst_model<- lm(perYear~year_0 + year_1 + year_2 + year_3 + year_4 + year_5, data = per_year)
fst_model_20<- lm(perYear~year_0 + year_1 + year_2 + year_3 + year_4 + year_5 + year_6 + year_7 + year_8+ year_9 + year_10 +year_11 + year_12 + year_13 + year_14 + year_15 + year_16 + year_17 + year_18 + year_19 + year_20-1, data= per_year)
##now adding year and icpsr fixed effects

fst_fixed<- lm(perYear~year_0 + year_1 + year_2 + year_3 + year_4 + year_5 + as.factor(icpsr) + as.factor(year), data = per_year )
fst_fixed_10<- lm(perYear~year_0 + year_1 + year_2 + year_3 + year_4 + year_5 + year_6 + year_7 + year_8+ year_9 + year_10+ as.factor(icpsr) + as.factor(year), data = per_year )
fst_fixed_20<- lm(perYear~year_0 + year_1 + year_2 + year_3 + year_4 + year_5 + year_6 + year_7 + year_8+ year_9 + year_10 +year_11 + year_12 + year_13 + year_14 + year_15 + year_16 + year_17 + year_18 + year_19 
        + as.factor(icpsr) + as.factor(year), data = per_year, subset = which(tenure<30) )

##alright, that looks good, we can demonstrate what happens more generally  



fst_model<- lm(perYear~year_0 + year_1 + year_2 + year_3 + year_4 + year_5 + year_6 + year_7 + year_8 + as.factor(icpsr) + as.factor(year), data = per_year, subset=which(tenure<10))



base_model<- lm(perYear~as.factor(tenure) , data = per_year)

##we can now estimate the model with icpsr and year fixed effects

fixed_eff<- lm(perYear~as.factor(tenure) + as.factor(icpsr) + as.factor(year), data = per_year)






##examining what happens when someone takes over a district from someone else
##to do this, we'll look at district transitions
##so we want to create a row for each district
state_dist<- paste(df$state,df$district_code,  sep='_' )

##we want to link the senate seats (or), do this senator by senator. 
##







##how much does this vary by year

df$state_dist<- state_dist

perDist<- df %>% group_by(year, state_dist, icpsr, chamber, tenure, state) %>% summarise(perYear=n()) %>% distinct()

##alright, now we want to identify when there is a new member.  to do this, we look for changes in the icpsr number



perDist$new_member<- ifelse(perDist$tenure==0, 1, 0)

perDist$second_year<- ifelse(perDist$tenure==1, 1, 0)
perDist$third_year<- ifelse(perDist$tenure==2, 1, 0)
perDist$fourth_year<- ifelse(perDist$tenure==3, 1, 0)
perDist$fifth_year<- ifelse(perDist$tenure==4, 1, 0)
perDist$sixth_year<- ifelse(perDist$tenure==5, 1, 0)


##do we have a new member in my state? 
##and does that affect my output? 

##first, we can examine what happens when there is a new member in a state.  among the old members, is there an increase?
##compared to the year before?


######

###doing the regressions with both chambers

##adding in the zeros here.  




both_base<- lm(perYear~new_member + second_year + third_year + as.factor(chamber) , data = perDist)

both_diff<- lm(perYear~new_member + second_year + third_year + as.factor(state_dist) + as.factor(year) , data = perDist )




state_level<- perDist %>% group_by(state, year) %>% summarise(numpers = n(), sumYear = sum(perYear), mean_new = mean(new_member)) 

state_new<- ifelse(state_level$mean_new>0, 1, 0)
state_level$state_new<- state_new

perDist<- left_join(perDist, select(state_level, state_new, mean_new, state, year), by = c('state', 'year'))


write.dta(perDist, '~/Dropbox/correspondence/data/DistrictLevel.dta')



spill_base_sen<- lm(perYear~ state_new + as.factor(state_dist) + as.factor(year), data = perDist, subset = which(chamber =='Senate' & new_member==0))

spill_inten_sen<- lm(perYear~mean_new + as.factor(state_dist) + as.factor(year), data = perDist, subset = which(chamber =='Senate' & new_member==0))

##let's do this overall
spill_base<- lm(perYear~ state_new + as.factor(icpsr) + as.factor(year), data = perDist, subset = which(new_member==0))

spill_inten<- lm(perYear~mean_new + as.factor(icpsr) + as.factor(year), data = perDist, subset = which(new_member==0))





##so the question is what, happens to a states total number of requests when there are new members
state_total<- perDist %>% group_by(state, year) %>% summarise(perYear = sum(perYear) , avg_new = mean(mean_new))

state_total$any_new<- ifelse(state_total$avg_new>0, 1, 0)


##evidence that the total number of requests goes down each year, particularly as a larger share of the 
##members are new.  
total_requests<- lm(perYear~any_new  + as.factor(state)  + as.factor(year) , data = state_total)
total_inten<- lm(perYear~avg_new  + as.factor(state)  + as.factor(year) , data = state_total)


write.dta(state_total, '~/Dropbox/correspondence/data/StateLevel.dta')


###collecting our information about the tenure differences 
##that is information that suggests that this isn't necessarily a demand issue.  We don't surges when other 
##individuals produce fewer constituent service requests

##What else can we do, we can examine when there is a change and there is a party change, with the idea being
##that a party change implies that there will be more staff turnover

df$gop<- ifelse(df$party=='(R)', 1, 0)
df$dem<- ifelse(df$party=='(D)', 1, 0)

perDist<- df %>% group_by(year, state_dist, icpsr, chamber, tenure, state, dem, gop) %>% summarise(perYear=n()) %>% distinct()
perDist$new_member<- ifelse(perDist$tenure==0, 1, 0)
##alright, now, let's see what happens when there is a new member and the parties shift

perH<- perDist %>% subset(chamber=='House')
party_shift<- rep(0, nrow(perH))

inds<- which(perH$new_member==1)
for(z in inds){
  row<- perH[z,]
  if(row$year > 2007){
  use<- which(perH$state_dist==row$state_dist & perH$year==(row$year - 1))
  if(len(use)>0){
  party_shift[z]<- ifelse(perH[use, ]$dem==row$dem, 1, 0)[1]}
}

}

perH$party_shift<- party_shift


base_effect<- lm(perYear~party_shift + new_member, data = perH)

diff_party<- lm(perYear~ party_shift + new_member + as.factor(state_dist) + as.factor(year), data = perH)


##same thing for the senate

perS<-  perDist %>% subset(chamber == 'Senate')
party_shift<- rep(0, nrow(perS))

inds<- which(perS$new_member==1)


##ok delaware has two new members, both are still democrats
##florida is a party shift, i'll hand code it 
##24 is WV and not a party change
##new jersey has two new senators and one new party shift
##47 is alabama, not a party shift
##51 is minnesota, not a party shift

for(z in inds[-c(11, 12, 13, 16, 17, 24, 36,37, 47,48 ,51)]){
  row<- perS[z,]
  if(row$year>2007){
    use<- which(perS$state_dist==row$state_dist & perS$year==(row$year -1))
    use2<- which(perS$state_dist==row$state_dist & perS$year == row$year)
    party_shift[z]<- ifelse(perS[use[which(!(perS[use,]$icpsr %in% perS[use2,]$icpsr))],]$dem==row$dem, 1, 0)


  }

}

party_shift[224]<- 1
party_shift[249]<- 1 ##this is florida
party_shift[250]<- 1 ##massachusetts
party_shift[684]<- 1 ##new jersey
party_shift[1029]<- 1 ##
  


##so we see negatives, but not big negatives
base_diff<- lm(perYear~party_shift + new_member, data = perS)
diff_diff<- lm(perYear~party_shift + new_member + as.factor(state) + as.factor(year) , data = perS)


##the general view seems to be that as people move into office, they learn and then once their done learning they can
##provide more constituent service requests.  So this is about a resource shock---when individuals get more resources
##and create those resources, they are able to provide more constituent service requests.  

##evidence that legislators who are in office after two years are well equipped to provide constituency service
##and some evidence that as resources increase, the 


##first, there are differences in types and those types are generall better able to address constituents



perDist_con<- df %>%subset(Type2=='Constituent Service') %>% group_by(year, state_dist, icpsr, chamber, tenure, state) %>% summarise(perYear=n()) %>% distinct()
perDist_pol<- df %>% subset(Type2=='Policy') %>% group_by(year, state_dist, icpsr, chamber, tenure, state) %>% summarise(perYear=n()) %>% distinct()
##we see it here too, that there is a difference by constituent service and policy. 









plot(rr$coef[2:10])


df  %>% group_by(tenure, year, icpsr) %>% summarise(perYear = n()) %>% group_by(tenure) %>% summarise(avg = mean(perYear)) %>% ggplot(aes(x = tenure, y = avg)) + geom_line()



##that is the career path, we can also examine how 


all <- df %>% filter(year < 2018) # %>% filter(congress != 115)
p2 <- filter(all, Type2 == "Policy") 
c2 <- filter(all, Type2 == "Constituent Service")


# prestige committee
Overall <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = all %>%
                 group_by(agency, year, congress, icpsr, prestige) %>%
                 summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige")  %>% mutate(model = "Overall")

Policy <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = p2 %>%
                 group_by(agency, year, congress, icpsr, prestige) %>%
                 summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "Policy")

Constituent <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = c2 %>%
                 group_by(agency, year, congress, icpsr, prestige) %>%
                 summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "Constituent")

prestige <- rbind(Overall, Policy, Constituent)



data <- df %>% 
  group_by(member_state, year, chamber) %>% 
  summarise(perYear = n()) %>% ungroup() %>%
  group_by(chamber) %>% 
  summarise(perType = round(mean(perYear))) %>% ungroup() %>% 
  mutate(Type = factor("Total", levels = c(levels(df$Type), "Total")))





###loading in the data from the cces in order to analyze the name recognition data.  

cc<- readRDS('/Users/justingrimmer/Dropbox/Correspondence/data/cces_cumulative.Rds')


cc$house_rec<- ifelse(cc$approval_rep=='Never Heard / Not Sure', 0, 1)

cc$senate_rec_1<- ifelse(cc$approval_sen1=='Never Heard / Not Sure', 0, 1)
cc$senate_rec_2<- ifelse(cc$approval_sen2=='Never Heard / Not Sure', 0, 1)


##now, we want to group this by year and name

house_rec<- cc %>% subset(year %in% seq(2006, 2018, by = 2)) %>% group_by(rep_icpsr, year) %>% summarise(avg_rec = mean(house_rec, na.rm=T))

sen1_rec<- cc %>% subset(year %in% seq(2006, 2018, by = 2)) %>% group_by(sen1_icpsr, year) %>% summarise(avg_rec = mean(senate_rec_1, na.rm=T))

sen2_rec<- cc %>% subset(year %in% seq(2006, 2018, by = 2)) %>% group_by(sen2_icpsr, year) %>% summarise(avg_rec = mean(senate_rec_2, na.rm=T))


##alright, generating the data for the house and then for the senate
colnames(house_rec)[1]<- 'icpsr'
house_rec$year<- as.character(house_rec$year)
house_rec$icpsr<- as.character(house_rec$icpsr)

use_sub<- df %>% subset(year %in% seq(2006, 2018, by = 2) )%>% dplyr::select(year, icpsr, tenure, prestige, oversight_committee, ranking_minority, prestige_chair, chair, majority, presidents_party) %>% group_by(icpsr, year) %>% summarise(count = n(), tenure =max(tenure), prestige = max(prestige), oversight_committee = max(oversight_committee) , prestige_chair = max(prestige_chair), chair = max(chair), ranking_minority = max(ranking_minority), majority = max(majority), presidents_party = max(presidents_party))


use_sub$year<- as.character(use_sub$year) 
use_sub$icpsr<- as.character(use_sub$icpsr) 


house_join<- left_join(house_rec, use_sub, by = c('year', 'icpsr')) 



colnames(sen2_rec)[1]<- colnames(sen1_rec)[1]<- 'icpsr'

sen_rec<- matrix(NA, nrow = 357 + 356, ncol = 3)
sen_rec <- as.data.frame(sen_rec)
sen_rec[1:357,]<- sen1_rec
sen_rec[358:(357 + 356), ]<- sen2_rec

colnames(sen_rec)<- colnames(sen1_rec)

sen_rec$year<- as.character(sen_rec$year)
sen_rec$icpsr<- as.character(sen_rec$icpsr)

sen_join<- left_join(sen_rec, use_sub, by = c('year', 'icpsr'))

sen_join$senate<- 1
house_join$senate<- 0 

sen_join %>% group_by(tenure) %>% summarise(avg = mean(avg_rec, na.rm=T)) %>% ggplot(aes(x = tenure, y = avg))  + geom_point() + geom_line() + xlim(c(0, 20)) + ylim(c(0.5 ,1)) 
house_join %>% group_by(tenure) %>% summarise(avg = mean(avg_rec, na.rm=T)) %>% ggplot(aes(x = tenure, y = avg))  + geom_point() + geom_line() + xlim(c(0, 20)) + ylim(c(0.5 ,1)) 

model1<- lm(avg_rec~as.factor(tenure) + as.factor(icpsr) + as.factor(year) , data = house_join)


model2<- lm(avg_rec~ prestige + oversight_committee + prestige_chair + chair + as.factor(icpsr) + as.factor(year) , data = house_join)

##so this suggests that there is some learning happening, though a very small amount and slowly over time. 
##there is no effect of a more prestigious committee assignemnt

name_rec<- matrix(NA, nrow = 3037 + 713, ncol = 13 )
name_rec<- as.data.frame(name_rec)

name_rec[1:3037,]<- house_join
name_rec[3038:(3037 + 713), ]<- sen_join

colnames(name_rec)<- colnames(house_join)



name_rec<- na.omit(name_rec)


name_rec$first<- ifelse(name_rec$tenure==1, 1, 0)
name_rec$third<- ifelse(name_rec$tenure==3, 1, 0)
name_rec$fifth<- ifelse(name_rec$tenure==5, 1, 0)
name_rec$seventh<- ifelse(name_rec$tenure==7, 1, 0)

name_rec$log_count<- log(name_rec$count + 1)

write.dta(name_rec, file = '~/Dropbox/correspondence/data/NameRec.dta')


house_gg<- name_rec %>% subset(senate==0) %>% ggplot(aes(x=avg_rec, y = log_count)) +
geom_point(cex = 1, pch =20) + geom_smooth(method = 'lm') + ylab('Log(Count + 1)') + 
 xlab('Proportion Recognized') 
senate_gg<- name_rec %>% subset(senate==1) %>% ggplot(aes(x=avg_rec, y = log_count)) + geom_point(cex = 1, pch =20) + geom_smooth(method = 'lm') + 
  ylab('Log(Count + 1)') + xlab('Proportion Recognized') 

ggsave(house_gg, file='~/Dropbox/Correspondence/Figs/HouseRecFig.pdf', height = 6, width = 6)
ggsave(senate_gg, file='~/Dropbox/Correspondence/Figs/SenateRecFig.pdf', height = 6, width = 6)




###cleaning data to make the analysis of the places with a lot of veterans. 

df1<- read.delim('/Users/justingrimmer/Dropbox/correspondence/data/ACS_Download/2009_Data.csv', sep=',')
df2<- read.delim('/Users/justingrimmer/Dropbox/correspondence/data/ACS_Download/2017_Data.csv', sep=',')

##columns we want
##HC01_EST_VC01 ##total civilian population estimates
##HC03_EST_VC01 ##total number of veterans among civilians
##HC04_EST_VC01 ##percent veterans


###HC01_EST_VC17 ##total estimated number of people 65-74
##HC02_EST_VC17 ##percent people 65-74

##HC01_EST_VC18 ##total people 75+ 
##HC02_EST_VC18 ##percent people 75+


##we need two tables for the 2009 data

##from the age table we need
##HC01_EST_VC01 ## Total population, overall. 
##HC01_EST_VC16 ## Total population 65-69
##HC01_EST_VC17 ## Total population 70-74
##HC01_EST_VC18 ## Total population 75-79
##HC01_EST_VC19 ## Total population 80-84
##HC01_EST_VC20 ## Total population 85 and over


use1<- df1 %>% select(GEO.display.label, HC01_EST_VC01, HC02_EST_VC01)

use2<- df2 %>% select(GEO.display.label, HC01_EST_VC01, HC03_EST_VC01, HC04_EST_VC01, HC01_EST_VC17, HC02_EST_VC17, HC01_EST_VC18,HC02_EST_VC18)

df3<- read.delim('/Users/justingrimmer/Dropbox/correspondence/data/ACS_Download/2009_Data_Age.csv', sep=',')


use3<- df3 %>% select(GEO.display.label, HC01_EST_VC01, HC01_EST_VC16, HC01_EST_VC17, HC01_EST_VC18, HC01_EST_VC19, HC01_EST_VC20)

##writing a function  to parse the label
library(stringr)
parse_label<- function(x){
    out<- grep('Congressional District', x)
    if(len(out)==1){
    match1<- regexpr('Congressional District [0-9]+|Congressional District \\(at Large\\)', x)
    content<- regmatches(as.character(x), match1)
    dist_num<- regmatches(as.character(x), regexpr('[0-9]+|\\(at Large\\)', x))
    dist_num<- ifelse(dist_num=='(at Large)', '1', dist_num)
    state<- gsub("^ ", '', strsplit(as.character(x), split = ',')[[1]][2])
    output<- paste(tolower(state), dist_num, sep='_')
    return(output)
  }
    if(len(out)==0){
      output<- NA
      return(output)
    }
}


use1_ids<- c()
for(z in 2:nrow(use1)){

  use1_ids[z]<- parse_label(use1[z,1])

}

use2_ids<- c()
for(z in 2:nrow(use2)){

  use2_ids[z]<- parse_label(use2[z,1])

}

use3_ids<- c()
for(z in 2:nrow(use3)){

  use3_ids[z]<- parse_label(use3[z,1])

}

##
##now arranging the data to join back to the original data.  

##this is the 2006-2009 data, veterans
use1_num<- use1[-1,]
use1_num[,2]<- as.numeric(as.character(use1_num[,2]))
use1_num[,3]<- as.numeric(as.character(use1_num[,3]))


##this is the 2006-2009 data, population

use3_num<- use3[-1,]
use3_num[,2]<- as.numeric(as.character(use3_num[,2]))
use3_num[,3]<- round((as.numeric(as.character(use3_num[,3]))/100)*use3_num[,2])
use3_num[,4]<- round((as.numeric(as.character(use3_num[,4]))/100)*use3_num[,2])
use3_num[,5]<- round((as.numeric(as.character(use3_num[,5]))/100)*use3_num[,2])
use3_num[,6]<- round((as.numeric(as.character(use3_num[,6]))/100)*use3_num[,2])
use3_num[,7]<- round((as.numeric(as.character(use3_num[,7]))/100)*use3_num[,2])

##this is the 2010-2019 data, population + veterans

use2_num<- use2[-1,]

use2_num[,2]<- as.numeric(as.character(use2_num[,2]))
use2_num[,3]<- as.numeric(as.character(use2_num[,3]))
use2_num[,4]<- as.numeric(as.character(use2_num[,4]))
use2_num[,5]<- as.numeric(as.character(use2_num[,5]))
use2_num[,6]<- as.numeric(as.character(use2_num[,6]))
use2_num[,7]<- as.numeric(as.character(use2_num[,7]))


colnames(use1_num)<- c('state_dist', 'total', 'veterans')
colnames(use2_num)<- c('state_dist', 'total', 'veterans', 'percent_veterans', 'total_65_74', 'percent_65_74', 'total_75', 'percent_75')
colnames(use3_num)<- c('state_dist', 'total', 'total_65_69', 'total_70_74', 'total_75_79', 'total_80_84', 'total_85')


use1_num[,1]<- use1_ids[-1]
use2_num[,1]<- use2_ids[-1]
use3_num[,1]<- use3_ids[-1]


use1_num$percent_veterans<- use1_num[,3]/use1_num[,2]
use2_num$total_65_plus<- use2_num$total_65_74 + use2_num$total_75
use2_num$percent_veterans<- use2_num$veterans/use2_num$total
use3_num$total_65_plus<- use3_num$total_65_69+ use3_num$total_70_74 + use3_num$total_75_79 + use3_num$total_80_84 + use3_num$total_85


##be careful with totals, because the total is not commensurable

##alright, now we're going to load the appropriate data.  


for(z in 1:nrow(use1_num)){
  use1_num$state[z]<- strsplit(use1_num[z,1], split='_')[[1]][1]
  use2_num$state[z]<- strsplit(use2_num[z,1], split='_')[[1]][1]
  use3_num$state[z]<- strsplit(use3_num[z,1], split='_')[[1]][1]

}


state1<- use1_num %>% group_by(state) %>% summarise(total = sum(total), veterans = sum(veterans))
state2<- use2_num %>% group_by(state) %>% summarise(total = sum(total), veterans = sum(veterans), total_65_plus = sum(total_65_plus))
state3<- use3_num %>% group_by(state) %>% summarise(total = sum(total), total_65_plus = sum(total_65_plus))

state1$state_dist<- rep(NA, nrow(state1))
state2$state_dist<- rep(NA, nrow(state2))
state3$state_dist<- rep(NA, nrow(state3))


for(z in 1:nrow(state1)){
  state1$state_dist[z]<- paste(state1[z,1], '0', sep='_')
  state2$state_dist[z]<- paste(state2[z,1], '0', sep='_')
  state3$state_dist[z]<- paste(state3[z,1], '0', sep='_')
}


use1_sub<- use1_num %>% select(state_dist, total, veterans)
state1_sub<-  state1 %>% select(state_dist, total, veterans)

use3_sub<- use3_num %>% select(total_65_plus)
state3_sub<- state3 %>% select(total_65_plus)


total_65_plus<- c(unlist(use3_sub[,1]), unlist(state3_sub[,1]))

use1_sub<- cbind(rbind(use1_sub, state1_sub), total_65_plus)

use2_sub<- use2_num %>% select(state_dist, total, veterans, total_65_plus)

state2_sub<- state2 %>% select(state_dist, total, veterans, total_65_plus)

use2_sub<- rbind(use2_sub, state2_sub)

new_agency2<- read.dta('~/Dropbox/correspondence/data/AgencyData.dta')


###first for VA
va<- new_agency2 %>% subset(agency=='VA')

va<- left_join(va, df %>% select(icpsr, year, state_dist) %>% distinct(), by = c('icpsr', 'year'))

va_early<- va %>% subset(year %in% c(2008, 2009, 2010)) 
va_late<- va %>% subset(year > 2010)


va_early<- left_join(va_early, use1_sub, by= 'state_dist')

va_late<- left_join(va_late, use2_sub, by = 'state_dist')

va<- rbind(va_early, va_late)

senate<- rep(0, nrow(va))
senate[grep('_0', va$state_dist)]<- 1

va$senate<- senate

write.dta(va, file='~/Dropbox/correspondence/data/MergedVetVA.dta')


ssa<- new_agency2 %>% subset(agency=='SSA')

ssa<- left_join(ssa, df %>% select(icpsr, year, state_dist) %>% distinct(), by = c('icpsr', 'year'))

ssa_early<- ssa %>% subset(year %in% c(2008, 2009, 2010)) 
ssa_late<- ssa %>% subset(year > 2010)


ssa_early<- left_join(ssa_early, use1_sub, by= 'state_dist')

ssa_late<- left_join(ssa_late, use2_sub, by = 'state_dist')

ssa<- rbind(ssa_early, ssa_late)

senate<- rep(0, nrow(ssa))
senate[grep('_0', ssa$state_dist)]<- 1

ssa$senate<- senate

write.dta(ssa, file='~/Dropbox/correspondence/data/MergedOldSSA.dta')

ssa<- read.dta('~/Dropbox/correspondence/data/MergedOldSSA.dta')
va<- read.dta('~/Dropbox/correspondence/data/MergedVetVA.dta')




lm(perYear~I(veterans/total) + first + second, data = va )

lm(perYear~I(total_65_plus/total) + first + second + third + fourth + fifth + sixth + as.factor(icpsr), data = ssa)



ssa<- read.dta('~/Dropbox/correspondence/data/MergedOldSSA.dta')
va<- read.dta('~/Dropbox/correspondence/data/MergedVetVA.dta')


parse2<- function(x){
  ee<- ifelse(strsplit(x, split ='_')[[1]][2]=='0', 1, 0)
  return(ee)
}


ssa$senate<- rep(NA, len=nrow(ssa))
for(z in 1:nrow(ssa)){
  ssa$senate[z]<- parse2(ssa$state_dist[z])
}

va$senate<- rep(NA, len = nrow(va))

for(z in 1:nrow(va)){
  va$senate[z]<- parse2(va$state_dist[z])
}

ssa$per_old<- ssa$total_65_plus/ssa$total
va$per_old<- va$veterans/va$total



##the most contacted agency is the VA. 
##let's look at how this varies with veterans





###creating the per-chamber data

per_agency<- df %>% group_by(icpsr, year, agency, tenure,max_year,  un_name, female, chair, ranking_minority, party_leader, party_whip, prestige, prestige_chair, oversight_committee, majority, presidents_party, chamber) %>% summarise(perYear = n()) 

##alright, creating the unique combinations of zeroes that we need to add to the data. 
##for each year, we identify the icpsr and the agencies.  we can then merge them.  

years<- seq(2000, 2019, by = 1)
store_years<- list()
a<- 0 
for(z in years){
  a<- a + 1
  subset<- which(df$year==z)
  icpsrs<- unique(df[subset,]$icpsr)
  agencies<- unique(df[subset,]$agency)
  temp_names<- c()
  combo<- len(icpsrs)*len(agencies)
  m<- 0 
  for(y in icpsrs){
    for(x in agencies){
      m<- m + 1
    temp_names[m]<- paste(paste(y, z, sep='-'), x, sep='-')
  }
}
  dd<- as.data.frame(temp_names)
  colnames(dd)<- 'un_name'
  store_years[[a]]<- left_join(dd, per_agency)  

}

new_agency<- matrix(NA, nrow = 1, ncol = ncol(per_agency))
colnames(new_agency)<- colnames(per_agency)

for(z in 1:len(store_years)){
  new_agency<- rbind(new_agency, store_years[[z]])


}


new_agency$perYear[which(is.na(new_agency$perYear))]<- 0 
new_agency2<- new_agency[-1,]

##alright now filling in theother information
parse<- function(x){
    ert<- strsplit(x, split='-')
}

new_names<- matrix(NA, nrow = nrow(new_agency2), ncol = 3)


for(z in 1:nrow(new_agency2)){
    ee<- parse(new_agency2$un_name[z])
    new_names[z,]<- c(ee[[1]])
    if(z%%1000==0){print(z)}
}


new_agency2[,1:3]<- new_names



df$icpsr<- as.character(df$icpsr)
df$year<- as.character(df$year)

out<- df %>% group_by(icpsr, year, agency) %>% summarize(chair = max(chair), ranking_minority = max(ranking_minority), 
        party_leader = max(party_leader), party_whip = max(party_whip), prestige = max(prestige), prestige_chair = max(prestige_chair), 
            oversight_committee = max(oversight_committee), majority = max(majority), presidents_party = max(presidents_party), max_year = max(max_year), tenure = mean(tenure), chamber = first(chamber))

ert<- left_join(new_agency2, out , by = c('icpsr', 'year', 'agency'))

new_agency2[,4:5]<- cbind(ert$tenure.y, ert$max_year.y)
new_agency2[,8:17]<- ert[,c(19:27, 30)]



new_agency2$first<- ifelse(new_agency2$tenure==0, 1, 0)
new_agency2$second<- ifelse(new_agency2$tenure==1, 1, 0)
new_agency2$third<- ifelse(new_agency2$tenure==2, 1, 0)
new_agency2$fourth<- ifelse(new_agency2$tenure==3, 1, 0)
new_agency2$fifth<- ifelse(new_agency2$tenure==4, 1, 0)
new_agency2$sixth<- ifelse(new_agency2$tenure==5, 1, 0)
new_agency2$seven<- ifelse(new_agency2$tenure==6, 1, 0)
new_agency2$eight<- ifelse(new_agency2$tenure==7, 1, 0)
new_agency2$nine<- ifelse(new_agency2$tenure==8, 1, 0)
new_agency2$ten<- ifelse(new_agency2$tenure==9, 1, 0)
new_agency2$eleven<- ifelse(new_agency2$tenure==10, 1, 0)
new_agency2$twelve<- ifelse(new_agency2$tenure==11, 1, 0)
new_agency2$thirteen<- ifelse(new_agency2$tenure==12, 1, 0)
new_agency2$fourteen<- ifelse(new_agency2$tenure==13, 1, 0)
new_agency2$fifteen_plus<- ifelse(new_agency2$tenure>13, 1, 0)


new_agency2$icpsr_agency<- paste(new_agency2$icpsr, new_agency2$agency, sep='_')


new_agency2<- na.omit(new_agency2[,-7])

house_age<- new_agency2 %>% subset(chamber=='House')

senate_age<- new_agency2 %>% subset(chamber=='Senate')

ert<- table(house_age$un_name)

house_age<- house_age[-which(house_age$un_name %in% names(ert[which(ert==2)]))[seq(2, 26, by = 2)],]

library(foreign)
write.dta(house_age, file = '~/Dropbox/correspondence/data/HouseAgencyData.dta')

ert<- table(senate_age$un_name)
senate_age<- senate_age[-which(senate_age$un_name %in% names(ert[which(ert==2)]))[seq(2, 10, by = 2)],]

write.dta(senate_age, file = '~/Dropbox/correspondence/data/SenateAgencyData.dta')


####let's group together and examine how proportion of types across various individuals

type_year<- df %>% subset(Type2 %in% c('Policy', 'Constituent Service') )%>% group_by(icpsr, year, Type2) %>% summarize(counts = n() ) 
##now we want to group this so we now the count for each year, to do that let's get the total count per f

total_count<- df %>% group_by(icpsr, year) %>% summarise(count = n() )
type_policy<- df %>% subset(Type2 %in% c('Policy') )%>% group_by(icpsr, year) %>% summarize(counts = n() ) 
type_con<- df %>% subset(Type2 %in% c('Constituent Service') )%>% group_by(icpsr, year) %>% summarize(counts = n() ) 



total_count<- left_join(total_count, type_con, by = c('icpsr', 'year'))
total_count<- left_join(total_count, type_policy, by = c('icpsr', 'year'))

ert<- df %>% group_by(icpsr, year) %>% summarise(tenure = max(tenure), max_year = max(max_year), oversight_committee = max(oversight_committee), 
                                            chair = max(chair), majority = max(majority), presidents_party = max(presidents_party), prestige = max(prestige), ranking_minority = max(ranking_minority))


total_count<- left_join(total_count, ert, by = c('icpsr', 'year'))
colnames(total_count)[4:5]<- c('Constituent', 'Policy')

total_count[which(is.na(total_count[,4])==T),4]<- 0
total_count[which(is.na(total_count[,5])==T),5]<- 0

total_count$ratio<- total_count$Constituent/(total_count$Constituent + total_count$Policy)



lm(ratio~prestige + chair + oversight_committee, data = total_count)


total_count$first<- ifelse(total_count$tenure==0, 1, 0)
total_count$second<- ifelse(total_count$tenure==1, 1, 0)
total_count$third<- ifelse(total_count$tenure==2, 1, 0)
total_count$fourth<- ifelse(total_count$tenure==3, 1, 0)
total_count$fifth<- ifelse(total_count$tenure==4, 1, 0)
total_count$sixth<- ifelse(total_count$tenure==5, 1, 0)
total_count$seven<- ifelse(total_count$tenure==6, 1, 0)
total_count$eight<- ifelse(total_count$tenure==7, 1, 0)
total_count$nine<- ifelse(total_count$tenure==8, 1, 0)
total_count$ten<- ifelse(total_count$tenure==9, 1, 0)
total_count$eleven<- ifelse(total_count$tenure==10, 1, 0)
total_count$twelve<- ifelse(total_count$tenure==11, 1, 0)
total_count$thirteen<- ifelse(total_count$tenure==12, 1, 0)
total_count$fourteen<- ifelse(total_count$tenure==13, 1, 0)


write.dta(total_count, file = '~/Dropbox/correspondence/data/ProportionContact.dta')



###putting together the 


###putting together the year by committee stuff
###creating an indicator for every observation

comm_split<- list()

for(z in 1:nrow(df)){
  comm_split[[z]]<- strsplit(df$committees[z], split='\\|')[[1]]

}

ert<- unlist(comm_split)

un_comm<- unique(ert)

un_comm<- un_comm[-which(un_comm %in% c('MAJORITY WHIP', 'MINORITY LEADER', 'MAJORITY LEADER', 'MINORITY WHIP', 'DEFICIT REDUCTION', 'BENGHAZI', 'SPEAKER', 'ASSISTANT MINORITY LEADER', 'VOTING IRREGULARITIES OF AUGUST 2', NA))]


comm_zero<- matrix(0, nrow = nrow(df), ncol =len(un_comm))
for(z in 1:nrow(comm_zero)){
  comm_zero[z,which(un_comm %in% comm_split[[z]])]<- 1
}


colnames(comm_zero)<- un_comm
library(foreign)
library(readstata13)
agent<- read.dta13('~/Dropbox/correspondence/data/AgencyData.dta')

colnames(agent)

comm_agent<- matrix(0, nrow = nrow(agent), ncol = len(un_comm))
colnames(comm_agent)<- un_comm


sy<- cbind(df$icpsr, df$year)

asy<- cbind(agent$icpsr, agent$year)

temp<- cbind(df$year, df$icpsr, comm_zero)
temp<- as.data.frame(temp)
colnames(temp)<- c('year', 'icpsr', un_comm)

temp2<- temp %>% group_by(icpsr, year) %>% select("ENERGY":"LABOR") %>% summarise_all( max)

temp2$icpsr<- as.character(temp2$icpsr)


agent<- left_join(agent, temp2 , by =c('icpsr', 'year'))


un_comm2<- tolower(un_comm)

un_comm3<- gsub(' ', '_', un_comm2)
colnames(agent)[35:77]<- un_comm3


write.dta(agent, file='~/Dropbox/Correspondence/Data/AgencyWithComm.dta')


###writing the code to do the heterogeneous effect.  First residualizing the data

library(plm)

d<- plm.data(agent, indexes = c('ia_fixed', 'year'))
d$year<- as.numeric(as.character(d$year))
d$use<- ifelse(d$year>2006 & d$year <2018, 1, 0)

agent_use<- agent[d$use==1,]
save(agent_use, file='~/Dropbox/correspondence/data/AgentCommitteeData.RData')
###doing the analysis across all the committees

library(glmnet)

###looking at the heterogeneous effects of being chair

chair<- model.matrix(~chair + as.factor(agency) + 
    as.factor(agency):chair + as.factor(icpsr) + 
      as.factor(year) ,  data = d[which(d$use==1),])

chair.reg<- cv.glmnet(x = chair, y = log(agent_use$perYear + 1), alpha = 1)

chair_out<- list()
chair_out[[1]]<- chair
chair_out[[2]]<- chair.reg

save(chair_out, file='~/Dropbox/correspondence/data/ChairRegression.RData') 



#####agriculture
load('~/Dropbox (Personal)/correspondence/data/AgentCommitteeData.RData')
library(glmnet) 
library(dplyr)



agric<- model.matrix(~agriculture + as.factor(agency) + 
  as.factor(agency):agriculture + as.factor(icpsr) + as.factor(year)  ,  data = agent_use  )

agric.reg<- cv.glmnet(x = agric, y = log(agent_use$perYear + 1), alpha = 1)

agric_list<- list()
agric_list[[1]]<- agric
agric_list[[2]]<- agric.reg

save(agric_list, file='~/Dropbox/correspondence/data/HetEffects/Agriculture.RData')

#####appropriations
approp<- model.matrix(~appropriations + as.factor(agency) + 
  as.factor(agency):appropriations + as.factor(icpsr) + as.factor(year)  ,  data = agent_use  )

approp.reg<- cv.glmnet(x = approp, y = log(agent_use$perYear + 1), alpha = 1)

approp_list<- list()
approp_list[[1]]<- approp
approp_list[[2]]<- approp.reg

save(approp_list, file='~/Dropbox/correspondence/data/HetEffects/Appropriations.RData')


#####armed_services
armed<- model.matrix(~armed_services + as.factor(agency) + 
  as.factor(agency):armed_services + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

armed.reg<- cv.glmnet(x = armed, y = log(agent_use$perYear + 1), alpha = 1)



armed_list<- list()
armed_list[[1]]<- armed
armed_list[[2]]<- armed.reg

save(armed_list, file='~/Dropbox/correspondence/data/HetEffects/ArmedServices.RData')

####budget
budget<- model.matrix(~budget + as.factor(agency) + 
  as.factor(agency):budget + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

budget.reg<- cv.glmnet(x = budget, y = log(agent_use$perYear + 1), alpha = 1)

budget_list<- list()
budget_list[[1]]<- budget
budget_list[[2]]<- budget.reg


save(budget_list, file='~/Dropbox/correspondence/data/HetEffects/Budget.RData')


####education
education<- model.matrix(~education + as.factor(agency) + 
  as.factor(agency):education + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

education.reg<- cv.glmnet(x = education, y = log(agent_use$perYear + 1), alpha = 1)

education_list<- list()
education_list[[1]]<- education
education_list[[2]]<- education.reg


save(education_list, file='~/Dropbox/correspondence/data/HetEffects/Education.RData')

####energy
energy<- model.matrix(~energy + as.factor(agency) + 
  as.factor(agency):energy + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

energy.reg<- cv.glmnet(x = energy, y = log(agent_use$perYear + 1), alpha = 1)


energy_list<- list()
energy_list[[1]]<- energy
energy_list[[2]]<- energy.reg


save(energy_list, file='~/Dropbox/correspondence/data/HetEffects/Energy.RData')


####ethics
ethics<- model.matrix(~ethics + as.factor(agency) + 
  as.factor(agency):ethics + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

ethics.reg<- cv.glmnet(x = ethics, y = log(agent_use$perYear + 1), alpha = 1)

ethics_list<- list()
ethics_list[[1]]<- ethics
ethics_list[[2]]<- ethics.reg



save(ethics_list, file='~/Dropbox/correspondence/data/HetEffects/Ethics.RData')


####finance
finance<- model.matrix(~financial_services + as.factor(agency) + 
  as.factor(agency):financial_services + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

finance.reg<- cv.glmnet(x = finance, y = log(agent_use$perYear + 1), alpha = 1)

finance_list<- list()
finance_list[[1]]<- finance
finance_list[[2]]<- finance.reg


save(finance_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Finance.RData')
rm(finance_list)
rm(finance)
rm(finance.reg)


####foreign
foreign<- model.matrix(~foreign + as.factor(agency) + 
  as.factor(agency):foreign + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

foreign.reg<- cv.glmnet(x = foreign, y = log(agent_use$perYear + 1), alpha = 1)


foreign_list<- list()
foreign_list[[1]]<- foreign
foreign_list[[2]]<- foreign.reg

save(foreign_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Foreign.RData')
rm(foreign_list)
rm(foreign)
rm(foreign.reg)


####homeland_security
homeland_security<- model.matrix(~homeland_security + as.factor(agency) + 
  as.factor(agency):homeland_security + as.factor(icpsr) + as.factor(year)  ,  data = agent_use)

homeland_security.reg<- cv.glmnet(x = homeland_security, y = log(agent_use$perYear + 1), alpha = 1)


homeland_list<- list()
homeland_list[[1]]<- homeland_security
homeland_list[[2]]<- homeland_security.reg

save(homeland_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Homeland.RData')
rm(homeland_list)
rm(homeland)
rm(homeland.reg)

####house admin
house_admin<- model.matrix(~house_administration + as.factor(agency) + 
  as.factor(agency):house_administration + as.factor(icpsr) + as.factor(year)  ,  data = agent_use)

house_admin.reg<- cv.glmnet(x = house_administration, y = log(agent_use$perYear + 1), alpha = 1)

house_admin_list<- list()
house_admin_list[[1]]<- house_admin
house_admin_list[[2]]<- house_admin.reg

save(house_admin_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/House_Admin.RData')
rm(house_admin_list)
rm(house_admin)
rm(house_admin.reg)


####judiciary
judiciary<- model.matrix(~judiciary + as.factor(agency) + 
  as.factor(agency):judiciary + as.factor(icpsr) + as.factor(year)  ,  data =agent_use )

judiciary.reg<- cv.glmnet(x = judiciary, y = log(agent_use$perYear + 1), alpha = 1)


judiciary_list<- list()
judiciary_list[[1]]<- judiciary
judiciary_list[[2]]<- judiciary.reg


save(judiciary_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Judiciary.RData')
rm(judiciary_list)
rm(judiciary)
rm(judiciary.reg)


####nat
nat_res<- model.matrix(~natural_resources + as.factor(agency) + 
  as.factor(agency):natural_resources + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

nat_res.reg<- cv.glmnet(x = nat_res, y = log(agent_use$perYear + 1), alpha = 1)

nat_res_list<- list()
nat_res_list[[1]]<- nat_res
nat_res_list[[2]]<- nat_res.reg

save(nat_res_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/NatRes.RData')
rm(nat_res_list)
rm(nat_res)
rm(nat_res.reg)

##rules

rules<- model.matrix(~rules + as.factor(agency) + 
  as.factor(agency):rules + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

rules.reg<- cv.glmnet(x = rules, y = log(agent_use$perYear + 1), alpha = 1)

rules_list<- list()
rules_list[[1]]<- rules
rules_list[[2]]<- rules.reg

save(rules_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Rules.RData')
rm(rules_list)
rm(rules)
rm(rules.reg)


##science

science<- model.matrix(~science + as.factor(agency) + 
  as.factor(agency):science + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

science.reg<- cv.glmnet(x = science, y = log(agent_use$perYear + 1), alpha = 1)

science_list<- list()
science_list[[1]]<- science
science_list[[2]]<- science.reg

save(science_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/science_list.RData')
rm(science_list)
rm(science)
rm(science.reg)

##small_business

small_business<- model.matrix(~small_business + as.factor(agency) + 
  as.factor(agency):small_business + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

small_business.reg<- cv.glmnet(x = small_business, y = log(agent_use$perYear + 1), alpha = 1)


small_business_list<- list()
small_business_list[[1]]<- small_business
small_business_list[[2]]<- small_business.reg

save(small_business_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/SmallBusiness.RData')
rm(small_business_list)
rm(small_business)
rm(small_business.reg)

##transportation

transportation<- model.matrix(~transportation + as.factor(agency) + 
  as.factor(agency):transportation + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

transportation.reg<- cv.glmnet(x = transportation, y = log(agent_use$perYear + 1), alpha = 1)


transportation_list<- list()
transportation_list[[1]]<-transportation
transportation_list[[2]]<-transportation.reg

save(transportation_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Transportation.RData')
rm(transportation_list)
rm(transportation)
rm(transportation.reg)


##veterans

veterans<- model.matrix(~veterans + as.factor(agency) + 
  as.factor(agency):veterans + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

veterans.reg<- cv.glmnet(x = veterans, y = log(agent_use$perYear + 1), alpha = 1)

veterans_list<- list()
veterans_list[[1]]<-veterans
veterans_list[[2]]<-veterans.reg

save(veterans_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Veterans.RData')
rm(veterans_list)
rm(veterans)
rm(veterans.reg)

##ways

ways<- model.matrix(~ways + as.factor(agency) + 
  as.factor(agency):ways + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

ways.reg<- cv.glmnet(x = ways, y = log(agent_use$perYear + 1), alpha = 1)

ways_list<- list()
ways_list[[1]]<-ways
ways_list[[2]]<-ways.reg

save(ways_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Ways.RData')
rm(ways_list)
rm(ways)
rm(ways.reg)


##banking

banking<- model.matrix(~banking + as.factor(agency) + 
  as.factor(agency):banking + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

banking.reg<- cv.glmnet(x = banking, y = log(agent_use$perYear + 1), alpha = 1)

banking_list<- list()
banking_list[[1]]<-banking
banking_list[[2]]<-banking.reg

save(banking_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Banking.RData')
rm(banking_list)
rm(banking)
rm(banking.reg)

##commmerce

commerce<- model.matrix(~commerce + as.factor(agency) + 
  as.factor(agency):commerce + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

commerce.reg<- cv.glmnet(x = commerce, y = log(agent_use$perYear + 1), alpha = 1)

commerce_list<- list()
commerce_list[[1]]<-commerce
commerce_list[[2]]<-commerce.reg

save(commerce_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Commerce.RData')
rm(commerce_list)
rm(commerce)
rm(commerce.reg)


##environment

environment<- model.matrix(~environment + as.factor(agency) + 
  as.factor(agency):environment + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

environment.reg<- cv.glmnet(x = environment, y = log(agent_use$perYear + 1), alpha = 1)


environment_list<- list()
environment_list[[1]]<-environment
environment_list[[2]]<-environment.reg

save(enviroment_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Enviroment.RData')
rm(environment_list)
rm(environment)
rm(environment.reg)


##foreign

foreign<- model.matrix(~foreign_relations + as.factor(agency) + 
  as.factor(agency):foreign_relations + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

foreign.reg<- cv.glmnet(x = foreign, y = log(agent_use$perYear + 1), alpha = 1)

foreign_list<- list()
foreign_list[[1]]<-foreign
foreign_list[[2]]<-foreign.reg

save(foreign_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/ForeignRelations.RData')

rm(foreign_list)
rm(foreign)
rm(foreign.reg)

##health

health<- model.matrix(~health + as.factor(agency) + 
  as.factor(agency):health + as.factor(icpsr) + as.factor(year)  ,  data = agent_use )

health.reg<- cv.glmnet(x = health, y = log(agent_use$perYear + 1), alpha = 1)

health_list<- list()
health_list[[1]]<-health
health_list[[2]]<-health.reg

save(health_list, file='~/Dropbox (Personal)/correspondence/data/HetEffects/Health.RData')
rm(health_list)
rm(health)
rm(health.reg)


###this code quickly analyzes the results of the heterogeneous effect models

het_effects<- function(object){
  output<- object[[2]]
  number<- coef(output, s = output$lambda.min)
  names<- rownames(number)[1063:1144]
  clean <- strsplit(names, split='\\)')
  clean_store<- rep(NA, len(clean))
  for(z in 1:len(clean_store)){
    clean_store[z]<- clean[[z]][2]
  }
  final_out<- c(number[3,1], number[1063:1144,1] + number[3,1])
  names(final_out)<- c('ABMC', clean_store)
  return(final_out)
}
len<- length 

##then we can use that to assess the variance in effects 
setwd("/Users/justingrimmer/Dropbox/Correspondence/data/HetEffects")
files<- dir()
library(glmnet)

het_effs<- list()

load('Agriculture.RData')
het_effs[[1]]<- het_effects(agric_list)
rm(agric_list)

load('Appropriations.RData')
het_effs[[2]]<- het_effects(approp_list)
rm(approp_list)

load('ArmedServices.RData')
het_effs[[3]]<- het_effects(armed_list)
rm(armed_list)

load('Budget.RData')
het_effs[[4]]<- het_effects(budget_list)
rm(budget_list)

load('Education.RData')
het_effs[[5]]<- het_effects(education_list)
rm(education_list)

load('Energy.RData')
het_effs[[6]]<- het_effects(energy_list)
rm(energy_list)

load('Ethics.RData')
het_effs[[7]]<- het_effects(ethics_list)
rm(ethics_list)

load('Finance.RData')
het_effs[[8]]<- het_effects(finance_list)
rm(finance_list)

load('Foreign.RData')
het_effs[[9]]<- het_effects(foreign_list)
rm(foreign_list)

load('Homeland.RData')
het_effs[[10]]<- het_effects(homeland_list)
rm(homeland_list)

load('Judiciary.RData')
het_effs[[11]]<- het_effects(judiciary_list)
rm(judiciary_list)

load('NatRes.RData')
het_effs[[12]]<- het_effects(nat_res_list)
rm(nat_res_list)

load('Rules.RData')
het_effs[[13]]<- het_effects(rules_list)
rm(rules_list)

load('science_list.RData')
het_effs[[14]]<- het_effects(science_list)
rm(science_list)

load('SmallBusiness.RData')
het_effs[[15]]<- het_effects(small_business_list)
rm(small_business_list)

load('Transportation.RData')
het_effs[[16]]<- het_effects(transportation_list)
rm(transportation_list)

load('Veterans.RData')
het_effs[[17]]<- het_effects(veterans_list)
rm(veterans_list)

load('Ways.RData')
het_effs[[18]]<- het_effects(ways_list)
rm(ways_list)


load('Banking.RData')
het_effs[[19]]<- het_effects(banking_list)
rm(banking_list)

load('Commerce.RData')
het_effs[[20]]<- het_effects(commerce_list)
rm(commerce_list)

load('ForeignRelations.RData')
het_effs[[21]]<- het_effects(foreign_list)
rm(foreign_list)


load('Health.RData')
het_effs[[22]]<- het_effects(health_list)
rm(health_list)


names(het_effs)<- c('Agriculture', 'Appropriations', 'Armed_Services', 'Budget', 
      'Education', 'Energy', 'Ethics', 'Finance', 'Foreign', 'Homeland', 
        'Judiciary', 'Natural_Resources' , 'Rules', 'Science', 'Small_Business', 
        'Transportation', 'Veterans', 'Ways', 'Banking', 'Commerce', 'ForeignRelations', 
        'Health')


mat_store<- matrix(NA, nrow = 83, ncol = len(het_effs))
for(z in 1:len(het_effs)){
  mat_store[,z]<- het_effs[[z]]
}



rownames(mat_store)<- names(het_effs[[1]])
colnames(mat_store)<- names(het_effs)


##House prestige
#Appropriations, Ways and Means, Rules, Budget,
#or Armed Services


plot(mat_store[81,], pch='')
text(mat_store[81,], labels = colnames(mat_store))

out<- cor(mat_store)


fig_plot<- function(z){
  plot(het_effs[[z]], pch='', main = names(het_effs)[z])
  text(het_effs[[z]], labels = names(het_effs[[z]]))
  abline(h = mean(het_effs[[z]]))

}

##alright, getting the max per committee 
max_per<- c()
for(z in 1:len(het_effs)){
  max_per[z]<- which.max(het_effs[[z]])
}


for(z in 1:len(het_effs)){
  fig_plot(z)
  readline('wait')
}


##so the question is: do legislators increase their attention to regulated committees
##we can show the overall change still implies that the vast majority are constituent focused. 
##but legislators now use this to affect the impression of their effectiveness. 

comm_cross<- read.delim('/Users/justingrimmer/Dropbox/correspondence/data/ACUS_corr.csv', sep=',')

split_names<- strsplit(rownames(mat_store), split= '_')

use_names<- rep(NA, len(split_names))
for(z in 1:len(use_names)){
  use_names[z]<- split_names[[z]][1]

}


use_cols<- gsub('_', ' ', colnames(mat_store))
use_cols[21]<- 'Foreign Relations'


reg_comm<- matrix(0, nrow= nrow(mat_store), ncol = ncol(mat_store))
for(z in 1:nrow(mat_store)){
  temp<- which(comm_cross$Row_ID == use_names[z])
  if(len(temp)>0){
    comms<- comm_cross[temp,]$Reporting.Committees
    for(j in 1:ncol(mat_store)){
      if(grepl(use_cols[j], comm_cross[temp,]$Reporting.Committees, ignore.case=T)==T){reg_comm[z,j]<- 1}
    }
  }
}


rownames(reg_comm)<- rownames(mat_store)
colnames(reg_comm)<- colnames(mat_store)

mean(mat_store[reg_comm==1])
mean(mat_store[reg_comm==0])


save(reg_comm, file= '~/Dropbox/correspondence/data/CommitteeAgencyRegulation.RData')

fig_plot<- function(z){
  plot(het_effs[[z]], pch='', main = names(het_effs)[z])
  text(het_effs[[z]], labels = names(het_effs[[z]]), col=ifelse(reg_comm[,z]==1, 'black', grey(0.5)))
  abline(h = mean(het_effs[[z]]))
}


###putting together the plots. let's label the large effects 


fig_plot2<- function(z, cutoff){
  orders<- order(het_effs[[z]], decreasing=T)
  plot(het_effs[[z]][orders],c(1:len(het_effs[[z]])), ylab = 'Agency', xlab = 'Effect Size', main = names(het_effs)[z][orders], pch='')
  #points(het_effs[[z]], pch=20, col=ifelse(reg_comm[,z]==1, gray(0, alpha= 0.5), grey(0.5, alpha = 0.5)))
  text(het_effs[[z]][orders],c(1:len(het_effs[[z]])), labels= ifelse(abs(het_effs[[z]][orders]) > cutoff , names(het_effs[[z]][orders]), ''), col=ifelse(reg_comm[orders,z]==1, gray(0), grey(0.5)), cex = 1.5)
  for(j in 1:len(het_effs[[z]])){
    arrows( 0,j, het_effs[[z]][orders][j], j ,len = 0, lwd =1.2,  , col=ifelse(reg_comm[orders,z]==1, gray(0), grey(0.5)))
  }


}

##effect, agency, comittee, regulated

out_mat<- matrix(NA, nrow = 1826, ncol = 4)
out_mat<- as.data.frame(out_mat)
a<- 0 
for(z in 1:83){
  for(j in 1:22){
    a<- a + 1
    out_mat[a,]<- c(mat_store[z,j], rownames(mat_store)[z], colnames(mat_store)[j], reg_comm[z,j])
  }
}

colnames(out_mat)<- c('Effect', 'Agency', 'committee', 'Oversight')

out_mat$Effect<- as.numeric(out_mat$Effect)
out_mat$regulate<- as.factor(out_mat$regulate)

levels(out_mat$Oversight)<- c('No', 'Yes')

setwd('/Users/justingrimmer/Dropbox/correspondence/figs/HetEffects')
comm_plot<- function(name){

g<- out_mat %>% subset(committee==name) %>% ggplot(aes(y= Effect, x = Agency)) + 
      geom_bar(stat= 'identity', aes(fill=Oversight))  + coord_flip()  + 
        scale_fill_manual(values = c(gray(0.5), gray(0))) + ggtitle(name) 

ggsave(g, file=paste(name, '.pdf', sep='') , height = 12, width = 8)
}

comms<- unique(out_mat$committee)

for(z in comms){
  comm_plot(z)
}


agent<- read.dta('~/Dropbox/Correspondence/Data/AgencyWithComm.dta')
load('~/Dropbox/correspondence/data/CommitteeAgencyRegulation.RData')
df$icpsr<- as.character(df$icpsr)
df$year<- as.numeric(df$year)

oversight2<- rep(0, nrow(agent))
comm_names<- colnames(agent)[35:77]
comm_names[25]<- 'foreignrelations'
col_names<- tolower(colnames(reg_comm))
row_names<- rownames(reg_comm)
for(z in 1:nrow(agent)){
  cols<- which(col_names==comm_names[which(agent[z,35:77]>0)])
  oversight2[z]<- ifelse(sum(reg_comm[which(row_names==agent$agency[z]),cols])>0, 1, 0)
  if(z%%1000==0){
  print(z)}
}

agent$oversight2<- oversight2

chair_oversight<- chair_non_oversight<- rep(0, nrow(agent))

iters<- which(agent$chair==1)

df$year<- as.numeric(df$year)

test<- df %>% subset(chair==1) %>% group_by(icpsr, year) %>% summarise(chair_of = first(chair_of))

test2<- left_join(agent  , test , by = c('icpsr', 'year'))

iters<- which(is.na(test2$chair_of)==F)

##changing relevant columns of reg_comm
colnames(reg_comm)[3]<- 'armed services'
colnames(reg_comm)[15]<- 'small business'
colnames(reg_comm)[21]<- 'foreign'
colnames(reg_comm)[12]<- 'resources'
colnames(reg_comm)[10]<- 'homeland security'
ee<- unique(tolower(colnames(reg_comm))) %in% unique(tolower(test2$chair_of))

##now, going through and identifying the relevant agencies
for(z in iters){
  col<- which(tolower(colnames(reg_comm))==tolower(test2$chair_of)[z])
  if(len(col)>0){
  row<- which(rownames(reg_comm)==test2$agency[z])
  chair_oversight[z]<- ifelse(sum(reg_comm[row,col])>0, 1, 0)
  chair_non_oversight[z]<- 1- chair_oversight[z]
}
print(z)
}




agent$chair_oversight<- chair_oversight
agent$chair_non_oversight<- chair_non_oversight


write.dta(agent, file='~/Dropbox/Correspondence/Data/AgencyCommOversightChair.dta')








