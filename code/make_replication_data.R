library(tidyverse)
library(magrittr)

# The minimal count data
load(here::here("data", "dcounts_min.Rdata"))
names(dcounts_min)

corr_counts <- dcounts_min %>% distinct(icpsr, chamber,  
                                   agency, year, 
                                   TYPE, per_icpsr_chamber_year_agency_type) #%>% filter(TYPE %in% c(1,2,3,4,5))

save(corr_counts, 
     file = here::here("data", "corr_counts.Rdata"))


## The data on members
## mostly from voteview + committee data from Stewart and Wu 
#TODO merge both sources of committee data first and then with voteview 
## but also including population from the census
#load(here::here("data", "members.Rdata"))


members_raw <- read_csv(here::here("data", "HSall_members.csv"))

members <- members_raw %>% 
  select(icpsr, bioname, congress, chamber, party_code, state_abbrev, district_code) %>% 
  filter(congress > 100, congress < 117) %>% 
  mutate(
  party = case_when(
    party_code == 100 ~ "(D)",
    party_code == 200 ~ "(R)",
    party_code == 328 ~ "(I)",
    F ~ NA
  ) ) 
  

members %>% filter(party =="(I)" ) %>% distinct(bioname)

# independents 
# who caucus with Dems 
ds <- c(        "JEFFORDS, James Merrill",
                "BARKLEY, Dean",
                "SANDERS, Bernard",
                "KING, Angus Stanley, Jr.")

# who caucus with GOP 
rs <- c("GOODE, Virgil H., Jr.") 
        
# as far as I can tell, Amash stopped caucusing with GOP, but sort of moot 
is <-  c("AMASH, Justin")  

members %<>% 
  mutate(party_caucus = 
           case_when(
             party == "(I)" & bioname %in% ds ~ "(D)",
             party == "(I)" & bioname %in% rs ~ "(R)",
             party == "(I)" & bioname %in% is ~ "(I)",
             TRUE ~ party ))

presidents <- members %>% filter(chamber == "President") %>% 
  select(congress, party_of_president = party) %>% arrange(-congress)

members %<>% 
  left_join(presidents) %>% 
  mutate(presidents_party = as.numeric(party_caucus == party_of_president ) ) %>% 
  filter(chamber != "President")

# Party size data 
# From voteview.com https://voteview.com/articles/data_help_parties
parties <- read_csv(here::here("data", "HSall_parties.csv"))

party_size <- parties %>% distinct(party_code, n_members, chamber, congress) %>% arrange(-congress) %>% 
  filter(chamber != "President") %>% 
  group_by(congress, chamber) %>%
  mutate(chamber_size = sum(n_members))

# fix error (this is the only one that will affect our variables)
party_size %<>% 
  mutate(
    n_members = case_when(
      chamber == "Senate" & congress == 110 & party_code == 100 ~ 51,
      chamber == "Senate" & congress == 110 & party_code == 200 ~ 49,
      T ~ n_members
    ),
    party_caucus = case_when(
      party_code == 100 ~ "(D)",
      party_code == 200 ~ "(R)",
      party_code == 328 ~ "(I)",
      T ~ NA
    )
  ) %>% select(-party_code)

members %<>% 
  left_join(party_size) %>% 
  mutate(
    party_size = n_members, 
    majority = 
           case_when(
             presidents_party == 1 & chamber == "Senate" & party_size == 50 ~ 1,
             chamber == "Senate" & party_size > 50 ~ 1, 
             chamber == "House" & party_size > 217 ~ 1, 
             T ~ 0
           ))

members %>% distinct(congress, party, chamber, majority) %>% filter(party !="(I)")

completeness <- members %>% 
  filter(chamber != "President") %>% 
  count(party, party_caucus, party_size, chamber, majority, presidents_party, party_of_president, congress) %>% arrange(-congress)

# check chamber size implied by voteview party data
completeness %>% 
  filter(!party == "(I)") %>% 
  group_by(congress, chamber) %>% 
  summarise(sum(party_size)) %>% arrange(-congress)


member_data <- members %>%
  ungroup() %>% 
  distinct( congress, chamber,
           bioname, #last_name, 
           icpsr, #cqlabel, 
           district_code,
           state_abbrev, #state, 
           district_code, #pop2010, 
           # committees, chair, ranking_minority, 
           majority, presidents_party, party#, yearelected
           ) %>%
  group_by(bioname) %>%
  # first year 
  mutate(first_cong = min(congress),
         first_year = 1787 + 2*first_cong) %>% 
  # subset to 2007-2020 where we have reliable correspondence count data 
  filter(congress > 109, congress < 117) %>% 
  ungroup()


# merge in district population 
states <- read_csv(here::here("data", "states.csv")) %>% select(state, pop2010)

stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}

member_data %<>% mutate(state =stateFromLower(state_abbrev)) %<>%
  left_join(states)



# merge in committee data for the 106th-115th 
load(here::here("data", "members_committees_106-115th.rda"))

member_data %<>%  left_join(members_committees)



# merge in new committee data for the 116th 2019-2020
load(here::here("data", "members_committees_116th.rda"))
members_committees <- members_committees_116th

members_committees$titles %>% str_split(";") %>% unlist() %>% unique()

members_committees %<>%
  select(name,
         icpsr = icpsr_id,
         titles,
         committees2 = committees) %>%
  mutate(congress = 116) %>%
  group_by(name) %>% 
  fill(icpsr, .direction = "updown") %>% 
  ungroup()

member_data %<>%  left_join(members_committees) %>%
  mutate(
    # because these come from two different datasets, need to replace NAs to avoide ifelse below yielding NA 
    titles = replace_na(titles, ""),
    positions = replace_na(positions, ""),
    chair = ifelse( str_detect(titles, "^Chair|;Chair|;Chairman|;Cochairman") | str_detect(positions, "Chair"), 
                     1, 0 ),
    ranking_minority = ifelse( str_detect(titles, "Ranking Member")  | str_detect(positions, "Ranking Minority"), 
                                1, 0 )
    ) %>%
  mutate(committees = coalesce(committees, committees2)) %>% 
  distinct()

# NO LONGER NEEDED 
# hack <- tibble(
#   congress = c(116, 116, 116, 116),
#   majority2 = c(1,0, 0, 1),
#   #party = c("(D)", "(D)","(R)","(R)"),
#   presidents_party = c(1,1,0,0),
#   chamber = c("House", "Senate", "House", "Senate")
# )
# 
# member_data <-member_data %>% left_join(hack)
#member_data %<>% mutate(majority = coalesce(majority, majority2))

# corrections to committee data 
corrections <- read_csv(here::here("data", "committee_corrections_116th.csv"))
corrections

# add in committee data corrections 
member_data <- member_data %>% 
  filter(!icpsr %in% corrections$icpsr | congress != 116) %>% 
  full_join(corrections)  %>%
  ungroup() %>% 
  distinct( congress, chamber,
            bioname, #last_name, 
            first_year,
            icpsr, #cqlabel, 
            district_code,
            state_abbrev, state, district_code, pop2010, 
            committees, chair, ranking_minority, 
            majority, presidents_party, party
  )


# look for missing committee data 
missing <- member_data %>% filter(congress == 116, is.na(committees))

missing <- member_data %>% filter(bioname %in% missing$bioname, congress==116) %>% arrange(bioname)

missing %>% 
   write_csv(here::here("data", "missing_committees_116th.csv"))


## NOTES ON SOME OF THE MISSING COMMITTEE DATA THAT WAS MISSING  
# AMASH switched parties

# Duffy resigned 
# CUMMINGS, Elijah Eugene died 

# ? CLYBURN, James Enos
# ? MCCARTHY, Kevin


# BISHOP, Dan came in after 
# GARCIA, Mike - 2020 special election 
# HILL, Katie - resigned 



save(member_data, 
     file = here::here("data", "member_data.Rdata"))
