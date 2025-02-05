# TRYING TO SHRINK THIS TO EVENTUALLY INSERT IT INTO replication.qmd

# This script takes the minimal master data file and creates member and district-level yearly count and ratio data files 
# These data files are then used to estimate the models in replication.qmd
# the key DVs are
# - perYear = counts per year (member level and district level)
# - ratio = ratio of policy work to constituency service 
testing = F

load(here::here("data", "corr_counts.Rdata"))
load(here::here("data", "member_data.Rdata"))

# just in case it was saved as grouped data 
corr_counts %<>% ungroup()

corr_counts %<>% mutate( icpsr_year = paste(icpsr, year, sep='_') )

# chamber switchers  in original data 
chamber_switchers <- corr_counts |> 
  distinct(icpsr_year, agency, chamber) |>
  add_count(icpsr_year, agency,  #party, 
            name = "n") |>
  filter(n > 1) |> 
  distinct(icpsr_year, chamber) |> 
  arrange(icpsr_year)

if(testing){
chamber_switchers
} 

# DROP CHAMBER SWITCHERS
corr_counts %<>% filter(!icpsr_year %in% chamber_switchers$icpsr_year)



############################
# CREATE COUNTS for models #
############################

##creating the member-level aggregate count variables

# TOTAL counts 
d <- corr_counts |>
  group_by(agency, icpsr, chamber,
           year) |>
  summarise(perYear = sum(per_icpsr_chamber_year_agency_type)) |> 
  ungroup()

# names(d)

# Constituent counts 
d_con <- corr_counts |>
  subset(TYPE %in% c(1, 2, 3)) |> 
  group_by(agency, icpsr, chamber, year) |>
  summarise(perYear_con = sum(per_icpsr_chamber_year_agency_type))

# Policy counts 
d_policy <- corr_counts |>
  subset(TYPE %in% c(4, 5)) |> 
  group_by(agency, icpsr, chamber, year) |>
  summarise(perYear_pol = sum(per_icpsr_chamber_year_agency_type))

d$perYear_con <- d_con$perYear_con
d$perYear_policy<- d_policy$perYear_pol

# congress year converters 
congress_years<- function(congress){
  years<- c(congress*2 + 1787, congress*2 + 1788 )
  return(years)
}

year_congress<- function(year){
  return(floor((year - 1787)/2))
}

# make congress to merge with member data 
d$congress<- year_congress(d$year)

# inspect duplicates 
d <- d |> 
  ungroup() |>
  distinct() |>
  add_count(icpsr, year, agency,  #party, 
            name = "n") 

if(testing){
d |>filter(n > 1) |> distinct(icpsr, year) 
} 

# LEFT join in member data 
d <- d |>  
  ungroup() |> 
  left_join(member_data, 
            by = c('congress', 'icpsr', "chamber") ) 


## count or repeated values (chamber and party switchers)
# note that in the full data, we use the dates of letters to attribute them to the proper party or chamber at the time
# but in yearly counts, we lose this level of detail, leading to undercounts for switchers
d <- d |> 
  ungroup() |>
  distinct() |>
  add_count(icpsr, year, agency,  #party, 
            name = "n") 

# # inspect for duplicates 
if(testing){
d |>filter(n > 1) |> distinct(icpsr, year) 
}


####################
# TRANSFORMATIONS #  
###################

# tenure for experience tests 
d <- d |> 
  group_by(icpsr) |>
  mutate(
    tenure = year - first_year,
    first = ifelse(tenure==0, 1, 0),
    second = ifelse(tenure==1, 1, 0),
    third = ifelse(tenure==2, 1, 0),
    fourth = ifelse(tenure==3, 1, 0),
    fifth = ifelse(tenure==4, 1, 0),
    sixth = ifelse(tenure==5, 1, 0),
    max_year = max(tenure)
  ) 

# fixed effects 
d <- d |> 
  ungroup() |> 
  mutate(
    icpsr_agency = paste(agency, icpsr, sep='_'),
    agency_year = paste(agency, year, sep='_')
  )

# indicator for whether they survived their first election 
d <- d |> 
  mutate(survive = ifelse(
    chamber =='House' & max_year>1 | chamber=='Senate' & max_year>5,
    1, 0)
  )


if(testing){
  names(d)
  # saved version of data 
  load(here::here("data/dcounts_tenure.Rdata"))
  
  vars <- c("agency", "icpsr",            "chamber",          "year",             
            "perYear",          # THIS WAS DIFFERENT when subsetting to 1-5 coding before counting 
            "perYear_con", "perYear_policy", # THESE ARE THE SAME 
            "congress",
            "bioname",          "last_name",        
            "cqlabel", "district_code",    "state_abbrev",     "state",
            "ranking_minority", "majority",         "presidents_party", "party",          
            "yearelected", "first_cong", "first_year", "tenure", "first", "second", "fifth", "sixth", "max_year", "icpsr_agency",
           # "agency_year",
            "survive" )
  
  anti_join(d |> select(vars), 
            dcounts_tenure |> select(any_of(vars)) )
}

dcounts_tenure <- d

#########
# RATIO # 
#########  

##  the ratio variable - at the legislator-year level 
## (not the legislator-year-agency level like above)
dcounts_ratio  <- d |>
  ungroup() |> 
  # variables for ratio models 
  group_by(year, icpsr, chamber,
           chair, ranking_minority,  
           majority, presidents_party,
           first, second, third, fourth, fifth, sixth)  |>
  summarise(perCon = sum(perYear_con), 
            perPol = sum(perYear_policy)) |>
  mutate(ratio = perCon/(perCon + perPol))

# check for duplicates  
if(testing){
  dcounts_ratio |> ungroup() |>
    group_by(year, icpsr) |> 
    add_count(name = "n") |> 
    filter(n >1)
  
  # check for chamber switchers 
  dcounts_ratio |> 
    mutate(icpsr_year = paste(icpsr, year, sep = "_")) |> 
    filter(icpsr_year %in% chamber_switchers$icpsr_year)
  
  new <- dcounts_ratio
  
  # check for changes from last version of data 
  load(here::here("data/dcounts_ratio.Rdata"))
  anti_join(new, dcounts_ratio)
  
  anti_join(new, dcounts_ratio) |> 
    mutate(icpsr_year = paste(icpsr, year, sep = "_")) |> 
    filter(!icpsr_year %in% chamber_switchers$icpsr_year)

  # restore 
  decounts_ratio <- new 
} 




#### PER DISTRICT COUNTS ######################
d %<>% mutate(decade = case_when(
  year < 2011 ~ '0', 
  year > 2010 ~ '1'))


d %<>% mutate(state_dist = case_when(
  chamber=='Senate'~ paste(state,district_code,  sep='_' ),
  chamber =='House'~ paste(paste(state, district_code, sep='_'), decade, sep='_')
))


dcounts_per_district<- d |>
  group_by(year, state_dist, icpsr, chamber, 
           tenure, first, second, third, fourth, fifth, sixth,
           state) |>
  summarise(perYear = sum(perYear),
            perYear_con = sum(perYear_con),
            perYear_policy = sum(perYear_policy)) |>
  distinct()

# FIXME this can be simplified, in the models they really should be the same 
dcounts_per_district %<>% 
  mutate(new_member = first,
         second_year = second,
         third_year = third,
         fourth_year = fourth,
         fifth_year = fifth,
         sixth_year = sixth
  )

if(testing){
new <- dcounts_per_district

# last version of data 
load(here::here("data/dcounts_per_district.Rdata"))

# they differ in perYear ? 
anti_join(new, dcounts_per_district)

summary(new)
summary(dcounts_per_district)
} 
