library(dplyr)

# The minimal count data
load(here::here("data", "dcounts_min.Rdata"))
names(dcounts_min)

corr_counts <- dcounts_min |> distinct(icpsr, chamber,  
                                   agency, year,
                                   TYPE, per_icpsr_chamber_year_agency_type) #|> filter(TYPE %in% c(1,2,3,4,5))

save(corr_counts, 
     file = here::here("data", "corr_counts.Rdata"))


## The data on members
## mostly from voteview
## but also including population from the census
load(here::here("data", "members.Rdata"))
names(members)

member_data <- members |>
  ungroup() |> 
  distinct( congress, chamber,
           bioname, last_name, 
           icpsr, cqlabel, district_code,
           state_abbrev, state, district_code, pop2010, 
           committees, chair, ranking_minority, 
           majority, presidents_party, party,
           yearelected) |>
  group_by(icpsr) |>
  mutate(first_cong = min(congress),
         first_year = 1787 + 2*first_cong)

save(member_data, 
     file = here::here("data", "member_data.Rdata"))


