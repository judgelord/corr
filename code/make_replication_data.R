library(tidyverse)
library(magrittr)

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
           bioname, last_name, id,
           icpsr, cqlabel, district_code,
           state_abbrev, state, district_code, pop2010, 
           committees, chair, ranking_minority, 
           majority, presidents_party, party,
           yearelected) |>
  group_by(icpsr) |>
  mutate(first_cong = min(congress),
         first_year = 1787 + 2*first_cong) |> 
  filter(congress > 109, congress < 117)

# merge in new committee data for the 116th 2019-2020
load(here::here("data", "members_committees_2020.rda"))

members_committees$titles |> str_split(";") |> unlist() |> unique()

member_data <-member_data |> left_join(members_committees |> 
                                select(icpsr = icpsr_id,
                                       titles,
                                       committees2 = committees) |>
                                mutate(congress = 116) ) |>
  mutate(
    chair2 = ifelse( str_detect(titles, "^Chair|;Chair|;Chairman|:Cochairman"), 
                     1, 0 ),
    ranking_minority2 = ifelse( str_detect(titles, "Ranking Member"), 
                                1, 0 )
    ) |>
  mutate(chair = coalesce(chair, chair2),
         ranking_minority = coalesce(ranking_minority, ranking_minority2),
         committees = coalesce(committees, committees2))

hack <- tibble(
  congress = c(116, 116, 116, 116),
  majority2 = c(1,0, 0, 1),
  #party = c("(D)", "(D)","(R)","(R)"),
  presidents_party = c(1,1,0,0),
  chamber = c("House", "Senate", "House", "Senate")
)

member_data <-member_data |> left_join(hack)

member_data <-member_data |> mutate(majority = coalesce(majority, majority2))

#FIXME This should have come from voteview, not sure what happened to the 116h

save(member_data, 
     file = here::here("data", "member_data.Rdata"))


