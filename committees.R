

d |> distinct(bioname, congress, chair) |> 
  group_by(congress) |> 
  count(is.na(chair))

d |> distinct(bioname, congress, ranking_minority) |> 
  group_by(congress) |> 
  count(is.na(ranking_minority))



d |> distinct(bioname, congress, committees) |> 
  group_by(congress) |> 
  count(is.na(committees))

test$chair |> is.na() |> sum()

library(tidyverse)


# Install the yaml package if it's not already installed
if (!require("yaml")) {
  install.packages("yaml")
}

# Load the yaml package
library("yaml")

y <- here::here("data", "committee-membership-2020.yaml") |>
  read_yaml()

names_y <- map(y, names) |> names()

extract <- function(x, name){
  d <- map_dfr(x, as_tibble)
  
  d$thomas_id <- name

  return(d)
}

membership <- map2_dfr(y, names_y, extract) 

membership <- membership |> 
  # match the other data from the same source 
  rename(bioguide_id = bioguide)


y <- here::here("data", "committees-current.yaml") |>
  read_yaml()


y[[1]]$rss_url

map(y[1], as_tibble)

extract <- function(x){
  #d <- map(x, as_tibble) #|> unnest(subcommittees) #|>  select(name, thomas_id)
  
  d <- x[[1]]$rss_url
  
  return(d)
}

committees <- map_dfr(.x = y, .f =  as_tibble)

save(committees, file = here::here("data", "committees_current.rda"))

committee_membership <- left_join(membership, 
                                  distinct(committees, thomas_id, committee = name, type) )

save(committee_membership, here::here("data", "committee_membership_2020.rda") )

# format committee data the same as the old stuff in the members data 
c <- committee_membership |> 
  mutate(committee = committee |>
           str_remove_all(".*ommittee on the |.*ommittee on | and.*|,.*|' Affairs| Affairs| Committee|Joint ") |> 
           str_to_upper()
           )

c1 <- c$committee |> unique()


c2 <- members$committees |> str_split("\\|") |> unlist() |> unique()

c2[!c2 %in% c1]
c1[!c1 %in% c2]

members_committees <- c |> 
  group_by(bioguide_id) |> 
  mutate(#chamber = str_to_sentence(type),  #FIXME 
         committees = paste(committee, collapse = "|") |> 
           str_remove_all("\\|NA\b*")|> 
           str_remove_all("\\|NA\b*")|> 
           str_remove_all("\\|NA\b*"),
         titles = paste(title, collapse = ";") |> 
           str_remove_all(";NA\b*")|> 
           str_remove_all(";NA\b*")|> 
           str_remove_all("^NA;*")
         ) |> 
  distinct(committees, titles, name) |> 
  ungroup() |> 
  add_count(bioguide_id) |> 
  arrange(-n)

members_committees$titles |> unique()

members_committees$committees |> unique()


## ID FOR MERGING WITH MEMBERS DATA 

## THIS WOULD WORK IF LEGISLATORS WERE UPDATED 
# library(legislators)
# members_committees <- members_committees |> extractMemberName("name", congress = 116)
# 

legislators_current <- read_csv(here::here("data", "legislators-current.csv"))
legislators_historical <- read_csv(here::here("data", "legislators-historical.csv"))

legislators <- full_join(legislators_current, legislators_historical)

members_committees <- members_committees |> left_join(legislators)


save(members_committees, file =  here::here("data", "members_committees_2020.rda"))
