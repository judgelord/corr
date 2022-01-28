
load(here::here("data", "all_contacts_paper.RData"))

library(tidyverse)
library(magrittr)

d <- all_contacts

# select needed vars 
names(d)

d %<>% select(ID, DATE,FROM,bioname, party_name, 
              committees, chair_of,
              oversight_committee, oversight_committee_chair,
              icpsr, agency, 
              SUBJECT,TYPE, ALT_TYPE, CERTAINTY,
              POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES)

policy <- "rule|hearing|oversight|legislat|budget|approp"

# filter down to policy events excluing requests for information
d  %<>%
  mutate(POLICY_EVENT = POLICY_EVENT %>% str_to_lower()) %>% 
  filter(#TYPE %in% c(4,5),
         str_detect(POLICY_EVENT, policy)) 
  #distinct(POLICY_EVENT) %>% str_to_lower()
  








# add blanks
d %<>% mutate(event_url = "",
              document_url = "",
              docket_url = "", 
              comment_url = "",  
              position = "",
              position_certainty = "",
              comment_type = "",
              coalition_comment = "",
              coalition_type = "",
              # org_name = organization, # run scratchpad/orgnames.R until this is a function
              org_name_short = "",
              org_type = "",
              ask = "",
              ask1 = "",
              ask2 = "",
              ask3 = "",
              success = "",
              success_certainty = "",
              sucess1 = "",
              success2 = "",
              success3 = "",
              response = "",
              pressure_phrases = "",
              accept_phrases = "",
              compromise_phrases = "",
              reject_phrases = "",
              notes = "")

replace_na_str <- . %>% replace_na("")

d %<>% mutate(across(everything(), replace_na_str)) 

d %<>% arrange(agency, ID)

write_csv(d, file = "data/letters_congress_clean.csv")

