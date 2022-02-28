library(devtools) # to get voteview

## Rvoteview dependencies can through errors, so this script creates members.Rdata, which limits the use of voteview and saves the augmented names
if(!"Rvoteview" %in% rownames(installed.packages())) {
  devtools::install_github("voteview/Rvoteview")
}
library(Rvoteview)

nom <- full_join(member_search(congress = c(109:120)) %>% select(-congresses),
                 member_search(congress = c(105:108)) %>% select(-congresses)
                 )  %>% 
  full_join(member_search(congress = c(101:104)) %>% select(-congresses) ) %>% 
  full_join(member_search(congress = c(97:101)) %>% select(-congresses) ) %>% 
  full_join(member_search(congress = c(93:96)) %>% select(-congresses) ) %>% 
  full_join(member_search(congress = c(90:93)) %>% select(-congresses) ) %>% 
  full_join(member_search(congress = c(86:89)) %>% select(-congresses) ) %>% 
  full_join(member_search(congress = c(82:85)) %>% select(-congresses) ) %>% 
  full_join(member_search(congress = c(81:84)) %>% select(-congresses) )%>% 
  full_join(member_search(congress = c(77:80)) %>% select(-congresses) )

save(nom, file = here::here("data", "nom.Rdata"))

  
