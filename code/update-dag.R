# This script updates the csvs and Rdata files used in the shiny app and NetLit examples 

library(googlesheets4)
library(googledrive)
library(tidyverse)
library(here)
library(igraph)
library(magrittr)

# refresh data from google sheet if token is present
if(gs4_has_token()){
  
  dag <- googledrive::drive_get(id = "1LACTrfsXGX2J7JlFgnF3bniUQpsMtKBVSQSYo0KPmkc") %>%
    gs4_get() %>% 
    read_sheet("corr") 
  1
  1
  
  write_csv(dag, here("data",  "dag.csv"))
  
  
} else { 
  warning("Google sheets is not authorized, run lines above to get auth tokens if you want to update the data.")}



