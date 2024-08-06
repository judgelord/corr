

library(modelsummary)
library(marginaleffects)
library(fixest)
library(tidyverse)
library(magrittr)
library(tidytext)
library(xml2)
library(knitr)
library(kableExtra)
library(here)
library(ggrepel)

# library(lfe)

library(ggplot2); theme_set(theme_bw())
options(
  ggplot2.continuous.color = "cividis",
  ggplot2.continuous.fill = "cividis"
)
scale_color_discrete <- function(...)
  scale_color_viridis_d(..., direction = -1)
scale_fill_discrete <- function(...)
  scale_fill_viridis_d(..., direction = -1)

knitr::opts_chunk$set(echo = TRUE, 
                      cache = TRUE, 
                      fig.width=8.5, 
                      split = T,
                      fig.align = 'center', 
                      fig.path='figs/',
                      fig.retina = 1,
                      warning=FALSE, 
                      message=FALSE)

kablebox <- . %>%  
  head(100) %>%
  knitr::kable() %>% 
  kable_styling() %>% 
  scroll_box(height = "200px")

kablebox_long <- . %>% 
  head(100) %>% 
  knitr::kable() %>% 
  kable_styling() %>% 
  scroll_box(height = "500px")