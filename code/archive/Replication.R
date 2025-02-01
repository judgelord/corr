## ----global.options, include=FALSE----------------------------------
library(modelsummary)
library(marginaleffects)
library(fixest)
library(tidyverse)
library(magrittr)
library(knitr)
library(kableExtra)
library(here)
library(ggrepel)


## ----data-----------------------------------------------------------
load(here::here("data", "dcounts_tenure.Rdata"))
load(here::here("data", "dcounts_ratio.Rdata"))
load(here::here("data", "dcounts_per_district.Rdata"))
load(here::here("data" , "members.Rdata"))


# var names for tables
dcounts_tenure %<>% 
  mutate(Legislator = icpsr,
         Legislator_x_Agency = icpsr_agency,
         Year_x_Agency = agency_year)

# total per agency
dcounts_tenure %>% group_by(agency) %>% summarise(n = perYear %>% sum()) %>% arrange(-n) %>% kablebox()

# total per legislator-agency pair 
dcounts_tenure %>% group_by(Legislator_x_Agency) %>% summarise(n = perYear %>% sum()) %>% arrange(-n) %>% kablebox()


## ----values, fig.width=5, fig.height=3------------------------------
modal <- dcounts_tenure %>% 
  group_by(icpsr) %>% 
  filter(first_year > 2006, first_year < 2013, max_year > 6) %>% 
  mutate(Chair = sum(chair, na.rm = T) >1) %>% 
  group_by(icpsr, year) %>%
  mutate(perLeg = sum(perYear)) %>% 
  filter(perYear == 1, perLeg == 73 ) %>% 
  select(perAgency = perYear, perLeg, icpsr, icpsr_agency, agency_year, first_year, max_year, Chair) %>% 
  distinct() %>% 
  ungroup() %>% left_join(members %>% select(bioname, icpsr))

modal %>%
  mutate(average = perLeg) %>% 
  count(average, bioname, first_year, max_year, Chair, sort = T, name = "agency_year_count == 1") %>% 
  kablebox()

modal %<>% filter(Chair)

dcounts_tenure %>% 
  group_by(icpsr, year) %>% 
  mutate (perLegislator = sum(perYear)) %>% 
  ungroup() %>% 
  mutate(meanLeg = mean(perLegislator),
         meanYear = mean(perYear)) %>%
  filter( abs(perLegislator - meanLeg) < 10,   
          abs(perYear - meanYear) < 10 )  %>%
  ggplot() +
  aes(x = perYear, y = perLegislator) + 
    geom_jitter() + 
  geom_vline(aes(xintercept = meanYear), color = "blue") + 
  geom_hline(aes(yintercept = meanLeg), color = "blue") + 
  geom_text_repel(aes(label = ifelse(icpsr_agency %in% modal$icpsr_agency & agency_year %in% modal$agency_year & perLegislator == 73, icpsr_agency, NA) ),
                  color = "red", box.padding = 0.5, max.overlaps = Inf) + 
  labs(x = "Contacts per Legislator per Agency per Year",
       y = "Contacts per Legislator per Year")

modal %>%
  distinct(perAgency, bioname, icpsr_agency, agency_year) %>% 
  kablebox()

## other variable means
# dcounts_tenure %>% summarise(across(everything(), mean_or_mode)) %>% kablebox()

# by chair and presidents' party
values <- tidyr::expand(dcounts_tenure,
                        agency = "Treasury_IRS",
                        chair = chair, 
                        ranking_minority	= FALSE, 
                        prestige = TRUE, 
                        first = FALSE,
                        second = FALSE,
                        third = FALSE,
                        fourth = FALSE,
                        fifth = FALSE, 
                        sixth = FALSE,
                        Legislator_x_Agency = "Treasury_IRS_20947",
                        Year_x_Agency = "Treasury_IRS_2015",
                        Legislator = "20947",
                        Year = "2015",
                        majority = TRUE,
                        presidents_party = presidents_party
                   ) %>% 
  drop_na(majority, chair)

# by year 
values_tenure <- tidyr::expand(dcounts_tenure,
                        agency = "Treasury_IRS",
                        chair = chair, 
                        ranking_minority	= FALSE, 
                        prestige = TRUE, 
                        first = first,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator_x_Agency = "Treasury_IRS_20947",
                        Year_x_Agency = "Treasury_IRS_2015",
                        Legislator = "20947",
                        Year = "2015",
                        majority = TRUE,
                        presidents_party = TRUE
                   ) %>% 
  drop_na(chair)


## -------------------------------------------------------------------
# helper functions to plot predicted values
ggchair <- function(predicted = predicted) {
  
  predicted$chair %<>% str_replace("0", "Not Chair") %>% str_replace("1", "Chair")
  
  predicted$presidents_party %<>% str_replace("0", "Not President's Party") %>% str_replace("1", "President's Party")

predicted %>%  
  ggplot() + 
  aes(x = "", 
      y = predicted, 
      fill = chair,
      shape = chair,
      color = chair,
      ymin = predicted - 1.96*std.error,
      ymax = predicted + 1.96*std.error) + 
  geom_pointrange(position =  position_dodge(width = -.5)  )  + 
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") +
      theme_minimal() + 
  theme(panel.border  = element_blank(),
        panel.grid.major.x = element_blank()) + facet_grid(. ~ presidents_party) 
}


ggtenure <- function(predicted = predicted) {
  
predicted %<>%
  # drop estimates from impossible values
      group_by(rowid, predicted, std.error, chair) %>% 
  mutate(sum = sum(first, second, third, fourth, fifth, sixth),
         more = ifelse(sum == 0, 1,0)) %>% 
    filter(sum < 2) %>% 
    pivot_longer(cols = c("first", "second", "third", "fourth", "fifth", "sixth", "more")) %>% 
    select(name, value) %>% 
    filter(value == 1) %>% 
    # clean up for presentation
    mutate(year_in_congress = name %>% 
             str_replace("more", "7\nor more") %>% 
             str_replace("sixth", "6") %>% 
             str_replace("fifth", "5") %>% 
             str_replace("fourth", "4") %>% 
             str_replace("third", "3") %>% 
             str_replace("second", "2") %>% 
             str_replace("first", "1") 
             ) %>% 
    ungroup()
  
# Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values 
# predicted %<>% full_join(dcounts_tenure2 %>% mutate(predicted = NA))
  
predicted %<>% filter(chair == 0 | name %in% c("sixth", "more"))

  predicted$chair %<>% str_replace("0", "Not Chair") %>% str_replace("1", "Chair")
  

predicted %>%  
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_congress, 
      y = predicted, 
      shape = chair,
      color = chair,
      ymin = predicted - 1.96*std.error,
      ymax = predicted + 1.96*std.error) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") +
      theme_minimal() + 
  theme(panel.border  = element_blank(),
        panel.grid.major.x = element_blank()) 
  
}



## ----cm-------------------------------------------------------------
# Coef Map
cm = c("chair" = "Committee Chair",
       "ranking_minority" = "Ranking Member",
       "prestige" = "Prestige Committee",
       "first" = "First Year",
       "second" = "Second Year",
       "third" = "Third Year",
       "fourth" = "Fourth Year",
       "fifth" = "Fifth Year",
       "sixth" = "Sixth Year",
       "majority" = "Majority",
       "presidents_party" = "President's Party",
       "Legislator" = "Legislator", 
       "Agency" = "Agency",
       "FE: icpsr_agency" = "Legislator × Agency Fixed Effects",
       "icpsr_agency" = "Legislator × Agency Fixed Effects")

coef_omit = "(Intercept)|majority|presidents_party"


setFixest_dict(cm)

gof_omit = "R.*|AIC|BIC|Log.*|Std.*"

# table formatting to match stata (sort of)
format_table <- . %>% 
  row_spec(row = 1, bold = T, hline_after = TRUE) %>% 
      kable_styling(font_size = 11) %>% 
                    #full_width = TRUE, 
                    #latex_options = c("repeat_header")) %>%
  str_replace("Num.Obs.", "Observations") %>% 
  str_replace("Std.Errors", "Robust/Clustered Std. Errors") %>% 
  str_replace("FE: Legislator_x_Agency", "Legislator x Agency FE") %>%
  str_replace("FE: Year_x_Agency", "Year x Agency FE") %>% 
  str_replace("FE: Year", "Year Fixed Effects") %>% 
  str_replace("FE: Legislator", "Legislator Fixed Effects") %>%
    str_replace_all("X|✓", "\\\\\\checkmark") %>% 
  # a random midrule appeared in the wrong place 
  #str_remove("\\midrule") %>% 
  #  extract just the table, no caption etc
  str_extract("\\\\begin\\{tabular\\}(\n|.)*tabular\\}") 


## ----m-total,  fig.show="hold", out.width="50%", fig.height=4.5, fig.cap= "Total Letters Per Year", fig.subcap= c("1","2","3","4")----
testing = F

# paper table 
# 1 
# cross-sectional 
m_total_cross <-feglm (perYear ~ 
                         chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency,
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary(m_total_cross, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_total_cross, horiz = T, drop = "(Intercept)") 



# 2
m_total_dnd <-feglm (perYear ~ 
                       chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator",
                    # ALT vcov = hetero ~ ssc(cluster.adj = TRUE),
           data = dcounts_tenure)

if(testing){modelsummary(m_total_dnd, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_total_dnd, horiz = T) 


# 3
m_total_2nd <-feglm (perYear ~ 
                       chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure %>% filter(survive == 1))

if(testing){modelsummary(m_total_2nd, gof_omit = "R.*", coef_omit = coef_omit)}
coefplot(m_total_2nd, horiz = T) 


# 4
m_logtotal_dnd <-feglm (log(perYear + 1) ~ 
                          chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary(m_logtotal_dnd, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_logtotal_dnd, horiz = T) 




## ----models_total---------------------------------------------------
models_total <- list(
  "(1)" = m_total_cross,
  "(2)" = m_total_dnd,
  "(3)" =  m_total_2nd,
  "(4)" = m_logtotal_dnd
)

rows <- tibble(
  term = c("Dependent Variable", 
           "Majority",
           "President's Party",
           "All Legislators", 
           "Served At Least 2nd Term"),
  `(1)` = c("Count", "✓", "✓", "✓", ""),
  `(2)` =c("Count", "✓", "✓", "✓", ""),
  `(3)` = c("Count", "✓","✓","", "✓"),
  `(4)` = c("Log(Count+1)","✓","✓", "✓", "") 
)

attr(rows, 'position') <- c(0, 20,21,22,23)


# HTML
modelsummary(models_total, stars = T, 
             add_rows = rows, 
             coef_map = cm,
             gof_omit = gof_omit,
             coef_omit = coef_omit,
             title = "The Effect of Expierence and Institutional Power on Legislator Contacts", 
             notes = list("Robust standard errors in parentheses, clustered by legislator.")) %>% row_spec(row = 1, bold = T, hline_after = TRUE) 

# TeX 
modelsummary(models_total, # stars = T, 
             add_rows = rows,               
             coef_map = cm, 
             gof_omit = gof_omit, 
             coef_omit = coef_omit,
             output = "latex",
             title = "\\\\footnotesize The Effect of Expierence and Institutional Power on Legislator Contacts", 
             notes = list("\\\\footnotesize Robust standard errors in parentheses, clustered by legislator.")) %>% 
  format_table() %>%
  write_lines(file = here::here("tables", "models_total.tex") )


beta <- m_total_dnd$coefficients %>%
  round(3) %>% 
  as_tibble(rownames = "beta") %>% 
  pivot_wider(names_from = beta)

se <- m_total_dnd$se %>%
  round(3) %>% as_tibble(rownames = "se") %>%
  pivot_wider(names_from = se)


## ----m-total-predicted,  fig.show="hold", out.width="50%", fig.cap= "Total", fig.subcap= c("1","2","3","4")----
predicted <- predictions(m_total_cross,
                         newdata = values)

# Cross-sectional predictions
predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
  ggchair() + 
  labs(title = "Predicted Total Letters per Year\n(Cross-Sectional)",
       x = "",
       y = "Predicted Total Letters Per Year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_total_cross,
                         newdata = values_tenure)

predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>% 
ggtenure() +
  labs(title = "Predicted Total Letters Per Year\n(Cross-Sectional)",
       x = "Years Serving in Congress",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 


# diff in diff 
predicted <- predictions(m_total_dnd,
                         newdata = values)


# predictions by chair and presidents' party
predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
  ggchair() + 
  labs(title = "Predicted Total Letters per Year\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_total_dnd,
                         newdata = values_tenure)

predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(title = "Predicted Total Letters Per Year\nDifference in Differences (Within-Legislator)",
       x = "Years Serving in Congress",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 


## ----m-con,  fig.show="hold", out.width="50%", fig.height=4.5, fig.cap="Constituency Service Letters Per Year", fig.subcap= c("1","2","3","4")----
testing = F

m_con_cross <-feglm (perYear_con ~ 
                       chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency,
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary(m_con_cross, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_con_cross, horiz = T, drop = "(Intercept)") 



# 2
m_con_dnd <-feglm (perYear_con ~ 
                     chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator",
           data = dcounts_tenure)

if(testing){modelsummary(m_con_dnd, gof_omit = "R.*", coef_omit = coef_omit)}
coefplot(m_con_dnd, horiz = T) 


# 3
m_con_2nd <-feglm (perYear_con ~ 
                     chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure %>% filter(survive == 1))

if(testing){modelsummary(m_con_2nd, gof_omit = "R.*", coef_omit = coef_omit)}
coefplot(m_con_2nd, horiz = T) 


# 4
m_logcon_dnd <-feglm (log(perYear_con + 1) ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary(m_logcon_dnd, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_logcon_dnd, horiz = T) 


## ----models_con-----------------------------------------------------
models_con <- list(
  "(1)" = m_con_cross,
  "(2)" = m_con_dnd,
  "(3)" =  m_con_2nd,
  "(4)" = m_logcon_dnd
)


modelsummary(models_con, stars = T, 
             add_rows = rows, 
             coef_map = cm,
             gof_omit = gof_omit, 
             coef_omit = coef_omit) %>% 
  row_spec(row = 1, bold = T, hline_after = TRUE) 

modelsummary(models_con, # stars = T, 
             add_rows = rows,               
             coef_map = cm, 
             gof_omit = gof_omit, 
             coef_omit = coef_omit,
             output = "latex",
             notes = list("\\\\footnotesize Robust standard errors in parentheses, clustered by legislator."))%>% 
  format_table() %>%
  write_lines(file = here::here("tables", "models_con.tex") )

beta <- m_con_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

se <- m_con_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)


## ----m-con-predicted,  fig.show="hold", out.width="50%", fig.cap= "Constituency Service", fig.subcap= c("1","2","3","4")----
# Cross-sectional predictions:
predicted <- predictions(m_con_cross,
                         newdata = values)


# A plot by chair & presidents' party
predicted %>%
  mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
  ggchair() + 
  labs(title = "Predicted Constituency Service Letters per Year\n(Cross-Sectional)",
       x = "",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_con_cross,
                         newdata = values_tenure)

predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(title = "Predicted Constituency Service Letters Per Year\n(Cross Sectional)",
       x = "Years Serving in Congress",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 



# Diff in diff predictions 
predicted <- predictions(m_con_dnd,
                         newdata = values)


# diff in diff plot
predicted %>%
  mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
  ggchair() + 
  labs(title = "Predicted Constituency Service Letters per Year\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_con_dnd,
                         newdata = values_tenure)

predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(title = "Predicted Constituency Service Letters Per Year\nDifference in Differences (Within-Legislator)",
       x = "Years Serving in Congress",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 


## ----m-policy,  fig.show="hold", out.width="50%", fig.height=4.5, fig.cap="Policy Letters Per Year", fig.subcap= c("1","2","3","4")----
testing = F

m_policy_cross <-feglm (perYear_policy ~ 
                          chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority +  presidents_party | Year_x_Agency,
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary(m_policy_cross, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_policy_cross, horiz = T, drop = "(Intercept)") 



# 2
m_policy_dnd <-feglm (perYear_policy ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator",
           data = dcounts_tenure)

if(testing){modelsummary(m_policy_dnd, gof_omit = "R.*", coef_omit = coef_omit)}
coefplot(m_policy_dnd, horiz = T) 


# 3
m_policy_2nd <-feglm (perYear_policy ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure %>% filter(survive == 1))

if(testing){modelsummary(m_tenure_2nd, gof_omit = "R.*", coef_omit = coef_omit)}
coefplot(m_policy_2nd, horiz = T) 


# 4
m_logcon_dnd <-feglm (log(perYear_policy + 1) ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary(m_logcon_dnd, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_logcon_dnd, horiz = T) 


## ----models_policy--------------------------------------------------
models_policy <- list(
  "(1)" = m_policy_cross,
  "(2)" = m_policy_dnd,
  "(3)" =  m_policy_2nd,
  "(4)" = m_logcon_dnd
)


modelsummary(models_policy, stars = T, 
             add_rows = rows, 
             coef_map = cm,
             gof_omit = gof_omit, 
             coef_omit = coef_omit,
             notes = list("Robust standard errors in parentheses, clustered by legislator." )) %>% 
  row_spec(row = 1, bold = T, hline_after = TRUE) 

modelsummary(models_policy, # stars = T, 
             add_rows = rows,               
             coef_map = cm, 
             gof_omit = gof_omit, 
             coef_omit = coef_omit,
             output = "latex",
             notes = list("\\\\footnotesize Robust standard errors in parentheses, clustered by legislator." )) %>% 
  format_table() %>%
   write_lines(file = here::here("tables", "models_policy.tex") )


## ----m-policy-predicted,  fig.show="hold", out.width="50%", fig.cap= "Policy", fig.subcap= c("1","2","3","4")----
predicted <- predictions(m_policy_cross,
                         newdata = values)

# Cross-sectional predictions
predicted %>%
  mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
  ggchair() +
  labs(title = "Predicted Policy Letters per Year\n(Cross-Sectional)",
       x = "",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_policy_cross,
                         newdata = values_tenure)

predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(title = "Predicted Policy Letters Per Year\n(Cross Sectional)",
       x = "Years Serving in Congress",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 


# Predictions for diff in diff plot 
predicted <- predictions(m_policy_dnd,
                         newdata = values)


# diff in diff plot
predicted %>%
  mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
  ggchair() +
  labs(title = "Predicted Policy Letters per Year\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "")  

# by tenure
predicted <- predictions(m_policy_dnd,
                         newdata = values_tenure)

predicted %>%
    mutate(predicted = predicted*90,
         std.error = std.error*90) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(title = "Predicted Policy Letters Per Year\nDifference in Differences (Within-Legislator)",
       x = "Years Serving in Congress",
       y = "Predicted Letters Per Year",
       fill = "",
       color = "",
       shape = "") 

beta <- m_policy_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

se <- m_policy_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)


## ----m-ratio,  fig.show="hold", out.width="50%", fig.height=4.5, fig.cap= "Ratio", fig.subcap= "c(1,2)"----
dcounts_ratio %<>% 
  mutate(Legislator = icpsr,
         Year = year) %>% 
  filter(year >2006 & year < 2019)

m_ratio_cross <- feglm(ratio ~
  chair + ranking_minority + prestige + majority +  presidents_party + 
  first + second + third + fourth + fifth + sixth | Year,
cluster = "Legislator",
data = dcounts_ratio)

if(testing){modelsummary(m_ratio_cross, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_ratio_cross, horiz = T, drop = "(Intercept)") 



# Diff in diff
m_ratio_dnd <- feglm(ratio ~
  chair + ranking_minority + prestige + majority + presidents_party + 
  first + second + third + fourth + fifth + sixth | 
    Legislator + Year,
cluster = "Legislator",
data = dcounts_ratio)

if(testing){modelsummary(m_ratio_dnd, gof_omit = "R.*", coef_omit = coef_omit)}

coefplot(m_ratio_dnd, horiz = T) 


## ----models_ratio---------------------------------------------------
models_ratio <- list(
  "(1)" = m_ratio_cross,
  "(2)" = m_ratio_dnd
)

rows <- tibble(
  term = c("Dependent Variable", "Majoirty", "President's Party"),
  `(1)` = c("Ratio", "✓", "✓"),
  `(2)` =c("Ratio", "✓", "✓")
)

attr(rows, 'position') <- c(0, 20,21)

modelsummary(models_ratio, stars = T, 
             add_rows = rows, 
             coef_map = cm,
             gof_omit = gof_omit, 
             coef_omit = coef_omit,
             notes = list("Robust standard errors in parentheses, clustered by legislator.") ) %>% row_spec(row = 1, bold = T, hline_after = TRUE) 

modelsummary(models_ratio, # stars = T, 
             add_rows = rows,               
             coef_map = cm, 
             gof_omit = gof_omit, 
             coef_omit = coef_omit,
             output = "latex",
             notes = list("\\\\footnotesize Robust standard errors in parentheses, clustered by legislator.") ) %>% 
  # format to match stata output (sort of)
  format_table() %>%
   write_lines(file = here::here("tables", "models_ratio.tex") )

beta <- m_ratio_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

se <- m_ratio_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)


## ----m-ratio-predicted, fig.show="hold", out.width="50%", fig.cap= "Ratio", fig.subcap= c("1","2","3","4")----
predicted <- predictions(m_ratio_cross,
                         newdata = values)



# Cross-sectional predictions
predicted %>%
ggchair() +
  labs(title = "Predicted Ratio of Constituent to Policy Work\n(Cross-Sectional)",
       x = "",
       y = "Predicted Ratio of\nConstituent to Policy Work",
       fill = "",
       color = "",
       shape = "")

# by tenure
predicted <- predictions(m_ratio_cross,
                         newdata = values_tenure)

predicted %>%
ggtenure() +
  labs(title = "Predicted Ratio of Constituent to Policy Work\n(Cross-Sectional)",
       x = "Years Serving in Congress",
       y = "Predicted Ratio of\nConstituent to Policy Work",
       fill = "",
       color = "",
       shape = "")


# Predictions for diff in diff plot 
predicted <- predictions(m_ratio_dnd,
                         newdata = values)


# diff in diff plot
predicted %>%
  ggchair() + 
  labs(title = "Predicted Ratio of Constituent to Policy Work\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "Predicted Ratio of\nConstituent to Policy Work",
       fill = "",
       color = "",
       shape = "")

# by tenure
predicted <- predictions(m_ratio_dnd,
                         newdata = values_tenure)

predicted %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(title = "Predicted Ratio of Constituent to Policy Work\nDifference in Differences (Within-Legislator)",
       x = "Years Serving in Congress",
       y = "Predicted Ratio of\nConstituent to Policy Work",
       fill = "",
       color = "",
       shape = "")


## ----m-district,  fig.show="hold", out.width="50%"------------------



