# How Shifting Priorities and Capacity Affect Policy Work and Constituency Service: Evidence from a Census of Legislator Requests to U.S. Federal Agencies
Devin Judge-Lord, Eleanor Neff Powell, Justin Grimmer

Manuscript: <https://judgelord.github.io/research/correspondence/>

Replication data on Dataverse: <https://doi.org/10.7910/DVN/LWOCW>

Project repo: <https://judgelord.github.io/correspondence/>

# Instructions for replication

1.  Save all files and folders from this dataverse project to the root-level of a project directory and ensure it is the active directory in R. This allows the `here` package to construct valid file paths.
    -   On dataverse, click Access Dataset and download the Original Format ZIP. The default download file will be called “dataverse_files.zip”
    -   Unzip the downloaded folder
    -   In RStudio, make a new project (File → New Project), select Existing Directory, and select the new unzipped folder “dataverse_files”
2.  run (render) “replication.qmd” to generate all figures and results
3.  run (render) “corr.qmd” to render manuscript PDF (“corr.pdf”)
4.  run (render) “si.qmd” to render Supplemental Information pdf (“si.pdf”)

Alternatively to step 2, if you only wish to reproduce the figures and results and not the manuscript, you can just run “replication.r”—the R code extracted from replication.qmd. Running “replication.r” will not save the figures in the /figs/ folder as required to render the manuscript.

If you are not working in an RStudio project, replication.qmd may still render, but replication.r will likely require a line pointing to the file path where you saved the dataverse files (e.g., `here::i_am(path = "Downloads/dataverse_files")` )

# Data files (in /data/ folder on Dataverse)

The main replication data

-   “data/corr_counts.Rdata” = Counts of legislator contacts per year per agency
    -   see complete codebook and coding procedure in Supplemental Information Part B (SI.pdf)

Required data on members of Congress

-   “data/member_data.rda” = legislator covariates

# Dataset codebooks (in /docs/ folder)

## 1. Counts of legislator contacts per year per agency

Codebook: “docs/corr_counts_codebook.pdf”

Data file: “data/corr_counts.Rdata”

Citation: Author-created data source (Devin Judge-Lord and Eleanor Neff Powell and Justin Grimmer 2025)

Variable names, descriptive labels, and values:

-   “icpsr” = Legislator id  
-   “chamber” = “House” or “Senate”  
-   “agency” = Agency acronym  
-   “year” = Year \[2007:2020\]  
-   “TYPE” = Type of Legislator Request. See codebook in Supplemental Information.
    -   1 ~ “Constituent (individual)”
    -   2 ~ “Constituent (corporate)”
    -   3 ~ “Constituent (501c3 or localgovernment)”
    -   4 ~ “Policy (corporate)”
    -   5 ~ “Policy (general)”
-   “per_icpsr_chamber_year_agency_type” = Count per legislator per year per agency per type \[0:1440\]

``` r
load(here::here("data", "corr_counts.Rdata"))

dim(corr_counts)
```

    [1] 4139982       6

``` r
head(corr_counts, 7)
```

      icpsr chamber agency year TYPE per_icpsr_chamber_year_agency_type
    1 10713   House   ABMC 2016                                       0
    2 10713   House   ABMC 2016    0                                  0
    3 10713   House   ABMC 2016    1                                  0
    4 10713   House   ABMC 2016    2                                  0
    5 10713   House   ABMC 2016    3                                  0
    6 10713   House   ABMC 2016    4                                  0
    7 10713   House   ABMC 2016    5                                  0

## 2. Data on members of Congress

Codebook: “docs/member_data_codebook.pdf”

Data file: “data/member_data.Rdata”

Citations: Author-created data source (Devin Judge-Lord and Eleanor Neff Powell and Justin Grimmer 2025), including:

-   year, chamber, and party from J. B. Lewis et al. (2022) via voteview.com (also available on dataverse)

-   committee positions from Stewart and Woon (2017) and @unitedstates-project (2025)

-   state population from U.S. Census Bureau (2019)

Variable names, descriptive labels, and values:

-   “congress” = Congress \[110-116\]
-   “chamber” = “House” or “Senate”
-   “bioname” = name from voteview.com
-   “first_year” = First year serving in Congress \[1989:2020\]
-   “icpsr” = ICPSR id number from voteview.com
-   “district_code” = District number \[0:53\]
-   “state_abbrev” = State abbreviation
-   “state” = State name
-   “pop2010” = State population in 2010 \[563,767:37,252,895\] from U.S. Census Bureau (2019)
-   “committees” = Committee assignments from Stewart and Woon (2017) and @unitedstates-project (2025)
-   “chair” = Committee chair \[0 = no, 1 = yes\] from Stewart and Woon (2017) and @unitedstates-project (2025)
-   “ranking_minority” = Ranking minority \[0 = no, 1 = yes\]
-   “majority” = Majority party \[0 = no, 1 = yes\]
-   “presidents_party” = Same party as the president \[0 = no, 1 = yes\]
-   “party” = Political party \[“(D)” = Democrat, “(R)” = Republican, “(I)” = Independent\]

For additional variables that are part of the broader data from this project but not required to replicate the analysis for this paper, including data on oversight relationships and agency characteristics from D. E. Lewis and Selin (2012), please contact the authors.

``` r
load(here::here("data", "member_data.Rdata"))

dim(member_data)
```

    [1] 3865   15

``` r
head(member_data)
```

      congress chamber                         bioname first_year icpsr
    1      110   House BONNER, Jr., Josiah Robins (Jo)       2003 20300
    2      110   House             ROGERS, Mike Dennis       2003 20301
    3      110   House                    DAVIS, Artur       2003 20302
    4      110   House    CRAMER, Robert E. (Bud), Jr.       1991 29100
    5      110   House           EVERETT, Robert Terry       1993 29300
    6      110   House         BACHUS, Spencer T., III       1993 29301
      district_code state_abbrev   state pop2010
    1             1           AL alabama 4780127
    2             3           AL alabama 4780127
    3             7           AL alabama 4780127
    4             5           AL alabama 4780127
    5             2           AL alabama 4780127
    6             6           AL alabama 4780127
                                                                   committees chair
    1 AGRICULTURE;APPROPRIATIONS;BUDGET;SCIENCE;STANDARDS OF OFFICIAL CONDUCT     0
    2                            AGRICULTURE;ARMED SERVICES;HOMELAND SECURITY     0
    3   HOUSE ADMINISTRATION;JUDICIARY;WAYS;VOTING IRREGULARITIES OF AUGUST 2     0
    4                                             APPROPRIATIONS;INTELLIGENCE     0
    5                                 AGRICULTURE;ARMED SERVICES;INTELLIGENCE     0
    6                                                      FINANCIAL SERVICES     0
      ranking_minority majority presidents_party party
    1                0        0                1   (R)
    2                0        0                1   (R)
    3                0        1                0   (D)
    4                0        1                0   (D)
    5                0        0                1   (R)
    6                1        0                1   (R)

``` r
member_data |> skimr::skim()
```

# Code files

-   “replication.qmd” uses `corr_counts` and `member data` to reproduce all analyses in the paper and Supplemental Information (rendered [here](https://judgelord.github.io/corr/replication))
    -   **NOTE:** This file produces intermediate data files saved to /data/, figures saved to /figs/, and model objects saved to /models/ directories, which are required to render the manuscript and SI. In addition to all figures in the paper, these intermediate files include:
        -   data/means.Rdata
        -   models/models_total.Rdata
        -   models/models_ratio.Rdata
        -   models/models_con.Rdata
        -   models/models_policy.Rdata
        -   models/models_district.Rdata
        -   models/models_district_policy.Rdata
        -   models/models_district_con.Rdata
        -   models/models_district_party.Rdata
        -   models/models_spillover.Rdata
        -   models/models_spillover_con.Rdata
        -   models/models_spillover_policy.Rdata
    -   “replication.r” is simply the R code extracted from “replication.qmd” — it will produce but not save figures

# Manuscript files

-   “corr.qmd” compiles the manuscript text, pulling in results from the intermediate files created by running replication.qmd
    -   “assets/congress2019.bib” is the required bib file
-   “si.qmd” compiles the Supplemental Information text, pulling in saved results from running replication.qmd
    -   “assets/congress2019.bib” is the required bib file
    -   “FOIA.csv” is a required data summary table

# Output files

-   “replication.pdf” is “replication.qmd” rendered to PDF
-   “replication.html” is “replication.qmd” rendered to HTML
-   “corr.pdf” is the manuscript text produced by “corr.qmd”
-   “si.pdf” is the Supplemental Information text produced by “si.qmd”

# Computing Environment

-   Computer Processor: Apple M2 Max, 12 Cores
-   Computer Memory (RAM): 96 GB

## Software

-   RStudio 2025.05.1+513

**NOTE:** Due to breaking changes in version 0.28.0 if the `marginaleffects` package, the code to make all figures using predicted values now requires loading an earlier version of this package. I used `marginaleffects_0.25.1`. One [way to do this](https://stackoverflow.com/questions/17082341/how-to-downgrade-an-r-package-by-installing-an-older-version):

``` r
if(packageVersion("marginaleffects") != "0.25.1"){
remove.packages("marginaleffects")
packageurl <- "http://cran.r-project.org/src/contrib/Archive/marginaleffects/marginaleffects_0.25.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
}
```

``` r
load(here::here("data", "sessionInfo.rda"))

sessionInfo
```

    R version 4.5.1 (2025-06-13)
    Platform: aarch64-apple-darwin20
    Running under: macOS Sequoia 15.6

    Matrix products: default
    BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
    LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

    time zone: America/Detroit
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] scales_1.4.0           ggrepel_0.9.6          kableExtra_1.4.0      
     [4] ineq_0.2-13            magrittr_2.0.3         lubridate_1.9.4       
     [7] forcats_1.0.0          stringr_1.5.1          dplyr_1.1.4           
    [10] purrr_1.0.4            readr_2.1.5            tidyr_1.3.1           
    [13] tibble_3.3.0           ggplot2_3.5.2          tidyverse_2.0.0       
    [16] fixest_0.12.1          marginaleffects_0.25.1 modelsummary_2.3.0.3  

    loaded via a namespace (and not attached):
     [1] tidyselect_1.2.1     viridisLite_0.4.2    farver_2.1.2        
     [4] fastmap_1.2.0        bayestestR_0.15.2    digest_0.6.37       
     [7] timechange_0.3.0     lifecycle_1.0.4      dreamerr_1.5.0      
    [10] compiler_4.5.1       rlang_1.1.6          sass_0.4.10         
    [13] tools_4.5.1          utf8_1.2.6           data.table_1.17.0   
    [16] knitr_1.50           ggsignif_0.6.4       labeling_0.4.3      
    [19] here_1.0.1           xml2_1.3.8           RColorBrewer_1.1-3  
    [22] pkgload_1.4.0        abind_1.4-8          tinytable_0.8.0     
    [25] withr_3.0.2          numDeriv_2016.8-1.1  grid_4.5.1          
    [28] datawizard_1.0.2     fansi_1.0.6          ggpubr_0.6.0        
    [31] insight_1.2.0.1      cli_3.6.5            rmarkdown_2.29      
    [34] crayon_1.5.3         generics_0.1.4       rstudioapi_0.17.1   
    [37] performance_0.13.0.6 tzdb_0.5.0           parameters_0.24.2.17
    [40] cachem_1.1.0         splines_4.5.1        stringmagic_1.2.0   
    [43] vctrs_0.6.5          Matrix_1.7-3         sandwich_3.1-1      
    [46] jsonlite_2.0.0       carData_3.0-5        litedown_0.7        
    [49] car_3.1-3            hms_1.1.3            rstatix_0.7.2       
    [52] Formula_1.2-5        systemfonts_1.2.2    jquerylib_0.1.4     
    [55] glue_1.8.0           stringi_1.8.7        gtable_0.3.6        
    [58] tables_0.9.31        pillar_1.10.2        htmltools_0.5.8.1   
    [61] R6_2.6.1             rprojroot_2.0.4      evaluate_1.0.3      
    [64] lattice_0.22-7       backports_1.5.0      broom_1.0.8         
    [67] bslib_0.9.0          Rcpp_1.0.14          svglite_2.1.3       
    [70] nlme_3.1-168         checkmate_2.3.2      mgcv_1.9-3          
    [73] xfun_0.52            zoo_1.8-14           pkgconfig_2.0.3     

Note: running replication.qmd or replication.r will install the required packages to render the manuscript, including `here`, `scales`, and `modelsummary`, but it will not update packages that you already have installed. You may get errors if you have versions of packages that are older than those above. For example, the “counts-per-year” figure requires ggplot2 v3.5 or higher.

The following packages may need to be installed to render qmd files (not required to merely reproduce the results using replication.r)

    base64enc
    digest
    evaluate
    glue
    highr
    htmltools
    jsonlite
    markdown
    mime
    rmarkdown
    stringi
    stringr
    xfun
    yaml

## Time to run replication.qmd

``` r
load(here::here("data", "time.rda"))
time
```

    Time difference of 1.751205 mins

# References

Devin Judge-Lord and Eleanor Neff Powell and Justin Grimmer. 2025. “The Effects of Shifting Priorities and Capacity on Elected Officials’ Policy Work and Constituency Service: Evidence from a Census of Legislator Requests to u.s. Federal Agencies, Replication Data.” *American Journal of Political Science*. Harvard Dataverse Network, at: <https://doi.org/10.7910/DVN/LWOCW>.

Lewis, David E., and Jennifer L. Selin. 2012. *<span class="nocase">ACUS Sourcebook of United States Executive Agencies</span>*. Administrative Conference of the United States.

Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet. 2022. “Voteview: Congressional Roll-Call Votes Database.” <https://voteview.com>. Date Accessed: Feb. 24, 2022.

Stewart, Charles, III, and Jonathan Woon. 2017. “Congressional Committee Assignments, 103rd to 115th Congresses, 1993–2017: House of Representatives.” <https://web.mit.edu/17.251/www/data_page.html#2>. Date Accessed: March 18, 2025.

@unitedstates-project, the. 2025. “Unitedstates/Congress-Legislators.” <https://unitedstates.github.io/> <https://github.com/unitedstates/congress-legislators>. Date Accessed: March 18, 2025.

U.S. Census Bureau. 2019. “State Population Totals: 2010-2019.” <https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html>. Date Accessed: March 18, 2025.
