# README


Replication code for “The Effects of Shifting Priorities and Capacity on
Congressional Policy Work and Constituency Service: Evidence from a
Census of Legislator Requests to U.S. Federal Agencies”

## Data files:

### The main replication data

1.  Counts of legislator contacts per year per agency:
    “data/corr_counts.Rdata” (on dataverse)

### Required data on members of Congress

1.  legislator covariates: “data/member_data.rda” including

-   year, chamber, and party retrieved from voteview.com (also available
    on dataverse)

-   committee positions from Stewart and Woon (2017) and
    @unitedstates-project (2025)

## Code files

1.  “docs/replication.qmd” uses `corr_counts` and `member data` to
    reproduce all analyses in the paper and Supplemental Information
    (rendered [here](https://judgelord.github.io/corr/replication))
2.  “docs/descriptives.qmd” uses `corr_counts` and `member_data` to
    produce all descriptive figures in the paper (rendered
    [here](https://judgelord.github.io/corr/descriptives))

## Data details

``` r
load(here::here("data", "corr_counts.Rdata"))

names(corr_counts)
```

    [1] "icpsr"                              "chamber"                           
    [3] "agency"                             "year"                              
    [5] "TYPE"                               "per_icpsr_chamber_year_agency_type"

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

-   “icpsr” = Legislator id  
-   “chamber” = “House” or “Senate”  
-   “agency” = Agency acronym  
-   “year” = Year  
-   “TYPE” = Type of Legislator Request. See codebook in Suplimental
    Information.  
-   “per_icpsr_chamber_year_agency_type” = Count per legislator per year
    per agency per type

``` r
load(here::here("data", "member_data.Rdata"))

names(member_data)
```

     [1] "congress"         "chamber"          "bioname"          "first_year"      
     [5] "icpsr"            "district_code"    "state_abbrev"     "state"           
     [9] "pop2010"          "committees"       "chair"            "ranking_minority"
    [13] "majority"         "presidents_party" "party"           

-   “congress” = Congress  
-   “chamber” = “House” or “Senate”  
-   “bioname” = name from voteview.com  
-   “first_year” = First year serving in Congress  
-   “icpsr” = ICPSR id number from voteview.com  
-   “district_code” = District number  
-   “state_abbrev” = State abbreviation  
-   “state” = State name  
-   “pop2010” = State population in 2010  
-   “committees” = Committee assignments  
-   “chair” = Committee chair \[0 = no, 1 = yes\]  
-   “ranking_minority” = Ranking minority \[0 = no, 1 = yes\]
-   “majority” = Majority party \[0 = no, 1 = yes\]
-   “presidents_party” = Same party as the president \[0 = no, 1 = yes\]
-   “party” = Political party

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
    1 AGRICULTURE|APPROPRIATIONS|BUDGET|SCIENCE|STANDARDS OF OFFICIAL CONDUCT     0
    2                            AGRICULTURE|ARMED SERVICES|HOMELAND SECURITY     0
    3   HOUSE ADMINISTRATION|JUDICIARY|WAYS|VOTING IRREGULARITIES OF AUGUST 2     0
    4                                             APPROPRIATIONS|INTELLIGENCE     0
    5                                 AGRICULTURE|ARMED SERVICES|INTELLIGENCE     0
    6                                                      FINANCIAL SERVICES     0
      ranking_minority majority presidents_party party
    1                0        0                1   (R)
    2                0        0                1   (R)
    3                0        1                0   (D)
    4                0        1                0   (D)
    5                0        0                1   (R)
    6                1        0                1   (R)

Stewart, Charles, III, and Jonathan Woon. 2017.
“<span class="nocase">Congressional Committee Assignments, 103rd to
115th Congresses, 1993–2017: House of Representatives</span>.”

@unitedstates-project, the. 2025.
“<span class="nocase">unitedstates/congress-legislators</span>.”
<https://unitedstates.github.io/>.
