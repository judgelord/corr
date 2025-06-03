# README


Replication code for “The Effects of Shifting Priorities and Capacity on
Congressional Policy Work and Constituency Service: Evidence from a
Census of Legislator Requests to U.S. Federal Agencies”

## Data files:

### The main replication data

1.  Counts of legislator contacts per year per agency:
    “data/corr_counts.Rdata” (on dataverse)

### Required data on members of Congress

1.  legislator covariates: “data/member_data.rda” including:

-   year, chamber, and party retrieved from voteview.com (also available
    on dataverse)

-   committee positions from Stewart and Woon (2017) and the
    @unitedstates-project (2025)

-   state population from the US Census

## Code files

-   “docs/replication.qmd” uses `corr_counts` and `member data` to
    reproduce all analyses in the paper and Supplemental Information
    (rendered [here](https://judgelord.github.io/corr/replication))

## Data details

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

-   “icpsr” = Legislator id  
-   “chamber” = “House” or “Senate”  
-   “agency” = Agency acronym  
-   “year” = Year  
-   “TYPE” = Type of Legislator Request. See codebook in Suplimental
    Information.
    -   1 ~ “Constituent (individual)”
    -   2 ~ “Constituent (corporate)”
    -   3 ~ “Constituent (501c3 or localgovernment)”
    -   4 ~ “Policy (corporate)”
    -   5 ~ “Policy (general)”
-   “per_icpsr_chamber_year_agency_type” = Count per legislator per year
    per agency per type

``` r
load(here::here("data", "member_data.Rdata"))

dim(member_data)
```

    [1] 3865   17

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
      ranking_minority majority presidents_party party nominate_dim1 nominate_dim2
    1                0        0                1   (R)         0.367         0.513
    2                0        0                1   (R)         0.379         0.377
    3                0        1                0   (D)        -0.270         0.454
    4                0        1                0   (D)        -0.132         0.612
    5                0        0                1   (R)         0.414         0.528
    6                1        0                1   (R)         0.387         0.228

Stewart, Charles, III, and Jonathan Woon. 2017.
“<span class="nocase">Congressional Committee Assignments, 103rd to
115th Congresses, 1993–2017: House of Representatives</span>.”

the @unitedstates-project. 2025.
“<span class="nocase">unitedstates/congress-legislators</span>.”
