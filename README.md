Replication code for "The Effects of Shifting Priorities and Capacity on Congressional Policy Work and Constituency Service: Evidence from a Census of Legislator Requests to U.S. Federal Agencies"

## Data files: 

###  The main replication data 

1. Counts of legislator contacts per year per agency: "data/corr_counts.Rdata" (on dataverse)

### Required data on members of Congress

2. legislator covariates: "data/members.Rdata" retrieved from voteview.com with code in `code/nominate.R`  (also available on dataverse)


## Code files

1. "docs/replication.qmd" uses `corr_counts` and `members` data to reproduce all analyses in the paper and Supplemental Information (rendered [here](https://judgelord.github.io/corr/replication))
2. "docs/descriptives.qmd" uses `corr_counts` and `members` data to produce all descriptive figures in the paper (rendered [here](https://judgelord.github.io/corr/descriptives)) 


