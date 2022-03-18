Replication code for "The Effects of Shifting Priorities and Capacity on Congressional Policy Work and Constituency Service: Evidence from a Census of Legislator Requests to U.S. Federal Agencies"

`code/replication_data.R ` combines three data sets to create the data required for each analysis. 

1. Counts of legislator contacts per year per agency `dcounts_min.Rdata`
2. Legislator-agency oversight relationships, `agency_vars.Rdata`
3. Legislator covariates, `members.Rdata`, retrieved from voteview using code in `code/nominate.R`

```
load(here::here("data", "dcounts_min.Rdata"))
load(here::here("data", "agency_vars.Rdata"))
load(here::here("data", "members.Rdata"))
```


Identical regression results can be replicated in R or Stata for all of the main models:
- R: `code/replication.R`
- Stata: `code/replication.do`

The appendix models are only in `replication.R`. 