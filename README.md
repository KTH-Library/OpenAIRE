
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OpenAIRE

<!-- badges: start -->

[![R-CMD-check](https://github.com/KTH-Library/OpenAIRE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KTH-Library/OpenAIRE/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

OpenAIRE is an open science initiative, which supports the Open Access
policy of the European Commission. This R package `OpenAIRE` provides
access from within R to the OpenAIRE RESTful APIs, which are intended
for metadata discovery and exploration only.

## Installation

You can install the development version of OpenAIRE like so:

``` r
# install.packages("devtools")
devtools::install_github("KTH-Library/OpenAIRE")
```

## Example

This is a basic example which shows you how to retrieve data from the
OpenAIRE search APIs:

``` r
library(OpenAIRE)

# all projects
openaire("projects") 
#> # A tibble: 50 × 10
#>    Project tit…¹ Proje…² Proje…³ Funder Fundi…⁴ Fundi…⁵ Fundi…⁶ SC39  Start Da…⁷
#>    <chr>         <chr>   <chr>   <chr>  <chr>   <chr>   <chr>   <lgl> <date>    
#>  1 ONCOGENE ACT… <NA>    N01ES0… NIH    NATION… <NA>    <NA>    FALSE 1986-09-30
#>  2 Semiparametr… <NA>    5R21ES… NIH    NATION… <NA>    <NA>    FALSE 2011-02-01
#>  3 SOUTHWEST ON… <NA>    5U10CA… NIH    NATION… <NA>    <NA>    FALSE 1978-01-01
#>  4 Biological R… <NA>    5R01GM… NIH    NATION… <NA>    <NA>    FALSE 1997-07-01
#>  5 MECHANISM OF… <NA>    5R01CA… NIH    NATION… <NA>    <NA>    FALSE 1995-04-01
#>  6 Mechanisms o… <NA>    1R21AI… NIH    NATION… <NA>    <NA>    FALSE 2012-02-06
#>  7 CORONARY ART… <NA>    1R01HL… NIH    NATION… <NA>    <NA>    FALSE 1987-09-30
#>  8 LAMPoles for… <NA>    7R21AI… NIH    NATION… <NA>    <NA>    FALSE 2015-07-01
#>  9 CHILDRENS IN… <NA>    2R44HL… NIH    NATION… <NA>    <NA>    FALSE 2002-09-01
#> 10 BIOBEHAVIORA… <NA>    5F31MH… NIH    NATION… <NA>    <NA>    FALSE 1994-09-01
#> # … with 40 more rows, 1 more variable: `End Date` <date>, and abbreviated
#> #   variable names ¹​`Project title`, ²​`Project Acronym`, ³​`Project ID`,
#> #   ⁴​`Funding Stream`, ⁵​`Funding Substream level 1`,
#> #   ⁶​`Funding Substream level 2`, ⁷​`Start Date`
```

### Filtering results

Parameters can be specified to filter search results:

``` r
# use parameters to filter projects
openaire("projects", params = api_params(
  format = "tsv", 
  proj_country = "SE",
  proj_org = "Royal Institute of Technology",
)) 
#> # A tibble: 800 × 10
#>    Project tit…¹ Proje…² Proje…³ Funder Fundi…⁴ Fundi…⁵ Fundi…⁶ SC39  Start Da…⁷
#>    <chr>         <chr>   <chr>   <chr>  <chr>   <chr>   <chr>   <lgl> <date>    
#>  1 A Higher Dim… <NA>    158937  SNSF   Careers Fellow… Doc.Mo… FALSE 2015-11-01
#>  2 Modeling and… <NA>    181788  SNSF   Careers Fellow… Early … FALSE 2018-08-01
#>  3 Determining … <NA>    EP/R02… UKRI   EPSRC   <NA>    <NA>    FALSE 2018-08-12
#>  4 Topological … <NA>    EP/P02… UKRI   EPSRC   <NA>    <NA>    FALSE 2017-08-31
#>  5 Datasounds, … <NA>    AH/V01… UKRI   AHRC    <NA>    <NA>    FALSE 2022-01-04
#>  6 NI: Microbia… <NA>    NE/S00… UKRI   NERC    <NA>    <NA>    FALSE 2018-12-31
#>  7 Nanodevices … NAQUOP  307687  EC     FP7     SP2     ERC     FALSE 2013-04-01
#>  8 Fiktionalitä… <NA>    119410  SNSF   Careers Intern… <NA>    FALSE 2007-10-01
#>  9 Fluid Spectr… FSA     308267  EC     FP7     SP2     ERC     FALSE 2012-11-01
#> 10 Highly Multi… MULTIP… 615458  EC     FP7     SP2     ERC     FALSE 2014-04-01
#> # … with 790 more rows, 1 more variable: `End Date` <date>, and abbreviated
#> #   variable names ¹​`Project title`, ²​`Project Acronym`, ³​`Project ID`,
#> #   ⁴​`Funding Stream`, ⁵​`Funding Substream level 1`,
#> #   ⁶​`Funding Substream level 2`, ⁷​`Start Date`
```

### Paging

A function provides crawling results, page by page:

``` r

openaire_crawl("projects", page_size = 100, params = api_params(
  format = "xml", 
  proj_country = "SE",
  proj_org = "Royal Institute of Technology")
)
#> Fetching approximately 801 hits in 9 batches of 100 records
#> # A tibble: 801 × 22
#>    collect…¹ origi…² code  title proje…³ beg_d…⁴ end_d…⁵ durat…⁶ ec_ar…⁷ oa_is…⁸
#>    <chr>     <chr>   <chr> <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#>  1 SNSF - S… snsf__… PA00… Nume… Numeri… 2007-0… 2007-0… 0       false   false  
#>  2 SNSF - S… snsf__… 1589… A Hi… A High… 2015-1… 2016-0… 0       false   false  
#>  3 SNSF - S… snsf__… 1817… Mode… Modeli… 2018-0… 2020-0… 0       false   false  
#>  4 UK Resea… ukri__… EP/R… Dete… Determ… 2018-0… 2021-0… 0       false   false  
#>  5 UK Resea… ukri__… EP/P… Topo… Topolo… 2017-0… 2021-0… 0       false   false  
#>  6 UK Resea… ukri__… AH/V… Data… Dataso… 2022-0… 2023-0… 0       false   false  
#>  7 UK Resea… ukri__… NE/S… NI: … NI: Mi… 2018-1… 2023-0… 0       false   false  
#>  8 CORDA - … corda_… 3076… Nano… Nanode… 2013-0… 2018-0… 0       false   false  
#>  9 SNSF - S… snsf__… 1194… Fikt… Fiktio… 2007-1… 2007-1… 0       false   false  
#> 10 CORDA - … corda_… 3082… Flui… Fluid … 2012-1… 2017-1… 0       false   false  
#> # … with 791 more rows, 12 more variables: ec_sc_39 <chr>, summary <chr>,
#> #   cost <chr>, funded_amount <chr>, currency <chr>, funder_shortname <chr>,
#> #   funder_name <chr>, funder_jurisdiction <chr>, funding_level_0_name <chr>,
#> #   data_inferred <chr>, data_deleted <chr>, data_trust <chr>, and abbreviated
#> #   variable names ¹​collected_from, ²​original_id, ³​project_title, ⁴​beg_date,
#> #   ⁵​end_date, ⁶​duration, ⁷​ec_art_293, ⁸​oa_is_mandated

# Convert list results into dataframe
open_aire_kth <- do.call("rbind",
  openaire_crawl("projects", page_size = 100,
  params = api_params(format = "xml", proj_country = "SE",proj_org = "Royal Institute of Technology")))
```
