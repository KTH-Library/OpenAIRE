
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

openaire("projects", page_size = 10) 
#> # A tibble: 10 × 10
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
#> # … with 1 more variable: `End Date` <date>, and abbreviated variable names
#> #   ¹​`Project title`, ²​`Project Acronym`, ³​`Project ID`, ⁴​`Funding Stream`,
#> #   ⁵​`Funding Substream level 1`, ⁶​`Funding Substream level 2`, ⁷​`Start Date`

openaire("projects", params = api_params(
  size = 3, format = "tsv", proj_org = "Royal Institute of Technology")
) |> knitr::kable()
```

| Project title                                                                                                             | Project Acronym    | Project ID         | Funder | Funding Stream     | Funding Substream level 1 | Funding Substream level 2 | SC39  | Start Date | End Date   |
|:--------------------------------------------------------------------------------------------------------------------------|:-------------------|:-------------------|:-------|:-------------------|:--------------------------|:--------------------------|:------|:-----------|:-----------|
| Exploring a New Approach to Bioethic - combining and contrasting key-features of early buddhist ethics and virtue ethics. | NA                 | 095012             | WT     | Medical Humanities | NA                        | NA                        | FALSE | 2011-03-01 | 2011-05-31 |
| Design and synthesis of dendritic prodrugs                                                                                | NA                 | 112366             | AKA    | NA                 | NA                        | NA                        | FALSE | 2006-01-01 | 2007-12-31 |
| UNDERSTANDING PRIVACY WITHIN EMERGING NET CULTURES                                                                        | SFRH/BD/60803/2009 | SFRH/BD/60803/2009 | FCT    | FARH               | NA                        | NA                        | FALSE | 2010-03-01 | 2014-02-28 |

``` r

openaire("datasets", params = api_params(size = 1, format = "tsv")) |> 
  knitr::kable()
```

| Title                                                            | Authors                                                                                              | Publication Year | DOI              | Download From                      | Publication type | Journal | Funder | Project Name (GA Number) | Access |
|:-----------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------|:-----------------|:-----------------|:-----------------------------------|:-----------------|:--------|:-------|:-------------------------|:-------|
| Schweizerisches Landesforstinventar - Ergebnistabelle Nr. 843438 | Abegg, M.;Brändli, U.-B.;Cioldi, F.;Fischer, C.;Herold, A.;Meile, R.;Rösler, E.;Speich, S.;Traub, B. | 2020-01-01       | 10.21258/1443627 | <https://doi.org/10.21258/1443627> | Dataset          | NA      | NA     | NA                       | NA     |

## Paging

A function provides crawling results, page by page:

``` r

openaire_crawl("projects", params = api_params(
  size = 50,
  format = "tsv", 
  proj_country = "SE",
  proj_org = "Royal Institute of Technology")
)
#> Fetching 801 hits in 17 batches of 50 records
#> Warning in force(otherwise): probably incompatible data types across chunks
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
