
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

This is a basic example which shows you how to solve a common problem:

``` r
library(OpenAIRE)

openaire_projects()
#> # A tibble: 521 × 10
#>    Project tit…¹ Proje…² Proje…³ Funder Fundi…⁴ Fundi…⁵ Fundi…⁶ SC39  Start Da…⁷
#>    <chr>         <chr>   <chr>   <chr>  <chr>   <chr>   <chr>   <lgl> <date>    
#>  1 Development … <NA>    151642  SNSF   Careers "Fello… Early … FALSE 2013-12-01
#>  2 Topological … <NA>    EP/P02… UKRI   EPSRC    <NA>   <NA>    FALSE 2017-08-31
#>  3 Determining … <NA>    EP/R02… UKRI   EPSRC    <NA>   <NA>    FALSE 2018-08-12
#>  4 Auto-organis… <NA>    68422   SNSF   Careers "Fello… Fellow… FALSE 2002-09-01
#>  5 Datasounds, … <NA>    AH/V01… UKRI   AHRC     <NA>   <NA>    FALSE 2022-01-04
#>  6 New Paradigm… M&M´S   277879  EC     FP7     "SP2"   ERC     FALSE 2011-11-01
#>  7 Virus-host i… VISIBLE 101064… EC     HE      "HORIZ… <NA>    FALSE 2022-12-01
#>  8 Purely Nonli… PNPCS   234798  EC     FP7     "SP3"   PEOPLE  FALSE 2009-05-01
#>  9 Visualizing … VISUAL… 618558  EC     FP7     "SP3"   PEOPLE  FALSE 2013-09-01
#> 10 Wood Nanotec… WoodNa… 742733  EC     H2020   "ERC"   ERC-ADG FALSE 2017-09-01
#> # … with 511 more rows, 1 more variable: `End Date` <date>, and abbreviated
#> #   variable names ¹​`Project title`, ²​`Project Acronym`, ³​`Project ID`,
#> #   ⁴​`Funding Stream`, ⁵​`Funding Substream level 1`,
#> #   ⁶​`Funding Substream level 2`, ⁷​`Start Date`
```
