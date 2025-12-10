# OpenAIRE

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
#>    `Project title`        `Project Acronym` `Project ID` Funder `Funding Stream`
#>    <chr>                  <chr>             <chr>        <chr>  <chr>           
#>  1 NeTS: Large: Collabor… <NA>              1801865      NSF    Directorate for…
#>  2 Yeni Hava Trafik Yöne… <NA>              111M167      TUBIT… 1001 - Araştırma
#>  3 The social production… <NA>              ES/G008841/1 UKRI   ESRC            
#>  4 On the Effects of Aut… <NA>              1946951      UKRI   ESRC            
#>  5 Evaluating integrated… <NA>              347862       AKA    <NA>            
#>  6 Crustal fault system … <NA>              DP0210719    ARC    Discovery Proje…
#>  7 Paediatric European R… PERS              241959       EC     FP7             
#>  8 REVISÃO DA FAMILIA NE… SFRH/BD/16563/20… SFRH/BD/165… FCT    PIDDAC          
#>  9 COMPOSIÇÃO ESCRITA DO… SFRH/BD/84264/20… SFRH/BD/842… FCT    FARH            
#> 10 A CIDADE E O ROAD MOV… SFRH/BD/21394/20… SFRH/BD/213… FCT    PIDDAC          
#> # ℹ 40 more rows
#> # ℹ 5 more variables: `Funding Substream level 1` <chr>,
#> #   `Funding Substream level 2` <chr>, SC39 <lgl>, `Start Date` <date>,
#> #   `End Date` <date>
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
#> # A tibble: 871 × 10
#>    `Project title`        `Project Acronym` `Project ID` Funder `Funding Stream`
#>    <chr>                  <chr>             <chr>        <chr>  <chr>           
#>  1 A Higher Dimensional … <NA>              158937       SNSF   Careers         
#>  2 Modeling and optimiza… <NA>              181788       SNSF   Careers         
#>  3 Determining the Effec… <NA>              EP/R028699/1 UKRI   EPSRC           
#>  4 Topological defects i… <NA>              EP/P024688/1 UKRI   EPSRC           
#>  5 Datasounds, datasets … <NA>              AH/V014668/1 UKRI   AHRC            
#>  6 NI: Microbial Dimethy… <NA>              NE/S007725/1 UKRI   NERC            
#>  7 Fiktionalitätsmerkmal… <NA>              119410       SNSF   Careers         
#>  8 SUSTAINABLE DESIGN OF… SUSTAIN-MS        237136       EC     FP7             
#>  9 Reliable Epidemic mon… REACT             101062523    EC     HE              
#> 10 Nature-inspired contr… NiCoFlow          708281       EC     H2020           
#> # ℹ 861 more rows
#> # ℹ 5 more variables: `Funding Substream level 1` <chr>,
#> #   `Funding Substream level 2` <chr>, SC39 <lgl>, `Start Date` <date>,
#> #   `End Date` <date>
```

### Paging

A function provides crawling results, page by page:

``` r

openaire_crawl("projects", page_size = 100, params = api_params(
  format = "xml", 
  proj_country = "SE",
  proj_org = "Royal Institute of Technology")
)
#> Fetching approximately 835 hits in 9 batches of 100 records
#> # A tibble: 835 × 22
#>    collected_from        original_id code  title project_title beg_date end_date
#>    <chr>                 <chr>       <chr> <chr> <chr>         <chr>    <chr>   
#>  1 SNSF - Swiss Nationa… snsf______… 1589… A Hi… A Higher Dim… 2015-11… 2016-04…
#>  2 SNSF - Swiss Nationa… snsf______… PA00… Nume… Numerical Si… 2007-02… 2007-02…
#>  3 SNSF - Swiss Nationa… snsf______… 1817… Mode… Modeling and… 2018-08… 2020-01…
#>  4 UK Research and Inno… ukri______… EP/R… Dete… Determining … 2018-08… 2021-02…
#>  5 UK Research and Inno… ukri______… EP/P… Topo… Topological … 2017-08… 2021-08…
#>  6 UK Research and Inno… ukri______… AH/V… Data… Datasounds, … 2022-01… 2023-09…
#>  7 UK Research and Inno… ukri______… NE/S… NI: … NI: Microbia… 2018-12… 2023-06…
#>  8 SNSF - Swiss Nationa… snsf______… 1194… Fikt… Fiktionalitä… 2007-10… 2007-12…
#>  9 CORDA - COmmon Resea… corda_____… 2371… SUST… SUSTAINABLE … 2009-05… 2011-04…
#> 10 CORDA - COmmon Resea… corda_____… 1010… Reli… Reliable Epi… 2022-09… 2025-08…
#> # ℹ 825 more rows
#> # ℹ 15 more variables: duration <chr>, ec_art_293 <chr>, oa_is_mandated <chr>,
#> #   ec_sc_39 <chr>, summary <chr>, cost <chr>, funded_amount <chr>,
#> #   currency <chr>, funder_shortname <chr>, funder_name <chr>,
#> #   funder_jurisdiction <chr>, funding_level_0_name <chr>, data_inferred <chr>,
#> #   data_deleted <chr>, data_trust <chr>
```
