# Fetch several pages of results

Fetch several pages of results

## Usage

``` r
openaire_crawl(
  entity = names(api_paths()),
  params = api_params(),
  page_size = 50L
)
```

## Arguments

- entity:

  one of the different entity types

- params:

  a list of parameters

- page_size:

  page size, by default 50

## Value

a data frame if possible
