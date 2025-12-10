# Retrieve OpenAIRE data for "projects", "research", "publications", "datasets", "software" or "other"

Retrieve OpenAIRE data for "projects", "research", "publications",
"datasets", "software" or "other"

## Usage

``` r
openaire(entity = names(api_paths()), page_size = 50, params = NULL)
```

## Arguments

- entity:

  one of "projects", "research", "publications", "datasets", "software"
  or "other"

- page_size:

  records returned, by default 50

- params:

  a set of parameters to the API, see api_params()

## Value

object with results, if possible a data frame
