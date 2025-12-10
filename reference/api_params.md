# Parameters for the API

Parameters for the API

## Usage

``` r
api_params(
  size = 10000,
  page = NULL,
  format = "xml",
  model = NULL,
  sort_by = NULL,
  sort_order = NULL,
  has_ec_funding = NULL,
  has_wc_funding = NULL,
  fp7_scientific_area = NULL,
  funder = NULL,
  funding_stream = NULL,
  proj_grant_id = NULL,
  proj_publication_id = NULL,
  proj_name = NULL,
  proj_acronym = NULL,
  proj_call_id = NULL,
  proj_start_year = NULL,
  proj_end_year = NULL,
  proj_country = NULL,
  proj_org = NULL,
  r_sort_by = NULL,
  r_doi = NULL,
  r_orcid = NULL,
  r_from_date = NULL,
  r_to_date = NULL,
  r_title = NULL,
  r_author = NULL,
  r_provider_id = NULL,
  r_project_id = NULL,
  r_has_project = NULL,
  r_grant_id = NULL,
  r_fp7_grant_id = NULL,
  r_is_oa = NULL,
  r_country = NULL,
  pub_publication_id = NULL,
  pub_original_id = NULL,
  pub_sdg = NULL,
  pub_fos = NULL,
  dataset_id = NULL,
  software_id = NULL,
  other_id = NULL
)
```

## Arguments

- size:

  Number of results per page, by default 10000

- page:

  which page to return (when paging), default NULL

- format:

  the respone format, one of "xml", "json", "tsv", "csv", by default
  "xml"

- model:

  the model to use, default NULL means "openaire" is used, and "sygma"
  is a simplified version

- sort_by:

  optional sorting parameter, the sorting order of the specified field,
  for example "projectstartdate,ascending", where other valid fields are
  "projectstartyear", "projectenddate", "projectendyear",
  "projectduration"

- sort_order:

  sorting order, one of ascending or descending.

- has_ec_funding:

  logical, use TRUE or FALSE for projects funded by EC or not

- has_wc_funding:

  logical, use TRUE or FALSE for projects funded by Wellcome Trust (or
  use funder = "wt")

- fp7_scientific_area:

  character

- funder:

  character, use one of "WT", "EC", "ARC", "ANDS", "NSF", "FCT", "NHMRC"

- funding_stream:

  character

- proj_grant_id:

  character, comma separated list of grant identifiers

- proj_publication_id:

  character, comma separated list of OpenAIRE identifiers

- proj_name:

  character, white-space separated list of keywords

- proj_acronym:

  character, gets the project with the given acronym, if any

- proj_call_id:

  character, search for projects by call identifier

- proj_start_year:

  character, year formatted as YYYY

- proj_end_year:

  character, year formatted as YYYY

- proj_country:

  character, comma separeted list of 2 letter country codes

- proj_org:

  character, white space separeted list of acronyms of participating
  institutions

- r_sort_by:

  character, sorting order of the specified field (one of
  "dateofcollection", "resultstoragedate", "resultstoragedate",
  "resultembargoenddate", "resultembargoendyear",
  "resultdateofacceptance", "resultacceptanceyear" followed by a comma
  and "ascending" or "descending")

- r_doi:

  character, comma separated list of DOIs

- r_orcid:

  character, comma separated list of ORCID iDs of authors

- r_from_date:

  character, date formatted as YYYY-MM-DD

- r_to_date:

  character, date formatted as YYYY-MM-DD

- r_title:

  character

- r_author:

  character, white-space separated list of names and/or surnames

- r_provider_id:

  character, comma separated list of OpenAIRE identifiers

- r_project_id:

  character, comma separated list of identifiers, will form a query with
  OR semantics

- r_has_project:

  character, one of "true" or "false", where the former variant "true"
  gets the research products that have a link to a project

- r_grant_id:

  character, given grant identifier of the project

- r_fp7_grant_id:

  character, research products associated to a FP7 project with the
  given grant number

- r_is_oa:

  character, one of "true" or "false", for OA research products or not

- r_country:

  character, two letter country code, such as "EN"

- pub_publication_id:

  character, comma separated list of OpenAIRE identifiers

- pub_original_id:

  character, comma separated list of original identifiers from providers

- pub_sdg:

  character, number of the Sustainable Development Goals from 1 to 17

- pub_fos:

  character, Field of Science classification value from
  <https://graph.openaire.eu/develop/athenarc_fos_hierarchy.json>

- dataset_id:

  character, comma separated list of OpenAIRE identifiers

- software_id:

  character, comma separated list of OpenAIRE identifiers

- other_id:

  character, comma separated list of OpenAIRE identifiers

## Value

a list of parameters to be used in an API call
