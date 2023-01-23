utils::globalVariables(".")

#remotes::install_github("subugoe/openairegraph")
#library(openairegraph)
#library(dplyr)

# https://graph.openaire.eu/develop/api.html#projects


api_routes_data <- function() {
  tibble::tribble(
    ~entity, ~route,
    "projects", "search/projects",
    "research", "search/researchProducts",
    "publications", "search/publications",
    "datasets", "search/datasets",
    "software", "search/software",
    "other", "search/other"
  )
}

#' Available paths in the API
#' @return a named character vector with entity names and the API route
#' @export
api_paths <- function() {
  d <- api_routes_data()
  setNames(d$route, d$entity)
}

api_params_data <- function() {

  tibble::tribble(
    ~api, ~r, ~default, ~entity,

    # common parameters for all entities
    "size", "size", 1e4, "all",
    "page", "page", NULL, "all",
    "format", "format", "xml", "all",
    "model", "model", NULL, "all",
    "sortBy", "sort_by", NULL, "all",
    "sortOrder", "sort_order", NULL, "all",
    "hasECFunding", "has_ec_funding", NULL, "all",
    "hasWCFunding", "has_wc_funding", NULL, "all",
    "FP7scientificArea", "fp7_scientific_area", NULL, "all",
    "funder", "funder", NULL, "all",
    "fundingStream", "funding_stream", NULL, "all",

    # project parameters
    "grantID", "proj_grant_id", NULL, "projects",
    "openairePublicationID", "proj_publication_id", NULL, "projects",
    "dataset_id", "proj_dataset_id", NULL, "projects",
    "name", "proj_title", NULL, "projects",
    "acronym", "proj_acronym", NULL, "projects",
    "callID", "proj_call_id", NULL, "projects",
    "startYear", "proj_start_year", NULL, "projects",
    "endYear", "proj_end_year", NULL, "projects",
    "participantCountries", "proj_country", NULL, "projects",
    "participantAcronyms", "proj_org", NULL, "projects",

    # params for research "products"
    "sortBy", "r_sort_by", NULL, "research",
    "doi", "r_doi", NULL, "research",
    "orcid", "r_orcid", NULL, "research",
    "fromDateAccepted", "r_from_date", NULL, "research",
    "toDateAccepted", "r_to_date", NULL, "research",
    "title", "r_title", NULL, "research",
    "author", "r_author", NULL, "research",
    "openaireProviderID", "r_provider_id", NULL, "research",
    "openaireProjectID", "r_project_id", NULL, "research",
    "hasProject", "r_has_project", NULL, "research",
    "projectID", "r_grant_id", NULL, "research",
    "FP7ProjectID", "r_fp7_grant_id", NULL, "research",
    "OA", "r_is_oa", NULL, "research",
    "country", "r_country", NULL, "research",

    # params for publications
    "openairePublicationID", "pub_publication_id", NULL, "publications",
    "originalId", "pub_original_id", NULL, "publications",
    "sdg", "pub_sdg", NULL, "publications",
    "fos", "pub_fos", NULL, "publications",

    # params for datasets
    "openaireDatasetID", "dataset_id", NULL, "datasets",

    # params for software
    "openaireSoftwareID", "software_id", NULL, "software",

    # params for other
    "openaireOtherID", "other_id", NULL, "other"
  )
}

#' @importFrom stats setNames
api_args <- function(.entity) {

  entity <- NULL

  d <-
    api_params_data() %>%
    dplyr::filter(entity %in% c("all", .entity))

  stats::setNames(d$default, d$r)
}

api_param_aliases <- function(.entity) {

  entity <- NULL

  d <-
    api_params_data() %>%
    dplyr::filter(entity %in% c("all", .entity))

  res <- d$api
  names(res) <- d$r
  res
}

# api_args("software")

comp <- function(x) Filter(Negate(is.null), x)

replace_quotes <- function(x) {
  # fix quote characters
  chartr("\u201c\u201d", "\"\"", x)
}

#' Parameters for the API
#'
#' @param size by default 10000
#' @param page which page to return (when paging), default NULL
#' @param format the respone format, one of "xml", "json", "tsv", "csv", by default "xml"
#' @param model the model to use, by default NULL
#' @param sort_by sorting parameter
#' @param sort_order sorting order
#' @param has_ec_funding logical
#' @param has_wc_funding logical
#' @param fp7_scientific_area character
#' @param funder character
#' @param funding_stream character
#' @param proj_grant_id character
#' @param proj_publication_id character
#' @param proj_dataset_id character
#' @param proj_title character
#' @param proj_acronym character
#' @param proj_call_id character
#' @param proj_start_year character
#' @param proj_end_year character
#' @param proj_country character
#' @param proj_org character
#' @param r_sort_by character
#' @param r_doi character
#' @param r_orcid character
#' @param r_from_date character
#' @param r_to_date character
#' @param r_title character
#' @param r_author character
#' @param r_provider_id character
#' @param r_project_id character
#' @param r_has_project character
#' @param r_grant_id character
#' @param r_fp7_grant_id character
#' @param r_is_oa character
#' @param r_country character
#' @param pub_publication_id character
#' @param pub_original_id character
#' @param pub_sdg character
#' @param pub_fos character
#' @param dataset_id character
#' @param software_id character
#' @param other_id = NULL
#' @return a list of parameters
#' @export
api_params <- function() {
  args <- comp(as.list(environment()))
  aliases <- api_param_aliases( api_routes_data()$entity)
  names(args) <- dplyr::recode(names(args), !!!aliases)
  args
}

formals(api_params) <- api_args(api_routes_data()$entity)


api_GET <- function(path = names(api_paths()), params = api_params(), verbose = FALSE) {

  if (is.null(path))
    stop("No path provided")

  p <- switch(match.arg(path), unname(api_paths()[path]))

  if (verbose)
    message("Fetching path: ", p, " with params: ", print(params))

  ct <- switch(params$format,
    xml = httr::content_type_xml(),
    json = httr::content_type_json(),
    httr::content_type("text/plain")
  )

  req <- httr::GET(
    url = sprintf("https://api.openaire.eu/%s", p),
    query = params#,
    #config = ct
  )

  httr::stop_for_status(req)

  if (verbose)
    message("Retrieved ", req$url)

  return (req)
}

api_parse <- function(
    x, format, entity = "projects",
    parse_openaire_xml = NULL, raw = FALSE) {

  if (raw)
    return(httr::content(x, as = "raw", encoding = "UTF-8"))

  switch(
    format,
    json = {
      parse_openaire_json(x, entity = entity)
    },
    tsv = {
      txt <- paste0(httr::content(x, encoding = "UTF-8", as = "text"), "\n") %>%
        replace_quotes()
      parse_openaire_rectangular(txt, entity = entity, format = "tsv")
    },
    csv = {
      txt <- paste0(httr::content(x, encoding = "UTF-8", as = "text"), "\n") %>%
        replace_quotes()
      parse_openaire_rectangular(txt, entity = entity, format = "csv")
    },
    xml = parse_openaire_xml(x, entity = entity),
    stop("'format' must be one of json, tsv, csv, or xml")
  )
}

parse_openaire_json <- function(x, entity) {

  tmp <- tryCatch(
    jsonlite::fromJSON(rawToChar(x$content), simplifyDataFrame = TRUE),
    error = function(e) e)

  if (inherits(tmp, "error")) {
    stop("invalid JSON, try setting raw=TRUE")
  } else {
    return (tmp)
  }

}

#   api_GET("other", api_params(size = 1, format = "xml")) %>%
# #    xml2::read_xml() %>% xml2::xml_structure()
#     xml2::read_xml() %>% as.character() %>% cat()

openaire_xpath <- function() {
  list(
    roots = list(
      projects = "metadata/oaf:entity/oaf:project",
      research = "metadata/oaf:entity/oaf:result",
      publications = "metadata/oaf:entity/oaf:result",
      datasets = "metadata/oaf:entity/oaf:result",
      software = "metadata/oaf:entity/oaf:result",
      other = "metadata/oaf:entity/oaf:result"
    ),
    projects = list(
      collected_from = "collectedfrom/@name",
      original_id = "originalId",
      code = "code",
      title = "title",
      project_title = "title",
      beg_date = "startdate",
      end_date = "enddate",
      duration = "duration",
      ec_art_293 = "ecarticle29_3",
      oa_is_mandated = "oamandatepublications",
      ec_sc_39 = "ecsc39",
      summary = "summary",
      cost = "totalcost",
      funded_amount = "fundedamount",
      currency = "currency",
      funder_shortname = "fundingtree/funder/shortname",
      funder_name = "fundingtree/funder/name",
      funder_jurisdiction = "fundingtree/funder/jurisdiction",
      funding_level_0_name = "fundingtree/funding_level_0/name",
      data_inferred = "datainfo/inferred",
      data_deleted = "datainfo/deletedbyinference",
      data_trust = "datainfo/trust"
    ),
    research = list(
      collected_from = "collectedfrom/@name",
      title = "title",
      best_accessrights = "bestaccessright/@classname",
      date_acceptance = "dateofacceptance",
      subject = "subject",
      language = "language/@classname",
      source = "source",
      type = "resulttype/@classname",
      journal_name = "journal",
      journal_eissn = "journal/@eissn",
      journal_ep = "journal/@ep",
      journal_issue = "journal/@iss",
      journal_vol = "journal/@vol",
      data_inferred = "datainfo/inferred",
      data_deleted = "datainfo/deletedbyinference",
      data_trust = "datainfo/trust"
    ),
    publications = list(
      collected_from = "collectedfrom/@name",
      title = "title",
      best_accessrights = "bestaccessright/@classname",
      date_acceptance = "dateofacceptance",
      description = "description",
      subject = "subject",
      language = "language/@classname",
      publisher = "publisher",
      type = "resulttype/@classname",
      data_inferred = "datainfo/inferred",
      data_deleted = "datainfo/deletedbyinference",
      data_trust = "datainfo/trust"
    ),
    datasets = list(
      collected_from = "collectedfrom/@name",
      original_id = "originalId",
      doi = "pid/@doi",
      title = "title",
      best_accessrights = "bestaccessright/@classname",
      date_acceptance = "dateofacceptance",
      embargo_date = "embargoeddate",
      description = "description",
      subject = "subject",
      language = "language/@classname",
      publisher = "publisher",
      type = "resulttype/@classname",
      data_inferred = "datainfo/inferred",
      data_deleted = "datainfo/deletedbyinference",
      data_trust = "datainfo/trust"
    ),
    software = list(
      collected_from = "collectedfrom/@name",
      original_id = "originalId",
      doi = "pid/@doi",
      title = "title",
      best_accessrights = "bestaccessright/@classname",
      date_acceptance = "dateofacceptance",
      description = "description",
      language = "language/@classname",
      date_relevant = "relevantdate",
      publisher = "publisher",
      type = "resulttype/@classname",
      data_inferred = "datainfo/inferred",
      data_deleted = "datainfo/deletedbyinference",
      data_trust = "datainfo/trust"
    ),
    other = list(
      collected_from = "collectedfrom/@name",
      original_id = "originalId",
      title = "title",
      best_accessrights = "bestaccessright/@classname",
      date_acceptance = "dateofacceptance",
      description = "description",
      language = "language/@classname",
      publisher = "publisher",
      type = "resulttype/@classname",
      data_inferred = "datainfo/inferred",
      data_deleted = "datainfo/deletedbyinference",
      data_trust = "datainfo/trust"
    )
  )
}

parse_openaire_xml <- function(x, entity = names(api_paths())) {

  xml <- xml2::read_xml(x$content)
  meta <- xml2::xml_find_all(xml, ns = xml2::xml_ns(xml), "/response/results/result")

  docroot_xpath <- openaire_xpath()$root |> getElement(entity)
  entities <- meta %>% xml2::xml_find_all(docroot_xpath)

  fetch <- function(x, xpath)
    x %>% xml2::xml_find_all(xpath) %>% xml2::xml_text() %>% or_na()

  details <- function(doc, entity) {

    xpath <- switch(entity,
      "projects" = openaire_xpath()$projects,
      "research" = openaire_xpath()$research,
      "publications" = openaire_xpath()$publications,
      "datasets" = openaire_xpath()$datasets,
      "software" = openaire_xpath()$software,
      "other" = openaire_xpath()$other
      )

    xpath  %>%
      purrr::map(function(x) setNames(fetch(doc, x), names(x)))
  }

  entities %>%
    purrr::map_dfr(
      function(x) details(x, entity) |> tibble::as_tibble()
    ) |>
    retype()

}

#' @importFrom utils capture.output write.csv
retype <- function(x) {
  txt <- utils::capture.output(utils::write.csv(x, stdout(), row.names = FALSE))
  paste0(txt, "\n") |> readr::read_csv(show_col_types = FALSE)
}

#' @importFrom readr problems col_character col_logical col_date
parse_openaire_rectangular <- function(txt,
  entity = names(api_paths()), format = c("csv", "tsv")) {

  # TODO: if required, implement specific data types for different entities
  ct <- switch(match.arg(entity),
    projects = readr::cols(`Project ID` = col_character(), SC39 = col_logical(),
      `Start Date` = col_date(format = ""),
      `End Date` = col_date(format = ""), .default = col_character()),
    research = readr::cols(),
    publications = readr::cols(),
    datasets = readr::cols(),
    software = readr::cols(),
    other = readr::cols()
  )

  res <- switch(
    match.arg(format),
    csv = suppressWarnings(readr::read_csv(txt, quote = '"', show_col_types = FALSE, col_types = ct)),
    tsv = suppressWarnings(readr::read_tsv(txt, show_col_types = FALSE, col_types = ct))
  )

  if (nrow(problems(res)) > 1) {
    warning("Error parsing CSV: ")
    warning("Issue on lines ", paste0(collapse = ", ", problems(res)$row))
    warning(paste(paste(collapse = "\n", unlist(strsplit(txt, "\r"))[problems(res)$row])), "\n")
  }

  return (res)

}

or_na <- function(x) {
  ifelse(length(x) == 1 && !purrr::is_null(x), x, NA_character_)
}

#' Retrive OpenAIRE data
#' @param entity one of "projects", "research", "publications", "datasets", "software" or "other"
#' @param page_size records returned, by default 50
#' @param params a set of parameters to the API, see api_params()
#' @return object with results, if possible a data frame
#' @export
openaire <- function(entity = names(api_paths()), page_size = 50, params = NULL) {

  if (is.null(params))
    params <- api_params(size = page_size, format = "tsv")

  res <- api_GET(path = entity, params = params)

  res %>% api_parse(entity = entity, format = params$format)

}

#' Fetch several pages of results
#' @param entity one of the different entity types
#' @param params a list of parameters
#' @param page_size page size, by default 50
#' @return a data frame if possible
#' @export
openaire_crawl <- function(
    entity = names(api_paths()),
    params = api_params(),
    page_size = 50L) {

  entity <- match.arg(entity)
  if (missing(params)) params <- api_params(size = page_size)

  l1 <- list(format = "json", size = page_size)
  params_json <- purrr::list_modify(params, !!!l1)

  res <-
    api_GET(path = force(entity), params = params_json) %>%
    httr::content()

  n <- res$response$header$total$`$`
  stopifnot(page_size == res$response$header$size)
  needs_crawl <- n > page_size

  if (needs_crawl) {

    n_pages <- n %/% page_size + ifelse(n %% page_size > 0, 1, 0)
    i <- 1:(n_pages)
    message(sprintf("Fetching %s hits in %s batches of %s records", n, n_pages, page_size))
    stopifnot(n < 1e4)
  }

  argz <-
    tibble::tibble(page = i, size = page_size) |>
    purrr::transpose() |>
    purrr::map(function(x) purrr::list_modify(params, !!!x))

  #return(argz)

  pb <- progress::progress_bar$new(
    format = "  processing [:bar] :percent in :elapsed (eta: :eta)",
    total = length(argz), clear = FALSE, width = 60
  )

  crawl <- function(x) {
    pb$tick()
    openaire(entity, params = x)
  }

  batch <- argz |> purrr::map(crawl)

  purrr::possibly(function(x) batch %>% purrr::map_dfr(dplyr::bind_rows), otherwise = {
    warning("probably incompatible data types across chunks")
    batch
  })()

}

#' @param size (integer) number of records
#' @param page (integer) page number of the search results
#' @param sort_order (character) sort order. one of ascending or descending.
#' optional
#' @param model (character) The data model of the response. Options:
#' "openaire" or "sygma". Model "sygma" is a simplified version of the
#' "openaire" model. For sygma, only the xml format is available. See
#' the relative XML schema
#' (<https://www.openaire.eu/schema/sygma/oaf_sygma_v2.1.xsd>).
#' Default: "openaire". Unfortunately, passing "model=openaire" leads to
#' no results AFAICT, so this parameter is left as `NULL` for now.
#' If you pass `model=sygma` we force `format=xml` (see above)
#' @param fp7_scientific_area (character) Search for FP7 entities by
#' scientific area
#' @param has_ec_funding (logical) If `TRUE` gets the entities funded by the
#' EC. If `FALSE` gets the entities related to projects not funded by the EC.
#' @param has_wt_funding (logical) If `TRUE` gets the entities funded by
#' Wellcome Trust. The results are the same as those obtained with
#' `funder="wt"`. If `FALSE` gets the entities related to projects not
#' funded by Wellcome Trust.
#' @param funder (character) Search for entities by funder. One of WT, EC,
#' ARC, ANDS, NSF, FCT, NHMRC
#' @param funding_stream (character) xx
#' @param keywords (character) vector of keywords
#' @param format (character) format to return, one of json, xml, csv
#' or tsv (default)

