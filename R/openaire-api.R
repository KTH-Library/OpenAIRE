utils::globalVariables(".")

#remotes::install_github("subugoe/openairegraph")
#library(openairegraph)
#library(dplyr)

# https://graph.openaire.eu/develop/api.html#projects

api_base_url <- function() {
  "https://api.openaire.eu/"
}

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
    #"dataset_id", "proj_dataset_id", NULL, "projects",
    "name", "proj_name", NULL, "projects",
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
#' @param size Number of results per page, by default 10000
#' @param page which page to return (when paging), default NULL
#' @param format the respone format, one of "xml", "json", "tsv", "csv",
#'     by default "xml"
#' @param model the model to use, default NULL means "openaire" is used,
#'     and "sygma" is a simplified version
#' @param sort_by optional sorting parameter, the sorting order of the specified
#'     field, for example "projectstartdate,ascending", where other valid fields
#'     are "projectstartyear", "projectenddate", "projectendyear", "projectduration"
#' @param sort_order sorting order, one of ascending or descending.
#' @param has_ec_funding logical, use TRUE or FALSE for projects funded by EC or not
#' @param has_wc_funding logical, use TRUE or FALSE for projects funded by Wellcome Trust (or use funder = "wt")
#' @param fp7_scientific_area character
#' @param funder character, use one of "WT", "EC", "ARC", "ANDS", "NSF", "FCT", "NHMRC"
#' @param funding_stream character
#' @param proj_grant_id character, comma separated list of grant identifiers
#' @param proj_publication_id character, comma separated list of OpenAIRE identifiers
#' @param proj_name character, white-space separated list of keywords
#' @param proj_acronym character, gets the project with the given acronym, if any
#' @param proj_call_id character, search for projects by call identifier
#' @param proj_start_year character, year formatted as YYYY
#' @param proj_end_year character, year formatted as YYYY
#' @param proj_country character, comma separeted list of 2 letter country codes
#' @param proj_org character, white space separeted list of acronyms of
#'     participating institutions
#' @param r_sort_by character, sorting order of the specified field (one of
#'     "dateofcollection", "resultstoragedate", "resultstoragedate",
#'     "resultembargoenddate", "resultembargoendyear", "resultdateofacceptance",
#'     "resultacceptanceyear" followed by a comma and "ascending" or "descending")
#' @param r_doi character, comma separated list of DOIs
#' @param r_orcid character, comma separated list of ORCID iDs of authors
#' @param r_from_date character, date formatted as YYYY-MM-DD
#' @param r_to_date character, date formatted as YYYY-MM-DD
#' @param r_title character
#' @param r_author character, white-space separated list of names and/or surnames
#' @param r_provider_id character, comma separated list of OpenAIRE identifiers
#' @param r_project_id character, comma separated list of identifiers,
#'     will form a query with OR semantics
#' @param r_has_project character, one of "true" or "false", where the former variant
#'    "true" gets the research products that have a link to a project
#' @param r_grant_id character, given grant identifier of the project
#' @param r_fp7_grant_id character, research products associated to a FP7
#'    project with the given grant number
#' @param r_is_oa character, one of "true" or "false", for OA research products or not
#' @param r_country character, two letter country code, such as "EN"
#' @param pub_publication_id character, comma separated list of OpenAIRE identifiers
#' @param pub_original_id character, comma separated list of original identifiers
#'    from providers
#' @param pub_sdg character, number of the Sustainable Development Goals from 1 to 17
#' @param pub_fos character, Field of Science classification value from
#'     <https://graph.openaire.eu/develop/athenarc_fos_hierarchy.json>
#' @param dataset_id character, comma separated list of OpenAIRE identifiers
#' @param software_id character, comma separated list of OpenAIRE identifiers
#' @param other_id character, comma separated list of OpenAIRE identifiers
#' @return a list of parameters to be used in an API call
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
  
  ct <- NULL

  if (!Sys.getenv("OPENAIRE_PAT") == "") {
    ct <- httr::add_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAIRE_PAT"))
    )
  }

  # ct <- switch(params$format,
  #   xml = httr::content_type_xml(),
  #   json = httr::content_type_json(),
  #   httr::content_type("text/plain")
  # )

  req <- httr::GET(
    url = sprintf(paste0(api_base_url(), "%s"), p), 
    query = params,
    config = ct
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
    retype(entity = entity)

}

#' @importFrom utils capture.output write.csv
retype <- function(x, entity) {
  txt <- utils::capture.output(utils::write.csv(x, stdout(), row.names = FALSE))
  paste0(txt, "\n") |> parse_openaire_rectangular(entity = entity, format = "csv")
}

#' @importFrom readr problems col_character col_logical col_date
parse_openaire_rectangular <- function(txt,
  entity = names(api_paths()), format = c("csv", "tsv")) {

  # TODO: if required, implement specific data types for different entities
  ct <- switch(match.arg(entity),
    projects = readr::cols(
      `Project ID` = col_character(),
      SC39 = col_logical(),
      `Start Date` = col_date(format = ""),
      `End Date` = col_date(format = ""),
      code = col_character(),
      .default = col_character()),
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

#' Retrieve OpenAIRE data for "projects", "research", "publications",
#'     "datasets", "software" or "other"
#' @param entity one of "projects", "research", "publications",
#'     "datasets", "software" or "other"
#' @param page_size records returned, by default 50
#' @param params a set of parameters to the API, see api_params()
#' @return object with results, if possible a data frame
#' @export
openaire <- function(entity = names(api_paths()), page_size = 50, params = NULL) {

  if (is.null(params))
    params <- api_params(size = page_size, format = "tsv")

  res <- api_GET(path = entity, params = params)

  #cnt <- httr::content(res)
  #cnt$response$results$result |> tibble::enframe() |> print()
  res |> api_parse(entity = entity, format = params$format)

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
    message(sprintf("Fetching approximately %s hits in %s batches of %s records", n, n_pages, page_size))
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

  batch <- argz |> purrr::map(crawl, .progress = TRUE)

  # TODO: remove when json is parsed into rectangular format...
  if (params$format == "json") return(batch)

  res <- tryCatch(dplyr::bind_rows(batch), error = function(e) {
    warning("failed merging chunks, incompatible data types across chunks?", e)
    batch
  })

  return(res)

}



openaire_projects_kth <- function() {

  a <- openaire("projects", 
    params = api_params(
      format = "tsv", 
      proj_country = "SE",
      proj_org = "Royal Institute of Technology",
    )
  )

  b <- openaire_crawl("projects", page_size = 200, 
    params = api_params(
      format = "xml", 
      proj_country = "SE",
      proj_org = "Royal Institute of Technology")
    )
  
  d <- 
    b |> dplyr::left_join(
      by = c(code = "project_id"),
      a |> dplyr::select(
        project_abbr = "Project Acronym", 
        project_id = "Project ID", 
        funder = "Funder", 
        funding_stream = "Funding Stream", 
        funding_level_1 = "Funding Substream level 1", 
        funding_level_2 = "Funding Substream level 2", 
        sc39 = "SC39", 
        beg = "Start Date", 
        end = "End Date"
      ))  

  return(d)
}

openaire_pat <- function() {

  "https://services.openaire.eu/uoa-user-management/api/users/getAccessToken?refreshToken=%s" |> 
    sprintf(openaire_refresh_token()) |> 
    httr2::request() |> 
    httr2::req_perform() |> 
    httr2::resp_body_json() |> 
    getElement("access_token")
            
}

openaire_refresh_token <- function() {
  "eyJhbGciOiJub25lIn0.eyJleHAiOjE3NDQyOTk0MzksImp0aSI6ImYzNzU0MjQwLTM0OTItNGE0NS04YzRiLWIxN2Y2NDVjMGY0MyJ9."
}

openaire_login <- function() {
  my_pat <- openaire_pat()
  Sys.setenv(OPENAIRE_PAT = my_pat)
  message("Login for this session uses OPENAIRE_PAT: ", Sys.getenv("OPENAIRE_PAT"))
  message("Rate limit for authenticated requests: 7200 requests per hour (2 per second)")
  invisible(my_pat == Sys.getenv("OPENAIRE_PAT"))
}

openaire_projects_search <- function(orgname = "KTH") {

  value <- subject <- NULL

  if (Sys.getenv("OPENAIRE_PAT") == "") {
    message("Warning: only 60 requests per hour allowed for non-authenticated requests.")
    message("Please issue openaire_login() first")
  } 

  q <- list(
    relOrganizationName = orgname,
    debugQuery = "false",
    page = 1,
    pageSize = 50,
    sortBy = "relevance DESC"
  )
  
  req_projects <- 
    "https://api.openaire.eu/" |> 
    httr2::request() |> 
    httr2::req_url_path("/graph/projects")

  if (Sys.getenv("OPENAIRE_PAT") != "") {
    req_projects <- 
      req_projects |> 
      httr2::req_auth_bearer_token(Sys.getenv("OPENAIRE_PAT"))
  }
  
  fetch_projects <- function(q) {
    req_projects |> 
      httr2::req_url_query(!!!q) |> 
      httr2::req_perform() |> 
      httr2::resp_body_json()
  }

  resp <- fetch_projects(q)
  
  h <- resp$header
  n_pages <- ceiling(h$numFound / h$pageSize) + ifelse(h$numFound > h$pageSize, 1, 0)

  if (!(n_pages > 1)) 
    return(resp)

  message("Retrieving ", n_pages, " pages, starting crawl...")
  qs <- 2:n_pages |> purrr::map(\(x) q |> purrr::list_modify(page = x))
  more <- qs |> purrr::map(fetch_projects, .progress = TRUE) |> purrr::map(c)
  message("Done")

  out <- 
    more #qs |> jsonlite::toJSON()  |> RcppSimdJson::fparse()  |> tibble::as_tibble()  #|> dplyr::pull(results)  |> 
    #purrr::compact()  |> dplyr::bind_rows()  |> tibble::as_tibble()

  #
  #out  |> purrr::map_dfr(\(y) y$results |> purrr::map(\(x) x |> purrr::compact()  |> dplyr::bind_cols())  |> dplyr::bind_rows())
  res <- 
    out  |> purrr::map(function(y) y |> getElement("results") |> purrr::map_dfr(function(x) 
       x  |> tibble::enframe() |> dplyr::filter(lengths(value) >= 1) |> tidyr::pivot_wider())) |> dplyr::bind_rows() |> 
      tidyr::unnest(c(
        "id", "acronym", "title", "websiteUrl", "startDate", "endDate", "callIdentifier", 
        "openAccessMandateForDataset", "openAccessMandateForPublications", "funding", "keywords", "summary")
      ) |> 
      tidyr::unnest_wider(c(
        "funding", "granted")
      ) |> 
      dplyr::mutate(subject = purrr::map_chr(subject, \(x) paste(collapse = "|", x)))
  
  return(res)
  

}

openaire_projects_kth2 <- function() {

  "https://services.openaire.eu/search/v2/api/resources2/" |> 
    httr2::request() |> 
    httr2::req_url_query(
        format = "json",
        query = "( (( KTH )))",
        type = "projects",
        #fq = "(projectcode<>\"unidentified\" )",
        page = 0,
        size = 10
      ) |> 
    httr2::req_perform() |> 
    httr2::resp_body_json()

}

#' @import dplyr
#' @importFrom stringr str_extract
openaire_projects_kth_csv <- function() {

  Funder <- Participants <- NULL
  
  projects <- 
    "https://services.openaire.eu/search/v2/api/reports" |> 
    httr2::request() |> 
    httr2::req_url_query(
        format = "csv",
        query = "( (( KTH )))",
        type = "projects"
      ) |> 
    httr2::req_perform() |> 
    httr2::resp_body_string()

  # "https://services.openaire.eu/search/v2/api//publications?format=json&fq=(relprojectid%20exact%20%22aka_________::80303b0e032abf95bb1e30cae7862944%22)&sortBy=resultdateofacceptance,descending&page=0&size=5" |>
  #   httr2::request() |> 
  #   httr2::req_perform() |> 
  #   httr2::resp_body_json() |> 
  #   _$results |> 
  #   purrr::map(c("result", "metadata", "oaf:entity", "oaf:result", "children", "result")) |> 
  #   tibble::enframe() |> 
  #   tidyr::unnest(value) |> 
  #   tidyr::unnest_wider(value) |> 
  #   tidyr::unnest_wider(title) |>
  #   tidyr::unnest(creator) |> 
  #   tidyr::unnest(creator)

  # i_broken <- function(csv, i_exclude = NULL) {

  #   x <- csv |> strsplit("\r\n") |> unlist() 

  #   if (!is.null(i_exclude)) x <- x[-i_exclude]

  #   r <- 
  #     x |> paste0(collapse = "\r\n") |> paste0("\r\n") |> 
  #     readr::read_csv(col_names = NULL, skip_empty_rows = TRUE, show_col_types = FALSE) |> readr::problems()

  #   print(r, n = nrow(r))

  #   r$row |> unique()
  # }

  # p <- projects  |> 
  #   gsub(pattern="\r\n", replacement = "##") |> 
  #   gsub(pattern="\n", replacement = "") |> 
  #   gsub(pattern="##", replacement = "\r\n")


  #idx <- which(projects |> strsplit(split = "\r\n") |> unlist() |> strsplit(split = "\"") |> lengths() != 4)

  re_funder_short <- "(.*?)([(]([^()]*)[)])$"

  projects |> readr::read_csv(
    show_col_types = FALSE, 
    skip_empty_rows = TRUE, 
    col_types = "ccDDc", 
    quote = "\""
  ) |> 
  suppressWarnings() |> 
  dplyr::filter(!is.na(Participants)) |> 
  dplyr::mutate(funder = stringr::str_extract(Funder, re_funder_short, group = 3)) |> 
  dplyr::mutate(funder_name = gsub(re_funder_short, replacement = "\\1", Funder)) |> 
  dplyr::rename(
    project_title = "Title", 
    beg = "Start Date", 
    end = "End Date", 
    participants = "Participants"
  ) |> 
    dplyr::select(-any_of("Funder"))

}

#' Projects for KTH and participating organisations
#' 
#' @export
#' @import dplyr tidyr readr purrr
openaire_projects_participants_kth <- function() {

  project_title <- code <- project_abbr <- beg <- end <- duration <- 
    cost <- funded_amount <- currency <- funder_shortname <- 
    funder_name <- funding_stream <- participants <- NULL

  openaire_login() |> suppressMessages()

  pk <- openaire_projects_kth()
  pks <- openaire_projects_kth_csv()

  projs <- 
    pk |> filter(!is.na(project_title)) |> 
    select(-any_of(c("funder_name", "funder"))) |>
    inner_join(by = c("project_title", "beg", "end"), 
      pks |> filter(!is.na(project_title))
    ) |> 
    mutate(across(
      c("ec_art_293", "oa_is_mandated", "ec_sc_39"), 
      \(x) case_match(x, "true" ~ TRUE, "false" ~ FALSE))
    ) |> 
    type_convert(col_types = cols(
      .default = col_character(),
      beg_date = col_date(format = ""),
      end_date = col_date(format = ""),
      duration = col_double(),
      cost = col_double(),
      funded_amount = col_double(),
      data_inferred = col_logical(),
      data_deleted = col_logical(),
      data_trust = col_double()
    ))

  #pk |> count(title, beg, end) |> filter(n > 1)

  # 70 dupes for the project_title, beg, end tuples, only (funding_stream, funding_level_1, funding_level_2) differ
#  projs |> count(project_title, beg, end) |> filter(n > 1) |> 
#    left_join(by = c("project_title", "beg", "end"), projs) 

#  re_funder_short <- "(.*?)([(]([^()]*)[)])$"
#  re_funder_short <- "[(]([^()]*)[)]$"

  p1 <- 
    projs |> 
    select(
      code, project_abbr, project_title, 
      beg, end, duration, 
      cost, funded_amount, currency,
      funder_shortname, funder_name, 
      funding_stream, 
      participants
    ) |> 
    distinct()
  
  # p2 <- 
  #   p1 |> 
  #   select(code, participants) |> 
  #   tidyr::separate_longer_delim(participants, ";") |> 
  #   filter(!is.na(participants)) |> 
  #   distinct()
  
  # list(projects = p1, projects_participants = p2)

  p1

}
