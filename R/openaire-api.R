utils::globalVariables(".")

#remotes::install_github("subugoe/openairegraph")
#library(openairegraph)
#library(dplyr)

# https://graph.openaire.eu/develop/api.html#projects



openaire_search <- function(
    countries = "se",
    acronyms = "KTH",
    page = 1,
    page_size = 10,
    format = c("xml", "json", "csv", "tsv")) {

  ct <- switch(match.arg(format), xml = httr::content_type_xml(),
    json = httr::content_type_json(), httr::content_type("text/plain"))

  httr::GET("https://api.openaire.eu/search/projects", query = list(
    page = page,
    size = page_size,
    participantCountries = countries,
    participantAcronyms = acronyms,
    format = format
  ), ct)

}

#' Projects given an acronym
#' @param countries the countries to filter by, comma-separated string of 2-letter ISO country codes, by default "SE"
#' @param acronyms the acronyms to filter by, whitespace-separated string, by default "KTH"
#' @import readr
#' @export
openaire_projects <- function(
    countries = "se",
    acronyms = "KTH") {

  openaire_search(countries, acronyms, format = "tsv", page_size = 1000) %>%
    suppressWarnings() %>%
    httr::content(show_col_types = FALSE)
}

#openaire_projects() %>% View()


or_na <- function(x)
  ifelse(length(x) == 1 && !purrr::is_null(x), x, NA_character_)

openaire_as_tibble <- function(...) {

  res <- openaire_search(..., format = "xml")  %>%
    httr::content()

  header <-
    xml2::xml_find_all(res, ns = xml2::xml_ns(res), "/response/header")

  query <- header %>% xml2::xml_find_all("query") %>% xml2::xml_text()
  page <- header %>% xml2::xml_find_all("page") %>% xml2::xml_text()
  n_pages <- header %>% xml2::xml_find_all("total") %>% xml2::xml_text()

  message(glue::glue("Using query {query}."))
  message(glue::glue("Found {n_pages} pages, currently on page  {page}"))

  # TODO implement paging!

  meta <-
    xml2::xml_find_all(res, ns = xml2::xml_ns(res), "/response/results/result")

  projects <- meta %>% xml2::xml_find_all("metadata/oaf:entity/oaf:project")

  projects %>% xml2::xml_find_all(".")

  fetch <- function(x, xpath)
    x %>% xml2::xml_find_all(xpath) %>% xml2::xml_text() %>% or_na()

  projects %>% purrr::map_dfr(.f = function(x) tibble::tibble(
    code = x %>% fetch("code"),
    project_title = x %>% fetch("title"),
    beg_date = x %>% fetch("startdate"),
    end_date = x %>% fetch("enddate"),
    duration = x %>% fetch("duration"),
    ec_art_293 = x %>% fetch("ecarticle29_3"),
    oa_is_mandated = x %>% fetch("oamandatepublications"),
    ec_sc_39 = x %>% fetch("ecsc39"),
    summary = x %>% fetch("summary"),
    collected_from = x %>% fetch("collectedfrom/@name"),
    currency = x %>% fetch("currency"),
    cost = x %>% fetch("totalcost"),
    funded_amount = x %>% fetch("fundedamount")
  ))
}


#openaire_as_tibble(page_size = 10)


openaire_search_crawl <- function() {

  page_size <- 50

  res <- openaire_search(format = "json", page_size = page_size) %>% httr::content()

  n <- res$response$header$total$`$`
  stopifnot(page_size == res$response$header$size)
  needs_crawl <- n > page_size

  if (needs_crawl) {

    n_pages <- n %/% page_size + ifelse(n %% page_size > 0, 1, 0)
    i <- 1:(n_pages)
    calls <- sprintf("openaire_search(format = 'json', page = %s, page_size = %s)", i, page_size)
    calls <- paste0(calls, " %>% httr::content()")
    message(sprintf("Fetching %s hits in %s batches of %s records", n, n_pages, page_size))

  }

  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent in :elapsed",
    total = n_pages, clear = FALSE, width= 60)

  call2df <- function(x) {
    pb$tick()
    Sys.sleep(0.01)
    force(rlang::eval_tidy(rlang::parse_quo(x, env = rlang::current_env()))) #%>%
    #as.data.frame() %>%
    #tibble::as_tibble() %>%
    #dplyr::select_if(function(y) !is.list(y))
  }

  batch <- calls %>% purrr::map(call2df)

  parse_results <- function(x) {
    x$response$results$result$metadata %>%
      as.data.frame() %>%
      tibble::as_tibble() %>%
      dplyr::select_if(function(y) !is.list(y))
  }

  content <- batch %>% purrr::map(parse_results)

  batch[[1]]$response$results$result %>%
    purrr::map_dbl(list("metadata", "oaf:entity", "oaf:project", "totalcost", "$"), .default = NA_real_)

  batch[[1]]$response$results$result[1] %>% purrr::map(function(x) tibble::tibble(
    totalcost = purrr::map_chr(x, list("metadata", "oaf:entity", "oaf:project", "totalcost", "$"), .default = NA_real_)
    )) %>%
    purrr::map("totalcost")#names)

  return(res)
}

