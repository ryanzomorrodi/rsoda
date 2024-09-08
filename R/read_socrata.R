read_socrata <- function(
  url,
  format = c("csv", "json", "geojson"),
  query = NULL,
  app_token = NULL,
  username = NULL,
  password = NULL,
  limit = 1000
) {
  url_components <- parse_url(url, format)
  url <- resource_url(url_components)
  format <- url_components$format

  req <- request_socrata(url) |>
    httr2::req_url_query(
      `$order` = ":id",
      `$limit` = limit,
      `$$app_token` = app_token
    )
  if (!is.null(query)) {
    req <- c(.req = list(req), parse_sql(query)) |>
      do.call(what = httr2::req_url_query)
  }
  if (all(!is.null(c(username, password)))) {
    req <- httr2::req_auth_basic(req, username, password)
  }

  resps <- httr2::req_perform_iterative(
    req,
    next_req = httr2::iterate_with_offset(
      "$offset",
      start = 0,
      offset = limit,
      resp_complete = is_complete(format)
    ),
    max_reqs = Inf
  )

  read_resp(resps) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), readr::parse_guess))
}

read_json <- function(resps) {
  resps |>
    lapply(httr2::resp_body_json, simplifyVector = TRUE) |>
    lapply(as.data.frame) |>
    dplyr::bind_rows()
}

read_csv <- function(resps) {
  resps |>
    lapply(httr2::resp_body_raw) |>
    lapply(readr::read_csv, col_types = readr::cols(.default = "c")) |>
    dplyr::bind_rows()
}

read_geojson <- function(resps) {
  resps |>
    lapply(httr2::resp_body_string) |>
    lapply(sf::read_sf) |>
    dplyr::bind_rows()
}

read_resp <- function(resps) {
  read <- switch(
    httr2::resp_content_type(resps[[1]]),
    `application/json` = read_json,
    `text/csv` = read_csv,
    `application/vnd.geo+json` = read_geojson,
  )

  read(resps)
}