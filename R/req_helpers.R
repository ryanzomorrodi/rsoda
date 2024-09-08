request_socrata <- function(url) {
  if (!curl::has_internet()) {
    stop("Your API call has errors. No Internet Connection.")
  }

  httr2::request(url) |>
    httr2::req_user_agent("rsoda R package") |>
    httr2::req_error(body = \(x) "Your API call has errors. No Results.")
}

json_is_complete <- function(resp) {
  length(httr2::resp_body_json(resp)) == 0
}

csv_is_complete <- function(resp) {
  csv <- httr2::resp_body_raw(resp) |>
    readr::read_csv(show_col_types = FALSE)

  nrow(csv) == 0
}

geojson_is_complete <- function(resp) {
  geojson <- httr2::resp_body_string(resp) |>
    sf::read_sf()

  nrow(geojson) == 0
}

is_complete <- function(format) {
  switch(
    format,
    `json` = json_is_complete,
    `csv` = csv_is_complete,
    `geojson` = geojson_is_complete,
  )
}
