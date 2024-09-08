meta_socrata <- function(url) {
  url_components <- parse_url(url)
  url <- meta_url(url_components)

  req <- request_socrata(url)
  
  resp <- httr2::req_perform(req)

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  body
}
