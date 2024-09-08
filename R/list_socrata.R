list_socrata <- function(url) {
  url_components <- httr2::url_parse(url)
  url <- list_url(url_components)

  req <- request_socrata(url)
  
  resp <- httr2::req_perform(req)

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  
  output <- body$dataset |> 
    as.data.frame()

  attr(output, "@context") <- body$`@context`
  attr(output, "@id") <- body$`@id`
  attr(output, "@type") <- body$`@type`
  attr(output, "conformsTo") <- body$conformsTo
  attr(output, "describedBy") <- body$describedBy

  output
}
