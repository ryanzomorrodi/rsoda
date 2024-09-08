parse_url <- function(url, format = NULL) {
  url_components <- httr2::url_parse(url)
  if(
    is.null(url_components$scheme) | 
    is.null(url_components$hostname) | 
    is.null(url_components$path)
  ) {
    stop(url, " does not appear to be a valid URL.")
  }

  if (identical(format, c("csv", "json", "geojson"))) {
    format <- "csv"
  }

  path_components <- strsplit(url_components$path, "/") |> 
    unlist()

  if (length(path_components) <= 2) {
    stop(url, " does not appear to be a valid URL.")
  } else if (path_components[2] == "resource") {
    resource_components <- strsplit(path_components[3], "\\.") |> 
      unlist()
    four_by_four <- resource_components[1]
    format <- resource_components[2]
  } else if (length(path_components) == 3) {
    stop(url, " does not appear to be a valid URL.")
  } else {
    four_by_four <- path_components[4]
  }

  if (!valid_four_by_four(four_by_four)) {
    stop(url, " does not appear to be a valid URL.")
  }

  list(
    hostname = url_components$hostname,
    four_by_four = four_by_four,
    format = format
  )
}

valid_four_by_four <- function(four_by_four) {
  grepl("^[a-z0-9]{4}-[a-z0-9]{4}$", four_by_four)
}

resource_url <- function(components) {
  paste(
    "https:/",
    components$hostname,
    "resource",
    paste0(components$four_by_four, ".", components$format),
    sep = "/"
  )
}

meta_url <- function(components) {
  paste(
    "https:/",
    components$hostname,
    "api/views/metadata/v1",
    components$four_by_four,
    sep = "/"
  )
}

list_url <- function(components) {
  paste(
    "https:/",
    components$hostname,
    "data.json",
    sep = "/"
  )
}
