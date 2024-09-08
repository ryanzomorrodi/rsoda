str_extract_all <- function(string, pattern, ignore.case = TRUE) {
  gregexpr(pattern, string) |>
    regmatches(x = string) |> 
    unlist()
}

parse_sql <- function(sql) {
  ops <- "SELECT|WHERE|ORDER BY|GROUP BY|HAVING"
  expressions <- strsplit(sql, ops)[[1]][-1] |> 
    sapply(trimws)
  operations <- str_extract_all(ops, sql, ignore.case = TRUE) |>
    tolower()

  names(expressions) <- paste0("$", sub(" by", "", operations))

  as.list(expressions)
}
