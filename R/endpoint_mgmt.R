#' Create a URL for retrieving Congress REST API
#'
#' @param endpoints a vector of endpoints
#' @param query_params a named list of query parameters. If no params, default to empty list
#' @param key API key. Default checks `congress_get_key()`
#'
#' @return
#' @export
#'
#' @examples
url_constructor <- function(endpoints, query_params = list(), key) {
  if (missing(key))
    key <-congress_get_key()

  base_url <- "https://api.congress.gov/v3"
  url <- paste(base_url, paste(endpoints, collapse =  "/"), sep = "/")

  query_params$api_key <- key
  qn <- names(query_params)
  qv <- query_params
  qstring <- paste(qn, qv, sep= "=", collapse = "&")

  url <- paste(url, qstring, sep = "?")
  url
}

extractor <- function(x, meta) {
  if (is.null(meta)) {x} else {x[meta]} |>
    as.data.frame() |>
    janitor::clean_names()
}

format_date <- function(x) {
  if (is.null(x))
    return (NULL)
  format(x, "%Y-%m-%dT%H:%M:%SZ")
}
