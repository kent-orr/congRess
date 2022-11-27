#' Retrieve Bills and Data related thereto
#'
#' @param congress the number of the congress which introduced the bill
#' @param type either hr, s, hjres, sjres, hconres, sconres, hres, or sres
#' @param number the bill number
#' @param meta one of actions  amendments  committees  cosponsors  relatedbills  subjects  summaries  text  titles to provide additional info on a bill
#' @param since a datetime object
#' @param until a datetime object. If null, curent datetime
#' @param rm_url urls and other pointers for the API are returned with results. This bool removes columns from returned df
#' @param key your congresapi key
#'
#' @return
#' @export
#'
#' @examples
#' get_congress_bill(since = Sys.time()-60^2*24)
get_congress_bill <- function(congress  = NULL,
                              type      = NULL,
                              number    = NULL,
                              meta      = NULL,
                              since     = NULL,
                              until     = NULL,
                              limit     = 20,
                              rm_url    = TRUE,
                              key       = congress_get_key()) {

  # Construct URL
  endpoints <- c("bill", congress, type, number, meta)

  # create URL
  create_url <- function(limit, offset_pg) {
    query_params <- list(fromDateTime = format_date(since),
                         toDateTime = format_date(until),
                         limit = limit,
                         offset = offset_pg)
    query_params <- query_params[!sapply(query_params, is.null)]
    if (is.null(c(query_params, recursive = TRUE)))
      query_params <- list()
    url <- url_constructor(endpoints = endpoints,
                           query_params = query_params,
                           key = key)
    url
  }

  # Send Request
  send_request <- function(url) {
    resp <- curl::curl_fetch_memory(url)
    stopifnot("Unsuccessful API Call" = resp$status_code == 200L)
    content <- resp$content |>
      rawToChar() |>
      jsonlite::fromJSON()

    # parse content based on endpoint params
    content <- if (!is.null(type) & is.null(number)) {content["bills"]} else {content}
    content <- if (!is.null(type) & !is.null(number) & is.null(meta)) {content["bill"]} else {content}
    content <- if (!is.null(meta) & !is.null(type) & !is.null(number)) {
      content[which(!names(content) %in% c("pagination", "request"))]
    } else {content}
    content <- content |>
      as.data.frame() |>
      data.table::as.data.table() |>
      janitor::clean_names()

    content
  }

  urls <- if (limit > 250) {
    v = list()
    off = 0:(limit %/% 250) * 250
    limit_ = c(rep(250, limit %/% 250), limit - 250 * limit %/% 250)
    for (i in seq_along(off)) {
      v[i] <- list(c(limit_[i], off[i]))
    }
    sapply(v, \(x) create_url(x[1], x[2]))
  } else {
    create_url(limit, 0)
  }

  content <- if (limit > 250) {
    cl <- parallel::makeCluster(parallel::detectCores()*.5)
    parallel::clusterExport(cl, list("urls", "send_request"), envir = environment())
    content <- try(pbapply::pblapply(urls, send_request))
    parallel::stopCluster(cl)
    data.table::rbindlist(content, fill = TRUE)
  } else {
    send_request(urls)
  }


  # remove columns with urls listed
  if (rm_url) {
    to_rm = c(names(content)[sapply(content, \(i) any(grepl("https", i)))],
              "request_content_type", "pagination_count", "request_format")
    if(length(to_rm) > 0)
      content <- content[,c(to_rm) := lapply(.SD, \(x) x = NULL), .SDcols = to_rm]
  }

  # message("result preview")
  # x[,.(number = bills_number, type = bills_type, title = stringr::str_trunc(bills_title, 30, "right"),
  #      date = bills_latest_action_action_date, time = bills_latest_action_action_time, action = bills_latest_action_text)][] |> print()
  # message("-----")

  content[]

}

