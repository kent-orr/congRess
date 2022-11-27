#' Retrieve Member information
#'
#' The bioguide IDs are the keys to indiviual members. You can find them [here](https://www.congress.gov/help/field-values/member-bioguide-ids)
#'
#' @param bioguideId
#' @param legislation
#' @param limit
#' @param key
#'
#' @return
#' @export
#'
#' @examples
get_member <- function(bioguideId = NULL,
                       limit = 250,
                       key = congress_get_key()) {


  endpoints <- c("member", bioguideId)

  # create URL
  create_url <- function(limit, offset_pg) {
    query_params <- list(limit = limit,
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

    content <- if (is.null(bioguideId)) {
      x <- content[["members"]] |>
        jsonlite::flatten() |>
        data.table::as.data.table()
      x[,`:=`(house_start = served.House[[1]]$start,
              house_end = served.House[[1]]$end,
              senate_start = served.Senate[[1]]$start,
              senate_end = served.Senate[[1]]$end
              )]
      x
    } else {
      member = content$member
      address = member$addressInformation  |> data.table::as.data.table()
      nm = data.table::data.table(
        bioguideId = bioguideId,
        current = member$currentMember,
        party = member$party,
        state = member$state,
        district = member$district,
        honorific = member$honorificName,
        fname = member$firstName,
        mname = member$middleName,
        lname = member$lastName,
        full_name = member$directOrderName,
        image = member$depiction$imageUrl
        )
      cbind(nm, address)
    }
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

  content[]

}

x = get_member(bioguideId)

legislation = NULL
# Construct URL
if (!is.null(legislation))
  legislation <- paste0(legislation, "-legislation")

bioguideId <- aoc_id <- "O000172"
bioguideId <- "C001041"
