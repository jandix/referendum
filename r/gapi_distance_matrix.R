gapi_distance_matrix <- function(origins,
                                 destinations, 
                                 mode = c("driving", 
                                          "walking", 
                                          "bicycling", 
                                          "transit"), 
                                 transit_mode = NULL,
                                 units = "metric",
                                 language = "en",
                                 api_key = Sys.getenv("GAPI_DIST_MATRIX")){
  
  # define base url 
  # return value json
  base_url <- "https://maps.googleapis.com/maps/api/distancematrix/json"
  
  # add parameters
  parameters <- list(origins = origins,
                     destinations = destinations,
                     mode = match.arg(mode),
                     units = units,
                     language = language,
                     key = api_key)
  
  # add transit mode parameter if mode is set to transit and transit_mode available
  if (mode == "transit" && !is_null(transit_mode)) {
    parameters <- c(parameters, transit_mode = transit_mode)
  }
  
  # add query parameters to url
  url <- httr::modify_url(base_url, query = parameters)
  
  # request url
  res <- httr::GET(url)
  
  # if no result stop
  if (httr::status_code(res) != 200) {
    stop(
      sprintf(
        "Request failed.\nError [%s]", 
        httr::status_code(res)
      ),
      call. = FALSE
    )
  }
  
  # extract content
  con <- httr::content(res, "text")
  
  # parse result
  json_res <- jsonlite::fromJSON(con)
  
  # define return list
  list(
    result = list(origin = json_res$origin_addresses,
                   destination = json_res$destination_addresses,
                   distance = json_res$rows$elements[[1]]$distance$value,
                   duration = json_res$rows$elements[[1]]$duration$value),
    meta = list(url = res$url)
  )
}