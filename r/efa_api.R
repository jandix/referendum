efa_api <- function (origin, 
                     destination, 
                     date = "20170904", 
                     time = "0800") {
  
  # paste url
  url <- paste0("www.efa-bw.de/nvbw/XML_TRIP_REQUEST2?",
                "language=de",
                "&itdLPxx_frames=false",
                "&tripMacro=nvbwLink",
                "&name_origin=", origin,
                "&type_origin=any",
                "&name_destination=", destination,
                "&type_destination=any",
                "&lineRestriction=400",
                "&routeType=LEASTTIME",
                "&useProxFootSearch=1",
                "&excludedMeans=12",
                "&excludedMeans=17",
                "&excludedMeans=18",
                "&excludedMeans=19",
                "&trITDepMOT=100",
                "&trITArrMOT=100",
                "&trITDepMOTvalue100=15",
                "&trITArrMOTvalue100=15",
                "&itdDate=", date,
                "&itdTime=", time)
  
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
  con <- httr::content(res, "text", 
                       encoding = "Latin1")
  # parse result
  xml_res <- xml2::read_xml(con)
  
  # find all "idtRoute" nodes
  idt_nodes <- xml2::xml_find_all(xml_res, ".//itdRoute")
  
  # extract the route
  routes <- xml2::xml_attrs(idt_nodes)
  
  # if no routes stop
  if (length(routes) < 1) {
    stop(
      sprintf(
        "Request failed.\nCan't find route based on DHIDs."
      ),
      call. = FALSE
    )
  }
  
  # define new variables
  duration <- 0
  changes <- 0
  
  # loop trough routes and extract duration and changes from each
  for (j in 1:length(routes)) {
    # parse time
    time <- lubridate::hm(routes[[j]][['publicDuration']])
    # convert time to minutes
    time <- lubridate::period_to_seconds(time) / 60
    # add up time
    duration <- duration + time
    # add up changes
    changes <- changes + as.numeric(routes[[j]][['changes']])
  }
  
  # calculate mean duration and changes
  duration <- duration / j
  changes <- changes / j
  
  # define return list
  list(
    result = list(origin = origin,
                  destination = destination,
                  duration = duration,
                  changes = changes),
    meta = list(url = url)
  )
}