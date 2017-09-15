setwd("D:\\NextCloud\\uni\\semester\\17_1\\computional_social_science\\Hausarbeit\\Code")

# define data source url        
url <- "https://de.wikipedia.org/wiki/Volksabstimmung_zu_Stuttgart_21"

# define table to choose
id <- 1

# define colnames
colnames <- c("district", "turnout", "yes", "no")

# fetch table using htmltab
referendum <- htmltab::htmltab(doc = url, 
                               which = id, 
                               encoding = "UTF-8",
                               colNames = colnames)

# replace decimal seperator
referendum$turnout <- stringr::str_replace_all(referendum$turnout, ",", ".")
referendum$yes <- stringr::str_replace_all(referendum$yes, ",", ".")
referendum$no <- stringr::str_replace_all(referendum$no, ",", ".")

referendum$turnout <- as.numeric(referendum$turnout)
referendum$yes <- as.numeric(referendum$yes)
referendum$no <- as.numeric(referendum$no)

# adjust encoding (probably only windows)
Encoding(referendum$district) <- "UTF-8"

# remove last row
referendum <- referendum[1:44, ]

# define new url
url <- "https://de.wikipedia.org/wiki/Liste_der_Land-_und_Stadtkreise_in_Baden-W%C3%BCrttemberg"

# define table to choose
id <- 2

# define colnames
colnames <- c("district", "capital")

# fetch table using htmltab
capitals <- htmltab::htmltab(doc = url, 
                             which = id, 
                             encoding = "UTF-8",
                             colNames = colnames)

# extract district and capital only
capitals <- capitals[1:44, 1:2]

# adjust encoding (probably only windows)
Encoding(capitals$district) <- "UTF-8"
Encoding(capitals$capital) <- "UTF-8"

# sort both tables
capitals <- capitals %>% dplyr::arrange(district)
referendum <- referendum %>% dplyr::arrange(district)

# join data frames
referendum <- cbind(capitals, referendum[ , 2:4])

# add missing capitals (cities)
referendum$capital <- ifelse(stringr::str_detect(referendum$capital, 
                                                 referendum$capital[2]),
                             referendum$district,
                             referendum$capital)

# remove all objects except referendum
rm(list=setdiff(ls(), "referendum"))

# remove "Stadtkreis" & "Landeshauptstadt"
referendum$capital <- stringr::str_replace_all(referendum$capital,
                                               "\\([A-Z][a-z]+\\)",
                                               "")

# get coordinates
coordinates <- ggmap::geocode(paste0(referendum$capital, ", Germany"))

# join coordinates
referendum <- cbind(referendum, coordinates)

# read zHV data
zHV <- read.csv2(file = "data/zhv.csv",
                 encoding = "UTF-8",
                 stringsAsFactors = F)

# remove all minor stations
zHV <- zHV %>% dplyr::filter(Type == "S")

# set longitude automatically to numeric
zHV$Longitude <- stringr::str_replace_all(zHV$Longitude, ",", ".")
zHV$Longitude <- as.numeric(zHV$Longitude)

# add coordinate & station columns
referendum$station_dhid <- NA
referendum$station_distance <- NA

# find closest station for each capital
i <- 1
while (i <= nrow(referendum)) {
  # calculate distance between each station and capital i
  zHV$distance <- geosphere::distHaversine(c(referendum$lon[i],
                                             referendum$lat[i]),
                                           cbind(zHV$Longitude, zHV$Latitude))
  # sort by distance
  zHV <- zHV %>% dplyr::arrange(zHV$distance)
  
  # fill the row at capital i
  referendum$station_dhid[i] <- zHV$DHID[1]
  referendum$station_distance[i] <- zHV$distance[1]
  
  # increment
  i <- i + 1
}

# remove all objects except referendum
rm(list=setdiff(ls(), "referendum"))

# define dhid for Stuttgart Main Station
stuttgart_dhid <- "de:08111:6115"

# get the efa api function
source("r/efa_api.R")

# add duration & changes columns
referendum$duration <- NA
referendum$changes <- NA

# find the duration and changes for each station
i <- 1
while (i <= nrow(referendum)) {
  # get duration and changes for each station dhid i
  result <- efa_api(origin = referendum$station_dhid[i],
                    destination = stuttgart_dhid)
  
  # fill the row at capital i
  referendum$duration[i] <- result$result$duration
  referendum$changes[i] <- result$result$changes
  
  # sleep to avoid blocking 
  Sys.sleep(1)
  
  # increment
  i <- i + 1
}

# get the Google Distance Matrix API function
source("r/gapi_distance_matrix.R")

# add car distance & duration columns
referendum$car_distance <- NA
referendum$car_duration <- NA

# find the car distance & duration for each capital
i <- 1
while (i <= nrow(referendum)) {
  # get car distance & duration for each capital i
  result <- gapi_distance_matrix(origins = paste0(referendum$capital[i], ", Germany"),
                                 destinations = "Stuttgart Hauptbahnhof")
  
  # fill the row at capital i
  referendum$car_distance[i] <- result$result$distance / 1000
  referendum$car_duration[i] <- result$result$duration / 60
  
  # increment
  i <- i + 1
}

# remove all objects except referendum
rm(list=setdiff(ls(), "referendum"))

# read states
states <- read.csv2(file = "data/states.csv",
                    encoding = "UTF-8",
                    stringsAsFactors = F,
                    header = F)

# set column names
colnames(states) <- c("district", "capital", "state")

# join referendum and states
referendum <- dplyr::left_join(referendum, states[ , c(1,3)],
                               by = "district")

# tables
variables <- c("changes", "car_distance", "duration", "car_duration")

export <- data.frame(variable = character(),
                     min = numeric(),
                     max = numeric(),
                     mean = numeric(),
                     median = numeric(),
                     stringsAsFactors = F)

for (variable in variables) {
  col <- referendum[ , variable]
  row <- data.frame(variable = variable,
                    min = min(col, na.rm = T),
                    max = max(col, na.rm = T),
                    mean = mean(col, na.rm = T),
                    median = median(col, na.rm = T))
  export <- rbind(export, row)
}

writeClipboard(print(xtable::xtable(export, digits = 2), include.rownames = F))


model1 <- lm(yes ~ state,
             data = referendum)
summary(model1)

model2 <- lm(yes ~ duration + state + duration:state,
             data = referendum)
summary(model2)

writeClipboard(stargazer::stargazer(model1, model2))
