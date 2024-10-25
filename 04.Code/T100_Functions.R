clean_T100 <- function(file_name = "02.Intermediate/Compile_T100.Rds"){
  unify_T100(target = file_name)
  T100_Carrier_ID(target = file_name)
  geocode_T100(target = file_name)
  service_relevancy_T100(target = file_name)
  t100_to_quarter()
  determineSharedRoutes_t100()
  set_up_backup_city_names()
  fix_subsidaries()
  gc()
}

# Fix carrier entries that are subsidies of other firms.
fix_subsidaries <- function(target = "02.Intermediate/Compile_T100.Rds"){
  input <- readRDS(target);
  
  input <- as.data.table(input)
  
  # Handle Subsidaries
  input[Carrier_Entity == "Endeavor Air Inc.", Carrier_Entity := "Delta Air Lines Inc."]
  input[Carrier_Entity == "Horizon Air", Carrier_Entity := "Alaska Airlines Inc."]
  input[Carrier_Entity == "Envoy Air", Carrier_Entity := "American Airlines Inc."]
  input[Carrier_Entity == "Piedmont Airlines", Carrier_Entity := "American Airlines Inc."]
  input[Carrier_Entity == "PSA Airlines Inc.", Carrier_Entity := "American Airlines Inc."]
  
  # Finally, handle the Virgin-America merger with Alaska
  input <- input[Carrier_Entity == "Virgin America" & Year == 2016 & Month >= 4,
                 Carrier_Entity := "Alaska Airlines Inc."]
  input <- input[Carrier_Entity == "Virgin America" & Year > 2016,
                 Carrier_Entity := "Alaska Airlines Inc."]

  # Now, combine all rows where the subsidiary and main firm serve the same route
  input <- input %>% group_by(Destination_City, Origin_City, Year, Month, Origin_Alpha,
                              Origin_Numeric, Origin_World, Destination_Alpha, Destination_Numeric,
                              Destination_World, Carrier_Entity, Distance, Origin_lon, 
                              Origin_lat, Destination_lon, Destination_lat) %>%
    summarize(Passengers_Enplaned = sum(Passengers_Enplaned, na.rm = TRUE),
              Freight_Enplaned = sum(Freight_Enplaned, na.rm = TRUE),
              Mail_Enplaned = sum(Mail_Enplaned, na.rm = TRUE))
  
  saveRDS(input, target)
  
}

geocode_T100 <- function(target = "02.Intermediate/Compile_T100.Rds"){
  t100 <- readRDS(target)
  
  if(!file.exists("02.Intermediate/City_Coordinates.rds")){
    geocode_cities()
  }
  
  geocoder <- readRDS("02.Intermediate/City_Coordinates.rds")
  
  # Geocode origin cities
  t100 <- merge(t100, geocoder, by.x = "Origin_City", by.y = "original")
  t100$Origin_lon <- t100$lon
  t100$Origin_lat <- t100$lat
  t100$lon <- NULL
  t100$lat <- NULL
  
  # Geocode Destination Cities
  t100 <- merge(t100, geocoder, by.x = "Destination_City", by.y = "original")
  t100$Destination_lon <- t100$lon
  t100$Destination_lat <- t100$lat
  t100$lon <- NULL
  t100$lat <- NULL
  
  
  write_rds(t100, target)
}

unify_T100 <- function(target = "02.Intermediate/Compile_T100.Rds"){
  file_head <- "01.Input/02.DomesticMarket"
  file_list <- list.files(path = file_head)
  
  compile <- c()
  for(i in 1:length(file_list)){
    current <- read.delim(paste(file_head,file_list[i], sep = "/"), header = FALSE,
                          sep = "|", fill = TRUE)
    
    if(ncol(current) == 20){
      if(is.na(unique(current$V20))){
        current$V20 <- NULL
      }
    }
    
    if(i == 1){
      compile <- current
    } else {
      compile <- rbind(compile, current)
    }
    compile <- unique(compile)
  }
  
  colnames(compile) <- c("Year", "Month", "Origin_Alpha",
                         "Origin_Numeric",
                         "Origin_World",
                         "Origin_City",
                         "Destination_Alpha",
                         "Destination_Numeric",
                         "Destination_World",
                         "Destination_City",
                         "Carrier_Alpha",
                         "Carrier_Entity",
                         "OAI_Group_Code",
                         "Distance",
                         "Service_class",
                         "Passengers_Enplaned",
                         "Freight_Enplaned",
                         "Mail_Enplaned",
                         "Carrier_World_Code")
  
  saveRDS(compile, file = target)
}

T100_Carrier_ID <- function(input_file = "02.Intermediate/Compile_T100.Rds",
                            output_file = "02.Intermediate/Compile_T100.Rds",
                            target = NA){
  if(!is.na(target)){
    input_file <- target;
    output_file <- target
  }
  
  T100 <- readRDS(input_file)
  airline_Codebook <- read_rds("02.Intermediate/Clean_Codebook.Rds")
  
  T100$Carrier_Entity.Orig <- T100$Carrier_Entity
  T100$Carrier_Entity <- factor(x = T100$Carrier_Entity, levels = airline_Codebook$CARRIER_ENTITY,
                                labels = airline_Codebook$CARRIER_NAME)
  
  T100$Carrier_Alpha <- NULL
  T100$Carrier_World_Code <- NULL
  
  saveRDS(T100, file = output_file)
}

service_relevancy_T100 <- function(target = "02.Intermediate/Compile_T100.Rds"){
  T100 <- readRDS(target)
  T100 <- T100 %>% filter(Service_class == "F")
  saveRDS(T100, file = target)
}

t100_to_quarter <- function(input = "02.Intermediate/Compile_T100.Rds",
                            output = "02.Intermediate/Compile_T100.Rds"){
  T100 <- readRDS(input)
  months <- 1:12;
  quarters <- c(rep(1, 3), rep(2, 3), rep(3,3), rep(4,3))
  
  quarter_decoder <- data.frame(Month = months, Quarter = quarters);
  
  T100 <- merge(T100, quarter_decoder, by.x = "Month", by.y = "Month",
                all.x = TRUE)
  
  T100 <- T100 %>% group_by(Year, Quarter, Origin_Alpha, Origin_City,
                            Destination_Alpha, 
                            Destination_City,
                            Carrier_Entity) %>%
    summarize(Passengers_Enplaned = sum(Passengers_Enplaned, na.rm = TRUE),
              Freight_Enplaned = sum(Freight_Enplaned, na.rm = TRUE),
              Mail_Enplaned = sum(Mail_Enplaned, na.rm = TRUE))
  
  saveRDS(T100, file = output)
}

determineSharedRoutes_t100 <- function(input_name = "02.Intermediate/Compile_T100.Rds"){
  t100 <- read_rds(input_name)
  airlines <- c("JetBlue Airways", "Spirit Air Lines")
  
  t100$Route <- paste(t100$Origin_City, "To", t100$Destination_City)
  
  interest_routes <- t100 %>% filter(Carrier_Entity %in% airlines)
  
  t00 <- t100 %>% filter(Route %in% interest_routes$Route)
  
  write_rds(t100, "02.Intermediate/contested_routes.rds")
}
# Some airlines are omitted from T100 despite being in T100 
# for a given quarter. This allows for recovery of City Names. 
set_up_backup_city_names <- function(input_name = "02.Intermediate/Compile_T100.Rds"){
  T100 <- as.data.table(readRDS(input_name))
  
  more_accurate_names <- T100 %>% select(Origin_City, Origin_Alpha) %>% mutate(Length = nchar(Origin_City)) %>%
    filter(Length == min(Length), .by = Origin_Alpha) %>% unique()%>% select(Origin_City, Origin_Alpha)
  
  more_accurate_names.dest <- T100 %>% select(Destination_Alpha, Destination_City) %>% mutate(Length = nchar(Destination_City)) %>%
    filter(Length == min(Length), .by = Destination_Alpha) %>% unique()%>% select(Destination_Alpha, Destination_City)
  
  
  T100$Origin_City <- NULL
  T100$Destination_City <- NULL

  T100 <- merge(T100, more_accurate_names, by.x = "Origin_Alpha", by.y = "Origin_Alpha",
                all.x = TRUE)
  
  T100 <- merge(T100, more_accurate_names.dest, by.x = "Destination_Alpha", by.y = "Destination_Alpha",
                all.x = TRUE)
  
  # First, Fix the Origin Cities
  T100[Origin_Alpha %in% c("ORD", "MDW") , Origin_City := "Chicago, IL"]
  T100[Origin_Alpha %in% c("CVG", "DAY") , Origin_City := "Cincinnati, OH"]
  T100[Origin_Alpha %in% c("CLE", "CAK") , Origin_City := "Cleveland, OH"]
  T100[Origin_Alpha %in% c("DWF", "DAL") , Origin_City := "Dallas, TX"]
  T100[Origin_Alpha %in% c("IAH", "HOU") , Origin_City := "Houston, TX"]
  T100[Origin_Alpha %in% c("LAX", "BUR", "LGB") , Origin_City := "Los Angeles, CA"]
  T100[Origin_Alpha %in% c("MIA", "FLL") , Origin_City := "Miami, FL"]
  T100[Origin_Alpha %in% c("LGA", "EWR", "JFK") , Origin_City := "New York, NY"]
  T100[Origin_Alpha %in% c("SFO", "OAK") , Origin_City := "San Francisco, CA"]
  T100[Origin_Alpha %in% c("TPA", "PIE") , Origin_City := "Tampa, FL"]
  T100[Origin_Alpha %in% c("DCA", "IAD", "BWI") , Origin_City := "Washington, DC"]
  
  T100 <- T100 %>% select(Origin_Alpha, Origin_City) %>%
    unique()
  colnames(T100) <- c("Origin.Alpha", "Origin.City")
  saveRDS(T100, "02.Intermediate/route_id_safety.rds")
}


get_nonstop_fulfillment <- function(input = "02.Intermediate/Compile_T100.Rds"){
  t100 <- readRDS(input)
  t100 <- t100 %>% ungroup() %>%
    select(Year, Quarter, Origin_Alpha, Destination_Alpha, Carrier_Entity) %>%
    unique() %>% as.data.table()
  
  # Fix Firms for Clarity
  carrier_replace <- function(data, Current, New){
    data <- data[Carrier_Entity == Current, Carrier_Entity := New];
    return(data)
  }
  
  t100 <- carrier_replace(t100, Current = "Endeavor Air Inc.", 
                          New = "Delta Air Lines Inc.")
  t100 <- carrier_replace(t100, Current = "Horizon Air",
                          New = "Alaska Airlines Inc.")
  t100 <- carrier_replace(t100, Current = "Envoy Air",
                          New = "American Airlines Inc.")
  t100 <- carrier_replace(t100, Current = "Piedmont Airlines",
                          New = "American Airlines Inc.")
  t100 <- carrier_replace(t100, Current = "PSA Airlines Inc.",
                          New = "American Airlines Inc.")
  
  # Finally, handle the Virgin-America merger with Alaska
  t100 <- t100[Carrier_Entity == "Virgin America" & Year == 2016 & Quarter >= 2,
               Carrier_Entity := "Alaska Airlines Inc."]
  t100 <- t100[Carrier_Entity == "Virgin America" & Year > 2016,
               Carrier_Entity := "Alaska Airlines Inc."]
  
  # First, Identify Number of Non-Stop Destinations out of Each Airport at 
  # Each point in Time
  total_market <- t100 %>% 
    select(Year, Quarter, Origin_Alpha, Destination_Alpha) %>%
    unique() %>% as.data.table(); 
  
  total_market[, Destinations.Available := .N, by = c("Year", "Quarter", "Origin_Alpha")]
  
  t100 <- merge(t100, total_market, 
                by = c("Year", "Quarter", "Origin_Alpha", "Destination_Alpha"))
  
  # Now, For Each Firm, Identify Number of Destinations They Serve
  t100[, Firm.Destinations := .N, 
       by = c("Carrier_Entity", "Year", "Quarter", "Origin_Alpha")]
  t100[, Firm.Ratio := Firm.Destinations / Destinations.Available]
  
  t100[, Carrier := Carrier_Entity]
  
  return(t100)
}
