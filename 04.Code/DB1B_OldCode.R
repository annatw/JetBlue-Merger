condense_db1b <- function(input, output, fares_min = 15, fares_max = 2000){
  DB1B <- read_rds(input)
  
  # In line with Shrago (2022), remove fares less than $15 to remove point redemptions
  DB1B <- as.data.table(DB1B); gc(); gc();
  orig_count <- nrow(DB1B)
  
  print(paste("Low Fares:", nrow(DB1B[MktFare < fares_min,]) / orig_count * 100, " Of Sample"))
  
  DB1B[MktFare < fares_min, MktFare := NA] 
  
  # In Line with Turner, Bet (2021 Both), remove fares greater than $1500 to avoid 'key punch errors'
  print(paste("High Fares:", nrow(DB1B[MktFare > fares_max,]) / orig_count * 100, " Of Sample"))
  
  DB1B[MktFare > fares_max, MktFare := NA]
  
  # Restrict to two layovers or fewer
  print(paste("High Legs:", nrow(DB1B[MktCoupons >= 3,]) / orig_count * 100, " Of Sample"))
  DB1B[MktCoupons >= 3, MktFare := NA]
  
  DB1B$NonStop <- DB1B$MktCoupons == 1
  
  
  market_group <- c("Year", "Quarter", "Origin", "Dest")
  product_group <- c(market_group, "Carrier", "NonStop", "AirportGroup", "MktCoupons", "MktMilesFlown")
  
  DB1B[, MktMilesFlown := mean(MktMilesFlown), by = product_group]
  DB1B[, Passengers.Product := sum(Passengers, na.rm = TRUE) * 10, by = product_group]
  DB1B[, Passengers.Inside.Market := sum(Passengers, na.rm = TRUE) * 10, by = market_group]
  
  # Remove small markets, products
  DB1B[Passengers.Product < 100, MktFare := NA]
  print(paste("Small Products:", nrow(DB1B[Passengers.Product < 100,]) / nrow(DB1B) * 100, " Of Sample"))
  
  DB1B[Passengers.Inside.Market < 500, MktFare := NA]
  # print(paste("Small Markets:", length(unique(DB1B[Passengers.Inside.Market < 500,]$market_group)) / length(unique(DB1B$market_group)) * 100, " Of Sample"))
  
  DB1B[is.na(MktFare), Passengers := NA]
  DB1B[, Avg.Fare := sum(MktFare * Passengers, na.rm= TRUE) / sum(Passengers, na.rm = TRUE),
       by = product_group]; gc(); gc();
  DB1B <- DB1B[!is.nan(Avg.Fare),]
  DB1B[, Product_Name := paste(Origin, Dest, Carrier, MktMilesFlown, NonStop, AirportGroup)]
  DB1B[, Product_Name := factor(Product_Name)]
  
  DB1B <- DB1B %>% select(Year, Quarter, Origin, Origin.City, Dest, Destination.City, 
                          Carrier, MktMilesFlown, NonStop,
                          Product_Name, Passengers.Product, Passengers.Inside.Market,
                          Avg.Fare, MktCoupons, NonStopMiles,
                          Origin.City, Destination.City, AirportGroup);
  
  DB1B <- as.data.table(DB1B);
  DB1B <- unique(DB1B); gc(); gc()
  write_rds(DB1B, output)
}



step_clarify <- function(DB1B){
  DB1B <- DB1B[Year > 2015,]
  airline_CodeBook <- readRDS("02.Intermediate/Clean_Codebook.Rds")
  airline_CodeBook <- airline_CodeBook %>% filter(CARRIER %in% DB1B$TkCarrier) %>% 
    select(-CARRIER_ENTITY) %>% unique()
  
  # 99 is a flag that a customer switched service operators on their flight.  
  DB1B[TkCarrier == "99", TkCarrier := NA]
  DB1B[OpCarrier == "99", OpCarrier := NA]
  
  # Drop All NA TkCarrier, OpCarrier
  DB1B <- DB1B %>% filter(!is.na(TkCarrier), !is.na(OpCarrier)) %>% 
    data.table()
  
  DB1B$Carrier <- factor(x = DB1B$TkCarrier, levels = airline_CodeBook$CARRIER,
                         labels = airline_CodeBook$CARRIER_NAME)
  DB1B$TkCarrier <- NULL
  
  DB1B <- DB1B[!is.na(Carrier),]
  
  DB1B$Origin <- factor(DB1B$Origin)
  DB1B$Dest <- factor(DB1B$Dest)
  DB1B$DestState <- factor(DB1B$DestState)
  DB1B$OriginState <- factor(DB1B$OriginState)
  
  DB1B$OriginCountry <- NULL # All US at this point
  DB1B$DestCountry <- NULL# All US at this point
  
  # Now, handle subsidaries
  carrier_replace <- function(data, Current, New){
    data <- data[Carrier == Current, Carrier := New];
    return(data)
  }
  
  DB1B <- carrier_replace(DB1B, Current = "Endeavor Air Inc.", 
                          New = "Delta Air Lines Inc.")
  DB1B <- carrier_replace(DB1B, Current = "Horizon Air",
                          New = "Alaska Airlines Inc.")
  DB1B <- carrier_replace(DB1B, Current = "Envoy Air",
                          New = "American Airlines Inc.")
  DB1B <- carrier_replace(DB1B, Current = "Piedmont Airlines",
                          New = "American Airlines Inc.")
  DB1B <- carrier_replace(DB1B, Current = "PSA Airlines Inc.",
                          New = "American Airlines Inc.")
  
  # Handle the US-American Merger
  DB1B <- DB1B[Carrier == "US Airways Inc." & Year >= 2014,
               Carrier := "American Airlines Inc."]
  
  # Handle the Virgin-America merger with Alaska
  DB1B <- DB1B[Carrier == "Virgin America" & Year == 2016 & Quarter >= 2,
               Carrier := "Alaska Airlines Inc."]
  DB1B <- DB1B[Carrier == "Virgin America" & Year > 2016,
               Carrier := "Alaska Airlines Inc."]
  
  # Now, assign all minor carriers to the category "Minor"
  keep_carriers <- c("American Airlines Inc.", "United Air Lines Inc.",
                     "Delta Air Lines Inc.", "Alaska Airlines Inc.",
                     "Hawaiian Airlines Inc.", "Virgin America",
                     "JetBlue Airways", "Allegiant Air",
                     "Southwest Airlines Co.",
                     "Frontier Airlines Inc.", "Spirit Air Lines")
  DB1B[!Carrier %in% keep_carriers, Carrier := "Minor Carrier"]
  # Now, Add Cities to Airports
  # Read in T100 data
  T100 <- as.data.table(read_rds("02.Intermediate/Compile_T100.Rds"))
  
  # BTS changed reporting of cities over the period of interest. We will restrict ourselves to the 
  # shorter, more human readable version.
  T100 <- T100 %>% select(Origin_City, Origin_Alpha) %>% mutate(Length = nchar(Origin_City)) %>%
    filter(Length == min(Length), .by = Origin_Alpha) %>% unique()%>% select(Origin_City, Origin_Alpha)
  
  # MKT is a bus route rather than an aviation route
  T100 <- T100 %>% filter(Origin_Alpha != "MKT")
  
  # Some city markets have multiple airports serving them, 
  # based on Brueckner et al (2013)
  # First, Fix the Cities
  T100[Origin_Alpha %in% c("ORD", "MDW") , Origin_City := "Chicago, IL"]
  T100[Origin_Alpha %in% c("CVG", "DAY") , Origin_City := "Cincinnati, OH"]
  T100[Origin_Alpha %in% c("CLE", "CAK") , Origin_City := "Cleveland, OH"]
  T100[Origin_Alpha %in% c("DWF", "DAL") , Origin_City := "Dallas/Fort Worth, TX"]
  T100[Origin_Alpha %in% c("IAH", "HOU") , Origin_City := "Houston, TX"]
  T100[Origin_Alpha %in% c("LAX", "BUR", "LGB") , Origin_City := "Los Angeles, CA"]
  T100[Origin_Alpha %in% c("MIA", "FLL") , Origin_City := "Miami, FL"]
  T100[Origin_Alpha %in% c("LGA", "EWR", "JFK") , Origin_City := "New York, NY"]
  T100[Origin_Alpha %in% c("SFO", "OAK") , Origin_City := "San Francisco, CA"]
  T100[Origin_Alpha %in% c("TPA", "PIE") , Origin_City := "Tampa, FL"]
  T100[Origin_Alpha %in% c("DCA", "IAD", "BWI") , Origin_City := "Washington, DC"]
  
  #Additional cities as of Shrago (2022)
  T100[Origin_Alpha %in% c("AZA", "PHX"), Origin_City := "Phoenix, AZ"]
  T100[Origin_Alpha %in% c("SFB", "MCO"), Origin_City := "Orlando, FL"]
  
  # Rename T100 columns
  colnames(T100) <- c("City", "Airport")
  
  T100.Origin <- T100; colnames(T100.Origin) <- c("Origin.City", "Origin.Airport");
  T100.Dest <- T100; colnames(T100.Dest) <- c("Destination.City", "Destination.Airport")
  
  
  # Merge Plane Data with City Information
  DB1B <- merge(DB1B, T100.Origin, by.x = "Origin", by.y = "Origin.Airport",
                all.x = TRUE); gc();
  DB1B <- merge(DB1B, T100.Dest, by.x = "Dest", by.y = "Destination.Airport",
                all.x = TRUE); gc()
  
  return(DB1B)
}

# Note, this uses the ticket as base, rather than market
# As such, ItinFares are used
step_condense <- function(DB1B, fares_min = 15, fares_max = 1500){
  # In line with Shrago (2022), remove fares less than $15 to remove point redemptions
  DB1B <- as.data.table(DB1B); gc(); gc();
  orig_count <- nrow(DB1B)
  
  DB1B[ItinFare < fares_min, ItinFare := NA] 
  
  # In Line with Turner, Bet (2021 Both), remove fares greater than $1500 to avoid 'key punch errors'
  DB1B[ItinFare > fares_max, ItinFare := NA]
  
  # Restrict to two layovers or fewer
  DB1B[Coupons > 5, MktFare := NA]
  DB1B$NonStop <- DB1B$Coupons == 2
  
  market_group <- c("Year", "Quarter", "Origin", "Dest")
  product_group <- c(market_group, "Carrier", "NonStop", "AirportGroup", "MktCoupons", "MktMilesFlown")
  
  DB1B[, Passengers.Product := sum(Passengers, na.rm = TRUE) * 10, by = product_group]
  DB1B[, Passengers.Inside.Market := sum(Passengers, na.rm = TRUE) * 10, by = market_group]
  
  DB1B[is.na(ItinFare), Passengers := NA]
  DB1B[, Avg.Fare := sum(ItinFare * Passengers, na.rm= TRUE) / sum(Passengers, na.rm = TRUE),
       by = product_group]; gc(); gc();
  DB1B[, Product_Name := paste(Origin, Dest, Carrier, Distance, NonStop, AirportGroup)]
  DB1B[, Product_Name := factor(Product_Name)]
  DB1B[, MktMilesFlown := Distance]
  
  DB1B <- DB1B %>% select(Year, Quarter, Origin, Origin.City, Dest, Destination.City, 
                          Carrier, MktMilesFlown, NonStop,
                          Product_Name, Passengers.Product, Passengers.Inside.Market,
                          Avg.Fare, Coupons, NonStopMiles,
                          Origin.City, Destination.City, AirportGroup) %>% as.data.table();
  DB1B <- unique(DB1B); gc(); gc()
  return(DB1B)
}

step_control_add <- function(plane_data){
  # JetFuel quarterly average price 
  jetfuel <- readRDS("02.Intermediate/jet_fuel.rds")
  
  jetfuel$Quarter <- as.integer(as.character(jetfuel$Quarter))
  
  plane_data <- merge(plane_data, jetfuel, by.x = c("Year", "Quarter"),
                      by.y = c("Year", "Quarter"), all.x = TRUE); gc();
  
  # Now, add in the demographic control data
  plane_data[, MatchYear := Year]
  plane_data[Quarter < 3, MatchYear := MatchYear - 1]
  
  demographics <- fread("02.Intermediate/MSA_Population.csv")
  demographics <- demographics %>% select(Airport, MSA_Name, Year, MSA_Population) %>%
    mutate(MSA_Population = as.numeric(MSA_Population))
  
  origins <- demographics; colnames(origins) <- c("Origin", "Origin_MSA", "MatchYear", "Origin.Population")
  destinations <- demographics; colnames(destinations) <- c("Dest", "Destination_MSA", "MatchYear", 
                                                            "Destination.Population")
  
  # First, Merge Current Year, then the next
  plane_data <- merge(plane_data, origins, by.x = c("Origin", "MatchYear"),
                      by.y = c("Origin", "MatchYear"), all.x = TRUE); gc(); gc();
  origins[, MatchYear := MatchYear - 1]
  origins[, Origin.Next.Pop := Origin.Population]
  origins[, Origin.Population := NULL]
  plane_data <- merge(plane_data, origins, by.x = c("Origin", "Origin_MSA", "MatchYear"),
                      by.y = c("Origin","Origin_MSA", "MatchYear"), all.x = TRUE)
  
  plane_data[Quarter == 3, Origin.Population := 11/12 * Origin.Population + 1/12 * Origin.Next.Pop]
  plane_data[Quarter == 4, Origin.Population := 8/12 * Origin.Population + 4/12 * Origin.Next.Pop]
  plane_data[Quarter == 1, Origin.Population := 5/12 * Origin.Population + 7/12 * Origin.Next.Pop]
  plane_data[Quarter == 2, Origin.Population := 2/12 * Origin.Population + 10/12 * Origin.Next.Pop]
  plane_data[, Origin.Next.Pop := NULL]
  
  
  plane_data <- merge(plane_data, destinations, by.x = c("Dest", "MatchYear"),
                      by.y = c("Dest","MatchYear"), all.x = TRUE); gc(); gc();
  destinations[, MatchYear := MatchYear + 1]
  destinations[, Destination.Next.Pop := Destination.Population]
  destinations[, Destination.Population := NULL]
  plane_data <- merge(plane_data, destinations, by.x = c("Dest", "Destination_MSA", "MatchYear"),
                      by.y = c("Dest", "Destination_MSA", "MatchYear"), all.x = TRUE)
  plane_data[Quarter == 3, Destination.Population := 11/12 * Destination.Population + 1/12 * Destination.Next.Pop]
  plane_data[Quarter == 4, Destination.Population := 8/12 * Destination.Population + 4/12 * Destination.Next.Pop]
  plane_data[Quarter == 1, Destination.Population := 5/12 * Destination.Population + 7/12 * Destination.Next.Pop]
  plane_data[Quarter == 2, Destination.Population := 2/12 * Destination.Population + 10/12 * Destination.Next.Pop]
  plane_data[, Destination.Next.Pop := NULL]
  plane_data[, MatchYear := NULL]
  
  
  
  # Now, add in Firm Presence in the Market
  
  Operator_Key <- plane_data %>% 
    select(Origin.City, Destination.City, Carrier, Year, Quarter) %>%
    unique() %>% filter(Carrier %in% c("Delta Air Lines Inc.",
                                       "Alaska Airlines Inc.",
                                       "American Airlines Inc.",
                                       "Hawaiian Airlines Inc.",
                                       "JetBlue Airways","Southwest Airlines Co.",
                                       "Frontier Airlines Inc.", "Allegiant Air",
                                       "Spirit Air Lines",
                                       "United Air Lines Inc.")) %>% 
    mutate(Operating = TRUE); gc();
  
  Operator_Key <- as.data.table(Operator_Key)
  Operator_Key$Carrier <- as.character(Operator_Key$Carrier)
  Operator_Key[Carrier == "Delta Air Lines Inc.",
               Carrier := "Delta"]
  Operator_Key[Carrier ==  "Alaska Airlines Inc.",
               Carrier := "Alaska"]
  Operator_Key[Carrier == "American Airlines Inc.",
               Carrier := "American"]
  Operator_Key[Carrier == "Hawaiian Airlines Inc.",
               Carrier := "Hawaiian"]
  Operator_Key[Carrier == "JetBlue Airways",
               Carrier := "JetBlue"]
  Operator_Key[Carrier == "Frontier Airlines Inc.",
               Carrier := "Frontier"]
  Operator_Key[Carrier == "Allegiant Air",
               Carrier := "Allegiant"]
  Operator_Key[Carrier == "Spirit Air Lines",
               Carrier := "Spirit"]
  Operator_Key[Carrier == "United Air Lines Inc.",
               Carrier := "United"]
  Operator_Key[Carrier == "Southwest Airlines Co.",
               Carrier := "Southwest"]
  
  # Go from long to wide with the above data table.
  Operator_Key <- dcast(Operator_Key, formula = Origin.City + Destination.City + Year + Quarter ~ Carrier + Operating)
  colnames(Operator_Key) <- c("Origin.City","Destination.City","Year", "Quarter", "Alaska_Prescence",
                              "Allegiant_Prescence", "American_Prescence", "Delta_Prescence",
                              "Frontier_Prescence", "Hawaiian_Prescence",      
                              "JetBlue_Prescence", "Southwest_Prescence", "Spirit_Prescence", "United_Prescence")
  
  # matchKey allows for more memory efficiency while merging.
  Operator_Key <- Operator_Key %>% mutate(matchKey = paste(Year, Quarter, Origin.City, Destination.City),
                                          Year = NULL, Quarter = NULL, Origin.City = NULL, Destination.City = NULL)
  plane_data <- plane_data %>% mutate(matchKey = paste(Year, Quarter, Origin.City, Destination.City))
  
  plane_data <- merge(plane_data, Operator_Key, by = "matchKey", all.x = TRUE); gc(); 
  
  # Handle NA values
  plane_data <- as.data.table(plane_data)
  plane_data[is.na(Delta_Prescence), Delta_Prescence := FALSE]
  plane_data[is.na(Alaska_Prescence), Alaska_Prescence := FALSE]
  plane_data[is.na(American_Prescence), American_Prescence := FALSE]
  plane_data[is.na(Hawaiian_Prescence), Hawaiian_Prescence := FALSE]
  plane_data[is.na(JetBlue_Prescence), JetBlue_Prescence := FALSE]
  plane_data[is.na(Frontier_Prescence), Frontier_Prescence := FALSE]
  plane_data[is.na(Allegiant_Prescence), Allegiant_Prescence := FALSE]
  plane_data[is.na(Spirit_Prescence), Spirit_Prescence := FALSE]
  plane_data[is.na(United_Prescence), United_Prescence := FALSE]
  plane_data[is.na(Southwest_Prescence), Southwest_Prescence := FALSE]
  
  plane_data$matchKey <- NULL
  
  # Any Market with the Firm Already operating in it does not count as 
  # entry 'potential'
  plane_data[Alaska_Prescence == TRUE, Alaska_Entry_Potential := FALSE]
  plane_data[American_Prescence == TRUE, American_Entry_Potential := FALSE]
  plane_data[JetBlue_Prescence == TRUE, JetBlue_Entry_Potential := FALSE]
  plane_data[Spirit_Prescence == TRUE, Spirit_Entry_Potential := FALSE]
  plane_data[United_Prescence == TRUE, United_Entry_Potential := FALSE]
  plane_data[Southwest_Prescence == TRUE, Southwest_Entry_Potential := FALSE]
  plane_data[Delta_Prescence == TRUE, Delta_Entry_Potential := FALSE]
  plane_data[Allegiant_Prescence == TRUE, Allegiant_Entry_Potential := FALSE]
  plane_data[Frontier_Prescence == TRUE, Frontier_Entry_Potential := FALSE]
  
  # Additionally, a firm does not care about its own prescence
  plane_data[Carrier == "Alaska Airlines Inc.", Alaska_Prescence := TRUE]
  plane_data[Carrier == "American Airlines Inc.", American_Prescence := TRUE]
  plane_data[Carrier == "JetBlue Airways", JetBlue_Prescence := TRUE]
  plane_data[Carrier == "Spirit Air Lines", Spirit_Prescence := TRUE]
  plane_data[Carrier == "United Air Lines Inc.", United_Prescence := TRUE]
  plane_data[Carrier == "Southwest Airlines Co.", Southwest_Prescence := TRUE]
  plane_data[Carrier == "Delta Air Lines Inc.", Delta_Prescence := TRUE]
  plane_data[Carrier == "Allegiant Air", Allegiant_Prescence := TRUE]
  plane_data[Carrier == "Frontier Airlines Inc.", Frontier_Prescence := TRUE]
  
  # Now, Add Price_Index Data
  price_index <- readRDS("02.Intermediate/price_index.rds")
  plane_data <- merge(x = plane_data, y = price_index,
                      all.x = TRUE, all.y = FALSE,
                      by = c("Year", "Quarter"))
  
  # Add Data on if an airport is a hub or focus city for a given airline
  hublist <- fread("01.Input/15.HubList/HubList.csv")
  hublist[Carrier == "Delta", Carrier := "Delta Air Lines Inc."]
  hublist[Carrier == "American", Carrier := "American Airlines Inc."]
  hublist[Carrier == "United", Carrier := "United Air Lines Inc."]
  hublist[Carrier == "Alaskan", Carrier := "Alaska Airlines Inc."]
  hublist[Carrier == "JetBlue", Carrier := "JetBlue Airways"]
  hublist[Carrier == "Spirit", Carrier := "Spirit Air Lines"]
  hublist[Carrier == "Hawaiian", Carrier :=  "Hawaiian Airlines Inc."]
  hublist[Carrier == "Southwest", Carrier := "Southwest Airlines Co."]
  hublist[Carrier == "Frontier", Carrier := "Frontier Airlines Inc."]
  hublist[Carrier == "Allegiant", Carrier := "Allegiant Air" ]
  
  hublist.origin <- hublist;
  hublist.dest <- hublist;
  
  colnames(hublist.origin) <- c("Origin", "Carrier", "Origin.Airport.Type")
  plane_data <- merge(plane_data, hublist.origin, all.x = TRUE, by = c("Origin", "Carrier"))
  colnames(hublist.dest) <- c("Dest", "Carrier", "Destination.Airport.Type")
  plane_data <- merge(plane_data, hublist.dest, all.x = TRUE, by = c("Dest", "Carrier"))
  plane_data[is.na(Origin.Airport.Type), Origin.Airport.Type := "Regular"]
  plane_data[is.na(Destination.Airport.Type), Destination.Airport.Type := "Regular"]
  plane_data[, Destination_Hub := Destination.Airport.Type == "Hub"]
  plane_data[, Origin_Hub := Origin.Airport.Type == "Hub"]
  
  
  # Now, Variable for if at least one intermediate airport was a hub for a given firm.
  plane_data[, IntermediateHub := FALSE]
  plane_data[, HubCheck := substr(x = AirportGroup, start = 4, stop = 1000)]
  
  for(i in 1:length(unique(hublist$Carrier))){
    current_hubs <- hublist %>% filter(Carrier %in% unique(hublist$Carrier)[i],
                                       HubType == "Hub");
    current_hubs <- current_hubs$AirportCode
    
    current_hubs[1] <- paste("(:", current_hubs[1], sep = "");
    current_hubs[length(current_hubs)] <- paste(current_hubs[length(current_hubs)], ")", sep = "")  
    current_hubs <- paste(current_hubs, sep = "", collapse = ")|(:")
    plane_data[Carrier == unique(hublist$Carrier)[i] &
                 grepl(pattern = current_hubs, x = HubCheck) == TRUE,
               IntermediateHub := TRUE]
  }
  
  plane_data[, HubCheck := NULL]
  
  # Calculate Share of NonStop Products out of Origin, Destination Airports
  plane_data[, Next.Airport := substr(x = AirportGroup, start = 5, stop = 7)]
  nonstop_plane <- plane_data[NonStop == TRUE, .(Year, Quarter, Origin, Next.Airport,
                                                 Carrier)]
  plane_data[, Next.Airport := NULL]
  nonstop_plane <- unique(nonstop_plane)
  
  # Evaluate all destinations available
  # (t = total)
  nonstop_plane_t <- nonstop_plane;
  nonstop_plane_t$Carrier <- NULL
  nonstop_plane_t <- unique(nonstop_plane_t)
  nonstop_plane_t[, Destinations.Available := .N, by = c("Year", "Quarter", "Origin")]
  
  nonstop_plane <- merge(nonstop_plane, nonstop_plane_t, by = c("Year", "Quarter", "Origin", "Next.Airport"))
  
  # Now, For Each Firm, Identify Number of Destinations They Serve
  nonstop_plane[, Firm.Destinations := .N, by = c("Carrier", "Year", "Quarter", "Origin")]
  nonstop_plane[, Firm.Ratio := Firm.Destinations / Destinations.Available * 100]
  
  ratio_data <- nonstop_plane %>%
    select(Year, Quarter, Carrier, Origin, Firm.Destinations, Firm.Ratio) %>% unique() %>%
    as.data.table()
  
  # First, Origin Ratio Data
  ratio_data_origins <- ratio_data
  colnames(ratio_data_origins) <- c("Year", "Quarter", "Carrier", "Origin", "Origin_Firm_Destinations", "Origin_Firm_Service_Ratio")
  ratio_data_destinations <- ratio_data
  colnames(ratio_data_destinations) <- c("Year", "Quarter", "Carrier", "Dest", "Destination_Firm_Destinations",
                                         "Destination_Firm_Service_Ratio")
  
  plane_data <- merge(plane_data, ratio_data_origins, by = c("Year", "Quarter", "Carrier", "Origin"),
                      all.x = TRUE);
  plane_data <- merge(plane_data, ratio_data_destinations, by = c("Year", "Quarter", "Carrier", "Dest"),
                      all.x = TRUE)
  
  # Remove Glitchy Obs for Now
  plane_data <- plane_data[!is.na(Destination_Firm_Destinations),]
  plane_data <- plane_data[!is.na(Origin_Firm_Destinations),]
  
  # North Eastern Alliance: Product, Control Variables
  plane_data[, NorthEastAlliance_Route := FALSE]
  plane_data[grepl(pattern = "JFK", x = AirportGroup), NorthEastAlliance_Route := TRUE]
  plane_data[grepl(pattern = "EWR", x = AirportGroup), NorthEastAlliance_Route := TRUE]
  plane_data[grepl(pattern = "LGA", x = AirportGroup), NorthEastAlliance_Route := TRUE]
  plane_data[grepl(pattern = "BOS", x = AirportGroup), NorthEastAlliance_Route := TRUE]
  
  plane_data[, NorthEastAlliance_Operator := Carrier %in% c("JetBlue Airways", "American Airlines Inc.")]
  
  plane_data[, NorthEastAlliance_Product := (Year %in% c(2021, 2022, 2023)) & 
               NorthEastAlliance_Route & NorthEastAlliance_Operator]
  
  # Include Average Income Data
  income_data.long <- readRDS("02.Intermediate/IncomeData.rds")
  
  colnames(income_data.long) <- c("Origin_MSA", "Year", "Origin_Income_PerCap")
  income_data.long$Year <- as.numeric(as.character(income_data.long$Year))
  plane_data <- merge(plane_data, income_data.long, all.x = TRUE,
                      by = c("Origin_MSA", "Year"))
  colnames(income_data.long) <- c("Destination_MSA", "Year", "Dest_Income_PerCap")
  plane_data <- merge(plane_data, income_data.long, all.x = TRUE,
                      by = c("Destination_MSA", "Year"))
  
  # Include Per Capita Covid Cases
  covid <- readRDS("02.Intermediate/Covid_State.rds")
  state_pop <- readRDS("02.Intermediate/State_Populations.rds")
  covid <- merge(covid, state_pop, by = c("Year", "State"),
                 all.x = TRUE)
  covid <- covid[!is.na(Population)]
  covid[, Per_Capita_Covid_Cases := Covid_Cases / Population * 1000]
  covid[, Per_Capita_Covid_Deaths := Covid_Deaths / Population * 1000]
  covid[, Population := NULL]
  
  plane_data[, Origin_State := substr(Origin.City,
                                      start = nchar(Origin.City) - 1, stop = nchar(Origin.City))]
  plane_data[, Destination_State := substr(Destination.City,
                                           start = nchar(Destination.City) - 1, stop = nchar(Destination.City))]
  
  colnames(covid) <- c("Year", "Origin_State", "Quarter", "Origin_Covid_Cases",
                       "Origin_Covid_Deaths", "Origin_PC_Covid_Cases", 
                       "Origin_PC_Covid_Deaths")
  
  plane_data <- merge(plane_data, covid, by = c("Year", "Quarter", "Origin_State"),
                      all.x = TRUE)
  
  colnames(covid) <- c("Year", "Destination_State", "Quarter", "Destination_Covid_Cases",
                       "Destination_Covid_Deaths", "Destination_PC_Covid_Cases", 
                       "Destination_PC_Covid_Deaths")
  
  plane_data <- merge(plane_data, covid, by = c("Year", "Quarter", "Destination_State"),
                      all.x = TRUE)
  
  plane_data[is.na(Origin_Covid_Cases), Origin_Covid_Cases := 0]
  plane_data[is.na(Origin_Covid_Deaths), Origin_Covid_Deaths := 0]
  plane_data[is.na(Origin_PC_Covid_Cases), Origin_PC_Covid_Cases := 0]
  plane_data[is.na(Origin_PC_Covid_Deaths), Origin_PC_Covid_Deaths := 0]
  plane_data[is.na(Destination_Covid_Cases), Destination_Covid_Cases := 0]
  plane_data[is.na(Destination_Covid_Deaths), Destination_Covid_Deaths := 0]
  plane_data[is.na(Destination_PC_Covid_Cases), Destination_PC_Covid_Cases := 0]
  plane_data[is.na(Destination_PC_Covid_Deaths), Destination_PC_Covid_Deaths := 0]
  
  # Include Lagged HHI As a Possible Control
  plane_data[, Firm_Share_Passengers := 100 * 
               sum(Passengers.Product) / Passengers.Inside.Market,
             by = c("Origin", "Dest", "Year", "Quarter", "Carrier")]
  plane_data[, Market_HHI := sum(Firm_Share_Passengers^2), 
             by = c("Origin", "Dest", "Year", "Quarter")]
  plane_data[, Year_Quarter := paste(Year, Quarter)]
  
  hhi_frame <- unique(plane_data[, .(Year_Quarter, Origin, Dest,
                                     Market_HHI)])
  
  hhi_frame <- hhi_frame[order(Year_Quarter),]
  hhi_frame[, Market_HHI.Lag := shift(Market_HHI, type = "lag"), 
            by = c("Origin", "Dest")]
  
  plane_data <- merge(plane_data, hhi_frame,
                      by = c("Origin", "Dest", "Year_Quarter",
                             "Market_HHI"),
                      all.x = TRUE)
  return(plane_data)
}

step_unify <- function(target = "02.Intermediate/Compile_Step_DB1B.rds",
                       years = c(2016:2019, 2021:2023),
                       round_trip_only = TRUE){
  db1b_market <- "01.Input/01.DB1B_Data_Market/"
  db1b_ticket <-  "01.Input/09.DB1B_Data_Ticket/"
  
  file_keys <- c();
  for(i in 1:length(years)){
    current_year <- years[i]
    if(current_year == 2023){
      quarters <- c(1,2,3)
    } else {
      quarters <- c(1,2,3,4)
    }
    for(j in 1:length(quarters)){
      file_keys <- c(file_keys, paste(current_year, "_", quarters, sep = ""))
    }
  }
  
  for(i in 1:length(file_keys)){
    current_key <- file_keys[i]
    current_market <- fread(paste(db1b_market, current_key, ".csv", sep = ""))
    current_ticket <- fread(paste(db1b_ticket, current_key, ".csv", sep = ""))
    current_merge <- merge(current_market, current_ticket)
    if(round_trip_only == TRUE){
      current_merge <- current_merge[RoundTrip == 1,]
    }
    remove(current_market); remove(current_ticket); gc()
    current_merge <- step_clarify(DB1B = current_merge); gc();
    current_merge <- step_condense(DB1B = current_merge); gc(); 
    current_merge <- step_control_add(plane_data = current_merge); gc(); 
    write_rds(current_merge, paste("02.Intermediate/Step_Construct/", file_keys[i], ".rds", sep = "")); gc();
    i; 
  }
  remove(current_merge); gc(); 
  
  compiled_db1b <- c()
  for(i in 1:length(file_keys)){
    if(i == 1){
      compiled_db1b <- readRDS(paste("02.Intermediate/Step_Construct/", file_keys[i], ".rds", sep = ""))
    } else {
      compiled_db1b <- rbind(compiled_db1b,
                             readRDS(paste("02.Intermediate/Step_Construct/", file_keys[i], ".rds", sep = "")))
    }
  }
  write_rds(compiled_db1b, target)
}