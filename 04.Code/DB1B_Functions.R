unify_DB1B_Ticket <- function(target = "02.Intermediate/Compile_DB1B_Ticket.rds"){
  file_head <- "01.Input/09.DB1B_Data_Ticket"
  file_list <- list.files(path = file_head)
  
  compile <- paste(file_head, file_list, sep = "/") %>%
    map_df(~fread(.))
  
  compile <- unique(compile)
  
  write_rds(compile, target)
}


unify_DB1B <- function(target = "02.Intermediate/Construct_DB1B/DB1B_Initial.Rds"){
  file_head <- "01.Input/01.DB1B_Data_Market"
  file_list <- list.files(path = file_head)

  readSave <- function(input){
    new <- fread(input, select = c("ItinID", "MktCoupons", "Year", "Quarter", "OriginAirportID",
                                   "OriginCityMarketID", "Origin", "OriginCountry",
                                   "OriginState", "Dest", "DestCountry", "DestCityMarketID",
                                   "DestState", "TkCarrier", "TkCarrierGroup", 
                                   "RPCarrier", "OpCarrier", 
                                   "OpCarrierGroup", "Passengers", "MktFare", "MktMilesFlown",
                                   "NonStopMiles", "AirportGroup"))
    output = paste("02.Intermediate/Initial_DB1B_ReadIn/", substr(input, start = 71, stop = 76), ".rds", sep = "")
    saveRDS(new, output)
  }
  
  files <- paste(file_head, file_list, sep = "/")
  foreach(name = 1:length(files)) %dopar% {
    readSave(files[name]); gc();
  }
  
  new_folder <- "02.Intermediate/Initial_DB1B_ReadIn/"
  file_list <- list.files(path = new_folder)
  
  combined_data <- c()
  for(i in 1:length(file_list)){
    if(i == 1){
      combined_data <- readRDS(paste(new_folder, file_list[i], sep = ""))
    } else {
      combined_data <- rbind(combined_data,
                             readRDS(paste(new_folder, file_list[i], sep = "")))
    }
  }
  
  gc(); 
  combined_data <- unique(combined_data)
  
  saveRDS(combined_data, target)
}

clarify_DB1B <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Initial.Rds",
                         output = "02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds",
                         mode = "ticket", subsidiary_handle = FALSE,
                         nea = "normal"){
  DB1B <- readRDS(input)
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

  if(mode == "ticket"){
    DB1B$Carrier <- factor(x = DB1B$TkCarrier, levels = airline_CodeBook$CARRIER,
                           labels = airline_CodeBook$CARRIER_NAME)
  } else if(mode == "operating"){
    DB1B$Carrier <- factor(x = DB1B$OpCarrier, levels = airline_CodeBook$CARRIER,
                           labels = airline_CodeBook$CARRIER_NAME)
  }
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
  
  
  if(subsidiary_handle == TRUE){
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
  } else {
    DB1B <- DB1B[!Carrier %in% c("Endeavor Air Inc.", "Horizon Air", "Envoy Air",
                                 "Piedmont Airlines",
                                 "PSA Airlines Inc.")]
  }
  
  # Handle the US-American Merger
  DB1B <- DB1B[Carrier == "US Airways Inc." & Year >= 2014,
               Carrier := "American Airlines Inc."]
  
  # Handle the Virgin-America merger with Alaska
  DB1B <- DB1B[Carrier == "Virgin America" & Year == 2016 & Quarter >= 2,
               Carrier := "Alaska Airlines Inc."]
  DB1B <- DB1B[Carrier == "Virgin America" & Year > 2016,
               Carrier := "Alaska Airlines Inc."]
  
  # Now, assign all minor carriers to the category "Minor"
  # keep_carriers <- c("American Airlines Inc.", "United Air Lines Inc.",
  #                    "Delta Air Lines Inc.", "Alaska Airlines Inc.",
  #                    "Hawaiian Airlines Inc.", "Virgin America",
  #                    "JetBlue Airways", "Allegiant Air",
  #                    "Southwest Airlines Co.",
  #                    "Frontier Airlines Inc.", "Spirit Air Lines")
  
  # keep_carriers <- c("American Airlines Inc.", "United Air Lines Inc.",
  #                    "Delta Air Lines Inc.", 
  #                    "JetBlue Airways", 
  #                    "Southwest Airlines Co.", "Spirit Air Lines")
  # 
  # 
  # DB1B[!Carrier %in% keep_carriers, Carrier := "Minor Carrier"]
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
  
  # If NEA set to weak, assign all routes touching NYC, BOS to "NEA Carrier"
  if(nea == "weak"){
    DB1B[Year >= 2020 & grepl(pattern = "JFK", x = AirportGroup) & Carrier %in%
           c("American Airlines Inc.", "JetBlue Airways"), Carrier := "NEA"]
    DB1B[Year >= 2020 & grepl(pattern = "EWR", x = AirportGroup)& Carrier %in%
           c("American Airlines Inc.", "JetBlue Airways"), Carrier := "NEA"]
    DB1B[Year >= 2020 & grepl(pattern = "LGA", x = AirportGroup)& Carrier %in%
           c("American Airlines Inc.", "JetBlue Airways"), Carrier := "NEA"]
    DB1B[Year >= 2020 & grepl(pattern = "BOS", x = AirportGroup)& Carrier %in%
           c("American Airlines Inc.", "JetBlue Airways"), Carrier := "NEA"]
  }
  
  saveRDS(DB1B, file = output)
}

condense_db1b <- function(input, output, fares_min = 15, fares_max = 2000){
  DB1B <- read_rds(input)
  
  # In line with Shrago (2022), remove fares less than $15 to remove point redemptions
  DB1B <- as.data.table(DB1B); gc(); gc();
  orig_count <- nrow(DB1B)
  
  print(paste("Low Fares:", nrow(DB1B[MktFare < fares_min,]) / orig_count * 100, " Of Sample"))
  print(paste("High Fares:", nrow(DB1B[MktFare > fares_max,]) / orig_count * 100, " Of Sample"))
  
  DB1B[MktFare < fares_min, MktFare := NA] 
  DB1B[MktFare > fares_max, MktFare := NA]
  
  # Remove All Tickets in Top/Bottom 1 Percent of Fare, Yield For a Quarter
  DB1B[, Yield := MktFare / MktMilesFlown]
  DB1B[, Bottom.Fare.Cutoff := unname(quantile(x = MktFare, probs = 0.01, na.rm = TRUE)), by = c("Year", "Quarter")]
  DB1B[, Top.Fare.Cutoff := unname(quantile(x = MktFare, probs = 0.99, na.rm = TRUE)), by = c("Year", "Quarter")]
  DB1B[, Bottom.Yield.Cutoff := unname(quantile(x = Yield, probs = 0.01, na.rm = TRUE)), by = c("Year", "Quarter")]
  DB1B[, Top.Yield.Cutoff := unname(quantile(x = Yield, probs = 0.99, na.rm = TRUE)), by = c("Year", "Quarter")]

  DB1B[MktFare < Bottom.Fare.Cutoff, MktFare := NA]
  DB1B[Yield < Bottom.Yield.Cutoff, MktFare := NA]
  DB1B[MktFare > Top.Fare.Cutoff, MktFare := NA]
  DB1B[Yield > Top.Yield.Cutoff, MktFare := NA]
  
  # Restrict to three layovers or fewer
  print(paste("High Legs:", nrow(DB1B[MktCoupons >= 4,]) / orig_count * 100, " Of Sample"))
  DB1B[MktCoupons >= 4, MktFare := NA]
  
  
  DB1B$NonStop <- DB1B$MktCoupons == 1
  
  market_group <- c("Year", "Quarter", "Origin", "Dest")
  product_group <- c(market_group, "Carrier", "NonStop")
 
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
  DB1B[, Product_Name := paste(Origin, Dest, Carrier, NonStop)]
  DB1B[, Product_Name := factor(Product_Name)]
  
  # TESTING: CAPTURING SPIRIT USAGE FEE
 # DB1B[Carrier == "Spirit Air Lines", Avg.Fare := Avg.Fare + 22.99 * MktCoupons]
  
  DB1B <- DB1B %>% select(Year, Quarter, Origin, Origin.City, Dest, Destination.City, 
                                          Carrier, MktMilesFlown, NonStop,
                                          Product_Name, Passengers.Product, Passengers.Inside.Market,
                                          Avg.Fare, NonStopMiles,
                                      Origin.City, Destination.City);
  
  DB1B <- as.data.table(DB1B);
  DB1B <- unique(DB1B); gc(); gc()
  write_rds(DB1B, output)
}


airport_service_ratios <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds",
                                   output = "02.Intermediate/Airport_Service_Ratios.Rds",
                                   merger = FALSE){
  db1b <- as.data.table(readRDS(input))
 
  if(merger == TRUE){
    db1b[Carrier == "Spirit Air Lines", Carrier := "JetBlue Airways"]
  }
  
  db1b[, Next.Airport := substr(x = AirportGroup, start = 5, stop = 7)]
  nonstop_plane <- db1b[, .(Year, Quarter, Origin, Next.Airport,
                                  Carrier)]
  db1b[, Next.Airport := NULL]
  nonstop_plane <- unique(nonstop_plane)
  
  # Evaluate all destinations available
  # (t = total)
  nonstop_plane_t <- nonstop_plane;
  nonstop_plane_t$Carrier <- NULL
  nonstop_plane_t <- unique(nonstop_plane_t)
  nonstop_plane_t[, Destinations.Available := .N, by = c("Year", "Quarter", "Origin")]
  nonstop_plane_t[, Next.Airport := NULL]
  nonstop_plane_t <- unique(nonstop_plane_t)
  
  # Now, For Each Firm, Identify Number of Destinations They Serve
  nonstop_plane[, Firm.Destinations := .N, by = c("Carrier", "Year", "Quarter", "Origin")]
  
  # Merge, Calculate Ratio
  nonstop_plane <- merge(nonstop_plane, nonstop_plane_t, by = c("Year", "Quarter", "Origin"),
                         all.x = TRUE)
  nonstop_plane[, Firm.Ratio := Firm.Destinations / Destinations.Available * 100]
 
  ratio_data <- nonstop_plane %>%
    select(Year, Quarter, Carrier, Origin, Firm.Destinations, Firm.Ratio) %>% unique() %>%
    as.data.table()
  
  if(merger == TRUE){
    keep_carriers <- c("American Airlines Inc.", "United Air Lines Inc.",
                       "Delta Air Lines Inc.", "Alaska Airlines Inc.",
                       "JetBlue Airways", "Allegiant Air",
                       "Southwest Airlines Co.",
                       "Frontier Airlines Inc.", "Spirit Air Lines")
    
    ratio_data[!Carrier %in% keep_carriers, Carrier := "Minor Carrier"]
    ratio_data <- ratio_data %>% group_by(Year, Quarter, Carrier, Origin) %>%
      summarize(Firm.Destinations = max(Firm.Destinations),
                Firm.Ratio = max(Firm.Ratio)) %>% unique() %>%
      as.data.table()
  }
  
  saveRDS(ratio_data, output)
}



add_control_variables <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Condensed.rds", 
                                  output = "02.Intermediate/DB1B_With_Controls.Rds"){
  # Read in DB1B Data
  plane_data <- as.data.table(readRDS(input))
  
  # JetFuel quarterly average price 
  jetfuel <- readRDS("02.Intermediate/JetFuel_Reports.rds")
  
  plane_data <- merge(plane_data, jetfuel, by.x = c("Year", "Quarter", "Carrier"),
                      by.y = c("Year", "Quarter", "Carrier"), all.x = TRUE); gc();
  
  jetfuel <- jetfuel %>% select(Year, Quarter, National_Average_JF_Price) %>% unique()
  plane_data.no_match <- plane_data[is.na(Jet_Fuel_Price),]
  plane_data.no_match[, Jet_Fuel_Price := NULL]
  plane_data.no_match[, National_Average_JF_Price := NULL]
  plane_data.no_match <- merge(jetfuel, plane_data.no_match)
  plane_data.match <- plane_data[!is.na(Jet_Fuel_Price),]
  
  plane_data <- rbind(plane_data.match, plane_data.no_match, fill = TRUE)
  
 # plane_data[is.na(Jet_Fuel_Price), Jet_Fuel_Price := National_Average_JF_Price]
   plane_data[, Jet_Fuel_Price := National_Average_JF_Price]
  
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
    select(Origin, Dest, Carrier, Year, Quarter) %>%
    unique() %>% filter(Carrier %in% c("American Airlines Inc.", "United Air Lines Inc.",
                                       "Delta Air Lines Inc.",
                                       "JetBlue Airways",
                                       "Southwest Airlines Co.", "Spirit Air Lines")) %>% 
    mutate(Operating = TRUE); gc();
  
  Operator_Key <- as.data.table(Operator_Key)
  Operator_Key$Carrier <- as.character(Operator_Key$Carrier)
  Operator_Key[Carrier == "Delta Air Lines Inc.",
               Carrier := "Delta"]
  # Operator_Key[Carrier ==  "Alaska Airlines Inc.",
  #              Carrier := "Alaska"]
  Operator_Key[Carrier == "American Airlines Inc.",
               Carrier := "American"]
  # Operator_Key[Carrier == "Hawaiian Airlines Inc.",
  #              Carrier := "Hawaiian"]
  Operator_Key[Carrier == "JetBlue Airways",
               Carrier := "JetBlue"]
  # Operator_Key[Carrier == "Frontier Airlines Inc.",
  #              Carrier := "Frontier"]
  # Operator_Key[Carrier == "Allegiant Air",
  #              Carrier := "Allegiant"]
  Operator_Key[Carrier == "Spirit Air Lines",
               Carrier := "Spirit"]
  Operator_Key[Carrier == "United Air Lines Inc.",
               Carrier := "United"]
  Operator_Key[Carrier == "Southwest Airlines Co.",
               Carrier := "Southwest"]
  
  # Go from long to wide with the above data table.
  Operator_Key <- dcast(Operator_Key, formula = Origin + Dest + Year + Quarter ~ Carrier + Operating)
  colnames(Operator_Key) <- c("Origin","Dest","Year", "Quarter", 
                              # "Alaska_Prescence", "Allegiant_Prescence", 
                              "American_Prescence", "Delta_Prescence",
                            #  "Frontier_Prescence", "Hawaiian_Prescence",
                              "JetBlue_Prescence", "Southwest_Prescence", "Spirit_Prescence", "United_Prescence")
  

  
  # matchKey allows for more memory efficiency while merging.
  Operator_Key <- Operator_Key %>% mutate(matchKey = paste(Year, Quarter, Origin, Dest),
                                          Year = NULL, Quarter = NULL, Origin = NULL, Dest = NULL)
  plane_data <- plane_data %>% mutate(matchKey = paste(Year, Quarter, Origin, Dest))
  
  plane_data <- merge(plane_data, Operator_Key, by = "matchKey", all.x = TRUE); gc(); 
  
  # Handle NA values
  plane_data <- as.data.table(plane_data)
  plane_data[is.na(Delta_Prescence), Delta_Prescence := FALSE]
  # plane_data[is.na(Alaska_Prescence), Alaska_Prescence := FALSE]
  plane_data[is.na(American_Prescence), American_Prescence := FALSE]
  # plane_data[is.na(Hawaiian_Prescence), Hawaiian_Prescence := FALSE]
  plane_data[is.na(JetBlue_Prescence), JetBlue_Prescence := FALSE]
 # plane_data[is.na(Frontier_Prescence), Frontier_Prescence := FALSE]
# plane_data[is.na(Allegiant_Prescence), Allegiant_Prescence := FALSE]
  plane_data[is.na(Spirit_Prescence), Spirit_Prescence := FALSE]
  plane_data[is.na(United_Prescence), United_Prescence := FALSE]
  plane_data[is.na(Southwest_Prescence), Southwest_Prescence := FALSE]
  
  plane_data$matchKey <- NULL
  
  # Now, Calculate out the Propensity to Enter Variable
  # First, generate small version of input data;
  data.small <- plane_data %>% select(Origin, Dest, Carrier, Year, Quarter) %>%
    unique() %>% as.data.table()
  
  
  viabilities.list <- foreach(year = c(rep(2016, 4), rep(2017, 4), rep(2018, 4),
                                       rep(2019, 4), rep(2020, 4), rep(2021, 4),
                                       rep(2022, 4), rep(2023, 3)),
                              quarter = c(rep(c(1,2,3,4),7),
                                          1, 2, 3)) %dopar% {
        propensity_to_enter(input_data = data.small[Year == year & Quarter == quarter]);
                                          }
  
  
  viabilities.table <- rbindlist(viabilities.list)
  
  # Merge above with the Main Data
  plane_data <- merge(x = plane_data, y = viabilities.table,
                    all.x = TRUE, all.y = FALSE)
  
  # Any Market with the Firm Already operating in it does not count as 
  # entry 'potential'
#  plane_data[Alaska_Prescence == TRUE, Alaska_Entry_Potential := FALSE]
  plane_data[American_Prescence == TRUE, American_Entry_Potential := FALSE]
  plane_data[JetBlue_Prescence == TRUE, JetBlue_Entry_Potential := FALSE]
  plane_data[Spirit_Prescence == TRUE, Spirit_Entry_Potential := FALSE]
  plane_data[United_Prescence == TRUE, United_Entry_Potential := FALSE]
  plane_data[Southwest_Prescence == TRUE, Southwest_Entry_Potential := FALSE]
  plane_data[Delta_Prescence == TRUE, Delta_Entry_Potential := FALSE]
 # plane_data[Allegiant_Prescence == TRUE, Allegiant_Entry_Potential := FALSE]
 # plane_data[Frontier_Prescence == TRUE, Frontier_Entry_Potential := FALSE]
  
  # Additionally, a firm does not care about its own prescence
 #  plane_data[Carrier == "Alaska Airlines Inc.", Alaska_Prescence := FALSE]
  plane_data[Carrier == "American Airlines Inc.", American_Prescence := FALSE]
  plane_data[Carrier == "JetBlue Airways", JetBlue_Prescence := FALSE]
  plane_data[Carrier == "Spirit Air Lines", Spirit_Prescence := FALSE]
  plane_data[Carrier == "United Air Lines Inc.", United_Prescence := FALSE]
  plane_data[Carrier == "Southwest Airlines Co.", Southwest_Prescence := FALSE]
  plane_data[Carrier == "Delta Air Lines Inc.", Delta_Prescence := FALSE]
 # plane_data[Carrier == "Allegiant Air", Allegiant_Prescence := FALSE]
 # plane_data[Carrier == "Frontier Airlines Inc.", Frontier_Prescence := FALSE]
  
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
  plane_data[, Destination_Hub := ! Destination.Airport.Type == "Regular"]
  plane_data[, Origin_Hub := ! Origin.Airport.Type == "Regular"]
  
  # Calculate Share of NonStop Products out of Origin, Destination Airports
  ratio_data <- readRDS("02.Intermediate/Airport_Service_Ratios.Rds")
  
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
  plane_data[is.na(Destination_Firm_Destinations), Destination_Firm_Destinations :=0]
  plane_data[is.na(Destination_Firm_Service_Ratio), Destination_Firm_Service_Ratio := 0]
  
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
       "Destination_Covid_Deaths", "Destination_PC_Covid_Cases", "Destination_PC_Covid_Deaths")
  
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
  
  # Read in State Income
  state_income <- readRDS("02.Intermediate/state_income.rds")
  colnames(state_income) <- c("Origin_State", "Year", "Quarter", "OriginState_Pop", 
                              "OriginState_Income")
  plane_data <- merge(plane_data, state_income, by = c("Origin_State", "Year", "Quarter"))
  colnames(state_income) <- c("Destination_State", "Year", "Quarter", "DestinationState_Pop",
                              "DestinationState_Income")
  plane_data <- merge(plane_data, state_income, by = c("Destination_State", "Year", "Quarter"))
  
  # Adjust to Compile Minor Carriers:
  keep_carriers <- c("American Airlines Inc.", "United Air Lines Inc.",
                     "Delta Air Lines Inc.", "Alaska Airlines Inc.",
                     "JetBlue Airways", "Allegiant Air",
                     "Southwest Airlines Co.",
                     "Frontier Airlines Inc.", "Spirit Air Lines")

  plane_data[!Carrier %in% keep_carriers, Carrier := "Minor Carrier"]
  plane_data[Carrier == "Minor Carrier", Origin.Airport.Type := "Regular"]
  plane_data[Carrier == "Minor Carrier", Destination.Airport.Type := "Regular"]
  plane_data[Carrier == "Minor Carrier", Origin_Hub := FALSE]
  plane_data[Carrier == "Minor Carrier", Destination_Hub := FALSE]
  
  plane_data <- plane_data %>% group_by(Carrier, Destination_State, Year, Quarter, Origin_State,
                                        Origin, Dest, Year_Quarter, Market_HHI, Destination_MSA,
                                        Origin_MSA, Origin.City, Destination.City, NonStop,
                                        Passengers.Inside.Market, NonStopMiles, Jet_Fuel_Price,
                                        National_Average_JF_Price, Origin.Population,
                                        Destination.Population, American_Prescence, Delta_Prescence, JetBlue_Prescence,
                                        Southwest_Prescence, Spirit_Prescence, United_Prescence, Southwest_Entry_Potential,
                                        Allegiant_Entry_Potential, Frontier_Entry_Potential, 
                                        Delta_Entry_Potential, Alaska_Entry_Potential, American_Entry_Potential,
                                        United_Entry_Potential, Spirit_Entry_Potential, price_index,
                                        Origin.Airport.Type, Destination.Airport.Type, Destination_Hub,
                                        Origin_Hub, Origin_Income_PerCap, Dest_Income_PerCap,Origin_Covid_Cases,            
                                        Origin_Covid_Deaths, Origin_PC_Covid_Cases,Origin_PC_Covid_Deaths,        
                                        Destination_Covid_Cases,Destination_Covid_Deaths,Destination_PC_Covid_Cases,    
                                        Destination_PC_Covid_Deaths,Firm_Share_Passengers,Market_HHI.Lag,                
                                        OriginState_Pop,OriginState_Income,DestinationState_Pop,          
                                        DestinationState_Income) %>%
    summarize(Passengers.Product = sum(Passengers.Product),
              MktMilesFlown = sum(Passengers.Product * MktMilesFlown) / sum(Passengers.Product),
              Avg.Fare = sum(Avg.Fare * Passengers.Product) / sum(Passengers.Product), 
              Origin_Firm_Service_Ratio = max(Origin_Firm_Service_Ratio),
              Origin_Firm_Destinations = max(Origin_Firm_Destinations),
              Destination_Firm_Service_Ratio = max(Destination_Firm_Service_Ratio),
              Destination_Firm_Destinations = max(Destination_Firm_Destinations)) %>%
    as.data.table()
                                        

  saveRDS(plane_data, output)
}

propensity_to_enter <- function(input_data){
 For_Viability_Check <- input_data
  
  Delta_Cities <- For_Viability_Check %>% filter(Carrier == "Delta Air Lines Inc.")
  Delta_Cities <- unique(c(Delta_Cities$Dest, Delta_Cities$Origin))
  
  Alaska_Cities <- For_Viability_Check %>% filter(Carrier == "Alaska Airlines Inc.")
  Alaska_Cities <- unique(c(Alaska_Cities$Dest, Alaska_Cities$Origin))
  
  American_Cities <- For_Viability_Check %>% filter(Carrier == "American Airlines Inc.")
  American_Cities <- unique(c(American_Cities$Dest, American_Cities$Origin))

  JetBlue_Cities <- For_Viability_Check %>% filter(Carrier == "JetBlue Airways")
  JetBlue_Cities <- unique(c(JetBlue_Cities$Dest, JetBlue_Cities$Origin))
  
  Spirit_Cities <- For_Viability_Check %>% filter(Carrier == "Spirit Air Lines")
  Spirit_Cities <- unique(c(Spirit_Cities$Dest, Spirit_Cities$Origin))
  
  United_Cities <- For_Viability_Check %>% filter(Carrier == "United Air Lines Inc.")
  United_Cities <- unique(c(United_Cities$Dest, United_Cities$Origin))
  
  Southwest_Cities <- For_Viability_Check %>% filter(Carrier == "Southwest Airlines Co.")
  Southwest_Cities <- unique(c(Southwest_Cities$Dest, Southwest_Cities$Origin))
  
  Frontier_Cities <- For_Viability_Check %>% filter(Carrier == "Frontier Airlines Inc.")
  Frontier_Cities <- unique(c(Frontier_Cities$Dest, Frontier_Cities$Origin))
  
  Allegiant_Cities <- For_Viability_Check %>% filter(Carrier == "Allegiant Air")
  Allegiant_Cities <- unique(c(Allegiant_Cities$Dest, Allegiant_Cities$Origin))
  
  
  For_Viability_Check[, Delta_Entry_Potential := as.logical(floor((Origin %in% Delta_Cities + 
                                                             Dest %in% Delta_Cities)/2))]

  For_Viability_Check[, Alaska_Entry_Potential := as.logical(floor((Origin %in% Alaska_Cities + 
                                                               Dest %in% Alaska_Cities)/2))]

  For_Viability_Check[, American_Entry_Potential := as.logical(floor((Origin %in% American_Cities + 
                                                                 Dest %in% American_Cities)/2))]

  For_Viability_Check[, JetBlue_Entry_Potential := as.logical(floor((Origin %in% JetBlue_Cities + 
                                                               Dest %in% JetBlue_Cities)/2))]

  For_Viability_Check[, Spirit_Entry_Potential := as.logical(floor((Origin %in% Spirit_Cities + 
                                                               Dest %in% Spirit_Cities)/2))]

  For_Viability_Check[, United_Entry_Potential := as.logical(floor((Origin %in% United_Cities + Dest %in% United_Cities)/2))]

  For_Viability_Check[, Southwest_Entry_Potential := as.logical(floor((Origin %in% Southwest_Cities + 
                                                                         Dest %in% Southwest_Cities)/2))]

  For_Viability_Check[, Allegiant_Entry_Potential := as.logical(floor((Origin %in% Allegiant_Cities + 
                                                                         Dest %in% Allegiant_Cities)/2))]
  
  For_Viability_Check[, Frontier_Entry_Potential := as.logical(floor((Origin %in% Frontier_Cities + 
                                                                        Dest %in% Frontier_Cities)/2))]
  
  
  gc(); gc()
  
  return(For_Viability_Check)
}

round_trip_fix <- function(ticket_input = "02.Intermediate/Compile_DB1B_Ticket.rds",
                           market_input = "02.Intermediate/Compile_DB1B.Rds",
                           output_target = "02.Intermediate/DB1B_Round_Trip_Only.Rds"){
  ticket_data <- read_rds(ticket_input); 
  ticket_data[, ItinID := as.double(ItinID)]
  ticket_data[, IDOrigin := paste(ItinID, OriginAirportID)];   gc(); 
  
  ticket_data <- ticket_data[, c('ItinID','OriginAirportID','IDOrigin', 'RoundTrip', 'ItinFare', 'MilesFlown',
                                 'Distance', 'Year', 'Quarter', 'Coupons')]; gc();
  
  market_data <- read_rds(market_input);
  market_data[, ItinID := as.double(ItinID)]
  market_data[, IDOrigin := paste(ItinID, OriginAirportID)];   gc();
 
  # market_data[, True_Origin := IDOrigin %in% ticket_data$IDOrigin]; gc(); 
 
   # Keep only the True Origin + Destination of Round Trip Flights
  # market_data <- market_data[True_Origin == TRUE]; gc();
  
  ticket_data <- ticket_data[IDOrigin %in% market_data$IDOrigin,]; gc(); 
  colnames(ticket_data) <- c("ItinID", "OriginAirportID", "IDOrigin", "RoundTrip",
                             "Ticket_ItinFare", "Ticket_MilesFlown", "Ticket_Distance",
                             "Ticket_Year", "Ticket_Quarter", "Ticket_Coupon")
  
  # Merge Data
  market_data <- market_data %>% left_join(ticket_data); gc();
  remove(ticket_data); gc(); gc(); 
  
  # Now, restrict to only round trips
  # market_data <- market_data[RoundTrip == TRUE,]; gc();
  
  
  write_rds(market_data, output_target)
}

identify_large_airports <- function(input = "02.Intermediate/DB1B_City_Loc.Rds",
                                    output = "03.Output/Large_Airports.csv",
                                    y = 2022, q = 2){
  starting_data <- read_rds(input)
  condensed_data <- starting_data %>% filter(Year == y, Quarter == q) %>%
    group_by(Origin, Origin.City, OriginState) %>% summarize(N = sum(Passengers)) %>%
    arrange(desc(N)); gc();
  
  write.csv(condensed_data[1:100,], output)
}

create_msa_pop_table <- function(output = "02.Intermediate/MSA_Population.csv"){
  file_head <- "01.Input/10.MSA_Population_Data"
  file_list <- list.files(path = file_head)
  
  tens <- fread(paste(file_head, file_list[1], sep = "/"))
  twenties <- fread(paste(file_head, file_list[2], sep = "/"))
  
  tens$POPESTIMATE042020 <- NULL
  twenties$NPOPCHG2020 <- NULL
  twenties$NPOPCHG2021 <- NULL
  twenties$NPOPCHG2022 <- NULL
  twenties$ESTIMATESBASE2020 <- NULL
  tens$POPESTIMATE2020 <- NULL
  
  # Cleveland's Code Changes in the 2020s
  twenties[CBSA == 17410, CBSA := 17460]
  
  # Some MSA Names Changed Between Periods
  # Adjust All Names to that of 2020
  tens$NAME <- NULL
  tens$LSAD <- NULL
  twenties$LSAD <- NULL
  # Merge files
  starting_point <- merge(tens, twenties, all.x = TRUE, all.y = TRUE, by = c("CBSA", "MDIV", "STCOU"))
  # Drop all Sub Identifiers
  starting_point <- as.data.table(starting_point)
  starting_point <- starting_point[is.na(STCOU)]
  starting_point <- starting_point[is.na(MDIV)]
  starting_point$MDIV <- NULL
  starting_point$STCOU <- NULL
  starting_point[, MSA_Code := CBSA / 10] # BLS Codes Reported Differently than Census in Format
  starting_point[, CBSA := NULL]

  # Load in the CrossWalk
  crosswalk <- fread("01.Input/12.Airport_MSA_Crosswalk/Large_Airports.csv", header = TRUE)
  crosswalk$V1 <- NULL
  # BLS Names had ' MSA' at the end
  crosswalk[, MSA_Name := trimws(substr(x = MSA_Name,start = 1, stop = nchar(MSA_Name)  - 4))]
  crosswalk[, MSA_Code := as.numeric(substr(x = MSA_Code, start = 2, stop = 5))]
  
  crosswalk <- merge(crosswalk, starting_point, by = "MSA_Code", all.x = TRUE)
  
  
  # Population Data for the San Juan MSA is kept in two different files by the census with a 
  # different format. I will manually input it here.
  crosswalk[MSA_Code == 4198, CENSUS2010POP := 2350126]
  crosswalk[MSA_Code == 4198, POPESTIMATE2011 := 2322216]
  crosswalk[MSA_Code == 4198, POPESTIMATE2012 := 2295419]
  crosswalk[MSA_Code == 4198, POPESTIMATE2013 := 2269908]
  crosswalk[MSA_Code == 4198, POPESTIMATE2014 := 2233776]
  crosswalk[MSA_Code == 4198, POPESTIMATE2015 := 2195941]
  crosswalk[MSA_Code == 4198, POPESTIMATE2016 := 2155066]
  crosswalk[MSA_Code == 4198, POPESTIMATE2017 := 2104671]
  crosswalk[MSA_Code == 4198, POPESTIMATE2018 := 2022139]
  crosswalk[MSA_Code == 4198, POPESTIMATE2019 := 2023227]
  crosswalk[MSA_Code == 4198, POPESTIMATE2020 := 2078951]
  crosswalk[MSA_Code == 4198, POPESTIMATE2021 := 2069054]
  crosswalk[MSA_Code == 4198, POPESTIMATE2022 := 2042800]
  crosswalk[MSA_Code == 4198, POPESTIMATE2023 := 2035733]
  crosswalk$ESTIMATESBASE2010 <- NULL;
  crosswalk$CENSUS2010POP <- NULL
  
  crosswalk$NAME <- NULL; 
  crosswalk <- crosswalk[, 1:19]
  colnames(crosswalk) <- c("MSA_Code", "Airport", "Airport_City", "Airport_State",
                           "MSA_Name", "Pop2010", "Pop2011", "Pop2012", "Pop2013",
                           "Pop2014", "Pop2015", "Pop2016", "Pop2017",
                           "Pop2018", "Pop2019", "Pop2020",
                           "Pop2021", "Pop2022", "Pop2023")
  crosswalk <- melt(crosswalk, id.vars = c("MSA_Code", "Airport", "Airport_City",
                                           "Airport_State", "MSA_Name"),
                    variable.name = "Year", value.name = "MSA_Population") %>% 
    as.data.table()
  crosswalk[, Year := substr(Year, start = 4, stop = 7)]
  write.csv(crosswalk, output)
}

db1b_operating_flights <- function(target = "02.Intermediate/DB1B_Operating.rds"){
  clarify_DB1B(input = "02.Intermediate/Construct_DB1B/DB1B_Initial.Rds",
               output = "02.Intermediate/Construct_DB1B/DB1B_Clarified_Op.Rds",
               mode = "operating"); gc();
  condense_db1b(input = "02.Intermediate/Construct_DB1B/DB1B_Clarified_Op.Rds",
                output = "02.Intermediate/Construct_DB1B/DB1B_Condensed_Op.rds")
  add_control_variables(input = "02.Intermediate/Construct_DB1B/DB1B_Condensed_Op.rds",
                        output = target); gc();
}
