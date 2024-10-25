# For Each Airport: Calculate Out Average Per-Mile Price on Direct Routes
nea_market_mile_price <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                                  output = "02.Intermediate/Airport_Mile_Price.rds"){
  db1b <- data.table(readRDS(input))
  db1b <- db1b[NonStop == TRUE,]
  db1b <- db1b[!is.na(Avg.Fare),]
  
  db1b[, Rev_Per_Mile := Avg.Fare / MktMilesFlown]
  db1b[, Rev_Passengers := Rev_Per_Mile * Passengers.Product]
  db1b[, City_Airline_Passengers := sum(Passengers.Product), 
       by = c("Year", "Quarter", "Origin", "Carrier")]
  db1b[, City_Passengers := sum(Passengers.Product),
       by = c("Year", "Quarter", "Origin")]
  db1b[, Avg_Rev_Per_Mile := sum(Rev_Passengers) / City_Airline_Passengers,
       by = c("Year", "Quarter", "Origin", "Carrier")]
  db1b[, City_Avg_Rev := sum(Rev_Passengers) / City_Passengers,
       by = c("Year", "Quarter", "Origin")]
  
  
  db1b <- db1b %>% select(Year, Quarter, Origin, Origin.City,
                          Origin_MSA, Carrier, Avg_Rev_Per_Mile,
                          City_Avg_Rev) %>%
    unique() %>% as.data.table()
  
  saveRDS(db1b, output)
}


nea_did_construct_data <- function(input =  "02.Intermediate/DB1B_With_Controls.Rds",
                                   Airline_output = "02.Intermediate/NEA_Airline_Yield.Rds",
                                   Market_output = "02.Intermediate/NEA_Market_Yield.Rds"){
  market_group <- c("Year", "Quarter", "Origin", "Origin_MSA", "Dest", "Destination_MSA")
  
  db1b <- as.data.table(readRDS(input))
  db1b <- db1b[!is.na(Origin_MSA),]
  db1b <- db1b[!is.na(Destination_MSA),]
  db1b <- db1b[!is.na(Origin.Population),]
  db1b <- db1b[!is.na(Destination.Population),]
  
  # Adjust Fares to be in Real Terms
  db1b[, Avg.Fare := Avg.Fare / (price_index / 100)]
  
  # Adjust Income to Be in Real Terms
  db1b[, OriginState_Income := OriginState_Income / (price_index / 100)]
  db1b[, DestinationState_Income := DestinationState_Income / (price_index / 100)]
  db1b[, Origin_Income_PerCap := Origin_Income_PerCap / (price_index / 100)]
  db1b[, Dest_Income_PerCap := Dest_Income_PerCap / (price_index / 100)]
  
  # Create Effective Q Variable
  db1b <- nea_create_period(db1b)
  
  db1b[, Product_Yield := Avg.Fare / MktMilesFlown]
  db1b[, Airline_Yield := sum(Product_Yield * Passengers.Product)/sum(Passengers.Product), 
       by = c(market_group, "Carrier")]
  db1b[, Airline_Average_Fare := sum(Avg.Fare * Passengers.Product)/sum(Passengers.Product),
       by = c(market_group, "Carrier")]
  db1b[, Airline_Average_Miles := sum(MktMilesFlown * Passengers.Product) / sum(Passengers.Product),
       by = c(market_group, "Carrier")]
  db1b[, Airline_Passengers := sum(Passengers.Product),
       by = c(market_group, "Carrier")]
  db1b[, Airline_Share_NonStop := sum(NonStop * Passengers.Product)/sum(Passengers.Product),
       by = c(market_group, "Carrier")]
  db1b[, Southwest_Prescence := max(Southwest_Prescence, na.rm = TRUE),
       by = market_group]
  db1b[, Spirit_Prescence := max(Spirit_Prescence, na.rm = TRUE),
       by = market_group]
  db1b[, JetBlue_Prescence := max(JetBlue_Prescence, na.rm = TRUE),
       by = market_group]
  db1b[, American_Prescence := max(American_Prescence, na.rm = TRUE),
       by = market_group]
  
  
  # Add Lag NonStop Variable
  db1b[, Period.Numeric := as.numeric(as.character(Period))]
  db1b <- setorder(db1b, Period.Numeric)
  db1b[, Airline_NonStop_Lag := shift(Airline_Share_NonStop,
                                      type = "lag"), by = c("Carrier",
                                                                   "Origin",
                                                                   "Dest")]
  
  db1b.Airline <- db1b %>% select(Year, Quarter, Period,
                                  Origin, Origin_MSA, Dest, Destination_MSA,
                                  Airline_Average_Fare, Airline_Average_Miles, 
                                  Carrier, Airline_Yield, Airline_Passengers,
                                  Destination.Airport.Type, Origin.Airport.Type,
                                  Origin_Firm_Destinations, Origin_Firm_Service_Ratio,
                                  Destination_Firm_Destinations, Destination_Firm_Service_Ratio,
                                  Jet_Fuel_Price, Origin_Income_PerCap,
                                  Dest_Income_PerCap, Origin.Population, Destination.Population,
                                  Southwest_Prescence, Spirit_Prescence, Airline_NonStop_Lag,
                                  Origin_Covid_Cases, Origin_Covid_Cases, Destination_Covid_Cases,
                                  Destination_Covid_Deaths,Origin_PC_Covid_Cases, Origin_PC_Covid_Deaths, 
                                  Destination_PC_Covid_Cases, Destination_PC_Covid_Deaths, Market_HHI, JetBlue_Prescence,
                                  American_Prescence, OriginState_Income, DestinationState_Income,
                                  OriginState_Pop, DestinationState_Pop) %>%
    unique() %>% 
    mutate(NEA_Origin = Origin %in% c("BOS", "EWR", "LGA", "JFK"),
           NEA_Destination = Dest %in% c("BOS", "EWR", "LGA", "JFK"),
           Income_GeomMean = sqrt(Origin_Income_PerCap) * sqrt(Dest_Income_PerCap),
            Pop_GeomMean = sqrt(Origin.Population) * sqrt(Destination.Population),
           StatePop_GeomMean = sqrt(OriginState_Pop) * sqrt(DestinationState_Pop),
           stateIncome_GeomMean = sqrt(OriginState_Income) * sqrt(DestinationState_Income), 
           Mkt = paste("Origin", "Dest"),
           Post_NEA = as.numeric(as.numeric(as.character(Period)) >= 0)) %>%
    as.data.table()
  
  db1b.Airline[, NEA_Market := as.numeric(NEA_Origin | NEA_Destination)]
  db1b.Airline[, BOS_Market := as.numeric(Origin == "BOS" | Dest == "BOS")]
  db1b.Airline[, EWR_Market := as.numeric(Origin == "EWR" | Dest == "EWR")]
  db1b.Airline[, LGA_Market := as.numeric(Origin == "LGA" | Dest == "LGA")]
  db1b.Airline[, JFK_Market := as.numeric(Origin == "JFK" | Dest == "JFK")]
  
  saveRDS(db1b.Airline, Airline_output)
  
  
  db1b[, Market_Yield := sum(Product_Yield * Passengers.Product) / sum(Passengers.Product),
       by = market_group]
  db1b[, Market_Avg_Fare := sum(Avg.Fare * Passengers.Product) / sum(Passengers.Product),
       by = market_group]
  db1b[, Market_Passengers := sum(Passengers.Product),
       by = market_group]
  db1b[, Market_Share_NonStop := sum(NonStop * Passengers.Product) / sum(Passengers.Product),
       by = market_group]
  
  db1b[, Period.Numeric := as.numeric(as.character(Period))]
  db1b <- setorder(db1b, Period.Numeric)
  db1b[, Market_Share_NonStop.Lag := shift(Market_Share_NonStop,
                                           type = "lag"),
       by = c("Origin", "Dest")]
  
  db1b <- db1b %>% select(Year, Quarter, Period,
                          Origin, Origin_MSA, Dest, Destination_MSA,Market_Avg_Fare, 
                          Destination.Airport.Type, Origin.Airport.Type, Origin_Income_PerCap,
                          Dest_Income_PerCap, Origin.Population, Destination.Population,
                          Southwest_Prescence, Spirit_Prescence, Market_Yield,
                          Market_Passengers, Market_Share_NonStop, Market_Share_NonStop.Lag, 
                          Origin_Covid_Cases, Origin_Covid_Cases, Destination_Covid_Cases,
                          Destination_Covid_Deaths,Origin_PC_Covid_Cases, Origin_PC_Covid_Deaths, 
                          Destination_PC_Covid_Cases, Destination_PC_Covid_Deaths, Market_HHI.Lag,
                          OriginState_Income, DestinationState_Income, OriginState_Pop, DestinationState_Pop) %>%
    unique() %>% ungroup() %>%
    mutate(NEA_Origin = Origin %in% c("BOS", "EWR", "LGA", "JFK"),
           NEA_Destination = Dest %in% c("BOS", "EWR", "LGA", "JFK"),
           Income_GeomMean = sqrt(Origin_Income_PerCap) * sqrt(Dest_Income_PerCap),
           Pop_GeomMean = sqrt(Origin.Population) * sqrt(Destination.Population),
           StatePop_GeomMean = sqrt(OriginState_Pop) * sqrt(DestinationState_Pop),
           stateIncome_GeomMean = sqrt(OriginState_Income) * sqrt(DestinationState_Income), 
           Market_Yield.log = log(Market_Yield),
           Market_Passengers.log = log(Market_Passengers),
           Origin_Dest.Pair = paste(Origin, Dest),
           Mkt = paste("Origin", "Dest"),
           Post_NEA = as.numeric(as.numeric(as.character(Period)) >= 0)) %>% 
    filter(!is.nan(Market_Yield.log),
           !is.na(Market_Yield.log),
           Market_Yield > 0) %>%
    as.data.table() 
  
  db1b[, NEA_Market := as.numeric(NEA_Origin | NEA_Destination)]
  db1b[, BOS_Market := as.numeric(Origin == "BOS" | Dest == "BOS")]
  db1b[,  EWR_Market := as.numeric(Origin == "EWR" | Dest == "EWR")]
  db1b[,  LGA_Market := as.numeric(Origin == "LGA" | Dest == "LGA")]
  db1b[, JFK_Market := as.numeric(Origin == "JFK" | Dest == "JFK")]
  
  saveRDS(db1b, Market_output)
}

nea_op_carrier_switch <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Initial.Rds",
                      output = "02.Intermediate/NEA_OPCarrier_Switch.Rds"){
  data <- readRDS(input)
  data <- data[MktCoupons <= 3,]; gc();
  
  data <- data[, .(Year, Quarter, Origin, OriginState, Dest, DestState, TkCarrier, TkCarrierGroup, OpCarrier,
                   OpCarrierGroup, Passengers, MktCoupons, AirportGroup, MktMilesFlown,
                   MktFare)]; gc()
  
  # Now, Add Price_Index Data
  price_index_data <- readRDS("02.Intermediate/price_index.rds")
  data <- merge(x = data, y = price_index_data,
                      all.x = TRUE, all.y = FALSE,
                      by = c("Year", "Quarter"))
  
  # Adjust Fares to be in Real Terms
  data[, MktFare := MktFare / (price_index / 100)]
  
  
  data <- data %>% group_by(Year, Quarter, Origin, OriginState, Dest,  DestState, TkCarrier, TkCarrierGroup, OpCarrier,
                            OpCarrierGroup, MktCoupons, AirportGroup, MktMilesFlown) %>%
    summarize(MktFare = sum(MktFare * Passengers) / sum(Passengers),
              Passengers = sum(Passengers)) %>%
    mutate(Carrier = TkCarrier) %>% as.data.table(); gc();

  # Identify Which Carriers are in OpGroup
  data[, American_Op := grepl(pattern = "AA", x = OpCarrierGroup)]
  data[, Delta_Op := grepl(pattern = "DL", x = OpCarrierGroup)]
  data[, United_Op := grepl(pattern = "UA", x = OpCarrierGroup)]
  data[, Spirit_Op := grepl(pattern = "NK", x = OpCarrierGroup)]
  data[, JetBlue_Op := grepl(pattern = "B6", x = OpCarrierGroup)]
  data[, Southwest_Op := grepl(pattern = "WN", x = OpCarrierGroup)]
  
  data[, AA_B6 := pmin(American_Op, JetBlue_Op)]
  
  data[, NEA_Market := FALSE]
  data[grepl(pattern = "JFK", x = AirportGroup), NEA_Market := TRUE]
  data[grepl(pattern = "EWR", x = AirportGroup), NEA_Market := TRUE]
  data[grepl(pattern = "LGA", x = AirportGroup), NEA_Market := TRUE]
  data[grepl(pattern = "BOS", x = AirportGroup), NEA_Market := TRUE]
  
  # Add Population Data
  # Now, add in the demographic control data
  data[, MatchYear := Year]
  data[Quarter < 3, MatchYear := MatchYear - 1]
  
  demographics <- fread("02.Intermediate/MSA_Population.csv")
  demographics <- demographics %>% select(Airport, MSA_Name, Year, MSA_Population) %>%
    mutate(MSA_Population = as.numeric(MSA_Population))
  
  origins <- demographics; colnames(origins) <- c("Origin", "Origin_MSA", "MatchYear", "Origin.Population")
  destinations <- demographics; colnames(destinations) <- c("Dest", "Destination_MSA", "MatchYear", 
                                                            "Destination.Population")
  
  # First, Merge Current Year, then the next
  data <- merge(data, origins, by.x = c("Origin", "MatchYear"),
                      by.y = c("Origin", "MatchYear"), all.x = TRUE); gc(); gc();
  origins[, MatchYear := MatchYear - 1]
  origins[, Origin.Next.Pop := Origin.Population]
  origins[, Origin.Population := NULL]
  data <- merge(data, origins, by.x = c("Origin", "Origin_MSA", "MatchYear"),
                      by.y = c("Origin","Origin_MSA", "MatchYear"), all.x = TRUE)
  
  data[Quarter == 3, Origin.Population := 11/12 * Origin.Population + 1/12 * Origin.Next.Pop]
  data[Quarter == 4, Origin.Population := 8/12 * Origin.Population + 4/12 * Origin.Next.Pop]
  data[Quarter == 1, Origin.Population := 5/12 * Origin.Population + 7/12 * Origin.Next.Pop]
  data[Quarter == 2, Origin.Population := 2/12 * Origin.Population + 10/12 * Origin.Next.Pop]
  data[, Origin.Next.Pop := NULL]
  
  
  data <- merge(data, destinations, by.x = c("Dest", "MatchYear"),
                      by.y = c("Dest","MatchYear"), all.x = TRUE); gc(); gc();
  destinations[, MatchYear := MatchYear + 1]
  destinations[, Destination.Next.Pop := Destination.Population]
  destinations[, Destination.Population := NULL]
  data <- merge(data, destinations, by.x = c("Dest", "Destination_MSA", "MatchYear"),
                      by.y = c("Dest", "Destination_MSA", "MatchYear"), all.x = TRUE)
  data[Quarter == 3, Destination.Population := 11/12 * Destination.Population + 1/12 * Destination.Next.Pop]
  data[Quarter == 4, Destination.Population := 8/12 * Destination.Population + 4/12 * Destination.Next.Pop]
  data[Quarter == 1, Destination.Population := 5/12 * Destination.Population + 7/12 * Destination.Next.Pop]
  data[Quarter == 2, Destination.Population := 2/12 * Destination.Population + 10/12 * Destination.Next.Pop]
  data[, Destination.Next.Pop := NULL]
  data[, MatchYear := NULL]
  
  # Remove NA Pop Data
  data <- data[!is.na(Destination.Population) & !is.na(Origin.Population),]
  
  # Remove Near Markets
  data <- data[, MinDist := min(MktMilesFlown), by = c("Year", "Quarter", "Origin", "Dest")]
  data <- data[MinDist >= 150,]
  
  # Include Per Capita Covid Cases
  covid <- readRDS("02.Intermediate/Covid_State.rds")
  state_pop <- readRDS("02.Intermediate/State_Populations.rds")
  covid <- merge(covid, state_pop, by = c("Year", "State"),
                 all.x = TRUE)
  covid <- covid[!is.na(Population)]
  covid[, Per_Capita_Covid_Cases := Covid_Cases / Population * 1000]
  covid[, Per_Capita_Covid_Deaths := Covid_Deaths / Population * 1000]
  covid[, Population := NULL]
  
  colnames(covid) <- c("Year", "OriginState", "Quarter", "Origin_Covid_Cases",
                       "Origin_Covid_Deaths", "Origin_PC_Covid_Cases", 
                       "Origin_PC_Covid_Deaths")
  
  data <- merge(data, covid, by = c("Year", "Quarter", "OriginState"),
                      all.x = TRUE)
  
  colnames(covid) <- c("Year", "DestState", "Quarter", "Destination_Covid_Cases",
                       "Destination_Covid_Deaths", "Destination_PC_Covid_Cases", 
                       "Destination_PC_Covid_Deaths")
  
  data <- merge(data, covid, by = c("Year", "Quarter", "DestState"),
                      all.x = TRUE)
  
  data[is.na(Origin_Covid_Cases), Origin_Covid_Cases := 0]
  data[is.na(Origin_Covid_Deaths), Origin_Covid_Deaths := 0]
  data[is.na(Origin_PC_Covid_Cases), Origin_PC_Covid_Cases := 0]
  data[is.na(Origin_PC_Covid_Deaths), Origin_PC_Covid_Deaths := 0]
  data[is.na(Destination_Covid_Cases), Destination_Covid_Cases := 0]
  data[is.na(Destination_Covid_Deaths), Destination_Covid_Deaths := 0]
  data[is.na(Destination_PC_Covid_Cases), Destination_PC_Covid_Cases := 0]
  data[is.na(Destination_PC_Covid_Deaths), Destination_PC_Covid_Deaths := 0]
  
  
  # Remove Small Markets
  # data.s <- data.s[Market_Passengers > 100,]
  
  saveRDS(data, output)
}

# Create Period Variable
nea_create_period <- function(input_data){
  input_data <- as.data.table(input_data)
  input_data <- input_data[Year >= 2018,]
  input_data <- input_data[Year != 2020,]
  input_data <- input_data[!(Year == 2021 & Quarter %in% c(1)),]
  input_data <- input_data[!(Year == 2023 & Quarter > 2),]
  
  input_data[, Period := -1000]
  input_data[Year == 2018, Period := -9 + Quarter]
  input_data[Year == 2019, Period := -5 + Quarter]
  input_data[Year == 2021, Period := Quarter - 2]
  input_data[Year == 2022, Period := 2 + Quarter]
  input_data[Year == 2023, Period := 6 + Quarter]
  input_data[, Period := factor(Period)]
  input_data[, Period := relevel(Period, ref = "-1")]
  
  return(input_data)
}


# Summary Statistics For Routes of 5 Carriers - American, JetBlue, Delta, United, Spirit
nea_2019_summary <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                             output_table = "06.Tables/NEA_Statistics.tex"){
  mean.make <- function(value){
    mean <- mean(value, na.rm = TRUE)
    return(sprintf(mean, fmt = "%#.2f"))
  }
  
  sd.make <- function(value){
    sd <- sd(value)
    return(paste("(", sprintf(sd, fmt = "%#.2f"), ")", sep = ""))
  }
  
  quarter_prescence_make <- function(data, mode = 0){
    data <- as.data.table(data)
    data <- unique(data[, .(Quarter, Route_Name)])
    data[, Quarter_Prescence := .N, by = c("Route_Name")]
    data <- unique(data[, .(Route_Name, Quarter_Prescence)])
    if(mode == 0){
      return(data$Quarter_Prescence)
    } else if(mode == 1){
      return(data)
    }
  }
  
  nea_places <- c("JFK", "LGA", "EWR", "BOS")
  
  db1b <- readRDS(input)
  
  # Restrict to solely direct routes
  db1b <- db1b[NonStop == TRUE,]
  db1b[, Route_Name := paste(Carrier, Origin, Dest)]
  
  # Restrict to following years: 2019, 2021, 2022
  db1b.2019 <- db1b[Year %in% c(2019),]
  db1b.2021 <- db1b[Year %in% c(2021),]
  db1b.2022 <- db1b[Year %in% c(2022),]
  db1b.2019[, Present.2021 := Route_Name %in% db1b.2021$Route_Name]
  db1b.2019[, Present.2022 := Route_Name %in% db1b.2022$Route_Name]
  
  db1b.2021 <- quarter_prescence_make(db1b.2021, mode = 1)
  db1b.2022 <- quarter_prescence_make(db1b.2022, mode = 1)
  
  
  db1b.2019 <- merge(db1b.2019, db1b.2021, all.x = TRUE, by = "Route_Name")
  db1b.2019$Quarter_Prescence21 <- db1b.2019$Quarter_Prescence
  db1b.2019[is.na(Quarter_Prescence21), Quarter_Prescence21 := 0]
  db1b.2019$Quarter_Prescence <- NULL
  db1b.2019 <- merge(db1b.2019, db1b.2022, all.x = TRUE, by = "Route_Name")
  db1b.2019$Quarter_Prescence22 <- db1b.2019$Quarter_Prescence
  db1b.2019[is.na(Quarter_Prescence22), Quarter_Prescence22 := 0]
  db1b.2019$Quarter_Prescence <- NULL
  
  db1b.2019.United <- db1b.2019[Carrier == "United Air Lines Inc.",]
  db1b.2019.delta <- db1b.2019[Carrier == "Delta Air Lines Inc.",]
  db1b.2019.spirit <- db1b.2019[Carrier == "Spirit Air Lines",]
  db1b.2019.american <- db1b.2019[Carrier == "American Airlines Inc.",]
  db1b.2019.amer.nea <- db1b.2019.american[Origin %in% nea_places | Dest %in% nea_places,]
  db1b.2019.jetblue <- db1b.2019[Carrier == "JetBlue Airways",]
  db1b.2019.jb.nea <- db1b.2019.jetblue[Origin %in% nea_places | Dest %in% nea_places,]
  

  # Row 0
  column_names <- c("Variable", "United", "Delta", "Spirit", "American", "American", 
              "JetBlue", "JetBlue")
  header.1 <- c("", "", "", "", "", "NEA Routes", "", "NEA Routes")
  
  # Row 1: Passengers
  passengers.m <- c("Passengers", mean.make(db1b.2019.United$Passengers.Product),
                    mean.make(db1b.2019.delta$Passengers.Product),
                    mean.make(db1b.2019.spirit$Passengers.Product),
                    mean.make(db1b.2019.american$Passengers.Product),
                    mean.make(db1b.2019.amer.nea$Passengers.Product),
                    mean.make(db1b.2019.jetblue$Passengers.Product),
                    mean.make(db1b.2019.jb.nea$Passengers.Product))
  
  passengers.s <- c("", sd.make(db1b.2019.United$Passengers.Product),
                       sd.make(db1b.2019.delta$Passengers.Product),
                       sd.make(db1b.2019.spirit$Passengers.Product),
                       sd.make(db1b.2019.american$Passengers.Product),
                       sd.make(db1b.2019.amer.nea$Passengers.Product),
                       sd.make(db1b.2019.jetblue$Passengers.Product),
                       sd.make(db1b.2019.jb.nea$Passengers.Product))
  
  # Row 2: Miles
  miles.m <- c("Distance (100 Miles)", mean.make(db1b.2019.United$MktMilesFlown),
               mean.make(db1b.2019.delta$MktMilesFlown),
               mean.make(db1b.2019.spirit$MktMilesFlown),
               mean.make(db1b.2019.american$MktMilesFlown),
               mean.make(db1b.2019.amer.nea$MktMilesFlown),
               mean.make(db1b.2019.jetblue$MktMilesFlown),
               mean.make(db1b.2019.jb.nea$MktMilesFlown))
  miles.s <- c("", sd.make(db1b.2019.United$MktMilesFlown),
               sd.make(db1b.2019.delta$MktMilesFlown),
               sd.make(db1b.2019.spirit$MktMilesFlown),
               sd.make(db1b.2019.american$MktMilesFlown),
               sd.make(db1b.2019.amer.nea$MktMilesFlown),
               sd.make(db1b.2019.jetblue$MktMilesFlown),
               sd.make(db1b.2019.jb.nea$MktMilesFlown))
    
  # Row 3: Price
  price.m <- c("Price (2017 USD)", mean.make(db1b.2019.United$Avg.Fare),
               mean.make(db1b.2019.delta$Avg.Fare),
               mean.make(db1b.2019.spirit$Avg.Fare),
               mean.make(db1b.2019.american$Avg.Fare),
               mean.make(db1b.2019.amer.nea$Avg.Fare),
               mean.make(db1b.2019.jetblue$Avg.Fare),
               mean.make(db1b.2019.jb.nea$Avg.Fare))
  
  price.s <- c("", sd.make(db1b.2019.United$Avg.Fare),
                sd.make(db1b.2019.delta$Avg.Fare),
                sd.make(db1b.2019.spirit$Avg.Fare),
                sd.make(db1b.2019.american$Avg.Fare),
                sd.make(db1b.2019.amer.nea$Avg.Fare),
                sd.make(db1b.2019.jetblue$Avg.Fare),
                sd.make(db1b.2019.jb.nea$Avg.Fare))
  
  # Row 4: Quarters Offered
  quarter.m <- c("Quarters Offered", mean.make(quarter_prescence_make(db1b.2019.United)),
               mean.make(quarter_prescence_make(db1b.2019.delta)),
               mean.make(quarter_prescence_make(db1b.2019.spirit)),
               mean.make(quarter_prescence_make(db1b.2019.american)),
               mean.make(quarter_prescence_make(db1b.2019.amer.nea)),
               mean.make(quarter_prescence_make(db1b.2019.jetblue)),
               mean.make(quarter_prescence_make(db1b.2019.jb.nea)))
  
  quarter.s <- c("",  sd.make(quarter_prescence_make(db1b.2019.United)),
                  sd.make(quarter_prescence_make(db1b.2019.delta)),
                  sd.make(quarter_prescence_make(db1b.2019.spirit)),
                  sd.make(quarter_prescence_make(db1b.2019.american)),
                  sd.make(quarter_prescence_make(db1b.2019.amer.nea)),
                  sd.make(quarter_prescence_make(db1b.2019.jetblue)),
                  sd.make(quarter_prescence_make(db1b.2019.jb.nea)))
  
  # Row 5: P(Offered 2021)
  offered.21.m <- c("P(Offered 2021)", mean.make(db1b.2019.United$Present.2021),
                    mean.make(db1b.2019.delta$Present.2021),
                    mean.make(db1b.2019.spirit$Present.2021),
                    mean.make(db1b.2019.american$Present.2021),
                    mean.make(db1b.2019.amer.nea$Present.2021),
                    mean.make(db1b.2019.jetblue$Present.2021),
                    mean.make(db1b.2019.jb.nea$Present.2021))
  offered.21.s <- c("", sd.make(as.numeric(db1b.2019.United$Present.2021)),
                    sd.make(as.numeric(db1b.2019.delta$Present.2021)),
                    sd.make(as.numeric(db1b.2019.spirit$Present.2021)),
                    sd.make(as.numeric(db1b.2019.american$Present.2021)),
                    sd.make(as.numeric(db1b.2019.amer.nea$Present.2021)),
                    sd.make(as.numeric(db1b.2019.jetblue$Present.2021)),
                    sd.make(as.numeric(db1b.2019.jb.nea$Present.2021)))
  
  # Row 6: Number of Quarters in 2021
  qcount.21.m <- c("Quarters Offered 2021", mean.make(db1b.2019.United$Quarter_Prescence21),
                    mean.make(db1b.2019.delta$Quarter_Prescence21),
                    mean.make(db1b.2019.spirit$Quarter_Prescence21),
                    mean.make(db1b.2019.american$Quarter_Prescence21),
                    mean.make(db1b.2019.amer.nea$Quarter_Prescence21),
                    mean.make(db1b.2019.jetblue$Quarter_Prescence21),
                    mean.make(db1b.2019.jb.nea$Quarter_Prescence21))
  qcount.21.s <- c("", sd.make(db1b.2019.United$Quarter_Prescence21),
                    sd.make(db1b.2019.delta$Quarter_Prescence21),
                    sd.make(db1b.2019.spirit$Quarter_Prescence21),
                    sd.make(db1b.2019.american$Quarter_Prescence21),
                    sd.make(db1b.2019.amer.nea$Quarter_Prescence21),
                    sd.make(db1b.2019.jetblue$Quarter_Prescence21),
                    sd.make(db1b.2019.jb.nea$Quarter_Prescence21))
  
  
  # Row 7: P(Offered 2022)
  offered.22.m <- c("P(Offered 2022)", mean.make(db1b.2019.United$Present.2022),
                    mean.make(db1b.2019.delta$Present.2022),
                    mean.make(db1b.2019.spirit$Present.2022),
                    mean.make(db1b.2019.american$Present.2022),
                    mean.make(db1b.2019.amer.nea$Present.2022),
                    mean.make(db1b.2019.jetblue$Present.2021),
                    mean.make(db1b.2019.jb.nea$Present.2022))
  offered.22.s <- c("", sd.make(db1b.2019.United$Present.2022),
                    sd.make(db1b.2019.delta$Present.2022),
                    sd.make(db1b.2019.spirit$Present.2022),
                    sd.make(db1b.2019.american$Present.2022),
                    sd.make(db1b.2019.amer.nea$Present.2022),
                    sd.make(db1b.2019.jetblue$Present.2022),
                    sd.make(db1b.2019.jb.nea$Present.2022))
  
  # Row 6: Number of Quarters in 2022
  qcount.22.m <- c("Quarters Offered 2022", mean.make(db1b.2019.United$Quarter_Prescence22),
                   mean.make(db1b.2019.delta$Quarter_Prescence22),
                   mean.make(db1b.2019.spirit$Quarter_Prescence22),
                   mean.make(db1b.2019.american$Quarter_Prescence22),
                   mean.make(db1b.2019.amer.nea$Quarter_Prescence22),
                   mean.make(db1b.2019.jetblue$Quarter_Prescence22),
                   mean.make(db1b.2019.jb.nea$Quarter_Prescence22))
  qcount.22.s <- c("", sd.make(db1b.2019.United$Quarter_Prescence22),
                   sd.make(db1b.2019.delta$Quarter_Prescence22),
                   sd.make(db1b.2019.spirit$Quarter_Prescence22),
                   sd.make(db1b.2019.american$Quarter_Prescence22),
                   sd.make(db1b.2019.amer.nea$Quarter_Prescence22),
                   sd.make(db1b.2019.jetblue$Quarter_Prescence22),
                   sd.make(db1b.2019.jb.nea$Quarter_Prescence22))
  
  # Summary Statistics - Number of Products
  product_count <- c("N Products", nrow(db1b.2019.United),
                     nrow(db1b.2019.delta),
                     nrow(db1b.2019.spirit),
                     nrow(db1b.2019.american),
                     nrow(db1b.2019.amer.nea),
                     nrow(db1b.2019.jetblue),
                     nrow(db1b.2019.jb.nea))
  route_count <- c("N Routes", length(unique(db1b.2019.United$Route_Name)),
                   length(unique(db1b.2019.delta$Route_Name)),
                   length(unique(db1b.2019.spirit$Route_Name)),
                   length(unique(db1b.2019.american$Route_Name)),
                   length(unique(db1b.2019.amer.nea$Route_Name)),
                   length(unique(db1b.2019.jetblue$Route_Name)),
                   length(unique(db1b.2019.jb.nea$Route_Name)))          
  
  summaryTable <- rbind(passengers.m, passengers.s,
                        miles.m, miles.s,
                        price.m, price.s,
                        quarter.m, quarter.s,
                        offered.21.m, offered.21.s,
                        qcount.21.m, qcount.22.s,
                        offered.22.m, offered.22.s,
                        qcount.22.m, qcount.22.s,
                        product_count,
                        route_count)
  rownames(summaryTable) <- NULL
  
  kbl(summaryTable,
      format = "latex", col.names = c("", "", "", "", "All", "NEA", "All", "NEA"),
      escape = FALSE, booktabs = TRUE) %>%
    row_spec(row = 16, hline_after = TRUE) %>%
    add_header_above(c("Variable" = 1, "United" = 1, "Delta" = 1, "Spirit" = 1,
                       "American" = 2, 
                       "JetBlue" = 2)) %>%
    pack_rows(group_label = "Summary Statistics", 17, 18) %>%
    save_kable(file = output_table)
  
}

share_nea_table <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                            output = "06.Tables/NEA_Percentage_Tied_Up.tex"){
  calculate_route_share <- function(carrier, data){
    output_row <- c("\\% NEA Routes")
    data <- data[Carrier == carrier, ]
    
    for(i in 1:length(unique(data$Year))){
      for(j in 1:4){
        if(unique(data$Year)[i] == 2023 & j == 4){
          break
        }
        current_data <- data[Year == unique(data$Year)[i] & Quarter == j,]       
        total_routes <- nrow(current_data)
        nea_routes <- nrow(current_data[NorthEastAlliance_Route == TRUE,])
        new_percent <- round(nea_routes / total_routes * 100)
        
        output_row <- c(output_row, new_percent)
      }
    }
    return(output_row)
  }
  
  calculate_passenger_share <- function(carrier, data){
    output_row <- c("\\% NEA Passengers")
    data <- data[Carrier == carrier,]
    
    for(i in 1:length(unique(data$Year))){
      for(j in 1:4){
        if(unique(data$Year)[i] == 2023 & j == 4){
          break
        }
        current_data <- data[Year == unique(data$Year)[i] & Quarter == j,]       
        nea_pass <- current_data[NorthEastAlliance_Route == TRUE,Passengers]
        non_nea_pass <- current_data[NorthEastAlliance_Route == FALSE, Passengers]
        new_percent <- round(nea_pass / (nea_pass + non_nea_pass) * 100)
        
        output_row <- c(output_row, new_percent)
      }
    }
    return(output_row)
  }
  
  calculate_passenger_revenue <- function(carrier, data){
    output_row <- c("\\% NEA Revenue")
    data <- data[Carrier == carrier,]
    
    for(i in 1:length(unique(data$Year))){
      for(j in 1:4){
        if(unique(data$Year)[i] == 2023 & j == 4){
          break
        }
        current_data <- data[Year == unique(data$Year)[i] & Quarter == j,]       
        nea_rev <- current_data[NorthEastAlliance_Route == TRUE, Revenue]
        non_nea_rev <- current_data[NorthEastAlliance_Route == FALSE, Revenue]
        new_percent <- round(nea_rev / (nea_rev + non_nea_rev) * 100)
        
        output_row <- c(output_row, new_percent)
      }
    }
    return(output_row)
  }
  
  
  db1b <- readRDS(input)
  
  # Interested in Data in Years 2017, 2018, 2019, 2021, 2022
  db1b <- db1b[Year %in% c(2017, 2018, 2019, 2021, 2022, 2023),]
  
  # Create Three Frames, one for routes, one for passengers, one for revenue
  db1b.routes <- unique(db1b[NonStop == TRUE, .(Year, Quarter, Carrier, Origin, Dest, NorthEastAlliance_Route)])
  
  db1b.passengers <- db1b %>% group_by(Year, Quarter, Carrier, NorthEastAlliance_Route) %>%
    summarize(Passengers = sum(Passengers.Product)) %>%
    data.table()
  
  db1b.revenue <- db1b %>% group_by(Year, Quarter, Carrier, NorthEastAlliance_Route) %>%
    summarize(Revenue = sum(Passengers.Product * Avg.Fare)) %>%
    data.table()
  
  # For each carrier of (JB, American, Delta, United), generate a row with % of each of the above frames in NEA.
  jetblue.routes <- calculate_route_share(carrier = "JetBlue Airways", data = db1b.routes)
  american.routes <- calculate_route_share(carrier = "American Airlines Inc.", data = db1b.routes)
  delta.routes <- calculate_route_share(carrier = "Delta Air Lines Inc.", data = db1b.routes)
  united.routes <- calculate_route_share(carrier = "United Air Lines Inc.", data = db1b.routes)
  spirit.routes <- calculate_route_share(carrier = "Spirit Air Lines", data = db1b.routes)
  
  jetblue.passengers <- calculate_passenger_share(carrier = "JetBlue Airways", data = db1b.passengers)
  american.passengers <- calculate_passenger_share(carrier = "American Airlines Inc.", data = db1b.passengers)
  delta.passengers <- calculate_passenger_share(carrier = "Delta Air Lines Inc.", data = db1b.passengers)
  united.passengers <- calculate_passenger_share(carrier = "United Air Lines Inc.", data = db1b.passengers)
  spirit.passengers <- calculate_passenger_share(carrier = "Spirit Air Lines", data = db1b.passengers)
  
  jetblue.revenue <- calculate_passenger_revenue(carrier = "JetBlue Airways", data = db1b.revenue)
  american.revenue <- calculate_passenger_revenue(carrier = "American Airlines Inc.", data = db1b.revenue)
  delta.revenue <- calculate_passenger_revenue(carrier = "Delta Air Lines Inc.", data = db1b.revenue)
  united.revenue <- calculate_passenger_revenue(carrier = "United Air Lines Inc.", data = db1b.revenue)
  spirit.revenue <- calculate_passenger_revenue(carrier = "Spirit Air Lines", data = db1b.revenue)
  
  
  col.head <- c("", rep(c(1,2,3,4),5), 1, 2, 3)
  
  out_tab <- rbind(jetblue.routes, jetblue.passengers, jetblue.revenue,
                   american.routes, american.passengers, american.revenue,
                   delta.routes, delta.passengers, delta.revenue,
                   united.routes, united.passengers, united.revenue,
                   spirit.routes, spirit.passengers, spirit.revenue)
  rownames(out_tab) <- NULL
  
  kbl(out_tab,
      format = "latex", col.names = col.head,
      escape = FALSE, booktabs = TRUE) %>%
    add_header_above(c(" " = 1 , "2017" = 4, "2018" = 4, "2019" = 4,
                       "2021" = 4, "2022" = 4, "2023" = 3)) %>%
    pack_rows(group_label = "JetBlue", 1, 3) %>%
    pack_rows(group_label = "American", 4, 6) %>%
    pack_rows(group_label = "Delta", 7, 9) %>%
    pack_rows(group_label = "United", 10, 12) %>%
    pack_rows(group_label = "Spirit", 13, 15) %>%
    save_kable(file = output)
}


market_yield_reg <- function(input = "02.Intermediate/NEA_Market_Yield.Rds",
                             output.table = "06.Tables/NEA_Market_Yield.tex",
                             output.graph = "05.Figures/NEA_Market_Yield_Graph.pdf",
                             output.graph_airport = "05.Figures/NEA_Airport_Yield_Graph.pdf"){
  nea_d <- readRDS(input)
  nea_d[, Mkt := paste(Origin, Dest)]
  nea_d[, Period := relevel(Period, ref = "-1")]
  
  reg1 <- lm(log(Market_Yield) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = nea_d)
  reg1.se <- sqrt(diag(vcovCL(reg1, cluster = nea_d$Mkt)))
  test1 <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  
  reg2 <- lm(log(Market_Yield) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(Income_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = nea_d)
  reg2.se <- sqrt(diag(vcovCL(reg2, cluster = nea_d$Mkt)))
  test2 <- coeftest(reg2, vcov = vcovCL, cluster = ~ Mkt)
  
  restrict_data <- nea_d[Period %in% c("-4", "-3", "-2", "0", "1", "2", "3", "-1"),]
  restrict_data[, Period := relevel(Period, ref = "-1")]
  
  reg3 <- lm(log(Market_Yield) ~ Period + NEA_Market + NEA_Market:Period + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(Income_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases +Destination_PC_Covid_Cases + 
               Market_HHI.Lag, 
             data = restrict_data)
  reg3.se <- sqrt(diag(vcovCL(reg3, cluster = restrict_data$Mkt)))
  test3 <- coeftest(reg3, vcov = vcovCL, cluster = ~Mkt)
  
  reg4 <- lm(log(Market_Yield) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(stateIncome_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = nea_d)
  reg4.se <- sqrt(diag(vcovCL(reg4, cluster = nea_d$Mkt)))
  test4 <- coeftest(reg4, vcov = vcovCL, cluster = ~ Mkt)
  
  
  reg5 <- lm(log(Market_Yield) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(stateIncome_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = restrict_data)
  reg5.se <- sqrt(diag(vcovCL(reg5, cluster = restrict_data$Mkt)))
  test5 <- coeftest(reg5, vcov = vcovCL, cluster = ~ Mkt)
  
  
  models <- list(reg1, reg2, reg3, reg4, reg5)
  se <- list(reg1.se, reg2.se, reg3.se, reg4.se, reg5.se)
  pvalues <- list(test1[,4], test2[,4], test3[,4], test4[,4], test5[,4])
  
  
  order <- list("Period-8:NEA_Market" = NA,
                "Period-7:NEA_Market" = NA,"Period-6:NEA_Market" = NA,
                "Period-5:NEA_Market" = NA,
                "Period-4:NEA_Market" = NA,"Period-3:NEA_Market" = NA,
                "Period-2:NEA_Market" = NA,"Period0:NEA_Market"  = NA, 
                "Period1:NEA_Market" = NA, "Period2:NEA_Market"  = NA,
                "Period3:NEA_Market"  = NA, "Period4:NEA_Market" = NA, "Period5:NEA_Market" = NA ,
                "Period6:NEA_Market" = NA, "Period7:NEA_Market" = NA, "Period8:NEA_Market" = NA)
  
  gof_rows <- list("Standard Controls" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
                   "Income Data" = c("", "MSA", "MSA", "State", "State"),
                   "Sample" = c("Full", "Full", "Two Years", "Full", "Two Years"))
  
  texreg(l = models, 
         file = output.table, single.row = FALSE, 
         stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = se, 
         override.pvalues = pvalues,
         custom.gof.rows = gof_rows,
         digits = 5,
         custom.coef.map = order)
  
  # Now, Graph
  # Make Graph of Column 4
  coeffs <- reg2$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = reg2.se)
  
  frame <- frame[grepl(pattern = ":NEA_Market", x = Value),]
  frame[, Value := gsub(pattern = ":NEA_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = "Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "Average Market Yield")
  ggsave(output.graph, units = "in", height = 3, width = 5)
  
  reg_airports <- lm(log(Market_Yield) ~ Period + BOS_Market + EWR_Market + LGA_Market + JFK_Market +
                       Period:BOS_Market + Period:EWR_Market + Period:LGA_Market + Period:JFK_Market + 
                       Market_Share_NonStop + log(Pop_GeomMean) + log(Income_GeomMean) + 
                       Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
                     data = nea_d)
  airports.se <- sqrt(diag(vcovCL(reg_airports, cluster = nea_d$Mkt)))

  coeffs <- reg_airports$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = airports.se)
  
  frame <- frame[grepl(pattern = "_Market", x = Value),]
  frame <- frame[grepl(pattern = ":", x = Value),]
  frame[, Airport := "NA"]
  frame[grepl(pattern = "BOS", x = Value), Airport := "BOS"]
  frame[grepl(pattern = "JFK", x = Value), Airport := "JFK"]
  frame[grepl(pattern = "LGA", x = Value), Airport := "LGA"]
  frame[grepl(pattern = "EWR", x = Value), Airport := "EWR"]
  frame[, Value := gsub(pattern = ":BOS_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":JFK_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":LGA_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":EWR_Market", replacement = "", x = Value)]
  
  frame[, Value := gsub(pattern = "Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "BOS"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "EWR"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "JFK"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "LGA"))
  
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    facet_grid(rows = vars(Airport)) + 
    labs(x = "Period", y = "Average Market Yield")
  ggsave(output.graph_airport, units = "in", height = 7, width = 5)
}

market_fare_reg <- function(input = "02.Intermediate/NEA_Market_Yield.Rds",
                            output.table = "06.Tables/NEA_Market_Fare.tex",
                            output.graph = "05.Figures/NEA_Market_Fare_Graph.pdf",
                            output.graph_airport = "05.Figures/NEA_Airport_Fare_Graph.pdf"){
  nea_d <- readRDS(input)
  nea_d[, Mkt := paste(Origin, Dest)]
  nea_d[, Period := relevel(Period, ref = "-1")]
  
  reg1 <- lm(log(Market_Avg_Fare) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = nea_d)
  reg1.se <- sqrt(diag(vcovCL(reg1, cluster = nea_d$Mkt)))
  test1 <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  
  reg2 <- lm(log(Market_Avg_Fare) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(Income_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = nea_d)
  reg2.se <- sqrt(diag(vcovCL(reg2, cluster = nea_d$Mkt)))
  test2 <- coeftest(reg2, vcov = vcovCL, cluster = ~ Mkt)
  
  restrict_data <- nea_d[Period %in% c("-4", "-3", "-2", "0", "1", "2", "3", "-1"),]
  restrict_data[, Period := relevel(Period, ref = "-1")]
  
  reg3 <- lm(log(Market_Avg_Fare) ~ Period + NEA_Market + NEA_Market:Period + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(Income_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + 
               Market_HHI.Lag, 
             data = restrict_data)
  reg3.se <- sqrt(diag(vcovCL(reg3, cluster = restrict_data$Mkt)))
  test3 <- coeftest(reg3, vcov = vcovCL, cluster = ~Mkt)
  
  reg4 <- lm(log(Market_Avg_Fare) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(stateIncome_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = nea_d)
  reg4.se <- sqrt(diag(vcovCL(reg4, cluster = nea_d$Mkt)))
  test4 <- coeftest(reg4, vcov = vcovCL, cluster = ~ Mkt)
  
  
  reg5 <- lm(log(Market_Avg_Fare) ~ Period + NEA_Market + Period:NEA_Market + 
               Market_Share_NonStop + log(Pop_GeomMean) + log(stateIncome_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
             data = restrict_data)
  reg5.se <- sqrt(diag(vcovCL(reg5, cluster = restrict_data$Mkt)))
  test5 <- coeftest(reg5, vcov = vcovCL, cluster = ~ Mkt)
  
  
  models <- list(reg1, reg2, reg3, reg4, reg5)
  se <- list(reg1.se, reg2.se, reg3.se, reg4.se, reg5.se)
  pvalues <- list(test1[,4], test2[,4], test3[,4], test4[,4], test5[,4])
  
  
  order <- list("Period-8:NEA_Market" = NA,
                "Period-7:NEA_Market" = NA,"Period-6:NEA_Market" = NA,
                "Period-5:NEA_Market" = NA,
                "Period-4:NEA_Market" = NA,"Period-3:NEA_Market" = NA,
                "Period-2:NEA_Market" = NA,"Period0:NEA_Market"  = NA, 
                "Period1:NEA_Market" = NA, "Period2:NEA_Market"  = NA,
                "Period3:NEA_Market"  = NA, "Period4:NEA_Market" = NA, "Period5:NEA_Market" = NA ,
                "Period6:NEA_Market" = NA, "Period7:NEA_Market" = NA, "Period8:NEA_Market" = NA)
 
   gof_rows <- list("Standard Controls" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
                    "Income Data" = c("", "MSA", "MSA", "State", "State"),
                    "Sample" = c("Full", "Full", "Two Years", "Full", "Two Years"))
  
  texreg(l = models, 
         file = output.table, single.row = FALSE, 
         stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = se, 
         override.pvalues = pvalues,
         digits = 5,
         custom.gof.rows = gof_rows,
         custom.coef.map = order)
  
  # Now, Graph
  # Make Graph of Column 4
  coeffs <- reg2$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = reg2.se)
  
  frame <- frame[grepl(pattern = ":NEA_Market", x = Value),]
  frame[, Value := gsub(pattern = ":NEA_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = "Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "Average Market Yield")
  ggsave(output.graph, units = "in", height = 3, width = 5)
  
  reg_airports <- lm(log(Market_Avg_Fare) ~ Period + BOS_Market + EWR_Market + LGA_Market + JFK_Market +
                       Period:BOS_Market + Period:EWR_Market + Period:LGA_Market + Period:JFK_Market + 
                       Market_Share_NonStop + log(Pop_GeomMean) + log(Income_GeomMean) + 
                       Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases + Market_HHI.Lag, 
                     data = nea_d)
  airports.se <- sqrt(diag(vcovCL(reg_airports, cluster = nea_d$Mkt)))
  
  coeffs <- reg_airports$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = airports.se)
  
  frame <- frame[grepl(pattern = "_Market", x = Value),]
  frame <- frame[grepl(pattern = ":", x = Value),]
  frame[, Airport := "NA"]
  frame[grepl(pattern = "BOS", x = Value), Airport := "BOS"]
  frame[grepl(pattern = "JFK", x = Value), Airport := "JFK"]
  frame[grepl(pattern = "LGA", x = Value), Airport := "LGA"]
  frame[grepl(pattern = "EWR", x = Value), Airport := "EWR"]
  frame[, Value := gsub(pattern = ":BOS_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":JFK_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":LGA_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":EWR_Market", replacement = "", x = Value)]
  
  frame[, Value := gsub(pattern = "Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "BOS"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "EWR"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "JFK"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "LGA"))
  
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    facet_grid(rows = vars(Airport)) + 
    labs(x = "Period", y = "Average Market Fare")
  ggsave(output.graph_airport, units = "in", height = 7, width = 5)
}

nea_pass_reg <- function(input = "02.Intermediate/NEA_Airline_Yield.Rds",
                            output.table = "06.Tables/NEA_Passengers.tex",
                            output.graph = "05.Figures/NEA_Passengers_Reg_Graph.pdf",
                            output.graph_airport = "05.Figures/NEA_Airport_Passengers_Reg_Graph.pdf"){
  nea_d <- readRDS(input)
  nea_d[, Mkt := paste(Origin, Dest)]
  nea_d[, Period := relevel(Period, ref = "-1")]
  
  nea_d[, NEA_Airline := Carrier %in% c("American Airlines Inc.", "JetBlue Airways")]
  nea_d[, NEA_Mkt_Pass := sum(Airline_Passengers), by = c("Mkt", "Period", "NEA_Airline")]
  nea_d <- nea_d[, .(NEA_Mkt_Pass, Period, Mkt, NEA_Market,
                     Southwest_Prescence, Spirit_Prescence, Origin_PC_Covid_Cases, 
                     Destination_PC_Covid_Cases,  Income_GeomMean, Pop_GeomMean,
                     EWR_Market, BOS_Market, JFK_Market, LGA_Market)]
  nea_d <- unique(nea_d)
  
  reg1 <- lm(log(NEA_Mkt_Pass) ~ Period + NEA_Market + Period:NEA_Market + 
               log(Pop_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, 
             data = nea_d)
  reg1.se <- sqrt(diag(vcovCL(reg1, cluster = nea_d$Mkt)))
  test1 <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  
  reg2 <- lm(log(NEA_Mkt_Pass) ~ Period + NEA_Market + Period:NEA_Market + 
               log(Pop_GeomMean) + log(Income_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, 
             data = nea_d)
  reg2.se <- sqrt(diag(vcovCL(reg2, cluster = nea_d$Mkt)))
  test2 <- coeftest(reg2, vcov = vcovCL, cluster = ~ Mkt)
  
  restrict_data <- nea_d[Period %in% c("-4", "-3", "-2", "0", "1", "2", "3", "-1"),]
  restrict_data[, Period := relevel(Period, ref = "-1")]
  
  reg3 <- lm(log(NEA_Mkt_Pass) ~ Period + NEA_Market + NEA_Market:Period + 
                log(Pop_GeomMean) + log(Income_GeomMean) + 
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases +Destination_PC_Covid_Cases, 
             data = restrict_data)
  reg3.se <- sqrt(diag(vcovCL(reg3, cluster = restrict_data$Mkt)))
  test3 <- coeftest(reg3, vcov = vcovCL, cluster = ~Mkt)
  
  models <- list(reg1, reg2, reg3)
  se <- list(reg1.se, reg2.se, reg3.se)
  pvalues <- list(test1[,4], test2[,4], test3[,4])
  
  order <- list("Period-8:NEA_Market" = NA,
                "Period-7:NEA_Market" = NA,"Period-6:NEA_Market" = NA,
                "Period-5:NEA_Market" = NA,
                "Period-4:NEA_Market" = NA,"Period-3:NEA_Market" = NA,
                "Period-2:NEA_Market" = NA,"Period0:NEA_Market"  = NA, 
                "Period1:NEA_Market" = NA, "Period2:NEA_Market"  = NA,
                "Period3:NEA_Market"  = NA, "Period4:NEA_Market" = NA, "Period5:NEA_Market" = NA ,
                "Period6:NEA_Market" = NA, "Period7:NEA_Market" = NA, "Period8:NEA_Market" = NA,
                "log(Pop_GeomMean)" = "log(Population Mean)",
                "log(Income_GeomMean)" = "log(Income Mean)", "Southwest_PrescenceTrue" = "Southwest Prescence",
                "Spirit_PrescenceTRUE" = "Spirit Prescence", "Origin_PC_Covid_Cases" = "Origin Covid",
                "Destination_PC_Covid_Cases" = "Destination Covid")
  
  
  texreg(l = models, 
         file = output.table, single.row = FALSE, 
         stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = se, 
         override.pvalues = pvalues,
         digits = 5,
         custom.coef.map = order)
  
  # Now, Graph
  # Make Graph of Column 4
  coeffs <- reg2$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = reg2.se)
  
  frame <- frame[grepl(pattern = ":NEA_Market", x = Value),]
  frame[, Value := gsub(pattern = ":NEA_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = "Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "AA + JB Passengers")
  ggsave(output.graph, units = "in", height = 3, width = 5)
  
  reg_airports <- lm(log(NEA_Mkt_Pass) ~ Period + BOS_Market + EWR_Market + LGA_Market + JFK_Market +
                       Period:BOS_Market + Period:EWR_Market + Period:LGA_Market + Period:JFK_Market + 
                       log(Pop_GeomMean) + log(Income_GeomMean) + 
                       Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, 
                     data = nea_d)
  airports.se <- sqrt(diag(vcovCL(reg_airports, cluster = nea_d$Mkt)))
  
  coeffs <- reg_airports$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = airports.se)
  
  frame <- frame[grepl(pattern = "_Market", x = Value),]
  frame <- frame[grepl(pattern = ":", x = Value),]
  frame[, Airport := "NA"]
  frame[grepl(pattern = "BOS", x = Value), Airport := "BOS"]
  frame[grepl(pattern = "JFK", x = Value), Airport := "JFK"]
  frame[grepl(pattern = "LGA", x = Value), Airport := "LGA"]
  frame[grepl(pattern = "EWR", x = Value), Airport := "EWR"]
  frame[, Value := gsub(pattern = ":BOS_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":JFK_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":LGA_Market", replacement = "", x = Value)]
  frame[, Value := gsub(pattern = ":EWR_Market", replacement = "", x = Value)]
  
  frame[, Value := gsub(pattern = "Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "BOS"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "EWR"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "JFK"),
                 data.table(Value = "-1", Fit_Value = 0, SE = 0, Airport = "LGA"))
  
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    facet_grid(rows = vars(Airport)) + 
    labs(x = "Period", y = "AA + JB Passengers")
  ggsave(output.graph_airport, units = "in", height = 7, width = 5)
}


# Recreate, Expand Zou (2023) Table 1
nea_zou_1 <- function(input.ticket = "02.Intermediate/DB1B_With_Controls.Rds",
                      input.op = "02.Intermediate/DB1B_Operating.rds",
                      output = "06.Tables/NEA_Zou_TB1_Replicate.tex"){
  db1b.ticket <- readRDS(input.ticket)
  db1b.op <- readRDS(input.op)
  prep_db1b <- function(input){
    input <- input[NonStop == TRUE,]
    input <- input[Carrier %in% c("American Airlines Inc.", "JetBlue Airways"),]
    input <- input[Origin %in% c("EWR", "JFK", "LGA", "BOS"),]
    
    input <- input[, .(Year, Quarter, Origin, Dest, Carrier)]
    input <- input[Year %in% c(2019, 2021, 2022, 2023),]
    input <- unique(input)
    
    # Want overlap of total number of routes, which at this point is percent of routes with two carriers
    db1b.summarize <- input %>% group_by(Year, Quarter, Origin, Dest) %>%
      summarize(N_Carriers = n()) %>% data.table()
    
    db1b.summarize[, N_Routes_Originating := .N, by = c("Year", "Quarter", "Origin")]
    db1b.summarize[, N_Shared := sum(N_Carriers - 1), by = c("Year", "Quarter", "Origin")]
    db1b.summarize[, Shared_Percent := round(N_Shared / N_Routes_Originating * 100, digits = 1)]
    db1b.summarize <- db1b.summarize[, .(Year, Quarter, Origin, Shared_Percent)]
    db1b.summarize <- unique(db1b.summarize)
    
    db1b.summarize.wide <- reshape(data = db1b.summarize, direction = "wide", idvar = c("Year", "Quarter"),
                                   timevar = "Origin")
    return(db1b.summarize.wide)
  }
  
  db1b.ticket.wide <- prep_db1b(db1b.ticket)
  db1b.op.wide <- prep_db1b(db1b.op)
  
  db1b <- merge(db1b.ticket.wide, db1b.op.wide, by = c("Year", "Quarter"), suffixes = c(".tk", ".op"))
  db1b <- db1b[, .(Year, Quarter, Shared_Percent.JFK.tk, Shared_Percent.JFK.op,
                   Shared_Percent.BOS.tk, Shared_Percent.BOS.op, 
                   Shared_Percent.LGA.tk, Shared_Percent.LGA.op,
                   Shared_Percent.EWR.tk, Shared_Percent.EWR.op)]
  
  db1b <- setorder(x = db1b, Quarter, -Year)
  
  db1b$Quarter <- NULL
  colnames(db1b) <- c("Year", rep(c("Ticket", "Operating"), times = 4))
  
  kbl(db1b,
      format = "latex",
      escape = FALSE, booktabs = TRUE) %>%
    add_header_above(c(" " = 1, "JFK" = 2, 
                       "BOS" = 2, "LGA" = 2, 
                       "EWR" = 2)) %>%
    pack_rows(group_label = "Q1", 1, 4) %>%
    pack_rows(group_label = "Q2", 5, 8) %>% 
    pack_rows(group_label = "Q3", 9, 12) %>%
    pack_rows(group_label = "Q4", 13, 15) %>%
    save_kable(file = output)
}

nea_operational_logit <- function(input = "02.Intermediate/NEA_OPCarrier_Switch.Rds",
                                  output.table = "06.Tables/NEA_Probability_Operating.tex",
                                  output.figure = "05.Figures/NEA_Probability_Operating_Graph.tex"){
  db1b <- readRDS(input)
  db1b <- nea_create_period(db1b)  
  
  db1b.s <- db1b %>% filter(MktCoupons == 1) %>% group_by(Year, Quarter, Origin, Dest, Period) %>%
    summarize(JetBlue_Op := max(JetBlue_Op),
              American_Op := max(American_Op),
              NEA_Market := max(NEA_Market),
              MktMilesFlown := min(MktMilesFlown) / 100) %>%
    mutate(Time = paste(Year, Quarter),
           Post_NEA = as.numeric(Year >= 2021),
           NEA_AA = American_Op * NEA_Market,
           NEA_JB = JetBlue_Op * NEA_Market,
           Mkt = paste(Origin, Dest)) %>%
    as.data.table()

  db1b.s[, NEA_Collaborator := NEA_AA]

  ols_jb0 <- lm(JetBlue_Op ~ NEA_Collaborator + 
                  Period:NEA_Collaborator, data = db1b.s); summary(ols_jb0)
  ols_jb0_se <- sqrt(diag(vcovCL(x = ols_jb0, cluster = ~Mkt)))
  ols_jb0_test <- coeftest(ols_jb0, vcov = vcovCL, cluster = ~ Mkt)
  ols_jb1 <- lm(JetBlue_Op ~ NEA_Collaborator + MktMilesFlown + 
                  Period:NEA_Collaborator, data = db1b.s); summary(ols_jb1)
  ols_jb1_se <- sqrt(diag(vcovCL(x = ols_jb1, cluster = ~Mkt)))
  ols_jb1_test <- coeftest(ols_jb1, vcov = vcovCL, cluster = ~ Mkt)
  
  
  db1b.s[, NEA_Collaborator := NEA_JB]
  ols_aa0 <- lm(American_Op ~ NEA_Collaborator + 
                  Period:NEA_Collaborator, data = db1b.s)
  ols_aa0_se <- sqrt(diag(vcovCL(x = ols_aa0, cluster = ~Mkt)))
  ols_aa0_test <- coeftest(ols_aa0, vcov = vcovCL, cluster = ~ Mkt)
  
  ols_aa1 <- lm(American_Op ~ NEA_Collaborator + MktMilesFlown + 
                  Period:NEA_Collaborator, data = db1b.s)
  ols_aa1_se <- sqrt(diag(vcovCL(x = ols_aa1, cluster = ~Mkt)))
  ols_aa1_test <- coeftest(ols_aa1, vcov = vcovCL, cluster = ~ Mkt)
  
  
  name_list <- list("NEA_Collaborator:Period-8" = NA,
        "NEA_Collaborator:Period-7" = NA,"NEA_Collaborator:Period-6" = NA,"NEA_Collaborator:Period-5" = NA,
     "NEA_Collaborator:Period-4" = NA,"NEA_Collaborator:Period-3" = NA,
     "NEA_Collaborator:Period-2" = NA,"NEA_Collaborator:Period0"  = NA, 
     "NEA_Collaborator:Period1" = NA, "NEA_Collaborator:Period2"  = NA,
      "NEA_Collaborator:Period3"  = NA, "NEA_Collaborator:Period4" = NA, "NEA_Collaborator:Period5" = NA ,
      "NEA_Collaborator:Period6" = NA, "NEA_Collaborator:Period7" = NA, "NEA_Collaborator:Period8" = NA)
  
  models <- list(ols_jb0, ols_jb1, ols_aa0, ols_aa1)
  se.list <- list(ols_jb0_se, ols_jb1_se, ols_aa0_se, ols_aa1_se)
  p.list <- list(ols_jb0_test[,4], ols_jb1_test[, 4], ols_aa0_test[, 4], ols_aa1_test[,4])
  texreg(l = models, table = FALSE,
        file = output.table, single.row = TRUE, stars = c(0.1, 0.05, 0.01),
        override.se = se.list,  override.pvalues = p.list,
        digits = 5, custom.coef.map = name_list,
        custom.header = list("JetBlue Op" = 1:2, "American OP" = 3:4))
}

# Regress on Dummy Variable for P(Market Had Customers Switch from JB to AA on an Itenerary)
nea_changes_recorded_estimate <- function(input = "02.Intermediate/NEA_OPCarrier_Switch.Rds",
                                          output.tab = "06.Tables/NEA_Probability_Switches.tex",
                                          output.graph = "05.Figures/NEA_Probability_Switches_Graph.pdf"){
  db1b <- readRDS(input)
  
  db1b <- nea_create_period(db1b)
  
  db1b.s <- db1b %>% filter(MktCoupons < 3) %>% group_by(Year, Quarter, Origin, Dest, Period) %>%
    summarize(Switches_Occured := max(AA_B6),
              NEA_Market := max(NEA_Market),
              MktMilesFlown := min(MktMilesFlown) / 100) %>%
    mutate(Mkt = paste(Origin, Dest)) %>%
    as.data.table()
    
  reg1 <- lm(Switches_Occured ~ NEA_Market + Period:NEA_Market,
             data = db1b.s)
  test <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  
  clustered_se <- sqrt(diag(vcovCL(x = reg1, cluster = ~Mkt)))
  
  order <- list("(Intercept)" = NA,"NEA_Market" = NA,"NEA_Market:Period-8" = NA,
      "NEA_Market:Period-7" = NA,"NEA_Market:Period-6" = NA,"NEA_Market:Period-5" = NA,"NEA_Market:Period-4" = NA,
      "NEA_Market:Period-3" = NA,"NEA_Market:Period-2" = NA,
      "NEA_Market:Period0" = NA, "NEA_Market:Period1" = NA,"NEA_Market:Period2" = NA,
       "NEA_Market:Period3" = NA,"NEA_Market:Period4" = NA,"NEA_Market:Period5"  = NA,"NEA_Market:Period6"  = NA,
       "NEA_Market:Period7" = NA,"NEA_Market:Period8" = NA)
  
  texreg(l = reg1, file = output.tab, single.row = TRUE, stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = clustered_se, 
         override.pvalues = test[, 4],
         digits = 5,
         custom.coef.map = order)
  
  
  # Now, Graph
  coeffs <- reg1$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = clustered_se)
  
  frame <- frame[grepl(pattern = "Market:Period", x = Value),]
  frame[, Value := gsub(pattern = "NEA_Market:Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(-0.01, 0.15),
                       labels = comma) + 
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "P(Switches Occured)")
    ggsave(filename = output.graph, units = "in", height = 3, width = 5)
}

# Regress on Number of Code Sharing Itineraries
nea_code_sharing_itenaries <- function(input = "02.Intermediate/NEA_OPCarrier_Switch.Rds",
                                          output.tab = "06.Tables/NEA_N_Switches.tex",
                                          output.graph = "05.Figures/NEA_N_Switches_Graph.pdf"){
  db1b <- readRDS(input)
  
  db1b <- nea_create_period(db1b)

  db1b[, MktMinMiles := min(MktMilesFlown), by = c("Year", "Quarter", "Origin", "Dest", "Period")]
  
  db1b.s <- db1b %>% filter(MktCoupons <= 3) %>% 
    filter(AA_B6 == TRUE) %>% group_by(Year, Quarter, Origin, Dest, MktMinMiles) %>%
    summarize(Passengers = sum(Passengers) * 10,
              NEA_Market := max(NEA_Market),
              MktMilesFlown := mean(MktMilesFlown) / 100) %>%
    mutate(Mkt = paste(Origin, Dest)) %>%
    as.data.table()
  
  # Cover 0 Obs


  reg1 <- lm(Switches_Occured ~ NEA_Market + Period:NEA_Market,
             data = db1b.s)
  test <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  
  clustered_se <- sqrt(diag(vcovCL(x = reg1, cluster = ~Mkt)))
  
  order <- list("(Intercept)" = NA,"NEA_Market" = NA,"NEA_Market:Period-8" = NA,
                "NEA_Market:Period-7" = NA,"NEA_Market:Period-6" = NA,"NEA_Market:Period-5" = NA,"NEA_Market:Period-4" = NA,
                "NEA_Market:Period-3" = NA,"NEA_Market:Period-2" = NA,
                "NEA_Market:Period0" = NA, "NEA_Market:Period1" = NA,"NEA_Market:Period2" = NA,
                "NEA_Market:Period3" = NA,"NEA_Market:Period4" = NA,"NEA_Market:Period5"  = NA,"NEA_Market:Period6"  = NA,
                "NEA_Market:Period7" = NA,"NEA_Market:Period8" = NA)
  
  texreg(l = reg1, file = output.tab, single.row = TRUE, stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = clustered_se, 
         override.pvalues = test[, 4],
         digits = 5,
         custom.coef.map = order)
  
  
  # Now, Graph
  coeffs <- reg1$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = clustered_se)
  
  frame <- frame[grepl(pattern = "Market:Period", x = Value),]
  frame[, Value := gsub(pattern = "NEA_Market:Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(-0.01, 0.09),
                       labels = comma) + 
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "P(Switches Occured)")
  ggsave(filename = output.graph, units = "in", height = 3, width = 5)
}

nea_price_neutrality <- function(input = "02.Intermediate/NEA_Airline_Yield.Rds",
                                       output.table = "06.Tables/NEA_Price_Neutrality.tex",
                                       output.graph = "05.Figures/NEA_Price_Neutrality_Graph.pdf"){
  db1b <- readRDS(input)

  db1b <- db1b %>% 
    mutate(Income_GeomMean = sqrt(Origin_Income_PerCap) * sqrt(Dest_Income_PerCap),
           Pop_GeomMean = sqrt(Origin.Population) * sqrt(Destination.Population),
           StatePop_GeomMean = sqrt(OriginState_Pop) * sqrt(DestinationState_Pop),
           stateIncome_GeomMean = sqrt(OriginState_Income) * sqrt(DestinationState_Income)) %>%
    as.data.table()
  
  # Identify All Markets In Which Both AA, JB Tickets Exist
  markets.relevant <- db1b %>% group_by(Year, Quarter, Origin, Dest) %>%
    summarize(JB_Operate = max(JetBlue_Prescence),
              AA_Operate = max(American_Prescence)) %>%
    filter(JB_Operate == TRUE, AA_Operate == TRUE) %>%
    mutate(Mkt_Name = paste(Year, Quarter, Origin, Dest))
  
  db1b[, Mkt := paste(Year, Quarter, Origin, Dest)]
  db1b <- db1b[Mkt %in% markets.relevant$Mkt_Name,];
  db1b[, Mkt := paste(Origin, Dest)]

  db1b[, NEA_Market := FALSE]
  db1b[Origin == "JFK" | Dest == "JFK", NEA_Market := TRUE]
  db1b[Origin == "EWR" | Dest == "EWR", NEA_Market := TRUE]
  db1b[Origin == "LGA" | Dest == "LGA", NEA_Market := TRUE]
  db1b[Origin == "BOS" | Dest == "BOS", NEA_Market := TRUE]
  
  
  db1b.jb <- db1b %>% filter(Carrier == "JetBlue Airways") %>%
    select(Year, Quarter, Origin, Dest, Airline_Average_Fare, Airline_Average_Miles) %>%
    mutate(JB.Fare = Airline_Average_Fare, Airline_Average_Fare = NULL,
           JB.Avg.Miles = Airline_Average_Miles, Airline_Average_Miles = NULL) %>% as.data.table()
  
  db1b.aa <- db1b %>% filter(Carrier == "American Airlines Inc.") %>%
    select(Year, Quarter, Origin, Dest, Airline_Average_Fare, Airline_Average_Miles) %>%
    mutate(AA.Fare = Airline_Average_Fare, Airline_Average_Fare = NULL,
           AA.Avg.Miles = Airline_Average_Miles, Airline_Average_Miles = NULL) %>% as.data.table()
  
  db1b.car_fares <- merge(db1b.jb, db1b.aa, by = c("Year", "Quarter", "Origin", "Dest"))
  
  db1b <- unique(db1b[, .(Year, Quarter, Origin, Dest,
                   Origin_PC_Covid_Cases, Destination_PC_Covid_Cases,
                   NEA_Market, Period, Mkt, Market_HHI, Southwest_Prescence, Spirit_Prescence,
                   Origin_Covid_Cases, Destination_Covid_Cases,
                   Income_GeomMean, Pop_GeomMean, StatePop_GeomMean, stateIncome_GeomMean)])

  db1b <- merge(db1b, db1b.car_fares, by = c("Year", "Quarter", "Origin", "Dest"))
  
  db1b[, Fare.Difference := AA.Fare - JB.Fare]
  db1b[, NEA_Market := as.numeric(NEA_Market)]
  
  restrict_data <- db1b[Period %in% c("-4", "-3", "-2", "0", "1", "2", "3", "-1"),]
  restrict_data[, Period := relevel(Period, ref = "-1")]
  
  
  reg1 <- lm(Fare.Difference ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg1)
  test1 <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  se1 <- sqrt(diag(vcovCL(x = reg1, cluster = ~Mkt)))
  
  reg2 <- lm(Fare.Difference ~ NEA_Market + Period + Period:NEA_Market + Spirit_Prescence + Southwest_Prescence +
               log(Pop_GeomMean) + log(Income_GeomMean) +
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg2)
  test2 <- coeftest(reg2, vcov = vcovCL, cluster = ~ Mkt)
  se2 <- sqrt(diag(vcovCL(x = reg2, cluster = ~Mkt)))
  
  reg3 <- lm(Fare.Difference ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) + log(Income_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = restrict_data); summary(reg3)
  test3 <- coeftest(reg3, vcov = vcovCL, cluster = ~ Mkt)
  se3 <- sqrt(diag(vcovCL(x = reg3, cluster = ~Mkt)))
  
  reg4 <- lm(Fare.Difference ~ NEA_Market + Period + Period:NEA_Market + Spirit_Prescence + Southwest_Prescence +
               log(Pop_GeomMean) +log(stateIncome_GeomMean) + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg2)
  test4 <- coeftest(reg4, vcov = vcovCL, cluster = ~ Mkt)
  se4 <- sqrt(diag(vcovCL(x = reg4, cluster = ~Mkt)))
  
  reg5 <- lm(Fare.Difference ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) + log(stateIncome_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = restrict_data); summary(reg3)
  test5 <- coeftest(reg5, vcov = vcovCL, cluster = ~ Mkt)
  se5 <- sqrt(diag(vcovCL(x = reg5, cluster = ~Mkt)))
  
  models <- list(reg1, reg2, reg3, reg4, reg5)
  pvalues <- list(test1[,4], test2[,4], test3[,4], test4[,4], test5[,4])
  se <- list(se1, se2, se3, se4, se5)
  
  order <- list("NEA_Market", "NEA_Market:Period-8" = NA,
                "NEA_Market:Period-7" = NA,"NEA_Market:Period-6" = NA,"NEA_Market:Period-5" = NA,"NEA_Market:Period-4" = NA,
                "NEA_Market:Period-3" = NA,"NEA_Market:Period-2" = NA,
                "NEA_Market:Period0" = NA, "NEA_Market:Period1" = NA,"NEA_Market:Period2" = NA,
                "NEA_Market:Period3" = NA,"NEA_Market:Period4" = NA,"NEA_Market:Period5"  = NA,"NEA_Market:Period6"  = NA,
                "NEA_Market:Period7" = NA,"NEA_Market:Period8" = NA)
  
  
  gof_rows <- list("Standard Controls" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
                   "Income Data" = c("", "MSA", "MSA", "State", "State"),
                   "Sample" = c("Full", "Full", "Two Years", "Full", "Two Years"))
  
  texreg(l = models, custom.gof.rows = gof_rows,
         file = output.table, single.row = FALSE, 
         stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = se, 
         override.pvalues = pvalues,
         digits = 5,
         custom.coef.map = order)
  
  # Make Graph of Column 2
  coeffs <- reg2$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = se2)
  
  frame <- frame[grepl(pattern = "Market:Period", x = Value),]
  frame[, Value := gsub(pattern = "NEA_Market:Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "AA Fare - JB Fare")
  ggsave(filename = output.graph, units = "in", height = 3, width = 5)
}

nea_price_neutrality_yield <- function(input = "02.Intermediate/NEA_Airline_Yield.Rds",
                                 output.table = "06.Tables/NEA_Price_Neutrality_Y.tex",
                                 output.graph = "05.Figures/NEA_Price_Neutrality_Graph_Y.pdf"){
  db1b <- readRDS(input)
  
  db1b <- db1b %>% 
    mutate(Income_GeomMean = sqrt(Origin_Income_PerCap) * sqrt(Dest_Income_PerCap),
        Pop_GeomMean = sqrt(Origin.Population) * sqrt(Destination.Population),
        StatePop_GeomMean = sqrt(OriginState_Pop) * sqrt(DestinationState_Pop),
        stateIncome_GeomMean = sqrt(OriginState_Income) * sqrt(DestinationState_Income)) %>%
    as.data.table()
  
  # Identify All Markets In Which Both AA, JB Tickets Exist
  markets.relevant <- db1b %>% group_by(Year, Quarter, Origin, Dest) %>%
    summarize(JB_Operate = max(JetBlue_Prescence),
              AA_Operate = max(American_Prescence)) %>%
    filter(JB_Operate == TRUE, AA_Operate == TRUE) %>%
    mutate(Mkt_Name = paste(Year, Quarter, Origin, Dest))
  
  db1b[, Mkt := paste(Year, Quarter, Origin, Dest)]
  db1b <- db1b[Mkt %in% markets.relevant$Mkt_Name,];
  db1b[, Mkt := paste(Origin, Dest)]
  
  db1b[, NEA_Market := FALSE]
  db1b[Origin == "JFK" | Dest == "JFK", NEA_Market := TRUE]
  db1b[Origin == "EWR" | Dest == "EWR", NEA_Market := TRUE]
  db1b[Origin == "LGA" | Dest == "LGA", NEA_Market := TRUE]
  db1b[Origin == "BOS" | Dest == "BOS", NEA_Market := TRUE]
  
  db1b.jb <- db1b %>% filter(Carrier == "JetBlue Airways") %>%
    select(Year, Quarter, Origin, Dest, Airline_Yield) %>%
    mutate(JB.Fare = Airline_Yield, Airline_Yield = NULL) %>% as.data.table()
  
  db1b.aa <- db1b %>% filter(Carrier == "American Airlines Inc.") %>%
    select(Year, Quarter, Origin, Dest, Airline_Yield) %>%
    mutate(AA.Fare = Airline_Yield, Airline_Yield = NULL) %>% as.data.table()
  
  db1b.car_fares <- merge(db1b.jb, db1b.aa, by = c("Year", "Quarter", "Origin", "Dest"))
  
  db1b <- unique(db1b[, .(Year, Quarter, Origin, Dest,
                          Origin_PC_Covid_Cases, Destination_PC_Covid_Cases,
                          NEA_Market, Period, Mkt, Market_HHI, Southwest_Prescence, Spirit_Prescence,
                          Origin_Covid_Cases, Destination_Covid_Cases,
                          Income_GeomMean, Pop_GeomMean, StatePop_GeomMean, stateIncome_GeomMean)])
  
  db1b <- merge(db1b, db1b.car_fares, by = c("Year", "Quarter", "Origin", "Dest"))
  
  db1b[, Yield.Difference := AA.Fare - JB.Fare]
  db1b[, NEA_Market := as.numeric(NEA_Market)]
  
  
  restrict_data <- db1b[Period %in% c("-4", "-3", "-2", "0", "1", "2", "3", "-1"),]
  restrict_data[, Period := relevel(Period, ref = "-1")]
  
  reg1 <- lm(Yield.Difference ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg1)
  test1 <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  se1 <- sqrt(diag(vcovCL(x = reg1, cluster = ~Mkt)))
  
  reg2 <- lm(Yield.Difference ~ NEA_Market + Period + Period:NEA_Market + Spirit_Prescence + Southwest_Prescence +
               log(Pop_GeomMean) + log(Income_GeomMean) +
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg2)
  test2 <- coeftest(reg2, vcov = vcovCL, cluster = ~ Mkt)
  se2 <- sqrt(diag(vcovCL(x = reg2, cluster = ~Mkt)))
  
  reg3 <- lm(Yield.Difference ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) + log(Income_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = restrict_data); summary(reg3)
  test3 <- coeftest(reg3, vcov = vcovCL, cluster = ~ Mkt)
  se3 <- sqrt(diag(vcovCL(x = reg3, cluster = ~Mkt)))
  
  reg4 <- lm(Yield.Difference ~ NEA_Market + Period + Period:NEA_Market + Spirit_Prescence + Southwest_Prescence +
               log(Pop_GeomMean) +log(stateIncome_GeomMean) + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg2)
  test4 <- coeftest(reg4, vcov = vcovCL, cluster = ~ Mkt)
  se4 <- sqrt(diag(vcovCL(x = reg4, cluster = ~Mkt)))
  
  reg5 <- lm(Yield.Difference ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) + log(stateIncome_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = restrict_data); summary(reg3)
  test5 <- coeftest(reg5, vcov = vcovCL, cluster = ~ Mkt)
  se5 <- sqrt(diag(vcovCL(x = reg5, cluster = ~Mkt)))
  
  models <- list(reg1, reg2, reg3, reg4, reg5)
  pvalues <- list(test1[,4], test2[,4], test3[,4], test4[,4], test5[,4])
  se <- list(se1, se2, se3, se4, se5)
  
  order <- list("NEA_Market", "NEA_Market:Period-8" = NA,
                "NEA_Market:Period-7" = NA,"NEA_Market:Period-6" = NA,"NEA_Market:Period-5" = NA,"NEA_Market:Period-4" = NA,
                "NEA_Market:Period-3" = NA,"NEA_Market:Period-2" = NA,
                "NEA_Market:Period0" = NA, "NEA_Market:Period1" = NA,"NEA_Market:Period2" = NA,
                "NEA_Market:Period3" = NA,"NEA_Market:Period4" = NA,"NEA_Market:Period5"  = NA,"NEA_Market:Period6"  = NA,
                "NEA_Market:Period7" = NA,"NEA_Market:Period8" = NA)
  
  
  gof_rows <- list("Standard Controls" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
                   "Income Data" = c("", "MSA", "MSA", "State", "State"),
                   "Sample" = c("Full", "Full", "Two Years", "Full", "Two Years"))
  
  texreg(l = models, custom.gof.rows = gof_rows,
         file = output.table, single.row = FALSE, 
         stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = se, 
         override.pvalues = pvalues,
         digits = 5,
         custom.coef.map = order)
  
  # Make Graph of Column 4
  coeffs <- reg2$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = se2)
  
  frame <- frame[grepl(pattern = "Market:Period", x = Value),]
  frame[, Value := gsub(pattern = "NEA_Market:Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "AA Yield - JB Yield")
  ggsave(filename = output.graph, units = "in", height = 3, width = 5)
}

nea_revenue <- function(input = "02.Intermediate/NEA_Airline_Yield.Rds",
                                 output.table = "06.Tables/NEA_Revenue.tex",
                                 output.graph = "05.Figures/NEA_Revenue.pdf"){
  db1b <- readRDS(input)
  
  db1b <- db1b %>% 
    mutate(Income_GeomMean = sqrt(Origin_Income_PerCap) * sqrt(Dest_Income_PerCap),
           Pop_GeomMean = sqrt(Origin.Population) * sqrt(Destination.Population),
           StatePop_GeomMean = sqrt(OriginState_Pop) * sqrt(DestinationState_Pop),
           stateIncome_GeomMean = sqrt(OriginState_Income) * sqrt(DestinationState_Income)) %>%
    as.data.table()
  
  # Identify All Markets In Which Both AA, JB Tickets Exist
  markets_keep <- db1b %>% group_by(Year, Quarter, Origin, Dest) %>%
    mutate(JB_Operate = max(JetBlue_Prescence),
              AA_Operate = max(American_Prescence)) %>%
    filter(JB_Operate == TRUE, AA_Operate == TRUE) %>%
    mutate(Mkt_Name = paste(Year, Quarter, Origin, Dest))
  
  db1b <- db1b %>% mutate(Mkt_Name = paste(Year, Quarter, Origin, Dest)) %>%
    filter(Mkt_Name %in% markets_keep$Mkt_Name) %>%
    as.data.table()
  
  db1b[, Mkt := paste(Origin, Dest)]
  db1b <- db1b[Carrier %in% c("American Airlines Inc.",
                              "JetBlue Airways"),]
  
  db1b[, Market_Revenue := sum(Airline_Passengers * Airline_Average_Fare),
       by = c("Year", "Quarter", "Origin", "Dest")]
  
  db1b[, NEA_Market := FALSE]
  db1b[Origin == "JFK" | Dest == "JFK", NEA_Market := TRUE]
  db1b[Origin == "EWR" | Dest == "EWR", NEA_Market := TRUE]
  db1b[Origin == "LGA" | Dest == "LGA", NEA_Market := TRUE]
  db1b[Origin == "BOS" | Dest == "BOS", NEA_Market := TRUE]
  
  
  db1b[, NEA_Market := as.numeric(NEA_Market)]
  
  restrict_data <- db1b[Period %in% c("-4", "-3", "-2", "0", "1", "2", "3", "-1"),]
  restrict_data[, Period := relevel(Period, ref = "-1")]
  
  
  reg1 <- lm(Market_Revenue ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg1)
  test1 <- coeftest(reg1, vcov = vcovCL, cluster = ~ Mkt)
  se1 <- sqrt(diag(vcovCL(x = reg1, cluster = ~Mkt)))
  
  reg2 <- lm(Market_Revenue ~ NEA_Market + Period + Period:NEA_Market + Spirit_Prescence + Southwest_Prescence +
               log(Pop_GeomMean) + log(Income_GeomMean) +
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg2)
  test2 <- coeftest(reg2, vcov = vcovCL, cluster = ~ Mkt)
  se2 <- sqrt(diag(vcovCL(x = reg2, cluster = ~Mkt)))
  
  reg3 <- lm(Market_Revenue ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) + log(Income_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = restrict_data); summary(reg3)
  test3 <- coeftest(reg3, vcov = vcovCL, cluster = ~ Mkt)
  se3 <- sqrt(diag(vcovCL(x = reg3, cluster = ~Mkt)))
  
  reg4 <- lm(Market_Revenue ~ NEA_Market + Period + Period:NEA_Market + Spirit_Prescence + Southwest_Prescence +
               log(Pop_GeomMean) +log(stateIncome_GeomMean) + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = db1b); summary(reg2)
  test4 <- coeftest(reg4, vcov = vcovCL, cluster = ~ Mkt)
  se4 <- sqrt(diag(vcovCL(x = reg4, cluster = ~Mkt)))
  
  reg5 <- lm(Market_Revenue ~ NEA_Market + Period + Period:NEA_Market +
               log(Pop_GeomMean) + log(stateIncome_GeomMean) +
               Southwest_Prescence + Spirit_Prescence + 
               Origin_PC_Covid_Cases + Destination_PC_Covid_Cases, data = restrict_data); summary(reg3)
  test5 <- coeftest(reg5, vcov = vcovCL, cluster = ~ Mkt)
  se5 <- sqrt(diag(vcovCL(x = reg5, cluster = ~Mkt)))
  
  models <- list(reg1, reg2, reg3, reg4, reg5)
  pvalues <- list(test1[,4], test2[,4], test3[,4], test4[,4], test5[,4])
  se <- list(se1, se2, se3, se4, se5)
  
  order <- list("NEA_Market", "NEA_Market:Period-8" = NA,
                "NEA_Market:Period-7" = NA,"NEA_Market:Period-6" = NA,"NEA_Market:Period-5" = NA,"NEA_Market:Period-4" = NA,
                "NEA_Market:Period-3" = NA,"NEA_Market:Period-2" = NA,
                "NEA_Market:Period0" = NA, "NEA_Market:Period1" = NA,"NEA_Market:Period2" = NA,
                "NEA_Market:Period3" = NA,"NEA_Market:Period4" = NA,"NEA_Market:Period5"  = NA,"NEA_Market:Period6"  = NA,
                "NEA_Market:Period7" = NA,"NEA_Market:Period8" = NA)
  
  
  gof_rows <- list("Standard Controls" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
                   "Income Data" = c("", "MSA", "MSA", "State", "State"),
                   "Sample" = c("Full", "Full", "Two Years", "Full", "Two Years"))
  
  texreg(l = models, custom.gof.rows = gof_rows,
         file = output.table, single.row = FALSE, 
         stars = c(0.1, 0.05, 0.01),
         table = FALSE, override.se = se, 
         override.pvalues = pvalues,
         digits = 5,
         custom.coef.map = order)
  
  # Make Graph of Column 2
  coeffs <- reg2$coefficients
  coeff_labels <- names(coeffs)
  coeffs <- unname(coeffs)
  frame <- data.table(Value = coeff_labels, Fit_Value = coeffs, 
                      SE = se2)
  
  frame <- frame[grepl(pattern = "Market:Period", x = Value),]
  frame[, Value := gsub(pattern = "NEA_Market:Period", replacement = "", x = Value)]
  frame <- rbind(frame, data.table(Value = "-1", Fit_Value = 0, SE = 0))
  frame[, Value := as.numeric(Value)]
  frame[, SE_Bound := 1.96 * SE]
  
  ggplot(frame, aes(x = Value, y = Fit_Value)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Fit_Value-SE_Bound, ymax = Fit_Value+SE_Bound)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "Total AA, JB Revenue")
  ggsave(filename = output.graph, units = "in", height = 3, width = 5)
}
