merger_simulation <- function(){
  generate_city_shares(); gc(); gc();
  generate_city_shares(output = "02.Intermediate/MarketSharesCity_LCC.Rds",
                       mode = "LCC"); gc(); gc();

  generate_airport_shares(); gc(); gc();
  generate_airport_shares(output = "02.Intermediate/Airport_Shares_LCC.Rds",
                          mode = "LCC")

  
  # Generate HHI Whole Market
  city_hhi(); gc();
  airport_hhi(); gc();
  
  # Generate HHI LCC
  city_hhi(input = "02.Intermediate/MarketSharesCity_LCC.Rds",
           output = "02.Intermediate/HHI_City_LCC_Market.Rds"); gc()
  
  airport_hhi(input = "02.Intermediate/Airport_Shares_LCC.Rds",
              output = "02.Intermediate/HHI_Airport_LCC_Market.Rds"); gc();
}

generate_city_shares <- function(input = "02.Intermediate/Compile_DB1B.Rds",
                                 output = "02.Intermediate/MarketSharesCity.Rds",
                                 mode = "NA"){
  if(mode == "NA"){
    market_disagregated_data <- readRDS(input)
  } else if(mode == "LCC"){
    market_disagregated_data <- readRDS(input) %>% 
    filter(RPCarrier %in% c("Southwest Airlines Co.", "Alaska Airlines Inc.",
              "Spirit Air Lines", "Hawaiian Airlines Inc.",
              "Frontier Airlines Inc.", "Allegiant Air",
              "JetBlue Airways"))
  }
  market_agregated_data2 <- market_disagregated_data %>% group_by(Origin.City,
                                                                  Destination.City,  
                                                                  RPCarrier, Year, Quarter) %>%
    summarize(Airport_Market_Passengers = sum(Passengers, na.rm = TRUE),
              MktFare.City = mean(MktFare, na.rm = TRUE),
              MktMilesFlown.City = mean(MktMilesFlown), na.rm = TRUE,
              Passengers.City = sum(Passengers, na.rm = TRUE),
              Delta_Prescence.City = max(Delta_Prescence, na.rm = TRUE),
              Alaska_Prescence.City = max(Alaska_Prescence, na.rm = TRUE),
              American_Prescence.City = max(American_Prescence, na.rm = TRUE),
              Hawaiian_Prescence.City = max(Hawaiian_Prescence, na.rm = TRUE),
              JetBlue_Prescence.City = max(JetBlue_Prescence, na.rm = TRUE),
              Frontier_Prescence.City = max(Frontier_Prescence, na.rm = TRUE),
              Allegiant_Prescence.City = max(Allegiant_Prescence, na.rm = TRUE),
              Spirit_Prescence.City = max(Spirit_Prescence, na.rm = TRUE),
              United_Prescence.City = max(United_Prescence, na.rm = TRUE),
              Delta_Entry_Potential.City = max(Delta_Entry_Potential, na.rm = TRUE),
              Alaska_Entry_Potential.City = max(Alaska_Entry_Potential, na.rm = TRUE),
              American_Entry_Potential.City = max(American_Entry_Potential, na.rm = TRUE),
              Hawaiian_Entry_Potential.City = max(Hawaiian_Entry_Potential, na.rm = TRUE),
              JetBlue_Entry_Potential.City = max(JetBlue_Entry_Potential, na.rm = TRUE),
              Frontier_Entry_Potential.City = max(Frontier_Entry_Potential, na.rm = TRUE),
              Allegiant_Entry_Potential.City = max(Allegiant_Entry_Potential, na.rm = TRUE),
              Spirit_Entry_Potential.City = max(Spirit_Entry_Potential, na.rm = TRUE),
              United_Entry_Potential.City = max(United_Entry_Potential, na.rm = TRUE))
  
  gc(); gc();
  
  
  total_market_passengers <- market_agregated_data2 %>% 
    group_by(Origin.City, Destination.City, Year, Quarter) %>%
    summarize(City_Market_Passengers = sum(Passengers.City, na.rm = TRUE)); gc(); gc()
  
  remove(market_disagregated_data); gc(); gc();
  
  city_data <- merge(market_agregated_data2, total_market_passengers,
                     by = c("Origin.City", "Destination.City", "Year", "Quarter"))
  
  city_data <- city_data %>% mutate(City.Share = Passengers.City/City_Market_Passengers)
  
  saveRDS(city_data, output)
}


generate_airport_shares <- function(input = "02.Intermediate/Compile_DB1B.Rds",
                                    output = "02.Intermediate/MarketSharesAirport.Rds",
                                    mode = "NA"){
  if(mode == "NA"){
    market_disagregated_data <- readRDS(input)
  } else if(mode == "LCC"){
    market_disagregated_data <- readRDS(input) %>%
      filter(RPCarrier %in% c("Southwest Airlines Co.", "Alaska Airlines Inc.",
                            "Spirit Air Lines", "Hawaiian Airlines Inc.",
                            "Frontier Airlines Inc.", "Allegiant Air",
                            "JetBlue Airways"))
  }

  market_agregated_data_port <- market_disagregated_data %>%
    mutate(Destination = Dest) %>%
    group_by(Origin, Destination, RPCarrier, Year, Quarter) %>%
    summarize(Passengers.Port = sum(Passengers, na.rm = TRUE),
              MktFare.Port = mean(MktFare, na.rm = TRUE),
              MktMilesFlown.Port = mean(MktMilesFlown, na.rm = TRUE),
              Oil_Price.Port = mean(Oil_Price, na.rm = TRUE),
              Delta_Prescence.Port = max(Delta_Prescence, na.rm = TRUE),
              Alaska_Prescence.Port = max(Alaska_Prescence, na.rm = TRUE),
              American_Prescence.Port = max(American_Prescence, na.rm = TRUE),
              Hawaiian_Prescence.Port = max(Hawaiian_Prescence, na.rm = TRUE),
              JetBlue_Prescence.Port = max(JetBlue_Prescence, na.rm = TRUE),
              Frontier_Prescence.Port = max(Frontier_Prescence, na.rm = TRUE),
              Allegiant_Prescence.Port = max(Allegiant_Prescence, na.rm = TRUE),
              Spirit_Prescence.Port = max(Spirit_Prescence, na.rm = TRUE),
              United_Prescence.Port = max(United_Prescence, na.rm = TRUE),
              Delta_Entry_Potential.Port = max(Delta_Entry_Potential, na.rm = TRUE),
              Alaska_Entry_Potential.Port = max(Alaska_Entry_Potential, na.rm = TRUE),
              American_Entry_Potential.Port = max(American_Entry_Potential, na.rm = TRUE),
              Hawaiian_Entry_Potential.Port = max(Hawaiian_Entry_Potential, na.rm = TRUE),
              JetBlue_Entry_Potential.Port = max(JetBlue_Entry_Potential, na.rm = TRUE),
              Frontier_Entry_Potential.Port = max(Frontier_Entry_Potential, na.rm = TRUE),
              Allegiant_Entry_Potential.Port = max(Allegiant_Entry_Potential, na.rm = TRUE),
              Spirit_Entry_Potential.Port = max(Spirit_Entry_Potential, na.rm = TRUE),
              United_Entry_Potential.Port = max(United_Entry_Potential, na.rm = TRUE))
  
  gc(); gc();
  
  # Calculate out Shares of Passengers
  airport_market_passengers <- market_disagregated_data %>% 
    mutate(Destination = Dest) %>%
    group_by(Origin, Destination, Year, Quarter) %>%
    summarize(Passengers.Total.Airport = sum(Passengers))
  
  market_agregated_data_port <- merge(market_agregated_data_port, airport_market_passengers, 
                                 by = c("Origin", "Destination", "Year", "Quarter"))
  
  market_agregated_data_port <- market_agregated_data_port %>% mutate(Airport.Share = Passengers.Port / Passengers.Total.Airport)
  
  saveRDS(market_agregated_data_port, output)
}



city_hhi <- function(input = "02.Intermediate/MarketSharesCity.Rds", 
                     output = "02.Intermediate/HHI_City_Market.Rds"){
  working_data <- readRDS(input)
  
  working_data$City.Share <- working_data$City.Share * 100
  current_HHI <- working_data %>% group_by(Origin.City, Destination.City,
                                           Quarter, Year) %>%
    summarize(HHI.City.Current = sum(City.Share^2, na.rm = TRUE))
  
  working_data <- merge(working_data, current_HHI);
  
  working_data <- as.data.table(working_data)
  
  working_data[,RPCarrier.New := RPCarrier];
  
  working_data[RPCarrier.New == "Spirit Air Lines", RPCarrier.New := "JetBlue Airways"]

  sim_HHI <- working_data %>% group_by(Origin.City, Destination.City, Quarter, Year, RPCarrier.New) %>%
    summarize(City.Share = sum(City.Share, na.rm = TRUE)) %>%
    group_by(Origin.City, Destination.City, Quarter, Year) %>%
    summarize(HHI.City.Simulation = sum(City.Share^2))
    
  working_data <- merge(working_data, sim_HHI)
  
  working_data$HHI.City.Increase = round(working_data$HHI.City.Simulation - working_data$HHI.City.Current, digits = 2)
  working_data$HHI.City.Percent.Increase = round(working_data$HHI.City.Increase / working_data$HHI.City.Current,
                                                 digits = 2)
  
  working_data <-  as.data.table(working_data)
  saveRDS(working_data, output)
}

airport_hhi <- function(input = "02.Intermediate/MarketSharesAirport.Rds", 
                       output = "02.Intermediate/HHI_Airport_Market.Rds"){
  working_data <- readRDS(input)
  
  working_data$Airport.Share <- working_data$Airport.Share * 100
  current_HHI <- working_data %>% group_by(Origin, Destination,
                                           Quarter, Year) %>%
    summarize(HHI.Port.Current = sum(Airport.Share^2))
  
  working_data <- merge(working_data, current_HHI);
  
  working_data <- as.data.table(working_data)
  
  working_data[,RPCarrier.New := RPCarrier];
  
  working_data[RPCarrier.New == "Spirit Air Lines", RPCarrier.New := "JetBlue Airways"]
  
  sim_HHI <- working_data %>% group_by(Origin, Destination, Quarter, Year, RPCarrier.New) %>%
    summarize(Airport.Share = sum(Airport.Share, na.rm = TRUE)) %>%
    group_by(Origin, Destination, Quarter, Year) %>%
    summarize(HHI.Port.Simulation = sum(Airport.Share^2, na.rm = TRUE))
  
  working_data <- merge(working_data, sim_HHI)
  
  working_data$HHI.Port.Increase = round(working_data$HHI.Port.Simulation - working_data$HHI.Port.Current, digits = 2)
  working_data$HHI.Port.Percent.Increase = round(working_data$HHI.Port.Increase / working_data$HHI.Port.Current,
                                                 digits = 2)
  
  working_data <-  as.data.table(working_data)
  saveRDS(working_data, output)
}