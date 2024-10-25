# Keep only routes that stay above 20 observations quarterly
populated_route_restrict <- function(data){
  fall_below <- data %>% group_by(Origin,
                                        Dest,
                                        Year, 
                                        Quarter) %>%
    summarize(N_Obs = n()); gc();
  fall_below <- fall_below %>% 
    mutate(Route = paste(Origin, Dest)) %>%
    select(-Origin, -Dest) %>%
    group_by(Route) %>%
    summarize(Min_Obs = min(N_Obs, na.rm = TRUE)) %>%
    filter(Min_Obs< 20); gc();
  
  data$Route <- paste(data$Origin, data$Dest)
  
  data <- data %>% filter(!Route %in% fall_below$Route)
  
  data$Route <- NULL
  
  return(data)
}

# Determine Which Markets had Entry by JB or 
# Spirit in time period of interest
entry_summary <- function(){
  analysis_data <- readRDS("02.Intermediate/Compile_DB1B.Rds")
  
  # Keep only relevant firms, direct tickets
  analysis_data <- analysis_data %>%
    filter(RPCarrier %in% c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc.",
                            "JetBlue Airways", "Alaska Airlines Inc.", "Southwest Airlines Co.", 
                            "Delta Air Lines Inc.", "American Airlines Inc.",
                            "United Air Lines Inc."),
           MktMilesFlown == NonStopMiles)
  
  
  # Restrict to only populated routes
  analysis_data <- populated_route_restrict(analysis_data %>%
                                  select(Origin, Dest,
                                         Year, Quarter,
                                         RPCarrier)); gc()
  
  
  markets <- analysis_data %>% group_by(Origin,
                                     Dest,
                                     Year, 
                                     Quarter,
                                     RPCarrier) %>%
    summarize(N_Obs = n()) %>% 
    mutate(Route = paste(Origin, Dest)) 
  
  route_list <- unique(markets$Route)
  Carrier_List <- unique(markets$RPCarrier)
  
  base.cross <- expand.grid(route_list, Carrier_List)
  colnames(base.cross) <- c("Route", "RPCarrier")
  
  new_data <- c()
  for(i in 2016:2022){
    for(j in 1:4){
      base.cross$Year <- i;
      base.cross$Quarter <- j;
      
      if(i != 2016){
        new_data <- rbind(new_data, base.cross)
      } else if (j != 1){
        new_data <- rbind(new_data, base.cross)
      } else {
        new_data <- base.cross
      }
    }
  }
  
  markets.reduce <- markets %>% group_by() %>%
    select(Route, Origin, Dest) %>% unique()
  
  new_data <- merge(new_data, markets.reduce,
                    by = "Route") %>% unique()
  
  gc(); 
  
  new_data <- unique(new_data); gc();
  
  new_data <- merge(new_data, markets,
                    by = c("Route", "RPCarrier", 
                           "Year", "Quarter",
                           "Origin",
                           "Dest"),
                    all.x = TRUE)
  
  new_data$Operated <- NA
  new_data <- as.data.table(new_data)
  new_data[is.na(N_Obs), Operated := FALSE]
  new_data[!is.na(N_Obs), Operated := TRUE]
  
  new_data$Period <- 4 * (new_data$Year - 2016) + new_data$Quarter
  
new_data[, c("Prev_Period", "Two_Back", "Three_Back", 
               "Four_Back") := shift(Operated, n = 1:4, fill = NA), by = c("RPCarrier",
                                                                          "Route")]
  new_data[, c("Next_Period", "Two_Forward", "Three_Forward", "Four_Forward") :=
             shift(Operated, n = 1:4, type = "lead",
                   fill = NA), by = c("RPCarrier",
                                              "Route")]
  
  new_data[, Entry := Prev_Period == FALSE & !is.na(N_Obs)]
  new_data[, Exit := Prev_Period == TRUE & is.na(N_Obs)]
  new_data[, year_valid_obs.Entry := Entry & Next_Period & Two_Forward & Three_Forward &
             !Prev_Period & !Two_Back & !Three_Back]
  new_data[, year_valid_obs.Exit := Exit & !Next_Period & !Two_Forward & !Three_Forward &
             Prev_Period & Two_Back & Three_Back]

  # Now, Reshape to Be Wide
  new_data.summarized <- new_data %>% group_by(RPCarrier,
                                               Year) %>%
    summarize(N_Entries = sum(Entry, na.rm = TRUE))
  
  new_data.wide <- new_data.summarized %>% 
    dcast(formula = RPCarrier ~ Year, 
                         value.var = "N_Entries")
  new_data.wide <- new_data.wide[c(2,3,5,1,4,7,8,6,9),]

  # Make Table
  kable(x = new_data.wide,
        col.names = c("Firm", 2016, 2017, 2018, 2019, 2020, 2021, 2022),
        format = "latex", booktabs = T,
        row.names = FALSE) %>%
    save_kable("06.Tables/Entry_Table.tex")
  
  # Number of Exits
  new_data.summarized <- new_data %>% group_by(RPCarrier,
                                               Year) %>%
    summarize(N_Exits = sum(Exit, na.rm = TRUE))
  new_data.wide <- new_data.summarized %>% 
    dcast(formula = RPCarrier ~ Year, 
          value.var = "N_Exits")
  new_data.wide <- new_data.wide[c(2,3,5,1,4,7,8,6,9),]

  # Make Table
  kable(x = new_data.wide,
        col.names = c("Firm", 2016, 2017, 2018, 2019, 2020, 2021, 2022),
        format = "latex", booktabs = T,
        row.names = FALSE) %>%
    save_kable("06.Tables/Exit_Table.tex")
  
  # Number of Long Entries
  new_data.summarized <- new_data %>% group_by(RPCarrier,
                                               Year) %>%
    summarize(N_Long_Entrance = sum(year_valid_obs.Entry, na.rm = TRUE))
  new_data.wide <- new_data.summarized %>% 
    dcast(formula = RPCarrier ~ Year, 
          value.var = "N_Long_Entrance")
  new_data.wide <- new_data.wide[c(2,3,5,1,4,7,8,6,9),]

 # Make Table
  kable(x = new_data.wide,
        col.names = c("Firm", 2016, 2017, 2018, 2019, 2020, 2021, 2022),
        format = "latex", booktabs = T,
        row.names = FALSE) %>%
    save_kable("06.Tables/Long_Entrance_Table.tex")
  
saveRDS(new_data, "02.Intermediate/Direct_Route_Entry.rds")
}

# Entry - Construct Data
construct_entry_data <- function(exclude = NA,
                                 filename = "02.Intermediate/Entry_Analysis_Data.rds"){
  routes_history <- readRDS("02.Intermediate/Direct_Route_Entry.rds")
  analysis_data <- readRDS("02.Intermediate/Compile_DB1B.Rds")
  
  analysis_data <- as.data.table(analysis_data)
  
  if(!is.na(exclude)){
    analysis_data <- analysis_data %>%filter(! RPCarrier %in% exclude)
  }
  
  analysis_data <- analysis_data[!is.na(MktFare)]
  
  analysis_data[, Avg.Fare := sum(MktFare * Passengers, na.rm= TRUE) / sum(Passengers, na.rm = TRUE),
                by = c("Year", "Quarter", "Origin", "Origin.City", "Dest", "Destination.City")]

  analysis_data[, Period :=  4 * (Year - 2016) + Quarter]
  
  analysis_data[, Observed.Passengers := sum(Passengers, na.rm = TRUE), 
                by = c("Year", "Quarter", "Origin", "Origin.City", "Dest", "Destination.City")]
  
  cols_to_remove <- c("ItinID", "MktCoupons", "TkCarrier", "TkCarrierGroup",
                      "RPCarrier", "OpCarrier", "OpCarrierGroup", 
                      "MktFare", "OPCarrierGroup")
  
  analysis_data[, (cols_to_remove) := NULL]
  
  analysis_data <- unique(analysis_data)
  
    gc(); gc();

    spirit_entry <- routes_history %>% 
      filter(Entry == TRUE,
        RPCarrier %in% c("Spirit Air Lines"),
        year_valid_obs.Entry == TRUE) %>%
      mutate(Entering_Carrier = RPCarrier); gc(); gc();
    
    spirit_entry <- spirit_entry %>% select(Route, Period, Entering_Carrier) %>%
      mutate(Relative_Period =  0,
             Event = 1:nrow(spirit_entry))
    
    spirit_entry_envelope <- c()
    for(i in 1:6){
      if(i %in% 1:3){
        new_per <- spirit_entry %>% mutate(Period = Period - i,
                                             Relative_Period = -i)
        
      } else{
        new_per <- spirit_entry %>% mutate(Period = Period + i - 3,
                                             Relative_Period = i - 3)
      }
      
      if(i == 1){
        spirit_entry_envelope <- new_per;
      } else{
        spirit_entry_envelope <- rbind(spirit_entry_envelope, new_per)
      }
    }
    
    
    spirit_cases <- rbind(spirit_entry,
                            spirit_entry_envelope)
    
    jb_entry <- routes_history %>% 
      filter(Entry == TRUE,
      RPCarrier %in% c("JetBlue Airways"), year_valid_obs.Entry == TRUE) %>%
      mutate(Entering_Carrier = RPCarrier) %>% 
      select(Route, Period, Entering_Carrier);
    
    jb_entry <- jb_entry %>%
      mutate(Relative_Period =  0,
             Event = 1:nrow(jb_entry))
    
    jb_entry_envelope <- c()
    for(i in 1:6){
      if(i %in% 1:3){
        new_per <- jb_entry %>% mutate(Period = Period - i,
                                           Relative_Period = -i)
        
      } else{
        new_per <- jb_entry %>% mutate(Period = Period + i - 3,
                                           Relative_Period = i - 3)
      }
      
      if(i == 1){
        jb_entry_envelope <- new_per;
      } else{
        jb_entry_envelope <- rbind(jb_entry_envelope, new_per)
      }
    }
    
    jb_cases <- rbind(jb_entry, jb_entry_envelope)
    
    
    spirit_cases <- spirit_cases %>% mutate(Match_Val = paste(Route, Period),
                                            Entering_Carrier = NULL)
    
    jb_cases <- jb_cases %>% mutate(Match_Val = paste(Route, Period),
                                    Entering_Carrier = NULL)
    
    colnames(spirit_cases) <- c("Route", "Period", "Spirit.Relative.Period", 
                                "Spirit.Entry.Event", "Match_Val")
    
    colnames(jb_cases) <-  c("Route", "Period", "JB.Relative.Period", 
                             "JB.Entry.Event", "Match_Val")
    
    analysis_data[, Route := paste(Origin, Dest)];
    analysis_data[, Match_Val := paste(Route, Period)]

    
    analysis_data <- merge(analysis_data, spirit_cases, all.x = TRUE)
    analysis_data <- merge(analysis_data, jb_cases, all.x = TRUE)
    saveRDS(analysis_data, "02.Intermediate/Entry_Analysis_Data.rds")
    gc(); gc();
}

entry_regress <- function(){
  analysis_data <- readRDS("02.Intermediate/Entry_Analysis_Data.rds")
  
  #Set up for Regression
  analysis_data <- analysis_data %>%
    mutate(Quarter = factor(Quarter),
           Year = factor(Year),
           Period = factor(Period),
           ln_Avg_Fare = log(Avg.Fare),
           Route = factor(Route))
  
  dependent_variable <- "ln_Avg_Fare";
  Prescence_Variables <- paste("Alaska_Prescence", "Allegiant_Prescence","American_Prescence",
                           "Delta_Prescence" ,"Frontier_Prescence","Hawaiian_Prescence",
                           "JetBlue_Prescence","Spirit_Prescence","United_Prescence",
                           sep = "+")
  Entry_Potential_Variables <- paste("Delta_Entry_Potential", "Alaska_Entry_Potential",
           "American_Entry_Potential","Hawaiian_Entry_Potential",
           "JetBlue_Entry_Potential","Frontier_Entry_Potential","Allegiant_Entry_Potential",
           "Spirit_Entry_Potential","United_Entry_Potential", sep = "+")
  
  # Standard_Controls <- paste("Period", "Route", "Oil_Price", sep = "+")
  Standard_Controls <- paste("Period", "Oil_Price", sep = "+")
  
  model1 <- as.formula(paste(dependent_variable, "~", Prescence_Variables, "+", Standard_Controls))
  
  estimation1 <- lm(formula = model1, data = analysis_data)
  summary(estimation1)
}
