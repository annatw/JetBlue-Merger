five_statistic_row_make <- function(name, vector){
  new_row <- c(name, round(mean(vector, na.rm = TRUE), digits = 2),
               paste("(", round(sd(vector, na.rm = TRUE), digits = 2), ")", sep = ),
               round(min(vector, na.rm = TRUE), digits = 2),
               round(median(vector,na.rm = TRUE), digits = 2),
               round(max(vector, na.rm = TRUE), digits = 2))
  return(new_row)
}

regions_of_note <- function(output = "06.Tables/regionsOfNote.tex"){
  db1b_data <- readRDS("02.Intermediate/Compile_DB1B.Rds")
  t100_data <- readRDS("02.Intermediate/Compile_T100_Q.Rds")
  
  db1b_data <- db1b_data %>% select(Origin, OriginState, Dest, DestState) %>% unique()
  
  t100_primary <- merge(db1b_data, t100_data, by.x = c("Origin", "Dest"),
                                  by.y = c("Origin_Alpha", "Destination_Alpha"),
                                  all.y = TRUE) %>% 
    filter(!is.na(OriginState)); gc()
  
  
  # Remove order of Origin, Destination States
  # Additionally, keep only relevant variables
  t100_primary <- t100_primary %>% mutate(OriginState = as.character(OriginState),
                                    DestState = as.character(DestState)) %>%
    select(OriginState, DestState, Carrier_Entity, Passengers_Enplaned,
           Year, Quarter); gc()
  
  t100_primary$firstState = pmin(t100_primary$OriginState, t100_primary$DestState); gc()
  t100_primary$secondState = pmax(t100_primary$OriginState, t100_primary$DestState); gc(); 
  
  # Generate firm level data for Spirit, JetBlue
  pass_data <- t100_primary %>% 
    filter(Carrier_Entity %in% c("Spirit Air Lines", 
                            "JetBlue Airways"),
           Year == 2022,
           Quarter == 2) %>%
    group_by(firstState, secondState, Carrier_Entity) %>%
    summarize(Firm.Passengers = sum(Passengers_Enplaned, na.rm = TRUE)) %>%
    filter(!is.na(Firm.Passengers)); gc() 
  
  pass_data <- dcast(pass_data, formula = firstState + secondState ~ Carrier_Entity,
                       value.var = "Firm.Passengers")
  
  # Generate total market data
  total_pass <- t100_primary %>% group_by(firstState, secondState) %>%
    filter(Year == 2022,
           Quarter == 2) %>%
    summarize(Market.Pass = sum(Passengers_Enplaned, na.rm = TRUE)); gc()
  
    pass_data <- as.data.table(merge(pass_data, total_pass,
                       all.x = TRUE, all.y = FALSE))
  
    # A few, seldom travelled routes will not be picked up by the DB1B data.
    pass_data <- pass_data[Market.Pass != 0,]
    
    # Generate Shares
    colnames(pass_data) <- c("firstState", "secondState", "JetBlue.Pass", "Spirit.Pass",
                             "Market.Pass")
    pass_data[is.na(JetBlue.Pass), JetBlue.Pass := 0];
    pass_data[is.na(Spirit.Pass), Spirit.Pass := 0];
    pass_data[, JetBlue.Share := round(JetBlue.Pass / Market.Pass * 100, digits = 2)]
    pass_data[, Spirit.Share := round(Spirit.Pass / Market.Pass * 100, digits = 2)]
    pass_data[, Combined_Share := JetBlue.Share + Spirit.Share]
    pass_data$Share_Increase <- pass_data$Combined_Share - pmax(pass_data$JetBlue.Share, pass_data$Spirit.Share)

    
    setorder(pass_data, cols = -"Share_Increase")
    
    pass_data.table <- pass_data[1:10,]
    
    pass_data.table <- pass_data.table[, c(1, 2, 3, 6, 4, 7, 8, 9)]
    
    pass_data.table$firstState <- factor(pass_data.table$firstState,
                                         levels = c("MA", "FL", "CT",
                                                    "NJ", "NY",  
                                                    "LA"),
                                         labels = c("Massachussets",
                                                    "Florida",
                                                    "Connecticut",
                                                    "New Jersey",
                                                    "New York",
                                                    "Louisiana"))
    
    pass_data.table$secondState <- factor(pass_data.table$secondState,
                                         levels = c("MA", "FL", "CT",
                                                    "NJ", "NY",  
                                                    "LA", "SC",
                                                    "PR", "NV"),
                                         labels = c("Massachussets",
                                                    "Florida",
                                                    "Connecticut",
                                                    "New Jersey",
                                                    "New York",
                                                    "Louisiana",
                                                    "South Carolina",
                                                    "Puerto Rico",
                                                    "Nevada"))
    
    # Make Table
    kable(x = pass_data.table,
          col.names = c("    ", "    ", "Passengers", "Share", "Passengers",
                        "Share", "Share", "Increase"),
          format = "latex", booktabs = T) %>%
      add_header_above(c(" ", " ", "JetBlue" = 2, "Spirit" = 2, "Joint" = 2)) %>%
    #  kable_classic() %>%
      save_kable(output)
}

# JetBlue, Spirit Overlap: Analyze Key Cities from judgment
spirit_jetblue_overlap_cities <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                                          output = "06.Tables/JetBlue_Spirit_Overlap_Cities.tex"){
  db1b <- readRDS(input)
  db1b <- db1b[!is.na(Origin_MSA),]
  db1b[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Origin", "Dest",
                                                             "Year", "Quarter")]
  db1b[, Spirit_Prescence := max(Spirit_Prescence), by = c("Origin", "Dest",
                                                             "Year", "Quarter")]
  db1b <- db1b[JetBlue_Prescence == 1,]
  db1b <- db1b[Spirit_Prescence == 1,]
  
  # For each year, origin market, sum the passengers
  db1b.s <- db1b %>% group_by(Year, Origin_MSA, Carrier) %>%
    summarize(Passengers = sum(Passengers.Product)) %>%
    as.data.table()
  
  db1b.s[, Passengers.Origin := sum(Passengers), by = c("Year", "Origin_MSA")]
  db1b.s <- db1b.s[Carrier %in% c("JetBlue Airways", "Spirit Air Lines")]
  db1b.s[, JB_SP_Pass := sum(Passengers), by = c("Year", "Origin_MSA")]
  db1b.s[, JB_SP_Pass_Share := JB_SP_Pass / Passengers.Origin]
  db1b.s <- db1b.s[, .(Year, Origin_MSA, JB_SP_Pass, JB_SP_Pass_Share)]
  db1b.s <- unique(db1b.s)
  
  # Want a Table with Two Panels
  db1b.sw <- reshape(db1b.s[, .(Year, Origin_MSA, JB_SP_Pass)], idvar = "Origin_MSA",
                     timevar = "Year", direction = "wide")
  db1b.sw[, Avg.Pass_Pre := (JB_SP_Pass.2017 + JB_SP_Pass.2018 + JB_SP_Pass.2019) / 3]
  db1b.sw[, Avg.Pass_Post := (JB_SP_Pass.2021 + JB_SP_Pass.2022 + JB_SP_Pass.2023) / 3]
  db1b.sw[, Avg.Pass := (JB_SP_Pass.2017 + JB_SP_Pass.2018 + JB_SP_Pass.2019 +
                           JB_SP_Pass.2021 + JB_SP_Pass.2022 + JB_SP_Pass.2023) / 6]
  db1b.sw <- db1b.sw[order(-Avg.Pass),]
  db1b.sw <- db1b.sw[, .(Origin_MSA, Avg.Pass, Avg.Pass_Post, Avg.Pass_Pre)]
  db1b.sw <- db1b.sw[1:10,]
  
  kable(x = db1b.sw,
        col.names = c("Origin MSA", "Average Passengers", "Pre-Pandemic Average", "Post-Pandemic Average"),
        format = "latex", 
        booktabs = T,
        row.names = FALSE,
        linesep = "") %>%
    save_kable(output)
  
}

hhi_airport_summary <- function(input = "02.Intermediate/HHI_Airport_Market.Rds",
                        output = "06.Tables/HHI_SummaryStats.tex",
                        mode = "NA"){
  
  hhi_data.22 <- read_rds(input) %>% 
    filter(Year == 2022, Quarter %in% c(1,2, 3, 4))
  hhi_data.21 <- read_rds(input) %>%
    filter(Year == 2021, Quarter %in% c(1, 2, 3,4))
  
  
  data <- as.data.table(rbind(hhi_data.21, hhi_data.22))

  data$Route <- paste(data$Origin, data$Dest)
  
  data$Route_Type <- "Neither"
  
  new_data <- c()
  
  get_data <- function(i, feed_in){
    if(i > 4){
      out <- data %>%
        filter(Year == 2022,
               Quarter == i - 4)
    } else if(i < 5){
      out <- data %>%
        filter(Year == 2021,
               Quarter == i)
    }
    return(out)
  }
  
  for(i in 1:8){
    temp_data <- get_data(i, feed_in = data)
    
    jetBlue_Routes <- temp_data[RPCarrier == "JetBlue Airways",]$Route
    spirit_Routes <- temp_data[RPCarrier == "Spirit Air Lines",]$Route
    both_routes <- spirit_Routes[spirit_Routes %in% jetBlue_Routes]
    temp_data[RPCarrier == "JetBlue Airways", Route_Type := "JetBlue (No Spirit)"];
    temp_data[RPCarrier == "Spirit Air Lines", Route_Type := "Spirit (No JetBlue)"];
    temp_data[Route %in% both_routes, Route_Type := "Both Firms"]
    
    
    if(i == 1){
      new_data <- temp_data 
    } else{
      new_data <- rbind(new_data, temp_data)
    }
  }
  
  rivalry_stats <- new_data %>% select(Year, Quarter, Route_Type, Route) %>% 
    unique() %>%
    group_by(Year, Quarter, Route_Type) %>%
    summarize(N = n()) %>% dcast(formula = Route_Type ~ Year + Quarter, 
                                 value.var = "N")
  
  # Now, figure out number of routes with JB+Spirit Only and how many with other LCC, Legacy
  new_data <- c()
  for(i in 1:8){
    temp_data <- get_data(i, feed_in = data)
    
    jetBlue_Routes <- temp_data[RPCarrier == "JetBlue Airways",]$Route
    spirit_Routes <- temp_data[RPCarrier == "Spirit Air Lines",]$Route
    both_routes <- spirit_Routes[spirit_Routes %in% jetBlue_Routes]
    
    # Keep only overlap routes
    temp_data <- temp_data[Route %in% both_routes,]
    
    count_data <- temp_data %>% select(Route, RPCarrier) %>%
      unique() %>% group_by(Route) %>% summarize(NumFirms = n())
    
    temp_data <- merge(temp_data, count_data, by = "Route")
    
    temp_data[NumFirms > 2, Route_Type := "Both Firms and Others"]
    temp_data[NumFirms == 2, Route_Type := "Both Firms Only"]
    
    temp_data$NumFirms <- NULL
    if(i == 1){
      new_data <- temp_data 
    } else{
      new_data <- rbind(new_data, temp_data)
    }
  }
  
  rivalry_stats.1 <- new_data %>% select(Year, Quarter, Route_Type, Route) %>% 
    unique() %>%
    group_by(Year, Quarter, Route_Type) %>%
    summarize(N = n()) %>% dcast(formula = Route_Type ~ Year + Quarter, 
                                 value.var = "N")
  
  if(mode != "LCC"){
  # Now, figure out number of routes with  Legacy
  new_data <- c()
  for(i in 1:8){
    temp_data <- get_data(i, feed_in = data)
    
    
    jetBlue_Routes <- temp_data[RPCarrier == "JetBlue Airways",]$Route
    spirit_Routes <- temp_data[RPCarrier == "Spirit Air Lines",]$Route
    both_routes <- spirit_Routes[spirit_Routes %in% jetBlue_Routes]
    
    # Keep only overlap routes
    temp_data <- temp_data[Route %in% both_routes,]
    
    count_data <- temp_data %>% select(Route, RPCarrier) %>%
      unique() %>% group_by(Route) %>% summarize(NumFirms = n())
    
    temp_data <- merge(temp_data, count_data, by = "Route")
    
    temp_data[NumFirms > 2, Route_Type := "Both Firms and Others"]
    temp_data[NumFirms == 2, Route_Type := "Both Firms Only"]
    
    temp_data <- temp_data %>% filter(Route_Type == "Both Firms and Others")
    
      count_data <- temp_data %>% 
        filter(RPCarrier %in% c("JetBlue Airways", "Spirit Air Lines",
                                "American Airlines Inc.", "United Air Lines Inc.",
                                "Delta Air Lines Inc.")) %>%
        select(Route, RPCarrier) %>%
        unique() %>% group_by(Route) %>% summarize(NumFirms = n())
      
      temp_data$NumFirms <- NULL;
      temp_data <- merge(temp_data, count_data, by = "Route")
      temp_data[NumFirms > 2, Route_Type := "Both Firms and Legacy Carrier(s)"]
      
      temp_data <- temp_data %>% filter(Route_Type == "Both Firms and Legacy Carrier(s)")
    

    temp_data$NumFirms <- NULL
    if(i == 1){
      new_data <- temp_data 
    } else{
      new_data <- rbind(new_data, temp_data)
    }
  } 
  rivalry_stats.2 <- new_data %>% select(Year, Quarter, Route_Type, Route) %>% 
    unique() %>%
    group_by(Year, Quarter, Route_Type) %>%
    summarize(N = n()) %>% dcast(formula = Route_Type ~ Year + Quarter, 
                                 value.var = "N")
  
  rivalry_stats <- rbind(rivalry_stats, rivalry_stats.1, rivalry_stats.2)
  rivalry_stats <- rivalry_stats %>% filter(!Route_Type == "Neither")
  
  rivalry_stats <- rivalry_stats[c(2,3,1,5,4,6),]
  
  } else {
    rivalry_stats <- rbind(rivalry_stats, rivalry_stats.1)
    rivalry_stats <- rivalry_stats %>% filter(!Route_Type == "Neither")
    rivalry_stats <- rivalry_stats[c(2,3,1,5,4),]
    
  }
  
  
  
  # Now, figure out number of routes with various HHI increases
  data$Route_Type <- "No Increase"
  new_data <- c()
  for(i in 1:8){
    temp_data <- get_data(i, feed_in = data)
    
    temp_data[HHI.Port.Increase >= 1, Route_Type := "HHI Increase Between 1-99"]
    temp_data[HHI.Port.Increase >= 100, Route_Type := "HHI Increase Between 100-199"]
    temp_data[HHI.Port.Increase >= 200, Route_Type := "HHI Increase Between 200-999"]
    temp_data[HHI.Port.Increase >= 1000, Route_Type := "HHI Increase Between 1000-2499"]
    temp_data[HHI.Port.Increase >= 2500, Route_Type := "HHI Increase Between 2500-5000"]

    temp_data <- temp_data %>% filter(HHI.Port.Increase >= 1)
    
    if(i == 1){
      new_data <- temp_data 
    } else{
      new_data <- rbind(new_data, temp_data)
    }
  }
  
  rivalry_stats.4 <- new_data %>% select(Year, Quarter, Route_Type, Route) %>% 
    unique() %>%
    group_by(Year, Quarter, Route_Type) %>%
    summarize(N = n()) %>% dcast(formula = Route_Type ~ Year + Quarter, 
                                 value.var = "N")
  rivalry_stats.4 <- as.data.table(rivalry_stats.4)
  
  rivalry_stats.4 <- rivalry_stats.4[c(1, 2, 4, 3, 5), ]
  
  rivalry_stats <-  rbind(rivalry_stats, rivalry_stats.4)
  
  data$Route_Type <- "NA"
  
  # Now, add rows for number of markets in each category under 2010 merger guidelines
  for(i in 1:8){
    temp_data <- get_data(i, feed_in = data)
    temp_data[HHI.Port.Current > 2500, Route_Type:= "Currently Highly Concentrated"]
    
    temp_data[HHI.Port.Current <= 2500, Route_Type:= "Currently Moderately Concentrated"]
    
    temp_data[HHI.Port.Current < 1500, Route_Type:= "Currently Unconcentrated"]
    
    if(i == 1){
      new_data <- temp_data 
    } else{
      new_data <- rbind(new_data, temp_data)
    }
  }
  
  new_data$Route_Type <- factor(new_data$Route_Type, levels = c("Currently Highly Concentrated",
                                                                "Currently Moderately Concentrated",
                                                                "Currently Unconcentrated"))
  
  rivalry_stats.5 <- new_data %>% select(Year, Quarter, Route_Type, Route) %>% 
    unique() %>%
    group_by(Year, Quarter, Route_Type, .drop = FALSE) %>%
    summarize(N = n()) %>% dcast(formula = Route_Type ~ Year + Quarter, 
                                 value.var = "N")
  
  
  
  for(i in 1:8){
    temp_data <- get_data(i, feed_in = data)
    temp_data[HHI.Port.Simulation > 2500, Route_Type:= "Simulation Highly Concentrated"]
    
    temp_data[HHI.Port.Simulation <= 2500, Route_Type:= "Simulation Moderately Concentrated"]
    
    temp_data[HHI.Port.Simulation < 1500, Route_Type:= "Simulation Unconcentrated"]
    
    if(i == 1){
      new_data <- temp_data 
    } else{
      new_data <- rbind(new_data, temp_data)
    }
  }
  
  new_data$Route_Type <- factor(new_data$Route_Type, levels = c("Simulation Highly Concentrated",
                                                                "Simulation Moderately Concentrated",
                                                                "Simulation Unconcentrated"))
  
  rivalry_stats.6 <- new_data %>% select(Year, Quarter, Route_Type, Route) %>% 
    unique() %>%
    group_by(Year, Quarter, Route_Type, .drop = FALSE) %>%
    summarize(N = n()) %>% dcast(formula = Route_Type ~ Year + Quarter, 
                                 value.var = "N")
  
    rivalry_stats.56 <- rbind(rivalry_stats.5, rivalry_stats.6)
    
    rivalry_stats.56 <- rivalry_stats.56[c(3,2,1,6,5,4),]
    
    rivalry_stats <- rbind(rivalry_stats, rivalry_stats.56)
    
  # Make Table
  kable(x = rivalry_stats,
        col.names = c("Market Type", "Q1", "Q2", "Q3", "Q4", "Q1", "Q2", "Q3", "Q4"),
       format = "latex", 
        booktabs = T,
       row.names = FALSE,
       linesep = "") %>%
    add_header_above(c(" ", "2021" = 4, "2022" = 4)) %>%
    save_kable(output)
  
}

major_city_table <- function(input_data = "02.Intermediate/Construct_DB1B/DB1B_Condensed.rds",
                             output_file = "06.Tables/FocusCities.tex"){
 
   t100_data <- read_rds(input_data) %>%
    filter(Year == 2022) %>%
    group_by(Origin.City, Carrier) %>%
    summarize(Passenger_Enplanements = sum(Passengers.Product, na.rm = TRUE))
  
  jb_data <- t100_data %>% filter(Carrier == "JetBlue Airways")
  total_boardings.jb <- sum(jb_data$Passenger_Enplanements, na.rm = TRUE)
  jb_data$Percent_Boardings <- round(jb_data$Passenger_Enplanements / total_boardings.jb,
                                     digits = 3)
  setorder(x = jb_data, cols = -"Passenger_Enplanements")
  jb_data <- jb_data[1:20,] %>% select(Origin.City,Passenger_Enplanements, Percent_Boardings)
  
  sp_data <- t100_data %>% filter(Carrier == "Spirit Air Lines")
  total_boardings.sp <- sum(sp_data$Passenger_Enplanements, na.rm = TRUE)
  sp_data$Percent_Boardings <- round(sp_data$Passenger_Enplanements / total_boardings.sp,
                                     digits = 3)
  setorder(x = sp_data, cols = -"Passenger_Enplanements")
  sp_data <- sp_data[1:20,] %>% select(Origin.City, Passenger_Enplanements, Percent_Boardings)

  jb_data <- as.data.table(jb_data); sp_data <- as.data.table(sp_data)
  
  jb_data$Cumulative <- cumsum(jb_data$Percent_Boardings);
  sp_data$Cumulative <- cumsum(sp_data$Percent_Boardings);
  
  result_data <- cbind(jb_data, sp_data)
  
  # Make Table
  kable(x = result_data,
        col.names = c("City", "Passengers", "%", "Cum. %",
                      "City", "Passengers", "%", "Cum. %"),
        format = "latex", booktabs = T) %>%
    add_header_above(c("JetBlue" = 4, "Spirit" = 4)) %>%
    save_kable(output_file)
}

overlap_city_table <- function(input_data = "02.Intermediate/Construct_DB1B/DB1B_Condensed.rds",
                             output_file = "06.Tables/OverlapCities.tex"){
  
  t100_data <- read_rds(input_data) %>%
    filter(Year == 2022) %>%
    as.data.table()
  
  t100_data[, Spirit_Prescence := max(Carrier == "Spirit Air Lines"), by = "Origin.City"]
  t100_data[, JetBlue_Prescence := max(Carrier == "JetBlue Airways"), by = "Origin.City"]
  
  t100_data <- t100_data %>%
    filter(Spirit_Prescence == 1, JetBlue_Prescence == 1) %>%
    mutate(JBSP = Carrier %in% c("JetBlue Airways", "Spirit Air Lines")) %>%
    group_by(Origin.City, JBSP) %>%
    summarize(Passenger_Enplanements = sum(Passengers.Product, na.rm = TRUE)) %>%
    as.data.table()
  
  t100_data[, TotalPassengers := sum(Passenger_Enplanements), by = "Origin.City"]
  t100_data[, Share := round(Passenger_Enplanements / TotalPassengers,
                             digits = 3)]
  t100_data <- t100_data[JBSP == TRUE,]

  setorder(x = t100_data, cols = -"Share")
  t100_data <- t100_data[1:20,]
  
  t100_data <- t100_data[, .(Origin.City, Passenger_Enplanements, TotalPassengers, Share)]
  
  # Make Table
  kable(x = t100_data,
        col.names = c("City", "Firm Passengers", "Total Passengers", "Share"),
        format = "latex", booktabs = T) %>%
    save_kable(output_file)
}


summary_statistics_product_level <- function(input = "02.Intermediate/Product_Data.rds",
                                             output = "06.Tables/SummaryStatistics_Product.tex"){
  product_data <- readRDS(input)
  product_data$MktCoupons <- as.numeric(product_data$MktCoupons)
  
  product_data_jb <- product_data[Carrier == "JetBlue Airways",]
  product_data_sp <- product_data[Carrier == "Spirit Air Lines",]
  product_data_lg <- product_data[Carrier %in% c("United Air Lines Inc.",
                                                 "Delta Air Lines Inc.",
                                                 "American Airlines Inc."),]

  colnames <- c("Variable", "All Firms", "JetBlue", "Spirit", "Legacy")
  
  # Miles Rows
  miles <- c("Miles (Thousands)", "", "", "", "")
  miles.min <- c("Min", min(round(product_data$MktMilesFlown, digits = 2)), 
                 min(round(product_data_jb$MktMilesFlown, digits = 2)),
                 min(round(product_data_sp$MktMilesFlown, digits = 2)),
                 min(round(product_data_lg$MktMilesFlown, digits = 2)))
  miles.median <- c("Median", paste(round(median(product_data$MktMilesFlown), digits = 2)),
                    paste(round(median(product_data_jb$MktMilesFlown), digits = 2)),
                    paste(round(median(product_data_sp$MktMilesFlown), digits = 2)),
                    paste(round(median(product_data_lg$MktMilesFlown), digits = 2)))
  miles.mean <- c("Mean", paste(round(mean(product_data$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_jb$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_sp$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_lg$MktMilesFlown), digits = 2)))
  miles.sd <- c("(SD)", paste("(", round(sd(product_data$MktMilesFlown), digits = 2), ")", sep = ""), 
                paste("(", round(sd(product_data_jb$MktMilesFlown), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_sp$MktMilesFlown), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_lg$MktMilesFlown), digits = 2), ")", sep = ""))
  miles.max <- c("Max", 
                 max(round(product_data$MktMilesFlown, digits = 2)), 
                 max(round(product_data_jb$MktMilesFlown, digits = 2)),
                 max(round(product_data_sp$MktMilesFlown, digits = 2)), 
                 max(round(product_data_lg$MktMilesFlown, digits = 2)))
  
  # Prices
  price <- c("Prices (100s 2017 USD)", "", "", "")
  
  price.min <- c("Min", round(min(product_data$prices),2), round(min(product_data_jb$prices),2),
                 round(min(product_data_sp$prices),2), round(min(product_data_lg$prices),2))
  price.median <- c("Median", round(median(product_data$prices),2), round(median(product_data_jb$prices),2),
                 round(median(product_data_sp$prices),2), round(median(product_data_lg$prices),2))
  price.mean <- c("Mean", paste(round(mean(product_data$prices), digits = 2)),
                  paste(round(mean(product_data_jb$prices), digits = 2)),
                  paste(round(mean(product_data_sp$prices), digits = 2)),
                  paste(round(mean(product_data_lg$prices), digits = 2)))
  price.sd <- c("(SD)", paste(" (", round(sd(product_data$prices), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_jb$prices), digits = 2), ")", sep = ""),
                paste(" (", round(sd(product_data_sp$prices), digits = 2), ")", sep = ""),
                paste(" (", round(sd(product_data_lg$prices), digits = 2), ")", sep = ""))
  price.max <- c("Max", round(max(product_data$prices),2), round(max(product_data_jb$prices), 2),
                 round(max(product_data_sp$prices),2), round(max(product_data_lg$prices), 2))
  
  # Passengers
  passengers <- c("Passengers", "", "", "")
  passengers.min <- c("Min", min(product_data$Passengers.Product), min(product_data_jb$Passengers.Product),
                      min(product_data_sp$Passengers.Product), min(product_data_lg$Passengers.Product))
  passengers.median <- c("Median", median(product_data$Passengers.Product), median(product_data_jb$Passengers.Product),
                      median(product_data_sp$Passengers.Product), median(product_data_lg$Passengers.Product))
  passengers.mean <- c("Mean", paste(round(mean(product_data$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_jb$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_sp$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_lg$Passengers.Product), digits = 2)))
  passengers.sd <- c("(SD)", 
                     paste("(", round(sd(product_data$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_jb$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_sp$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_lg$Passengers.Product), digits = 2), ")", sep = ""))
  passengers.max <- c("Max", max(product_data$Passengers.Product), max(product_data_jb$Passengers.Product),
                      max(product_data_sp$Passengers.Product), max(product_data_lg$Passengers.Product))
  
  # Origin Share
  originShare.min <- c("Min", min(round(product_data$Origin_Firm_Service_Ratio,2)),
                            min(round(product_data_jb$Origin_Firm_Service_Ratio,2)),
                            min(round(product_data_sp$Origin_Firm_Service_Ratio,2)),
                            min(round(product_data_lg$Origin_Firm_Service_Ratio,2)))
  originShare.median <- c("Median", round(median(product_data$Origin_Firm_Service_Ratio), 2),
                        round(median(product_data_jb$Origin_Firm_Service_Ratio), 2),
                        round(median(product_data_sp$Origin_Firm_Service_Ratio), 2),
                        round(median(product_data_lg$Origin_Firm_Service_Ratio), 2))
  originShare.mean <- c("Mean", round(mean(product_data$Origin_Firm_Service_Ratio), 2),
                             round(mean(product_data_jb$Origin_Firm_Service_Ratio), 2),
                             round(mean(product_data_sp$Origin_Firm_Service_Ratio), 2),
                             round(mean(product_data_lg$Origin_Firm_Service_Ratio), 2))
  originShare.sd <- c("(SD)", paste("(", round(sd(product_data$Origin_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_jb$Origin_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_sp$Origin_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_lg$Origin_Firm_Service_Ratio),2), ")", sep = ""))
  originShare.max <- c("Max", max(round(product_data$Origin_Firm_Service_Ratio,2)),
                            max(round(product_data_jb$Origin_Firm_Service_Ratio,2)),
                            max(round(product_data_sp$Origin_Firm_Service_Ratio,2)),
                            max(round(product_data_lg$Origin_Firm_Service_Ratio,2)))
  
  # Destination Share
  destinationShare.min <- c("Min", min(round(product_data$Destination_Firm_Service_Ratio,2)),
                            min(round(product_data_jb$Destination_Firm_Service_Ratio,2)),
                            min(round(product_data_sp$Destination_Firm_Service_Ratio,2)),
                            min(round(product_data_lg$Destination_Firm_Service_Ratio,2)))
  destinationShare.median <- c("Median", round(median(product_data$Destination_Firm_Service_Ratio), 2),
                             round(median(product_data_jb$Destination_Firm_Service_Ratio), 2),
                             round(median(product_data_sp$Destination_Firm_Service_Ratio), 2),
                             round(median(product_data_lg$Destination_Firm_Service_Ratio), 2))
  destinationShare.mean <- c("Mean", round(mean(product_data$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_jb$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_sp$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_lg$Destination_Firm_Service_Ratio), 2))
  destinationShare.sd <- c("(SD)", paste("(", round(sd(product_data$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_jb$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_sp$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_lg$Destination_Firm_Service_Ratio),2), ")", sep = ""))
  destinationShare.max <- c("Max", max(round(product_data$Destination_Firm_Service_Ratio,2)),
                            max(round(product_data_jb$Destination_Firm_Service_Ratio,2)),
                            max(round(product_data_sp$Destination_Firm_Service_Ratio,2)),
                            max(round(product_data_lg$Destination_Firm_Service_Ratio,2)))
  
  # Extra Miles
  extraMiles.min <- c("Min", min(round(product_data$Extra_Miles,2)),
                            min(round(product_data_jb$Extra_Miles,2)),
                            min(round(product_data_sp$Extra_Miles,2)),
                            min(round(product_data_lg$Extra_Miles,2)))
  extraMiles.median <- c("Median", round(median(product_data$Extra_Miles), 2),
                       round(median(product_data_jb$Extra_Miles), 2),
                       round(median(product_data_sp$Extra_Miles), 2),
                       round(median(product_data_lg$Extra_Miles), 2))
  extraMiles.mean <- c("Mean", round(mean(product_data$Extra_Miles), 2),
                             round(mean(product_data_jb$Extra_Miles), 2),
                             round(mean(product_data_sp$Extra_Miles), 2),
                             round(mean(product_data_lg$Extra_Miles), 2))
  extraMiles.sd <- c("(SD)", paste("(", round(sd(product_data$Extra_Miles),2), ")", sep = ""),
                           paste("(", round(sd(product_data_jb$Extra_Miles),2), ")", sep = ""),
                           paste("(", round(sd(product_data_sp$Extra_Miles),2), ")", sep = ""),
                           paste("(", round(sd(product_data_lg$Extra_Miles),2), ")", sep = ""))
  extraMiles.max <- c("Max", max(round(product_data$Extra_Miles,2)),
                            max(round(product_data_jb$Extra_Miles,2)),
                            max(round(product_data_sp$Extra_Miles,2)),
                            max(round(product_data_lg$Extra_Miles,2)))
  
  
  # Gas Price * Miles
  gasMiles.min <- c("Min", round(min(product_data$GasMiles),2),
                    round(min(product_data_jb$GasMiles),2), round(min(product_data_sp$GasMiles),2),
                    round(min(product_data_lg$GasMiles),2))
  gasMiles.median <- c("Median", round(median(product_data$GasMiles),2),
                    round(median(product_data_jb$GasMiles),2), round(median(product_data_sp$GasMiles),2),
                    round(median(product_data_lg$GasMiles),2))
  gasMiles.mean <- c("Mean", round(mean(product_data$GasMiles),2),
                     round(mean(product_data_jb$GasMiles),2), round(mean(product_data_sp$GasMiles),2),
                     round(mean(product_data_lg$GasMiles),2))
  gasMiles.sd <- c("(SD)", paste("(",round(mean(product_data$GasMiles),2), ")", sep = ""),
                     paste("(", round(mean(product_data_jb$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_sp$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_lg$GasMiles),2), ")", sep = ""))
  gasMiles.max <- c("Max", round(min(product_data$GasMiles),2),
                    round(min(product_data_jb$GasMiles),2), round(min(product_data_sp$GasMiles),2),
                    round(min(product_data_lg$GasMiles),2))
  
  # NonStop
  NonStop.stat <- c("Share Nonstop", round(mean(product_data$NonStop),2),
               round(mean(product_data_jb$NonStop),2), round(mean(product_data_sp$NonStop),2),
               round(mean(product_data_lg$NonStop),2))
  # Tourism
  Tourism.stat <- c("Share Tourist Route", round(mean(product_data$Tourism),2),
                    round(mean(product_data_jb$Tourism),2), round(mean(product_data_sp$Tourism),2),
                    round(mean(product_data_lg$Tourism),2))

  # Observations
  obs <- c("Observations", nrow(product_data), nrow(product_data_jb), nrow(product_data_sp),
           nrow(product_data_lg))
  
  # Make Table
  summary_table <- rbind(price.min, price.median, price.mean, price.sd, price.max,
         passengers.min, passengers.median, passengers.mean, passengers.sd, passengers.max,
         miles.min, miles.median, miles.mean, miles.sd, miles.max,
         originShare.min, originShare.median, originShare.mean, originShare.sd, originShare.max,
         destinationShare.min, destinationShare.median, destinationShare.mean, destinationShare.sd, destinationShare.max,
         extraMiles.median, extraMiles.mean, extraMiles.sd, extraMiles.max,
         gasMiles.min, gasMiles.median, gasMiles.mean, gasMiles.sd, gasMiles.max, 
                         NonStop.stat, Tourism.stat, obs)
  colnames(summary_table) <- colnames
  rownames(summary_table) <- NULL
  
  # Generate Table with kableExtra
  kbl(summary_table,
      format = "latex", 
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Price (100s 2017 USD)", 1, 5) %>%
    pack_rows(group_label = "Number of Passengers", 6, 10) %>%
    pack_rows(group_label = "Miles Flown (Thousands)", 11, 15) %>%
    pack_rows(group_label = "Origin Route Share (Percent)", 16, 20) %>%
    pack_rows(group_label = "Destination Route Share (Percent)", 21, 25) %>%
    pack_rows(group_label = "Extra Miles", 26, 29) %>%
    pack_rows(group_label = "JetFuel * Market Miles", 30, 34) %>%
    pack_rows(group_label = "Binary Variables", 35,36) %>% 
    row_spec(row = 36, hline_after = TRUE) %>%
    save_kable(file = output)
}

summary_statistics_product_level_slim <- function(input = "02.Intermediate/Product_Data.rds",
                                                  output = "06.Tables/SummaryStatistics_Product_Slim.tex"){
  product_data <- readRDS(input)
  product_data$MktCoupons <- as.numeric(product_data$MktCoupons)
  
  product_data_jb <- product_data[Carrier == "JetBlue Airways",]
  product_data_sp <- product_data[Carrier == "Spirit Air Lines",]
  product_data_lg <- product_data[Carrier %in% c("United Air Lines Inc.",
                                                 "Delta Air Lines Inc.",
                                                 "American Airlines Inc."),]
  
  colnames <- c("Variable", "All Firms", "JetBlue", "Spirit", "Legacy")
  
  # Miles Rows
  miles.mean <- c("Miles", paste(round(mean(product_data$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_jb$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_sp$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_lg$MktMilesFlown), digits = 2)))
  miles.sd <- c("(SD)", paste("(", round(sd(product_data$MktMilesFlown), digits = 2), ")", sep = ""), 
                paste("(", round(sd(product_data_jb$MktMilesFlown), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_sp$MktMilesFlown), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_lg$MktMilesFlown), digits = 2), ")", sep = ""))

  # Prices
  price.mean <- c("Prices", paste(round(mean(product_data$prices), digits = 2)),
                  paste(round(mean(product_data_jb$prices), digits = 2)),
                  paste(round(mean(product_data_sp$prices), digits = 2)),
                  paste(round(mean(product_data_lg$prices), digits = 2)))
  price.sd <- c("(SD)", paste(" (", round(sd(product_data$prices), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_jb$prices), digits = 2), ")", sep = ""),
                paste(" (", round(sd(product_data_sp$prices), digits = 2), ")", sep = ""),
                paste(" (", round(sd(product_data_lg$prices), digits = 2), ")", sep = ""))
  # Passengers
  passengers.mean <- c("Passengers", paste(round(mean(product_data$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_jb$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_sp$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_lg$Passengers.Product), digits = 2)))
  passengers.sd <- c("(SD)", 
                     paste("(", round(sd(product_data$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_jb$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_sp$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_lg$Passengers.Product), digits = 2), ")", sep = ""))

  # Origin Share
  originShare.mean <- c("Origin Share", round(mean(product_data$Origin_Firm_Service_Ratio), 2),
                        round(mean(product_data_jb$Origin_Firm_Service_Ratio), 2),
                        round(mean(product_data_sp$Origin_Firm_Service_Ratio), 2),
                        round(mean(product_data_lg$Origin_Firm_Service_Ratio), 2))
  originShare.sd <- c("(SD)", paste("(", round(sd(product_data$Origin_Firm_Service_Ratio),2), ")", sep = ""),
                      paste("(", round(sd(product_data_jb$Origin_Firm_Service_Ratio),2), ")", sep = ""),
                      paste("(", round(sd(product_data_sp$Origin_Firm_Service_Ratio),2), ")", sep = ""),
                      paste("(", round(sd(product_data_lg$Origin_Firm_Service_Ratio),2), ")", sep = ""))

  # Destination Share
  destinationShare.mean <- c("Dest Share", round(mean(product_data$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_jb$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_sp$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_lg$Destination_Firm_Service_Ratio), 2))
  destinationShare.sd <- c("(SD)", paste("(", round(sd(product_data$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_jb$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_sp$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_lg$Destination_Firm_Service_Ratio),2), ")", sep = ""))

  # Extra Miles
  extraMiles.mean <- c("Extra Miles", round(mean(product_data$Extra_Miles), 2),
                       round(mean(product_data_jb$Extra_Miles), 2),
                       round(mean(product_data_sp$Extra_Miles), 2),
                       round(mean(product_data_lg$Extra_Miles), 2))
  extraMiles.sd <- c("(SD)", paste("(", round(sd(product_data$Extra_Miles),2), ")", sep = ""),
                     paste("(", round(sd(product_data_jb$Extra_Miles),2), ")", sep = ""),
                     paste("(", round(sd(product_data_sp$Extra_Miles),2), ")", sep = ""),
                     paste("(", round(sd(product_data_lg$Extra_Miles),2), ")", sep = ""))

  # Gas Price * Miles
  gasMiles.mean <- c("Mean", round(mean(product_data$GasMiles),2),
                     round(mean(product_data_jb$GasMiles),2), round(mean(product_data_sp$GasMiles),2),
                     round(mean(product_data_lg$GasMiles),2))
  gasMiles.sd <- c("(SD)", paste("(",round(mean(product_data$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_jb$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_sp$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_lg$GasMiles),2), ")", sep = ""))

  # NonStop
  NonStop.stat <- c("Share Nonstop", round(mean(product_data$NonStop),2),
                    round(mean(product_data_jb$NonStop),2), round(mean(product_data_sp$NonStop),2),
                    round(mean(product_data_lg$NonStop),2))
  # Tourism
  Tourism.stat <- c("Share Tourist Route", round(mean(product_data$Tourism),2),
                    round(mean(product_data_jb$Tourism),2), round(mean(product_data_sp$Tourism),2),
                    round(mean(product_data_lg$Tourism),2))
  
  # Observations
  obs <- c("Observations", nrow(product_data), nrow(product_data_jb), nrow(product_data_sp),
           nrow(product_data_lg))
  
  # Make Table
  summary_table <- rbind(price.mean, price.sd,
                       passengers.mean, passengers.sd,
                        miles.mean, miles.sd, originShare.mean, originShare.sd,
                        destinationShare.mean, destinationShare.sd, 
                          extraMiles.mean, extraMiles.sd,
                       gasMiles.mean, gasMiles.sd, 
                         NonStop.stat, Tourism.stat, obs)
  colnames(summary_table) <- colnames
  rownames(summary_table) <- NULL
  
  # Generate Table with kableExtra
  kbl(summary_table,
      format = "latex", 
      escape = TRUE, booktabs = TRUE, linesep = "") %>%
    row_spec(row = 16, hline_after = TRUE) %>%
    save_kable(file = output)
}

summary_statistics_product_two_period <- function(post_pandemic_in = "02.Intermediate/Product_Data.rds",
                                                  pre_pandemic_in = "02.Intermediate/prepandemic.rds",
                                                  output = "06.Tables/SummaryStatistics_Product_TwoPeriod.tex"){
 period_rows <- function(input){
  product_data <- readRDS(input)
  
  price_row <- five_statistic_row_make(name = "Price (2017 USD)", product_data$prices * 100)
  passengers_row <- five_statistic_row_make(name = "Passengers", product_data$Passengers.Product)
  mktMiles_row <- five_statistic_row_make(name = "Distance (1000s)", product_data$MktMilesFlown)
  exMiles_row <- five_statistic_row_make(name = "Extra Distance", product_data$Extra_Miles)
  nonstop_row <- five_statistic_row_make(name = "Nonstop", product_data$NonStop)
  origin_dest <- five_statistic_row_make(name = "Origin Destinations",
                                         product_data$Origin_Firm_Destinations)
  origin_prescence <- five_statistic_row_make(name = "Origin Prescence (\\%)",
                                              product_data$Origin_Firm_Service_Ratio)
  delta <- five_statistic_row_make(name = "Delta", product_data$Carrier == "Delta Air Lines Inc.")
  american <- five_statistic_row_make(name = "American", product_data$Carrier == "American Airlines Inc.")
  united <- five_statistic_row_make(name = "United", product_data$Carrier == "United Air Lines Inc.")
  southwest <- five_statistic_row_make(name = "Southwest", product_data$Carrier == "Southwest Airlines Co.")
  jetblue <- five_statistic_row_make(name = "JetBlue", product_data$Carrier == "JetBlue Airways")
  spirit <- five_statistic_row_make(name = "Spirit", product_data$Carrier == "Spirit Air Lines")
  minor_carrier <- five_statistic_row_make(name = "Other Carrier", product_data$Carrier == "Minor Carrier")
  obs_row <- c("Observations", nrow(product_data), "", "", "", "")
  
  return(rbind(price_row, passengers_row,
                 mktMiles_row, exMiles_row, nonstop_row,
                 origin_dest, origin_prescence, delta,
                 american, united, southwest, jetblue,
                 spirit, minor_carrier, obs_row))
 }
 
 title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
 post_pandemic <- period_rows(post_pandemic_in)
 pre_pandemic <- period_rows(pre_pandemic_in)
  
 output_table <- rbind(pre_pandemic, post_pandemic)
 rownames(output_table) <- NULL
 
 kbl(output_table,
     format = "latex", col.names = title_row,
     escape = TRUE, booktabs = TRUE) %>%
   pack_rows(group_label = "Pre-Pandemic", 1, 15) %>%
   pack_rows(group_label = "Post-Pandemic", 16, 30) %>%
   save_kable(file = output)
}

summary_statistics_market_level <- function(input = "02.Intermediate/Product_Data.rds",
                                            output = "06.Tables/SummaryStatistics_Market.tex"){
  data <- read_rds(input)
  
  # Within the analysis data, Spirit has Prescence = FALSE on its routes
  # Fix this for both firms.
  data[, Spirit_Prescence := max(Spirit_Prescence), by = market_ids]
  data[, JetBlue_Prescence := max(JetBlue_Prescence), by = market_ids]
  
  table_header <- c("Variable", "All Markets", "JetBlue", "Spirit", "JetBlue-Spirit")
  
  # First, Number of Firms in Market
  data.numFirms <- data %>% select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()

  data.numFirms.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>% 
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>% 
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.Compete <- data %>% filter(JetBlue_Prescence == TRUE, Spirit_Prescence == TRUE) %>%
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  
  numFirms <- c("Number of Firms", "", "", "", "")
  numFirms.min <- c("Min", min(data.numFirms$N), min(data.numFirms.JetBlue$N), min(data.numFirms.Spirit$N),
                    min(data.numFirms.Compete$N))
  numFirms.median <- c("Median", 
                       round(median(data.numFirms$N), 2),
                       paste(round(median(data.numFirms.JetBlue$N), 2)),
                       paste(round(median(data.numFirms.Spirit$N), 2)),
                       paste(round(median(data.numFirms.Compete$N), 2)))
  numFirms.mean <- c("Mean", 
                     round(mean(data.numFirms$N), 2),
                     paste(round(mean(data.numFirms.JetBlue$N), 2)),
                     paste(round(mean(data.numFirms.Spirit$N), 2)),
                     paste(round(mean(data.numFirms.Compete$N), 2)))
  numFirms.sd <- c("(SD)", paste("(", round(sd(data.numFirms$N), 2), ")", sep = ""), 
                   paste("(", round(sd(data.numFirms.JetBlue$N), 2), ")", sep = ""),
                   paste("(", round(sd(data.numFirms.Spirit$N), 2), ")", sep = ""),
                   paste("(", round(sd(data.numFirms.Compete$N), 2), ")", sep = ""))
  numFirms.max <- c("Max", max(data.numFirms$N), max(data.numFirms.JetBlue$N), max(data.numFirms.Spirit$N),
                    max(data.numFirms.Compete$N))                  

  # Now, Number of Passengers within Market
  data.Passengers <- data %>% select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.Compete <- data %>% filter(Spirit_Prescence == TRUE,
                                             JetBlue_Prescence == TRUE) %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  
  numPassengers <- c("Passengers in Market", "", "", "", "")
  numPassengers.min <- c("Min", min(data.Passengers$Passengers), min(data.Passengers.JetBlue$Passengers),
                         min(data.Passengers.Spirit$Passengers), min(data.Passengers.Compete$Passengers))
  numPassengers.median <- c("Median", median(data.Passengers$Passengers), 
                            median(data.Passengers.JetBlue$Passengers),
                            median(data.Passengers.Spirit$Passengers), 
                            median(data.Passengers.Compete$Passengers))
  numPassengers.mean <- c("Mean", paste(round(mean(data.Passengers$Passengers), 2)),
                     paste(round(mean(data.Passengers.JetBlue$Passengers), 2)),
                     paste(round(mean(data.Passengers.Spirit$Passengers), 2)),
                     paste(round(mean(data.Passengers.Compete$Passengers), 2)))
  numPassengers.sd <- c("(SD)", 
                        paste("(", round(sd(data.Passengers$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.JetBlue$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.Spirit$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.Compete$Passengers), 2), ")", sep = ""))
  numPassengers.max <- c("Max", max(data.Passengers$Passengers), max(data.Passengers.JetBlue$Passengers), 
                         max(data.Passengers.Spirit$Passengers),
                         max(data.Passengers.Compete$Passengers))                  
  
   
  # Num Products in Market
  data.products <- data %>% select(market_ids, Product_Name) %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>%
    select(market_ids, Product_Name) %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>%
    select(market_ids, Product_Name) %>% unique() %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.Compete <- data %>% filter(Spirit_Prescence == TRUE,
                                           JetBlue_Prescence == TRUE) %>%
    select(market_ids, Product_Name) %>% unique() %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  
  
  numProducts <- c("Products in Market", "", "", "", "")
  numProducts.min <- c("Min", min(data.products$Products), min(data.products.JetBlue$Products),
                       min(data.products.Spirit$Products), min(data.products.Compete$Products))
  numProducts.median <- c("Median", median(data.products$Products), median(data.products.JetBlue$Products),
                          median(data.products.Spirit$Products), median(data.products.Compete$Products))
  numProducts.mean <- c("Mean", paste(round(mean(data.products$Products), 2)),
                          paste(round(mean(data.products.JetBlue$Products), 2)),
                          paste(round(mean(data.products.Spirit$Products), 2)),
                        paste(round(mean(data.products.Compete$Products), 2)))
  numProducts.sd <- c("(SD)",
                      paste("(", round(sd(data.products$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.JetBlue$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.Spirit$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.Compete$Products), 2), ")", sep = ""))
  numProducts.max <- c("Max", max(data.products$Products), max(data.products.JetBlue$Products), max(data.products.Spirit$Products),
                       max(data.products.Compete$Products))                  
  
  # Market Concentration
  data.hhi <- data %>% select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  data.hhi.JB <- data %>%filter(JetBlue_Prescence == TRUE) %>% select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
    
  data.hhi.SP <- data %>%filter(Spirit_Prescence == TRUE) %>% select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
                                                                                           
  data.hhi.Compete <- data %>%filter(Spirit_Prescence == TRUE, JetBlue_Prescence == TRUE) %>% 
    select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
                                                                                   
  hhi <- c("HHI", "", "", "", "")
  hhi.min <- c("Min", round(min(data.hhi$HHI),2), round(min(data.hhi.JB$HHI),2), round(min(data.hhi.SP$HHI), 2),
               round(min(data.hhi.Compete$HHI), 2))
  hhi.median <- c("Median", round(median(data.hhi$HHI),2), round(median(data.hhi.JB$HHI),2), 
                  round(median(data.hhi.SP$HHI), 2), round(median(data.hhi.Compete$HHI), 2))
  hhi.mean <- c("Mean", paste(round(mean(data.hhi$HHI), 2)),
                        paste(round(mean(data.hhi.JB$HHI), 2)),
                        paste(round(mean(data.hhi.SP$HHI), 2)),
                paste(round(mean(data.hhi.Compete$HHI), 2)))
  hhi.sd <- c("(SD)", 
              paste("(", round(sd(data.hhi$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.JB$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.SP$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.Compete$HHI), 2), ")", sep = ""))
  hhi.max <- c("Max", round(max(data.hhi$HHI),2), round(max(data.hhi.JB$HHI),2), round(max(data.hhi.SP$HHI),2),
               round(max(data.hhi.Compete$HHI), 2))                  
  
  
  # Minimum Miles in Market
  data.minmiles <- data %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.JB <- data %>% filter(JetBlue_Prescence == TRUE) %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.SP <- data %>%filter(Spirit_Prescence == TRUE) %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.Compete <- data %>% filter(Spirit_Prescence == TRUE, JetBlue_Prescence == TRUE) %>% 
    select(market_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  minmiles.min <- c("Min", round(min(data.minmiles$market_min_miles), 2), round(min(data.minmiles.JB$market_min_miles), 2),
                    round(min(data.minmiles.SP$market_min_miles), 2), round(min(data.minmiles.Compete$market_min_miles), 2))
  minmiles.median <- c("Median",  round(median(data.minmiles$market_min_miles), 2), round(median(data.minmiles.JB$market_min_miles), 2),
                     round(median(data.minmiles.SP$market_min_miles), 2), round(median(data.minmiles.Compete$market_min_miles), 2))
  minmiles.mean <- c("Mean",  round(mean(data.minmiles$market_min_miles), 2), round(mean(data.minmiles.JB$market_min_miles), 2),
                     round(mean(data.minmiles.SP$market_min_miles), 2), round(mean(data.minmiles.Compete$market_min_miles), 2))
  minmiles.sd <- c("(SD)", paste("(", round(sd(data.minmiles$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.JB$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.SP$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.Compete$market_min_miles), 2), ")", sep = ""))
  minmiles.max <- c("Max", round(max(data.minmiles$market_min_miles), 2), round(max(data.minmiles.JB$market_min_miles), 2),
                    round(max(data.minmiles.SP$market_min_miles), 2), round(max(data.minmiles.Compete$market_min_miles), 2))
  
  # Record Years
  year.min <- c("Min", min(data$Year), min(data[JetBlue_Prescence == TRUE,]$Year), 
                min(data[Spirit_Prescence == TRUE,]$Year), min(data[Spirit_Prescence == TRUE & JetBlue_Prescence == TRUE,]$Year))
  year.max <- c("Max", max(data$Year), max(data[JetBlue_Prescence == TRUE,]$Year), max(data[Spirit_Prescence == TRUE,]$Year),
                max(data[JetBlue_Prescence == TRUE & Spirit_Prescence == TRUE,]$Year))
  
  # Now, Observations
  markets <- data %>% select(market_ids) %>% unique() 
  
  markets.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>%  select(market_ids) %>% unique() 
  
  markets.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>% select(market_ids) %>% unique()
  
  markets.Compete <- data %>% filter(Spirit_Prescence == TRUE, JetBlue_Prescence == TRUE) %>%
    select(market_ids) %>% unique()
  
  obs <- c("Number of Markets", length(unique(markets$market_ids)), length(unique(markets.JetBlue$market_ids)),
           length(unique(markets.Spirit$market_ids)),
           length(unique(markets.Compete$market_ids)))
  
  summaryTable <- rbind(numFirms.min, numFirms.median, numFirms.mean, numFirms.sd, numFirms.max,
                        numProducts.min, numProducts.median, numProducts.mean, numProducts.sd, numProducts.max,
                        numPassengers.min, numPassengers.median, numPassengers.mean, numPassengers.sd, numPassengers.max,
                        hhi.min, hhi.median, hhi.mean, hhi.sd, hhi.max, 
                        minmiles.min, minmiles.median, minmiles.mean, minmiles.sd, minmiles.max,
                        obs)
  colnames(summaryTable) <- table_header
  rownames(summaryTable) <- NULL
  
  kbl(summaryTable,
      format = "latex", 
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Number of Firms", 1, 5) %>%
    pack_rows(group_label = "Number of Products", 6, 10) %>%
    pack_rows(group_label = "Number of Passengers", 11, 15) %>%
    pack_rows(group_label = "HHI", 16, 20) %>%
    pack_rows(group_label = "Market Minimum Distance", 21, 25) %>%
    row_spec(row = 25, hline_after = TRUE) %>%
    save_kable(file = output)
}


summary_statistics_market_level_slim <- function(input = "02.Intermediate/Product_Data.rds",
                                            output = "06.Tables/SummaryStatistics_Market_Slim.tex"){
  data <- read_rds(input)
  
  # Within the analysis data, Spirit has Prescence = FALSE on its routes
  # Fix this for both firms.
  data[, Spirit_Prescence := max(Spirit_Prescence), by = market_ids]
  data[, JetBlue_Prescence := max(JetBlue_Prescence), by = market_ids]
  
  table_header <- c("Variable", "All Markets", "JetBlue", "Spirit", "JetBlue-Spirit")
  
  # First, Number of Firms in Market
  data.numFirms <- data %>% select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>% 
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>% 
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.Compete <- data %>% filter(JetBlue_Prescence == TRUE, Spirit_Prescence == TRUE) %>%
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  
  numFirms.mean <- c("Number of Firms", 
                     round(mean(data.numFirms$N), 2),
                     paste(round(mean(data.numFirms.JetBlue$N), 2)),
                     paste(round(mean(data.numFirms.Spirit$N), 2)),
                     paste(round(mean(data.numFirms.Compete$N), 2)))
  numFirms.sd <- c("(SD)", paste("(", round(sd(data.numFirms$N), 2), ")", sep = ""), 
                   paste("(", round(sd(data.numFirms.JetBlue$N), 2), ")", sep = ""),
                   paste("(", round(sd(data.numFirms.Spirit$N), 2), ")", sep = ""),
                   paste("(", round(sd(data.numFirms.Compete$N), 2), ")", sep = ""))

  # Now, Number of Passengers within Market
  data.Passengers <- data %>% select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.Compete <- data %>% filter(Spirit_Prescence == TRUE,
                                             JetBlue_Prescence == TRUE) %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  
  numPassengers.mean <- c("Passengers in Market", paste(round(mean(data.Passengers$Passengers), 2)),
                          paste(round(mean(data.Passengers.JetBlue$Passengers), 2)),
                          paste(round(mean(data.Passengers.Spirit$Passengers), 2)),
                          paste(round(mean(data.Passengers.Compete$Passengers), 2)))
  numPassengers.sd <- c("(SD)", 
                        paste("(", round(sd(data.Passengers$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.JetBlue$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.Spirit$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.Compete$Passengers), 2), ")", sep = ""))

  
  # Num Products in Market
  data.products <- data %>% select(market_ids, Product_Name) %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>%
    select(market_ids, Product_Name) %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>%
    select(market_ids, Product_Name) %>% unique() %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.Compete <- data %>% filter(Spirit_Prescence == TRUE,
                                           JetBlue_Prescence == TRUE) %>%
    select(market_ids, Product_Name) %>% unique() %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  
  
  numProducts <- c("Products in Market", "", "", "", "")
  numProducts.min <- c("Min", min(data.products$Products), min(data.products.JetBlue$Products),
                       min(data.products.Spirit$Products), min(data.products.Compete$Products))
  numProducts.median <- c("Median", median(data.products$Products), median(data.products.JetBlue$Products),
                          median(data.products.Spirit$Products), median(data.products.Compete$Products))
  numProducts.mean <- c("Products in Market", paste(round(mean(data.products$Products), 2)),
                        paste(round(mean(data.products.JetBlue$Products), 2)),
                        paste(round(mean(data.products.Spirit$Products), 2)),
                        paste(round(mean(data.products.Compete$Products), 2)))
  numProducts.sd <- c("(SD)",
                      paste("(", round(sd(data.products$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.JetBlue$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.Spirit$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.Compete$Products), 2), ")", sep = ""))
  numProducts.max <- c("Max", max(data.products$Products), max(data.products.JetBlue$Products), max(data.products.Spirit$Products),
                       max(data.products.Compete$Products))                  
  
  # Market Concentration
  data.hhi <- data %>% select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  data.hhi.JB <- data %>%filter(JetBlue_Prescence == TRUE) %>% select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  data.hhi.SP <- data %>%filter(Spirit_Prescence == TRUE) %>% select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  data.hhi.Compete <- data %>%filter(Spirit_Prescence == TRUE, JetBlue_Prescence == TRUE) %>% 
    select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  
  hhi <- c("HHI", "", "", "", "")
  hhi.min <- c("Min", round(min(data.hhi$HHI),2), round(min(data.hhi.JB$HHI),2), round(min(data.hhi.SP$HHI), 2),
               round(min(data.hhi.Compete$HHI), 2))
  hhi.median <- c("Median", round(median(data.hhi$HHI),2), round(median(data.hhi.JB$HHI),2), 
                  round(median(data.hhi.SP$HHI), 2), round(median(data.hhi.Compete$HHI), 2))
  hhi.mean <- c("HHI", paste(round(mean(data.hhi$HHI), 2)),
                paste(round(mean(data.hhi.JB$HHI), 2)),
                paste(round(mean(data.hhi.SP$HHI), 2)),
                paste(round(mean(data.hhi.Compete$HHI), 2)))
  hhi.sd <- c("(SD)", 
              paste("(", round(sd(data.hhi$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.JB$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.SP$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.Compete$HHI), 2), ")", sep = ""))
  hhi.max <- c("Max", round(max(data.hhi$HHI),2), round(max(data.hhi.JB$HHI),2), round(max(data.hhi.SP$HHI),2),
               round(max(data.hhi.Compete$HHI), 2))                  
  
  
  # Minimum Miles in Market
  data.minmiles <- data %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.JB <- data %>% filter(JetBlue_Prescence == TRUE) %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.SP <- data %>%filter(Spirit_Prescence == TRUE) %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.Compete <- data %>% filter(Spirit_Prescence == TRUE, JetBlue_Prescence == TRUE) %>% 
    select(market_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  minmiles.min <- c("Min", round(min(data.minmiles$market_min_miles), 2), round(min(data.minmiles.JB$market_min_miles), 2),
                    round(min(data.minmiles.SP$market_min_miles), 2), round(min(data.minmiles.Compete$market_min_miles), 2))
  minmiles.median <- c("Median",  round(median(data.minmiles$market_min_miles), 2), round(median(data.minmiles.JB$market_min_miles), 2),
                       round(median(data.minmiles.SP$market_min_miles), 2), round(median(data.minmiles.Compete$market_min_miles), 2))
  minmiles.mean <- c("Average Direct Miles",  round(mean(data.minmiles$market_min_miles), 2), round(mean(data.minmiles.JB$market_min_miles), 2),
                     round(mean(data.minmiles.SP$market_min_miles), 2), round(mean(data.minmiles.Compete$market_min_miles), 2))
  minmiles.sd <- c("(SD)", paste("(", round(sd(data.minmiles$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.JB$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.SP$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.Compete$market_min_miles), 2), ")", sep = ""))
  minmiles.max <- c("Max", round(max(data.minmiles$market_min_miles), 2), round(max(data.minmiles.JB$market_min_miles), 2),
                    round(max(data.minmiles.SP$market_min_miles), 2), round(max(data.minmiles.Compete$market_min_miles), 2))
  
  # Record Years
  year.min <- c("Min", min(data$Year), min(data[JetBlue_Prescence == TRUE,]$Year), 
                min(data[Spirit_Prescence == TRUE,]$Year), min(data[Spirit_Prescence == TRUE & JetBlue_Prescence == TRUE,]$Year))
  year.max <- c("Max", max(data$Year), max(data[JetBlue_Prescence == TRUE,]$Year), max(data[Spirit_Prescence == TRUE,]$Year),
                max(data[JetBlue_Prescence == TRUE & Spirit_Prescence == TRUE,]$Year))
  
  # Now, Observations
  markets <- data %>% select(market_ids) %>% unique() 
  
  markets.Spirit <- data %>% filter(Spirit_Prescence == TRUE) %>%  select(market_ids) %>% unique() 
  
  markets.JetBlue <- data %>% filter(JetBlue_Prescence == TRUE) %>% select(market_ids) %>% unique()
  
  markets.Compete <- data %>% filter(Spirit_Prescence == TRUE, JetBlue_Prescence == TRUE) %>%
    select(market_ids) %>% unique()
  
  obs <- c("Number of Markets", length(unique(markets$market_ids)), length(unique(markets.JetBlue$market_ids)),
           length(unique(markets.Spirit$market_ids)),
           length(unique(markets.Compete$market_ids)))
  
  summaryTable <- rbind(numFirms.mean, numFirms.sd,
                        numProducts.mean, numProducts.sd,
                        numPassengers.mean, numPassengers.sd, 
                        hhi.mean, hhi.sd,
                        minmiles.mean, minmiles.sd, 
                        obs)
  colnames(summaryTable) <- table_header
  rownames(summaryTable) <- NULL
  
  kbl(summaryTable,
      format = "latex", 
      escape = TRUE, booktabs = TRUE,
      linesep = "") %>%
    row_spec(row = 10, hline_after = TRUE) %>%
    save_kable(file = output)
}


summary_statistics_firm_type <- function(input = "02.Intermediate/Product_Data.rds",
                                         output_product = "06.Tables/SummaryStatistics_Firm_Type_Product.tex",
                                         output_market = "06.Tables/SummaryStatistics_Firm_Type_Market.tex"){
  product_data <- readRDS(input)
  
  product_data$MktCoupons <- as.numeric(product_data$MktCoupons)
  
  product_data_lg <- product_data[Carrier %in% c("United Air Lines Inc.", "American Airlines Inc.",
                                                 "Delta Air Lines Inc."),]
  product_data_lc <- product_data[Carrier %in% c("Alaska Airlines Inc.", "Hawaiian Airlines Inc.",
                                                 "JetBlue Airways", "Southwest Airlines Co."),]
  product_data_ulcc <- product_data[Carrier %in% c("Spirit Air Lines",
                                                   "Allegiant Air", "Frontier Airlines Inc."),]
  
  colnames <- c("Variable", "All Firms", "Legacy", "LCC", "ULCC")
  
  # Miles Rows
  miles <- c("Miles (Hundreds)", "", "", "", "")
  miles.min <- c("Min", min(product_data$MktMilesFlown), min(product_data_lg$MktMilesFlown),
                 min(product_data_lc$MktMilesFlown),min(product_data_ulcc$MktMilesFlown))
  miles.median <- c("Median", paste(round(median(product_data$MktMilesFlown), digits = 2)),
                    paste(round(median(product_data_lg$MktMilesFlown), digits = 2)),
                    paste(round(median(product_data_lc$MktMilesFlown), digits = 2)),
                    paste(round(median(product_data_ulcc$MktMilesFlown), digits = 2)))
  miles.mean <- c("Mean", paste(round(mean(product_data$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_lg$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_lc$MktMilesFlown), digits = 2)),
                  paste(round(mean(product_data_ulcc$MktMilesFlown), digits = 2)))
  miles.sd <- c("(SD)", paste("(", round(sd(product_data$MktMilesFlown), digits = 2), ")", sep = ""), 
                paste("(", round(sd(product_data_lg$MktMilesFlown), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_lc$MktMilesFlown), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_ulcc$MktMilesFlown), digits = 2), ")", sep = ""))
  miles.max <- c("Max", max(product_data$MktMilesFlown), max(product_data_lg$MktMilesFlown),
                 max(product_data_lc$MktMilesFlown), max(product_data_ulcc$MktMilesFlown))
  
  # Number of Connections
  connections <- c("Connections", "", "", "")
  connections.min <- c("Min", min(product_data$MktCoupons), min(product_data_lg$MktCoupons),
                       min(product_data_lc$MktCoupons), min(product_data_ulcc$MktCoupons))
  connections.mean <- c("Mean", paste(round(mean(product_data$MktCoupons), digits = 2)),
                        paste(round(mean(product_data_lg$MktCoupons), digits = 2)),
                        paste(round(mean(product_data_lc$MktCoupons), digits = 2)),
                        paste(round(mean(product_data_ulcc$MktCoupons), digits = 2)))
  connections.sd <- c("(SD)", 
                      paste("(", round(sd(product_data$MktCoupons), digits = 2), ")", sep = ""),
                      paste("(", round(sd(product_data_lg$MktCoupons), digits = 2), ")", sep = ""),
                      paste("(", round(sd(product_data_lc$MktCoupons), digits = 2), ")", sep = ""),
                      paste("(", round(sd(product_data_ulcc$MktCoupons), digits = 2), ")", sep = ""))
  connections.max <- c("Max", max(product_data$MktCoupons),max(product_data_lg$MktCoupons),
                       max(product_data_lc$MktCoupons), max(product_data_ulcc$MktCoupons))
  
  # Prices
  price <- c("Prices (100s 2017 USD)", "", "", "")
  
  price.min <- c("Min", round(min(product_data$prices),2), round(min(product_data_lg$prices),2),
                 round(min(product_data_lc$prices),2), round(min(product_data_ulcc$prices),2))
  price.median <- c("Median", round(median(product_data$prices),2), round(median(product_data_lg$prices),2),
                    round(median(product_data_lc$prices),2), round(median(product_data_ulcc$prices),2))
  price.mean <- c("Mean", paste(round(mean(product_data$prices), digits = 2)),
                  paste(round(mean(product_data_lg$prices), digits = 2)),
                  paste(round(mean(product_data_lc$prices), digits = 2)),
                  paste(round(mean(product_data_ulcc$prices), digits = 2)))
  price.sd <- c("(SD)", paste(" (", round(sd(product_data$prices), digits = 2), ")", sep = ""),
                paste("(", round(sd(product_data_lg$prices), digits = 2), ")", sep = ""),
                paste(" (", round(sd(product_data_lc$prices), digits = 2), ")", sep = ""),
                paste(" (", round(sd(product_data_ulcc$prices), digits = 2), ")", sep = ""))
  price.max <- c("Max", round(max(product_data$prices),2), round(max(product_data_lg$prices), 2),
                 round(max(product_data_lc$prices),2), round(max(product_data_ulcc$prices), 2))
  
  # Passengers
  passengers <- c("Passengers", "", "", "")
  passengers.min <- c("Min", min(product_data$Passengers.Product), min(product_data_lg$Passengers.Product),
                      min(product_data_lc$Passengers.Product), min(product_data_ulcc$Passengers.Product))
  passengers.median <- c("Median", median(product_data$Passengers.Product), median(product_data_lg$Passengers.Product),
                         median(product_data_lc$Passengers.Product), median(product_data_ulcc$Passengers.Product))
  passengers.mean <- c("Mean", paste(round(mean(product_data$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_lg$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_lc$Passengers.Product), digits = 2)),
                       paste(round(mean(product_data_ulcc$Passengers.Product), digits = 2)))
  passengers.sd <- c("(SD)", 
                     paste("(", round(sd(product_data$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_lg$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_lc$Passengers.Product), digits = 2), ")", sep = ""),
                     paste("(", round(sd(product_data_ulcc$Passengers.Product), digits = 2), ")", sep = ""))
  passengers.max <- c("Max", max(product_data$Passengers.Product), max(product_data_lg$Passengers.Product),
                      max(product_data_lc$Passengers.Product), max(product_data_ulcc$Passengers.Product))
  
  # Destination Share
  destinationShare.min <- c("Min", min(round(product_data$Destination_Firm_Service_Ratio,2)),
                            min(round(product_data_lg$Destination_Firm_Service_Ratio,2)),
                            min(round(product_data_lc$Destination_Firm_Service_Ratio,2)),
                            min(round(product_data_ulcc$Destination_Firm_Service_Ratio,2)))
  destinationShare.mean <- c("Mean", round(mean(product_data$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_lg$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_lc$Destination_Firm_Service_Ratio), 2),
                             round(mean(product_data_ulcc$Destination_Firm_Service_Ratio), 2))
  destinationShare.sd <- c("(SD)", paste("(", round(sd(product_data$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_lg$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_lc$Destination_Firm_Service_Ratio),2), ")", sep = ""),
                           paste("(", round(sd(product_data_ulcc$Destination_Firm_Service_Ratio),2), ")", sep = ""))
  destinationShare.max <- c("Max", max(round(product_data$Destination_Firm_Service_Ratio,2)),
                            max(round(product_data_lg$Destination_Firm_Service_Ratio,2)),
                            max(round(product_data_lc$Destination_Firm_Service_Ratio,2)),
                            max(round(product_data_ulcc$Destination_Firm_Service_Ratio,2)))
  
  # Gas Price * Miles
  gasMiles.min <- c("Min", round(min(product_data$GasMiles),2),
                    round(min(product_data_lg$GasMiles),2), round(min(product_data_lc$GasMiles),2),
                    round(min(product_data_ulcc$GasMiles),2))
  gasMiles.median <- c("Median", round(median(product_data$GasMiles),2),
                       round(median(product_data_lg$GasMiles),2), round(median(product_data_lc$GasMiles),2),
                       round(median(product_data_ulcc$GasMiles),2))
  gasMiles.mean <- c("Mean", round(mean(product_data$GasMiles),2),
                     round(mean(product_data_lg$GasMiles),2), round(mean(product_data_lc$GasMiles),2),
                     round(mean(product_data_ulcc$GasMiles),2))
  gasMiles.sd <- c("(SD)", paste("(",round(mean(product_data$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_lg$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_lc$GasMiles),2), ")", sep = ""),
                   paste("(", round(mean(product_data_ulcc$GasMiles),2), ")", sep = ""))
  gasMiles.max <- c("Max", round(min(product_data$GasMiles),2),
                    round(min(product_data_lg$GasMiles),2), round(min(product_data_lc$GasMiles),2),
                    round(min(product_data_ulcc$GasMiles),2))
  
  # NonStop
  NonStop.stat <- c("Share Nonstop", round(mean(product_data$NonStop),2),
                    round(mean(product_data_lg$NonStop),2), round(mean(product_data_lc$NonStop),2),
                    round(mean(product_data_ulcc$NonStop),2))
  
  # Intermediate Stop
  IntermediateStop <- c("Hub Layover", round(mean(product_data$IntermediateHub)),
                        round(mean(product_data_lg$IntermediateHub)), round(mean(product_data_lc$IntermediateHub)),
                        round(mean(product_data_ulcc$IntermediateHub)))
  
  # Observations
  obs <- c("Observations", nrow(product_data), nrow(product_data_lg), nrow(product_data_lc),
           nrow(product_data_ulcc))
  
  # Make Table
  summary_table <- rbind(price.min, price.median, price.mean, price.sd, price.max,
                         miles.min, miles.median, miles.mean, miles.sd, miles.max,
                         passengers.min, passengers.median, passengers.mean, passengers.sd, passengers.max,
                         connections.min, connections.mean, connections.sd, connections.max,
                         destinationShare.min, destinationShare.mean, destinationShare.sd, destinationShare.max,
                         gasMiles.min, gasMiles.median, gasMiles.mean, gasMiles.sd, gasMiles.max, 
                         NonStop.stat, IntermediateStop, obs)
  colnames(summary_table) <- colnames
  rownames(summary_table) <- NULL
  
  # Generate Table with kableExtra
  kbl(summary_table,
      format = "latex", 
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Price (100s 2017 USD)", 1, 5) %>%
    pack_rows(group_label = "Miles Travelled", 6, 10) %>%
    pack_rows(group_label = "Number of Passengers", 11, 15) %>%
    pack_rows(group_label = "Connections", 16, 19) %>%
    pack_rows(group_label = "Destination Route Share (Percent)", 20, 23) %>%
    pack_rows(group_label = "JetFuel * Market Miles", 24, 28) %>%
    pack_rows(group_label = "Binary Variables", 29, 30) %>%
    row_spec(row = 30, hline_after = TRUE) %>%
    save_kable(file = output_product)
  
  # Generate Data for Each Key Subset
  data <- product_data;
  data_leg <- product_data %>% filter(market_ids %in% 
                          product_data[Carrier %in% c("United Air Lines Inc.", "American Airlines Inc.",
                                                    "Delta Air Lines Inc."),]$market_ids ) %>%
    as.data.table()
  
  data_lcc <- product_data %>% filter(market_ids %in% 
              product_data[Carrier %in%  c("Alaska Airlines Inc.", "Hawaiian Airlines Inc.",
                             "JetBlue Airways", "Southwest Airlines Co."),]$market_ids) %>%
    as.data.table();
  
  data_ulcc <- product_data %>% filter(market_ids %in% 
                           product_data[Carrier %in%  c("Spirit Air Lines",
                       "Allegiant Air", "Frontier Airlines Inc."),]$market_ids) %>% 
    as.data.table();

    # First, Number of Firms
  data.numFirms <- data %>% select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.leg <- data_leg %>%
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.lcc <- data_lcc %>%
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  data.numFirms.ulcc <- data_ulcc %>%
    select(market_ids, Carrier) %>% unique() %>%
    group_by(market_ids) %>% summarize(N = n()) %>% data.table()
  
  
  numFirms <- c("Number of Firms", "", "", "", "")
  numFirms.min <- c("Min", min(data.numFirms$N), min(data.numFirms.leg$N), min(data.numFirms.lcc$N),
                    min(data.numFirms.ulcc$N))
  numFirms.mean <- c("Mean", 
                     round(mean(data.numFirms$N), 2),
                     paste(round(mean(data.numFirms.leg$N), 2)),
                     paste(round(mean(data.numFirms.lcc$N), 2)),
                     paste(round(mean(data.numFirms.ulcc$N), 2)))
  numFirms.sd <- c("(SD)", paste("(", round(sd(data.numFirms$N), 2), ")", sep = ""), 
                   paste("(", round(sd(data.numFirms.leg$N), 2), ")", sep = ""),
                   paste("(", round(sd(data.numFirms.lcc$N), 2), ")", sep = ""),
                   paste("(", round(sd(data.numFirms.ulcc$N), 2), ")", sep = ""))
  numFirms.max <- c("Max", max(data.numFirms$N), max(data.numFirms.leg$N), max(data.numFirms.lcc$N),
                    max(data.numFirms.ulcc$N)) 
  
  # Now, Number of Products
  data.products <- data %>% select(market_ids, Product_Name) %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.leg <- data_leg %>%
    select(market_ids, Product_Name) %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.lcc <- data_lcc %>%
    select(market_ids, Product_Name) %>% unique() %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  data.products.ulcc <- data_ulcc %>%
    select(market_ids, Product_Name) %>% unique() %>% group_by(market_ids) %>%
    summarize(Products := n()) %>% data.table();
  
  numProducts <- c("Products in Market", "", "", "", "")
  numProducts.min <- c("Min", min(data.products$Products), min(data.products.leg$Products),
                       min(data.products.lcc$Products), min(data.products.ulcc$Products))
  numProducts.median <- c("Median", median(data.products$Products), median(data.products.leg$Products),
                          median(data.products.lcc$Products), median(data.products.ulcc$Products))
  numProducts.mean <- c("Mean", paste(round(mean(data.products$Products), 2)),
                        paste(round(mean(data.products.leg$Products), 2)),
                        paste(round(mean(data.products.lcc$Products), 2)),
                        paste(round(mean(data.products.ulcc$Products), 2)))
  numProducts.sd <- c("(SD)",
                      paste("(", round(sd(data.products$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.leg$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.lcc$Products), 2), ")", sep = ""),
                      paste(" (", round(sd(data.products.ulcc$Products), 2), ")", sep = ""))
  numProducts.max <- c("Max", max(data.products$Products), max(data.products.leg$Products), 
                       max(data.products.lcc$Products),
                       max(data.products.ulcc$Products))                  
  
  # Number of Passengers
  # Now, Number of Passengers within Market
  data.Passengers <- data %>% select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.leg <- data_leg %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.lcc <- data_lcc %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  data.Passengers.ulcc <- data_ulcc %>%
    select(market_ids, Passengers.Product) %>% group_by(market_ids) %>%
    summarize(Passengers := sum(Passengers.Product)) %>% data.table();
  
  numPassengers <- c("Passengers in Market", "", "", "", "")
  numPassengers.min <- c("Min", min(data.Passengers$Passengers), min(data.Passengers.leg$Passengers),
                         min(data.Passengers.lcc$Passengers), min(data.Passengers.ulcc$Passengers))
  numPassengers.median <- c("Median", median(data.Passengers$Passengers), 
                            median(data.Passengers.leg$Passengers),
                            median(data.Passengers.lcc$Passengers), 
                            median(data.Passengers.ulcc$Passengers))
  numPassengers.mean <- c("Mean", paste(round(mean(data.Passengers$Passengers), 2)),
                          paste(round(mean(data.Passengers.leg$Passengers), 2)),
                          paste(round(mean(data.Passengers.lcc$Passengers), 2)),
                          paste(round(mean(data.Passengers.ulcc$Passengers), 2)))
  numPassengers.sd <- c("(SD)", 
                        paste("(", round(sd(data.Passengers$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.leg$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.lcc$Passengers), 2), ")", sep = ""),
                        paste("(", round(sd(data.Passengers.ulcc$Passengers), 2), ")", sep = ""))
  
  numPassengers.max <- c("Max", max(data.Passengers$Passengers), max(data.Passengers.leg$Passengers), 
                         max(data.Passengers.lcc$Passengers),
                         max(data.Passengers.ulcc$Passengers))                  
  
  # Market Concentration
  data.hhi <- data %>% select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  data.hhi.leg <- data %>% filter(market_ids %in% data_leg$market_ids) %>%
    select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  data.hhi.lcc <- data %>% filter(market_ids %in% data_lcc$market_ids) %>%
    select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  data.hhi.ulcc <- data %>% filter(market_ids %in% data_ulcc$market_ids) %>%
    select(market_ids, firm_ids, Passengers.Product, Passengers.Inside.Market) %>% 
    unique() %>% group_by(market_ids, firm_ids, Passengers.Inside.Market) %>% summarize(MarketShare := sum(Passengers.Product)) %>%
    mutate(MarketShare := MarketShare / Passengers.Inside.Market * 100) %>% 
    group_by(market_ids) %>% summarize(HHI := sum(MarketShare^2)) %>% data.table();
  
  
  hhi <- c("HHI", "", "", "", "")
  hhi.min <- c("Min", round(min(data.hhi$HHI),2), round(min(data.hhi.leg$HHI),2), round(min(data.hhi.lcc$HHI), 2),
               round(min(data.hhi.ulcc$HHI), 2))
  hhi.median <- c("Median", round(median(data.hhi$HHI),2), round(median(data.hhi.leg$HHI),2), 
                  round(median(data.hhi.lcc$HHI), 2), round(median(data.hhi.ulcc$HHI), 2))
  hhi.mean <- c("Mean", paste(round(mean(data.hhi$HHI), 2)),
                paste(round(mean(data.hhi.leg$HHI), 2)),
                paste(round(mean(data.hhi.lcc$HHI), 2)),
                paste(round(mean(data.hhi.ulcc$HHI), 2)))
  hhi.sd <- c("(SD)", 
              paste("(", round(sd(data.hhi$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.leg$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.lcc$HHI), 2), ")", sep = ""),
              paste("(", round(sd(data.hhi.ulcc$HHI), 2), ")", sep = ""))
  hhi.max <- c("Max", round(max(data.hhi$HHI),2), round(max(data.hhi.leg$HHI),2), round(max(data.hhi.lcc$HHI),2),
               round(max(data.hhi.ulcc$HHI), 2))                  
  
  # Minimum Miles in Market
  data.minmiles <- data %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.leg <- data_leg %>% 
    unique() %>% data.table();
  
  data.minmiles.lcc <- data_lcc %>% select(market_ids, firm_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  data.minmiles.ulcc <- data_ulcc %>% 
    select(market_ids, market_min_miles) %>% 
    unique() %>% data.table();
  
  minmiles.min <- c("Min", round(min(data.minmiles$market_min_miles), 2), round(min(data.minmiles.leg$market_min_miles), 2),
                    round(min(data.minmiles.lcc$market_min_miles), 2), round(min(data.minmiles.ulcc$market_min_miles), 2))
  minmiles.median <- c("Median",  round(median(data.minmiles$market_min_miles), 2), round(median(data.minmiles.leg$market_min_miles), 2),
                       round(median(data.minmiles.lcc$market_min_miles), 2), round(median(data.minmiles.ulcc$market_min_miles), 2))
  minmiles.mean <- c("Mean",  round(mean(data.minmiles$market_min_miles), 2), round(mean(data.minmiles.leg$market_min_miles), 2),
                     round(mean(data.minmiles.lcc$market_min_miles), 2), round(mean(data.minmiles.ulcc$market_min_miles), 2))
  minmiles.sd <- c("(SD)", paste("(", round(sd(data.minmiles$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.leg$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.lcc$market_min_miles), 2), ")", sep = ""),
                   paste("(", round(sd(data.minmiles.ulcc$market_min_miles), 2), ")", sep = ""))
  minmiles.max <- c("Max", round(max(data.minmiles$market_min_miles), 2), round(max(data.minmiles.leg$market_min_miles), 2),
                    round(max(data.minmiles.lcc$market_min_miles), 2), round(max(data.minmiles.ulcc$market_min_miles), 2))
  
  # Record Years
  year.min <- c("Min", min(data$Year), min(data_leg$Year), 
                min(data_lcc$Year), min(data_ulcc$Year))
  year.max <- c("Max", max(data$Year), max(data_leg$Year), 
                max(data_lcc$Year), max(data_ulcc$Year))
  
  obs <- c("Number of Markets", length(unique(data$market_id)), length(unique(data_leg$market_id)),
           length(unique(data_lcc$market_id)), length(unique(data_ulcc$market_id)))
  
  # Average Price Elasticity
  # JB Elasticity
  # Spirit Elasticity
  
  
  summaryTable <- rbind(numFirms.min, numFirms.mean, numFirms.sd, numFirms.max,
                        numProducts.min, numProducts.median, numProducts.mean, numProducts.sd, numProducts.max,
                        numPassengers.min, numPassengers.median, numPassengers.mean, numPassengers.sd, numPassengers.max,
                        hhi.min, hhi.median, hhi.mean, hhi.sd, hhi.max,
                        minmiles.min, minmiles.median, minmiles.mean, minmiles.sd, minmiles.max,
                        year.min, year.max,
                        obs)
  colnames(summaryTable) <- c("Variable", "All Markets", "Legacy", "Low-Cost", "Ultra Low-Cost")
  rownames(summaryTable) <- NULL
  
  kbl(summaryTable,
      format = "latex", 
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Number of Firms", 1, 4) %>%
    pack_rows(group_label = "Number of Products", 5, 9) %>%
    pack_rows(group_label = "Number of Passengers", 10, 14) %>%
    pack_rows(group_label = "HHI", 15, 19) %>%
    pack_rows(group_label = "Market Minimum Distance", 20, 24) %>%
    pack_rows(group_label = "Year", 25, 26) %>%
    row_spec(row = 26, hline_after = TRUE) %>%
    save_kable(file = output_market)
  
}

markets_by_concentration_table <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                           output = "06.Tables/Markets_By_Concentration.tex"){
  db1b <- readRDS(input)
  db1b <- db1b[!is.na(Origin_MSA),]
  
  # For each origin, calculate out the share of passengers on JetBlue and Spirit Flights,
  # on a Quarterly Basis
  db1b[, Origin_Passengers := sum(Passengers.Product, na.rm = TRUE), by = c("Origin_MSA", "Year_Quarter")]
  summary <- db1b %>% filter(Carrier %in% c("JetBlue Airways", "Spirit Air Lines")) %>%
    group_by(Origin_MSA, Year_Quarter, Year, Quarter) %>%
    summarize(JetBlue_Spirit_Share = sum(Passengers.Product, na.rm = TRUE) / mean(Origin_Passengers) * 100) %>%
    group_by(Origin_MSA, Year) %>%
    summarize(JetBlue_Spirit_Share = round(mean(JetBlue_Spirit_Share), digits = 3)) %>%
    as.data.table()

  summary[is.na(JetBlue_Spirit_Share), JetBlue_Spirit_Share := 0]
  
  summary.wide <- dcast(summary, formula = Origin_MSA ~ Year,
                        fill = 0) %>% as.data.table()
  
  colnames(summary.wide) <- c("Origin_MSA", paste("Y", 2016:2023, sep = ""))
  summary.wide[, Y2020 := NULL]
  summary.wide[, Avg.Percent := round((Y2016 + Y2017 + Y2018 + Y2019 + Y2021 + 
                                   Y2022 + Y2023)/7, digits = 2)]
  summary.wide <- summary.wide[order(-Avg.Percent),]
  summary.wide <- summary.wide[1:10,]
  
  kbl(summary.wide,
      format = "latex", 
      escape = TRUE, booktabs = TRUE) %>%
    save_kable(output)
}

markets_by_hhi_increase_table <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                                          output = "06.Tables/HHI_Increase_Table.tex"){
  db1b <- readRDS(input)

  # First, Calculate Companies Share of Passengers Inside Market
  db1b[, Passengers_Departing := sum(Passengers.Product), by = c("Year", "Quarter",
                                                                 "Origin_MSA")]
  db1b[, Passengers_Departing_Carrier := sum(Passengers.Product), by = c("Year", "Quarter",
                                                                         "Origin_MSA",
                                                                         "Carrier")]
  db1b[, Carrier_Index := (Passengers_Departing_Carrier / Passengers_Departing * 100)^2]
  
  db1b[, Carrier_Merger := Carrier]
  db1b[Carrier_Merger == "Spirit Air Lines", Carrier_Merger := "JetBlue Airways"]
  db1b[, Passengers_Departing_C_Merger := sum(Passengers.Product), by = c("Year", "Quarter",
                                                                          "Origin_MSA",
                                                                          "Carrier_Merger")]
  db1b[, Carrier_Merger_Index := (Passengers_Departing_C_Merger / Passengers_Departing * 100)^2]
  
  db1b <- db1b[, .(Origin_MSA, Year, Quarter, Carrier_Index, Carrier_Merger_Index)]
  db1b <- unique(db1b)
  db1b[, HHI := sum(Carrier_Index), by = c("Year", "Quarter", "Origin_MSA")]
  db1b[, HHI.Merger := sum(Carrier_Merger_Index), by = c("Year", "Quarter",
                                                         "Origin_MSA")]
  
  db1b <- db1b %>% select(Year, Quarter, Origin_MSA, HHI, HHI.Merger) %>%
    mutate(HHI.Change := HHI.Merger - HHI) %>% 
    unique() %>% filter(!is.na(Origin_MSA)) %>% as.data.table()
  
  db1b[, HHI.Change.Year.Avg := round(mean(HHI.Change), digits = 1), by = c("Year", "Origin_MSA")]
  
  db1b.wide <- db1b %>% select(Origin_MSA, Year, HHI.Change.Year.Avg) %>%
    unique() %>%
    reshape(timevar = "Year", idvar = "Origin_MSA", direction = "wide") %>%
    as.data.table()
  
  # Order by Average 2023 Change
  db1b.wide <- db1b.wide[order(-HHI.Change.Year.Avg.2022),]
  colnames(db1b.wide) <- c("Origin", "2016", "2017", "2018", "2019", "2020", "2021", "2022", 
                           "2023")
  db1b.wide <- db1b.wide[1:10,]
  
  db1b.wide[, Origin := factor(x = Origin, levels = c("San Juan-Carolina-Caguas, PR",
            "Boston-Cambridge-Newton, MA-NH", "Miami-Fort Lauderdale-West Palm Beach, FL",
            "Hartford-West Hartford-East Hartford, CT", "Orlando-Kissimmee-Sanford, FL", 
            "New York-Newark-Jersey City, NY-NJ-PA", "Cape Coral-Fort Myers, FL", 
            "Richmond, VA", "Detroit-Warren-Dearborn, MI", "New Orleans-Metairie, LA"),
            labels = c("San Juan, PR", "Boston, MA", 
                       "Miami, FL", "Hartford, CT", "Orlando, FL",
                       "New York, NY", "Cape Coral, FL", 
                       "Richmond, VA", "Detroit, MI",
                       "New Orleans, LA"))]
  
  kbl(db1b.wide,
      format = "latex", 
      escape = TRUE, booktabs = TRUE) %>%
    save_kable(output)
}

seat_removal_table <- function(input = "02.Intermediate/Fleet_Compilation.rds",
                               output = "06.Tables/JetBlue_Spirit_Fleet.tex"){
  fleet <- readRDS(input)
  fleet <- fleet[Carrier %in% c("JetBlue Airways",  "Spirit Air Lines"),]
  fleet <- fleet[Year == 2022,]
  
  fleet.summary <- fleet %>% group_by(Carrier, 
                                      Manufacturer, Model, NumberSeats) %>%
    summarize(N = n()) %>%
    mutate(Total_Seats = N * NumberSeats) %>% as.data.table()
  
  # Now, clarify a few models
  # PSGR = Passenger, not a model identifier
  fleet.summary[, Model := gsub(pattern = "-PSGR", x = Model, replacement = "")]
  fleet.summary[, Model := gsub(pattern = "A-", x = Model, replacement = "A")]
  fleet.summary[, Model := gsub(pattern = "NX", x = Model, replacement = "neo")]
  
  # Last three digits relate to thrust ratings and engine types, likely irrelevant for seat analysis
  fleet.summary[, Model := gsub(pattern = "-300", x = Model, replacement = "")]
  fleet.summary[, Model := gsub(pattern = "-232", x = Model, replacement = "")]
  fleet.summary[, Model := gsub(pattern = "-231", x = Model, replacement = "")]
  fleet.summary[, Model := gsub(pattern = "-271", x = Model, replacement = "")]
  fleet.summary[, Model := gsub(pattern = "-100", x = Model, replacement = "")]
  
  colnames(fleet.summary) <- c("Carrier", "Manufacturer", "Model", "Seats", "Count", "Total Seats")
  fleet.summary$Carrier <- NULL
  
  kbl(fleet.summary,
      format = "latex", 
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "JetBlue", 1, 9) %>%
    pack_rows(group_label = "Spirit", 10, 12) %>%
    save_kable(output)
}

calculate_route_share <- function(carrier, competitor, label, data){
  output_row <- c(paste("\\%", label, "Rival"))
  data <- data[Carrier == carrier, ]
  data <- data %>% select(Year, Quarter, Origin, Dest, Alaska_Prescence,
                          American_Prescence, Delta_Prescence, United_Prescence,
                          JetBlue_Prescence, Spirit_Prescence) %>%
    unique() %>% as.data.table()
  
  for(i in 1:length(unique(data$Year))){
    for(j in 1:4){
      if(unique(data$Year)[i] == 2023 & j == 4){
        break
      }
      current_data <- data[Year == unique(data$Year)[i] & Quarter == j,]       
      total_routes <- nrow(current_data)
      compete_routes <- nrow(current_data[current_data[[competitor]] == TRUE,])
      compete_percent <- round(compete_routes / total_routes * 100)
      
      output_row <- c(output_row, compete_percent)
    }
  }
  return(output_row)
}

route_competition_table <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                                    output = "06.Tables/Route_Competition_Table.tex"){
  db1b <- readRDS(input) %>% filter(Year >= 2018, Year != 2020) %>% as.data.table()
  
  # JetBlue Rows
  JetBlue_Spirit <- calculate_route_share(carrier = "JetBlue Airways", 
                                          competitor = "Spirit_Prescence",
                                          label = "Spirit", data = db1b)
  JetBlue_American <- calculate_route_share(carrier = "JetBlue Airways", 
                                          competitor = "American_Prescence",
                                          label = "American", data = db1b)
  JetBlue_Delta <- calculate_route_share(carrier = "JetBlue Airways", 
                                          competitor = "Delta_Prescence",
                                          label = "Delta", data = db1b)
  # Spirit Rows
  Spirit_JetBlue <- calculate_route_share(carrier = "Spirit Air Lines",
                                          competitor =  "JetBlue_Prescence",
                                          label = "JetBlue", data = db1b)
  Spirit_American <- calculate_route_share(carrier = "Spirit Air Lines",
                                           competitor =  "American_Prescence",
                                           label = "American", data = db1b)
  Spirit_Delta <- calculate_route_share(carrier = "Spirit Air Lines",
                                        competitor = "Delta_Prescence", data = db1b,
                                        label = "Delta")
  
  
  # Alaska Rows
  Alaska_JetBlue <- calculate_route_share(carrier = "Alaska Airlines Inc.",
                                          competitor = "JetBlue_Prescence",
                                          label = "JetBlue", data = db1b)
  Alaska_Spirit <- calculate_route_share(carrier = "Alaska Airlines Inc.",
                                         competitor = "Spirit_Prescence",
                                         label = "Spirit", data = db1b)
  Alaska_American <- calculate_route_share(carrier = "Alaska Airlines Inc.",
                                           competitor = "American_Prescence",
                                           label = "American", data = db1b)
  Alaska_Delta <- calculate_route_share(carrier = "Alaska Airlines Inc.",
                                        label = "Delta", competitor = "Delta_Prescence",
                                        data = db1b)
  
  # American Rows
  American_JetBlue <- calculate_route_share(carrier = "American Airlines Inc.",
                                            competitor = "JetBlue_Prescence",
                                            label = "JetBlue", data = db1b)
  American_Spirit <- calculate_route_share(carrier = "American Airlines Inc.",
                                          competitor = "Spirit_Prescence",
                                          label = "Spirit", data = db1b) 
  American_Alaskan <- calculate_route_share(carrier = "American Airlines Inc.",
                                            competitor = "Alaska_Prescence",
                                            label = "Alaska", data = db1b)
  American_Delta <- calculate_route_share(carrier = "American Airlines Inc.",
                                          competitor = "Delta_Prescence",
                                          label = "Delta", data = db1b)
  
  # Delta Rows
  Delta_JetBlue <- calculate_route_share(carrier = "Delta Air Lines Inc.",
                                         competitor = "JetBlue_Prescence",
                                         label = "JetBlue", data = db1b)
  Delta_Spirit <- calculate_route_share(carrier = "Delta Air Lines Inc.",
                                         competitor = "Spirit_Prescence", 
                                         label = "Spirit", data = db1b)
  Delta_Alaskan <- calculate_route_share(carrier = "Delta Air Lines Inc.",
                                         competitor = "Alaska_Prescence",
                                         label = "Alaska", data = db1b)
  Delta_American <- calculate_route_share(carrier = "Delta Air Lines Inc.",
                                          competitor = "American_Prescence",
                                          label = "American", data = db1b)
  
  out_tab <- rbind(JetBlue_Spirit, JetBlue_American, JetBlue_Delta,
                   Spirit_JetBlue, Spirit_American, Spirit_Delta,
                   American_JetBlue, American_Spirit, American_Alaskan, American_Delta,
                   Alaska_American, Alaska_Delta,
                   Delta_JetBlue, Delta_Spirit, Delta_Alaskan, Delta_American)
  rownames(out_tab) <- NULL
  out_tab <- as.data.table(out_tab)
  
  col.head <- c("", rep(c(1,2,3,4),4), 1, 2, 3)
  
  kbl(out_tab,
      format = "latex", col.names = col.head,
      escape = FALSE, booktabs = TRUE) %>%
    add_header_above(c(" " = 1, "2018" = 4, "2019" = 4,
                       "2021" = 4, "2022" = 4, "2023" = 3)) %>%
    pack_rows(group_label = "JetBlue", 1, 3) %>%
    pack_rows(group_label = "Spirit", 4, 6) %>%
    pack_rows(group_label = "American", 7, 10) %>%
    pack_rows(group_label = "Alaska", 11, 12) %>%
    pack_rows(group_label = "Delta", 13, 16) %>%
    save_kable(file = output)
}


summary_product <- function(input = "02.Intermediate/Product_Data.rds",
                                    output = "06.Tables/SummaryStatistics_Product_Revised.tex"){
  product_data <- readRDS(input)
  
  price_row <- five_statistic_row_make(name = "Price (2017 USD)", product_data$prices * 100)
  passengers_row <- five_statistic_row_make(name = "Passengers", product_data$Passengers.Product)
  mktMiles_row <- five_statistic_row_make(name = "Distance (1000s)", product_data$MktMilesFlown)
  exMiles_row <- five_statistic_row_make(name = "Extra Distance", product_data$Extra_Miles)
  nonstop_row <- five_statistic_row_make(name = "Nonstop", product_data$NonStop)
  origin_dest <- five_statistic_row_make(name = "Origin Destinations",
                                         product_data$Origin_Firm_Destinations)
  origin_prescence <- five_statistic_row_make(name = "Origin Prescence (\\%)",
                                              product_data$Origin_Firm_Service_Ratio)
  delta <- five_statistic_row_make(name = "Delta", product_data$Carrier == "Delta Air Lines Inc.")
  american <- five_statistic_row_make(name = "American", product_data$Carrier == "American Airlines Inc.")
  united <- five_statistic_row_make(name = "United", product_data$Carrier == "United Air Lines Inc.")
  southwest <- five_statistic_row_make(name = "Southwest", product_data$Carrier == "Southwest Airlines Co.")
  jetblue <- five_statistic_row_make(name = "JetBlue", product_data$Carrier == "JetBlue Airways")
  spirit <- five_statistic_row_make(name = "Spirit", product_data$Carrier == "Spirit Air Lines")
  minor_carrier <- five_statistic_row_make(name = "Other Carrier", product_data$Carrier == "Minor Carrier")
  obs_row <- c("Observations", nrow(product_data), "", "", "", "")
  
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  frame <- rbind(price_row, passengers_row,
                 mktMiles_row, exMiles_row, nonstop_row,
                 origin_dest, origin_prescence, delta,
                 american, united, southwest, jetblue,
                 spirit, minor_carrier, obs_row)
  
  kbl(frame,
      format = "latex", col.names = title_row,
      row.names = F,
      escape = FALSE, booktabs = TRUE,
      linesep = "") %>%
    save_kable(output)
}

summary_product_sp_jb_focus <- function(input = "02.Intermediate/Product_Data.rds",
                                        output = "06.Tables/SummaryStatistics_Product_FocusFirms.tex"){
  product_data <- readRDS(input)
  product_data.jb <- product_data[Carrier == "JetBlue Airways",]
  product_data.sp <- product_data[Carrier == "Spirit Air Lines",]
  
  price_row.jb <- five_statistic_row_make(name = "Price (2017 USD)", product_data.jb$prices * 100)
  passengers_row.jb <- five_statistic_row_make(name = "Passengers", product_data.jb$Passengers.Product)
  mktMiles_row.jb <- five_statistic_row_make(name = "Distance (1000s)", product_data.jb$MktMilesFlown)
  exMiles_row.jb <- five_statistic_row_make(name = "Extra Distance", product_data.jb$Extra_Miles)
  nonstop_row.jb <- five_statistic_row_make(name = "Nonstop", product_data.jb$NonStop)
  origin_dest.jb <- five_statistic_row_make(name = "Origin Destinations",
                                         product_data.jb$Origin_Firm_Destinations)
  origin_prescence.jb <- five_statistic_row_make(name = "Origin Prescence (%)",
                                              product_data.jb$Origin_Firm_Service_Ratio)
  obs_row.jb <- c("Observations", nrow(product_data.jb), "", "", "", "")
  
  
  price_row.sp <- five_statistic_row_make(name = "Price (2017 USD)", product_data.sp$prices * 100)
  passengers_row.sp <- five_statistic_row_make(name = "Passengers", product_data.sp$Passengers.Product)
  mktMiles_row.sp <- five_statistic_row_make(name = "Distance (1000s)", product_data.sp$MktMilesFlown)
  exMiles_row.sp <- five_statistic_row_make(name = "Extra Distance", product_data.sp$Extra_Miles)
  nonstop_row.sp <- five_statistic_row_make(name = "Nonstop", product_data.sp$NonStop)
  origin_dest.sp <- five_statistic_row_make(name = "Origin Destinations",
                                            product_data.sp$Origin_Firm_Destinations)
  origin_prescence.sp <- five_statistic_row_make(name = "Origin Prescence (\\%)",
                                                 product_data.sp$Origin_Firm_Service_Ratio)
  obs_row.sp <- c("Observations", nrow(product_data.sp), "", "", "", "")
  
  title_row<- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  title_row.sp <- c("Spirit", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  
  frame <- rbind(price_row.jb, passengers_row.jb,
                 mktMiles_row.jb, exMiles_row.jb, nonstop_row.jb,
                 origin_dest.jb, origin_prescence.jb, obs_row.jb,
                 price_row.sp, passengers_row.sp,
                 mktMiles_row.sp, exMiles_row.sp, nonstop_row.sp,
                 origin_dest.sp, origin_prescence.sp, obs_row.sp)
  
  kbl(frame,
      format = "latex", col.names = title_row,
      row.names = F,
      escape = FALSE, booktabs = TRUE,
      linesep = "") %>%
    group_rows(group_label = "JetBlue", start_row = 1, end_row = 8) %>%
    row_spec(row = 7, hline_after = TRUE) %>%
    group_rows(group_label = "Spirit", start_row = 9, end_row = 16) %>% 
    row_spec(row = 15, hline_after = TRUE) %>%
    save_kable(output)
}

summary_market <- function(input = "02.Intermediate/Product_Data.rds",
                                  output = "06.Tables/SummaryStatistics_Market.tex"){
  product_data <- readRDS(input)
  
  product_data.market <- product_data %>% group_by(Year, Quarter, Origin, Dest) %>%
    summarize(Tourism = max(Tourism), 
              Share.NonStop = mean(NonStop),
              MinMiles = min(MktMilesFlown),
              Num_Products_In_Market = mean(Num_Products_In_Market),
              Population.GeomMean = mean(sqrt(Origin.Population * Destination.Population)),
              Passengers.In.Market = mean(Passengers.Inside.Market),
              Southwest_Prescence = max(Southwest_Prescence),
              Delta_Prescence = max(Delta_Prescence),
              United_Prescence = max(United_Prescence),
              American_Prescence = max(American_Prescence),
              JetBlue_Prescence = max(JetBlue_Prescence),
              Spirit_Prescence = max(Spirit_Prescence))
  
  products.row <- five_statistic_row_make("Number of Products",
                                          product_data.market$Num_Products_In_Market) 
  nonstop.row <- five_statistic_row_make(name = "Share Nonstop Products", 
                                         vector = product_data.market$Share.NonStop)
  potential.row <- five_statistic_row_make("Market Size (Thousands)", 
                                           vector = product_data.market$Population.GeomMean / 1000)
  passengers.row <- five_statistic_row_make("Customers in Market", 
                                            product_data.market$Passengers.In.Market)
  miles.row <- five_statistic_row_make("Direct Distance", product_data.market$MinMiles)
  tourism.row <- five_statistic_row_make("Tourist Market",
                                         product_data.market$Tourism)
  delta <- five_statistic_row_make("Delta Prescence",
                                   product_data.market$Delta_Prescence)
  united <- five_statistic_row_make("United Presence",
                                    product_data.market$United_Prescence)
  american <- five_statistic_row_make("American Prescence", 
                                      product_data.market$American_Prescence)
  jetblue <- five_statistic_row_make("JetBlue Prescence",
                                     product_data.market$JetBlue_Prescence)
  southwest <- five_statistic_row_make("Southwest Prescence",
                                       product_data.market$Southwest_Prescence)
  spirit <- five_statistic_row_make("Spirit Prescence",
                                    product_data.market$Spirit_Prescence)
  obs <- c("Observations", nrow(product_data.market), "", "", "", "")
  
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  
  frame <- rbind(products.row, nonstop.row, potential.row, passengers.row, miles.row,
                 tourism.row, delta, united, american, southwest, jetblue, spirit, obs)
  kbl(frame,
      format = "latex", col.names = title_row,
      row.names = F,
      escape = FALSE, booktabs = TRUE,
      linesep = "") %>%
    row_spec(row = 12, hline_after = T) %>%
    save_kable(output)
}

summary_market_focus_firms <-  function(input = "02.Intermediate/Product_Data.rds",
                                        output = "06.Tables/SummaryStatistics_Market.Focus.tex"){
  product_data <- readRDS(input)
  
  product_data.market <- product_data %>% group_by(Year, Quarter, Origin, Dest) %>%
    summarize(Tourism = max(Tourism), 
              Share.NonStop = mean(NonStop),
              MinMiles = min(MktMilesFlown),
              Num_Products_In_Market = mean(Num_Products_In_Market),
              Population.GeomMean = mean(sqrt(Origin.Population * Destination.Population)),
              Passengers.In.Market = mean(Passengers.Inside.Market),
              Southwest_Prescence = max(Southwest_Prescence),
              Delta_Prescence = max(Delta_Prescence),
              United_Prescence = max(United_Prescence),
              American_Prescence = max(American_Prescence),
              JetBlue_Prescence = max(JetBlue_Prescence),
              Spirit_Prescence = max(Spirit_Prescence)) %>%
    as.data.table()
  
  jb.data <- product_data.market[JetBlue_Prescence == TRUE,]
  sp.data <- product_data.market[Spirit_Prescence == TRUE,]
  
  # JetBlue Rows
  products.jb <- five_statistic_row_make("Number of Products",
                                         jb.data$Num_Products_In_Market) 
  nonstop.jb <- five_statistic_row_make(name = "Share Nonstop Products", 
                                         vector = jb.data$Share.NonStop)
  potential.jb <- five_statistic_row_make("Market Size (Thousands)", 
                                           vector = jb.data$Population.GeomMean / 1000)
  passengers.jb<- five_statistic_row_make("Customers in Market", 
                                          jb.data$Passengers.In.Market)
  miles.jb <- five_statistic_row_make("Direct Distance", jb.data$MinMiles)
  tourism.jb <- five_statistic_row_make("Tourist Market",
                                        jb.data$Tourism)
  delta.jb <- five_statistic_row_make("Delta Prescence",
                                      jb.data$Delta_Prescence)
  united.jb <- five_statistic_row_make("United Presence",
                                       jb.data$United_Prescence)
  american.jb <- five_statistic_row_make("American Prescence", 
                                         jb.data$American_Prescence)
  jetblue.jb <- five_statistic_row_make("JetBlue Prescence",
                                        jb.data$JetBlue_Prescence)
  southwest.jb <- five_statistic_row_make("Southwest Prescence",
                                          jb.data$Southwest_Prescence)
  spirit.jb <- five_statistic_row_make("Spirit Prescence",
                                       jb.data$Spirit_Prescence)
  obs.jb <- c("Observations", nrow(jb.data), "", "", "", "")
  
  # Spirit Rows
  products.sp <- five_statistic_row_make("Number of Products",
                                         sp.data$Num_Products_In_Market) 
  nonstop.sp <- five_statistic_row_make(name = "Share Nonstop Products", 
                                        vector = sp.data$Share.NonStop)
  potential.sp <- five_statistic_row_make("Market Size (Thousands)", 
                                          vector = sp.data$Population.GeomMean / 1000)
  passengers.sp<- five_statistic_row_make("Customers in Market", 
                                          sp.data$Passengers.In.Market)
  miles.sp <- five_statistic_row_make("Direct Distance", sp.data$MinMiles)
  tourism.sp <- five_statistic_row_make("Tourist Market",
                                        sp.data$Tourism)
  delta.sp <- five_statistic_row_make("Delta Prescence",
                                      sp.data$Delta_Prescence)
  united.sp <- five_statistic_row_make("United Presence",
                                       sp.data$United_Prescence)
  american.sp <- five_statistic_row_make("American Prescence", 
                                         sp.data$American_Prescence)
  jetblue.sp <- five_statistic_row_make("JetBlue Prescence",
                                        sp.data$JetBlue_Prescence)
  southwest.sp <- five_statistic_row_make("Southwest Prescence",
                                          sp.data$Southwest_Prescence)
  spirit.sp <- five_statistic_row_make("Spirit Prescence",
                                       sp.data$Spirit_Prescence)
  obs.sp <- c("Observations", nrow(sp.data), "", "", "", "")
  
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  
  frame <- rbind(products.jb, nonstop.jb, potential.jb, passengers.jb, miles.jb,
                 tourism.jb, delta.jb, united.jb, american.jb, southwest.jb, 
                 jetblue.jb, spirit.jb, obs.jb,
                 products.sp, nonstop.sp, potential.sp, passengers.sp, miles.sp,
                 tourism.sp, delta.sp, united.sp, american.sp, southwest.sp, 
                 jetblue.sp, spirit.sp, obs.sp)
  kbl(frame,
      format = "latex", col.names = title_row,
      row.names = F,
      escape = FALSE, booktabs = TRUE,
      linesep = "") %>%
    group_rows(group_label = "JetBlue Markets", start_row = 1, end_row = 13) %>%
    row_spec(row = 12, hline_after = T) %>%
    group_rows(group_label = "Spirit Markets", start_row = 14, end_row = 26) %>%
    row_spec(row = 25, hline_after = T) %>%
    save_kable(output)
}
