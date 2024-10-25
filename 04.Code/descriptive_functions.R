determine_market_firm_counts <- function(input = "02.Intermediate/DB1B.T100_Merged.Rds"){
  merged_data <- readRDS(input) 
  
  # Following vector has firm classifications based on Bachwich and Wittman (2017)
  spirit_jetblue <- c("JetBlue Airways",
                      "Spirit Air Lines");
  ulcc <- c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc.")
  lcc <- c("JetBlue Airways", "Alaska Airlines Inc.", "Southwest Airlines Co.", "Virgin America")
  legacy <- c("Delta Air Lines Inc.", "American Airlines Inc.", "United Air Lines Inc.")
  
  # Function that counts number of firms operating a market in a given quarter. 
  count_firms <- function(data.input = merged_data, firmVec, result_name){
    count_generate <- data.input %>% filter(RPCarrier %in% firmVec) %>%
      select(RPCarrier, Origin.City, Destination.City, Quarter, Year) %>% unique() %>%
      group_by(Origin.City, Destination.City, Quarter,Year) %>% summarize(N = n())
    
    colnames(count_generate) <- c(colnames(count_generate)[1:4], result_name)
    
    data.input <- merge(data.input, count_generate, by = c("Origin.City", "Destination.City", "Quarter", 
                                                           "Year"),
                        all.x = TRUE)
    return(data.input)
  }
  
  # Calculate number of LCC, ULCC, Legacy Carriers in each market, as well as number of JB, Spirit
  
  merged_data <- count_firms(data = merged_data, firmVec = spirit_jetblue, result_name = "JBSpirit.Count"); gc()
  merged_data <- count_firms(data = merged_data, firmVec = ulcc, result_name = "ULCC.Count"); gc()
  merged_data <- count_firms(data = merged_data, firmVec = lcc, result_name = "LCC.Count"); gc()
  merged_data <- count_firms(data = merged_data, firmVec = legacy, result_name = "Legacy.Count"); gc()

  # Add 0 for NA cells
  merged_data <- as.data.table(merged_data)
  merged_data[is.na(JBSpirit.Count), JBSpirit.Count := 0]
  merged_data[is.na(ULCC.Count), ULCC.Count := 0]
  merged_data[is.na(LCC.Count), LCC.Count := 0]
  merged_data[is.na(Legacy.Count), Legacy.Count := 0]
  gc();
  
  main_tally <- merged_data %>% select(Destination.City, Origin.City, 
                                        Year, Quarter, JBSpirit.Count, 
                                        ULCC.Count, LCC.Count, Legacy.Count) %>%
    unique()
  
  write.csv(main_tally, "03.Output/TallyOfOperators.csv")
  
  # Determine all markets where the LCC share is at risk of being a monopoly.
  # For this, if Spirit and JetBlue both operate and no other ULCC, LCC carriers 
  # operate, keep
  at_risk_monopolize <- merged_data %>% select(Destination.City, Origin.City, 
                                         Year, Quarter, JBSpirit.Count, 
                                         ULCC.Count, LCC.Count, Legacy.Count) %>%
    filter(Year %in% c(2022), JBSpirit.Count == 2, 
           ULCC.Count == 1, LCC.Count == 1) %>% select(Quarter, Destination.City, Origin.City, JBSpirit.Count, 
                                                       ULCC.Count, LCC.Count, Legacy.Count) %>%  unique()
  
  write.csv(at_risk_monopolize, "03.Output/Low_Cost_Monopoly.csv")
  
  at_risk_monopolize.count <- at_risk_monopolize %>% group_by(Legacy.Count, Quarter) %>% summarize(N = n())
  
  write.csv(at_risk_monopolize.count, "03.Output/Low_Cost_Monopoly.Count.csv")
  
  monopoly <- merged_data %>% select(Destination.City, Origin.City, 
                                     Year, Quarter, JBSpirit.Count, 
                                     ULCC.Count, LCC.Count, Legacy.Count) %>%
    filter(Year %in% c(2022), JBSpirit.Count == 2, 
           ULCC.Count == 1, LCC.Count == 1, Legacy.Count == 0) %>% 
    select(Quarter,Destination.City, Origin.City, JBSpirit.Count, 
           ULCC.Count, LCC.Count, Legacy.Count) %>% 
    unique()
  
  write.csv(monopoly, "03.Output/Monopoly_All_Risk.csv")
  
  monopoly.count <- monopoly %>% group_by(Quarter) %>% summarize(N = n())
  write.csv(monopoly.count, "03.Output/Quarter_Monopoly.csv")
  gc();
}

determine_spirit_entry_risk <- function(input = "02.Intermediate/DB1B.T100_Merged.Rds"){
  merged_data <- readRDS(input) 
  
  spirit_routes <- merged_data %>% filter(RPCarrier == "Spirit Air Lines")
  
}

product_data_descriptive_plots <- function(input = "02.Intermediate/Product_Data.rds"){
  product_data <- read_rds(input)

  product_data <- product_data %>% select(prices, shares, Extra_Miles, MktMilesFlown,
                                   MktMilesFlown_Sq, Num_Products_In_Market,
                                   Origin_Firm_Service_Ratio, Destination_Firm_Service_Ratio,
                                  GasMiles) %>% as.data.table()
  
  for(i in 1:ncol(product_data)){
    current_column <- product_data[, ..i]
    colnames(current_column) <- "GraphVar"
    
    ggplot(current_column, aes(x = GraphVar)) +
      geom_density() +     
      theme(panel.background = element_blank(), 
       axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
       panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = colnames(product_data)[i])
    
    file_name <- paste("05.Figures/Product_Data_Plots/", colnames(product_data)[i], 
                       ".pdf")  
    
      ggsave(file_name, units = "in", width = 4.5, height = 3)
  }
  gc();
}

product_data_line_graphs <- function(input = "02.Intermediate/mix_output_file.rds"){
  product_data <- read_rds(input)
  
  product_data <- product_data %>% select(Year, Quarter, prices, shares, Extra_Miles, MktMilesFlown,
                                          MktMilesFlown_Sq, Num_Products_In_Market,
                                          Origin_Firm_Service_Ratio, Destination_Firm_Service_Ratio,
                                          GasMiles) %>% as.data.table()
  product_data[, Month := 0]
  product_data[Quarter == 1, Month := 2]
  product_data[Quarter == 2, Month := 5]
  product_data[Quarter == 3, Month := 8]
  product_data[Quarter == 4, Month := 11]
  product_data[, Date := my(paste(Month, Year))]
  product_data[, Month := NULL]
  product_data[, Quarter := NULL]
  product_data[, Year := NULL]
  
  for(i in 1:ncol(product_data)){
    print(colnames(product_data)[i])
    if(colnames(product_data)[i] != "Date"){
    current_column <- product_data %>% select(Date, colnames(product_data)[i]) %>%
      as.data.table()
    colnames(current_column) <- c("Date", "GraphVar")
    current_column[, GraphVar := mean(GraphVar, na.rm = TRUE), by = c("Date")]
    current_column <- unique(current_column)

    ggplot(current_column, aes(x = Date, y = GraphVar)) +
      geom_line() +     
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(y = colnames(product_data)[i])
    
    file_name <- paste("05.Figures/Product_Data_Line_Graphs/", colnames(product_data)[i], 
                       ".pdf")  
    
    ggsave(file_name, units = "in", width = 4.5, height = 3)
    }
  }
}

price_histograms <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds"){
  db1b <- readRDS(input)
  
  db1b[, Month := 0]
  db1b[Quarter == 1, Month := 2]
  db1b[Quarter == 2, Month := 5]
  db1b[Quarter == 3, Month := 8]
  db1b[Quarter == 4, Month := 11]
  db1b[, Date := my(paste(Month, Year))]
  
  # Rove High MktCoupons Count
  db1b <- db1b[MktCoupons <= 3,]

  # Remove Point Redemptions
  db1b <- db1b[MktFare > 15,]
  db1b <- db1b[MktFare <= 3500,]
  
  dates <- unique(db1b$Date)
  for(i in 1:length(dates)){
    db1b.current <- db1b[Date == dates[i], .(Date, MktFare)]
    
    ggplot(db1b.current, aes(x = MktFare)) +
      geom_density() +     
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "MktFare")
    
    file_name <- paste("05.Figures/MktFare_Density/", dates[i], 
                       ".pdf")  
    ggsave(file_name, units = "in", width = 5, height = 3)
    gc();
  }
}

overlap_graphs <- function(input_data = "02.Intermediate/DB1B_Round_Trip_Only.Rds",
          firms = c("JetBlue Airways", "Spirit Air Lines"),
          firm.prescences = c("JetBlue_Prescence", "Spirit_Prescence"),
          folder = "05.Figures/JB_Spirit_Overlap/",
          represented_cities_graph = "most_represented_cities.pdf",
          represented_cities_table = "06.Tables/JB_Spirit_most_represented_cities_table.tex",
          overlap_over_time_graph = "05.Figures/JB_Spirit_overlap_routes_over_time.pdf"){
  
  product_data <- read_rds(input_data) 
  product_data <- product_data[RPCarrier %in% firms]; gc()
  product_data <- product_data %>% group_by(Year, Quarter,
                                            Origin_MSA, Destination_MSA,
                                            across(all_of(firm.prescences))) %>%
    filter(NonStop == TRUE) %>%
    summarize(Passengers := sum(Passengers)) %>% as.data.table()
  
  # Small Airports on US VI and PR cause the code to trip up; remove them
  product_data <- product_data[!is.na(Origin_MSA)]
  product_data <- product_data[!is.na(Destination_MSA)]
  
  # Remove all Non-Overlapped Routes
  product_data.prescences <- product_data[, ..firm.prescences]
  valid_vec <- as.numeric(product_data.prescences[, 1] == 1 & product_data.prescences[, 2] == 1)
  product_data <- product_data[valid_vec == 1,]
  
  
  for(y in 2021:2023){
    for(q in 1:4){
      if(y == 2023 & q > 2){
        break;
      }
      
      product_data.restrict <- product_data[Year == y,]; gc(); 
      product_data.restrict <- product_data.restrict[Quarter == q,]; gc();
      cols_to_keep <- c("Origin_MSA", "Passengers")
      product_data.restrict <- product_data.restrict[, ..cols_to_keep]
      
      gc();
      product_data.restrict <- unique(product_data.restrict)
      colnames(product_data.restrict) <- c("City", "Passengers")
      product_data.restrict <- product_data.restrict[order(-Passengers),]
      product_data.restrict[, Rank := 1:nrow(product_data.restrict)]
      product_data.restrict[, Passenger.Share := Passengers / sum(Passengers) * 100]
      product_data.restrict <- product_data.restrict[Rank < 11,]
      
      product_data.restrict[,Rank := factor(Rank)]
      ggplot(product_data.restrict, aes(x = Rank, y = Passenger.Share)) +
        geom_col() +
        theme(panel.background = element_blank(), 
              axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 22)) +
        labs(x = "Rank", y = "Share of Originating Passengers") 
        ggsave(filename = paste(folder, "Y", y, "Q", q, "_", represented_cities_graph,
                                sep = ""), 
               units = "in", width = 5, height = 3)
    }
    gc();
  }
  
  # Now, Generate a Table for Each Period with the Share of Passengers 
  # for the Top Ten Overall Origin Cities
  # Restrict to Post-Pandemic Years
  product_data <- product_data[Year %in% c(2021, 2022, 2023),]
  product_data <- unique(product_data[, Destination_MSA := NULL])
  product_data <- unique(product_data[, Passengers := sum(Passengers), by = c("Year", "Quarter", "Origin_MSA")])
  product_data[, Share.Passengers := Passengers / sum(Passengers) * 100, by = c("Year", "Quarter")]
  product_data[, Rank := frank(-Share.Passengers), by = c("Year", "Quarter")]
  product_data[, Avg.Rank := mean(Rank), by = "Origin_MSA"]
  cities_table <- unique(product_data[, .(Origin_MSA, Avg.Rank)])
  cities_table <- cities_table[, City_Avg_Rank_Rank := frank(Avg.Rank)]
  cities_table <- cities_table[City_Avg_Rank_Rank <= 10]
  product_data <- product_data[Origin_MSA %in% cities_table$Origin_MSA,]
  product_data <- product_data[, .(Year, Quarter, Origin_MSA, Share.Passengers, Avg.Rank)];
  product_data[, time := paste(Year, "Q", Quarter)];
  product_data[, Quarter := NULL];
  product_data[, Year := NULL];
  product_data.wide <- reshape(product_data, idvar = c("Origin_MSA", "Avg.Rank"),
                               times = time, direction = "wide") %>% as.data.table()
  product_data.wide <- product_data.wide[order(Avg.Rank),]
  colnames(product_data.wide) <- c("Origin Market", "Rank", rep(c("Q1","Q2","Q3","Q4"),2), "Q1", "Q2", "Q3")
  
  product_data.wide[, Origin.City := substr(`Origin Market`, start = 1, 
            stop = regexpr(text = `Origin Market`,
                           pattern = "-") - 1)]
  product_data.wide[, Origin.State := substr(`Origin Market`, 
                                             start = regexpr(text = `Origin Market`,
                                                             pattern = ",") + 2,
                                             stop = regexpr(text = `Origin Market`,
                                                            pattern = ",") + 3)]
  product_data.wide[, `Origin Market` := paste(Origin.City, Origin.State, sep = ", ")]
  product_data.wide[`Origin Market` == "Washington, DC", `Origin Market` := "Washington DC"]
  product_data.wide[, Origin.City := NULL]
  product_data.wide[, Origin.State := NULL]
  
   kbl(product_data.wide, booktabs = T,
       format = "latex", digits = 1) %>%
     add_header_above(c(" ", " ", "2021" = 4, "2022" = 4, "2023" = 3)) %>%
     add_header_above(c(" ", " ", "Passenger Departure Share" = 11)) %>%
    save_kable(file = represented_cities_table)
}

graph_ridership <- function(){
  airlines <- c("Spirit Air Lines", 
                "JetBlue Airways")
  
  t100 <- readRDS("02.Intermediate/Compile_T100.Rds")  %>% 
    filter(Carrier_Entity %in% airlines)
  
  t100$Route <- paste(t100$Origin_City, "To", t100$Destination_City)
  
  route_status <- t100 %>% select(Route, Year, Month, Carrier_Entity) %>% 
    unique() %>% group_by(Route, Year, Month) %>% 
    summarize(N_Operators = n())
  
  t100 <- merge(t100, route_status, by.x = c("Route", "Year", "Month"),
                by.y = c("Route", "Year", "Month"), all.x = TRUE)
  
  t100$Time <- lubridate::my(paste(t100$Month, t100$Year, sep = " "))
  
  t100.summarized <- t100 %>% group_by(Time, Carrier_Entity, N_Operators) %>%
    summarize(passengers = sum(Passengers_Enplaned) / 1000) %>%
    mutate(Route_Type = paste(Carrier_Entity, N_Operators),
           Route_Type = factor(x = Route_Type, 
                               levels = c("JetBlue Airways 1",
                                          "JetBlue Airways 2",
                                          "Spirit Air Lines 1",
                                          "Spirit Air Lines 2"),
                               labels = c("JetBlue Only", "JetBlue Contested", 
                                          "Spirit Only", "Spirit Contested")),
           N_Operators = factor(x = N_Operators,
                                levels = c(1, 2),
                                labels = c("Non-Contested Routes",
                                           "Contested Routes")))
  
  ggplot(t100.summarized, aes(x = Time, y = passengers)) +
    geom_line(aes(linetype = N_Operators, color = Carrier_Entity)) + 
    labs(x = "Time", y = "Monthly Boardings (Thousands)") + 
    labs(linetype = "Route Type", color = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2007", "April 2023"))) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2750))
  
  ggsave("05.Figures/T100/Ridership_Graph.pdf", units = "in", 
         width = 8, height = 5)
  
}

graph_routes <- function(){
  airlines <- c("Spirit Air Lines", 
                "JetBlue Airways")
  
  t100 <- readRDS("02.Intermediate/Compile_T100.Rds")  %>% 
    filter(Carrier_Entity %in% airlines)
  
  t100$Time <- lubridate::my(paste(t100$Month, t100$Year, sep = " "))
  
  
  t100$Route <- paste(t100$Origin_Alpha, "To", t100$Destination_Alpha)
  
  t100.RouteCount <- t100 %>% 
    select(Route, Time, Carrier_Entity) %>%
    unique() %>% group_by(Time, Carrier_Entity) %>%
    summarize(N = n())
  
  
  ggplot(t100.RouteCount, aes(x = Time, y = N)) +
    geom_line(aes(color = Carrier_Entity)) + 
    labs(x = "Time", y = "Number of Routes") + 
    labs(color = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2007", "April 2023"))) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 600))
  
  ggsave("05.Figures/T100/Route_Graph.pdf", units = "in", 
         width = 8, height = 5)
}


carrier_group_ridership <- function(){
  t100 <- as.data.table(readRDS("02.Intermediate/Compile_T100.Rds"))
  
  # Assign Carriers to Groups 
  t100$Carrier_Class <- "NA";
  
  # Group Carriers by Type
  # All of these carriers start as Low Cost Carriers
  t100[Carrier_Entity %in% c("Spirit Air Lines",
                             "Allegiant Air",
                             "Frontier Airlines Inc."), Carrier_Class := "Low Cost"]
  
  # They then transition to be ULCC
  t100[Carrier_Entity == "Spirit Air Lines" & Year >= 2011, Carrier_Class := "Ultra Low Cost"]
  t100[Carrier_Entity == "Spirit Air Lines" & Year == 2010 & Month >=4, Carrier_Class := "Ultra Low Cost"]
  t100[Carrier_Entity == "Allegiant Air" & Year >= 2013, Carrier_Class := "Ultra Low Cost"]
  t100[Carrier_Entity == "Allegiant Air" & Year == 2012 & Month >=4, Carrier_Class := "Ultra Low Cost"]
  t100[Carrier_Entity == "Frontier Airlines Inc." & Year >= 2014, Carrier_Class := "Ultra Low Cost"]
  t100[Carrier_Entity == "Frontier Airlines Inc." & Year == 2013 & Month >= 10, Carrier_Class := "Ultra Low Cost"]
  
  
  t100[Carrier_Entity %in% c("Virgin America",
                             "JetBlue Airways",
                             "Southwest Airlines Co.",
                             "Alaska Airlines Inc."), Carrier_Class := "Low Cost"]
  t100[Carrier_Entity %in% c("Delta Air Lines Inc.",
                             "US Airways Inc.", "United Air Lines Inc.",
                             "American Airlines Inc."), Carrier_Class := "Legacy"]
  
  t100 <- t100 %>% filter(Carrier_Class %in% c("Ultra Low Cost", "Low Cost", "Legacy"),
                          Year <= 2022) %>% # Keep only complete years
    mutate(Passengers_Enplaned = Passengers_Enplaned / 1000000) %>%
    group_by(Carrier_Class, Year) %>% 
    summarize(Passengers_Enplaned = sum(Passengers_Enplaned)) 
  
  t100$Carrier_Class <- factor(x = t100$Carrier_Class, levels = c("Legacy", "Low Cost", "Ultra Low Cost"))
  
  ggplot(t100, aes(x = Year, y = Passengers_Enplaned, fill = Carrier_Class)) +
    geom_col() +scale_fill_manual(values = c("grey", "black", "darkgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Passenger Enplanements (Millions)",
                                             fill = "Carrier Type") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 750))
  
  
  ggsave("05.Figures/T100/Industry_Ridership_Graph.pdf", units = "in", height = 4, width = 8)
  
  # Now, Enplanements By Year
  summary <- t100 %>% group_by(Year, Carrier_Class) %>% summarize(Enplanements = sum(Passengers_Enplaned))
  write.csv(summary, "03.Output/enplanements_national_by_class.csv")
}

graph_route_ridership <- function(){
  t100 <- as.data.table(readRDS("02.Intermediate/Compile_T100.Rds"))
  t100.spjb <- t100 %>% 
    filter(Carrier_Entity %in% c("JetBlue Airways","Spirit Air Lines")) %>%
    filter(Year == 2022, Month == 4) %>%
    mutate(Carrier_Entity = as.character(Carrier_Entity))
  
  ggplot(t100.spjb, aes(x = Passengers_Enplaned)) +
    geom_histogram() + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Monthly Passengers", y = "Occurences") +
    facet_grid(cols = vars(Carrier_Entity))
  
  
  ggsave("05.Figures/T100/route_ridership.pdf", units = "in",
         width = 8, height = 5)
}

firm_revenue_graphs <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds"){
  db1b_data <- readRDS(input)
  
  firms <- unique(db1b_data$Carrier)
  firms <- firms[firms %in% c("Allegiant Air", "American Airlines Inc.",
                              "Delta Air Lines Inc.", "United Air Lines Inc.",
                              "Alaska Airlines Inc.", "Frontier Airlines Inc.",
                              "JetBlue Airways", "Spirit Air Lines", 
                              "Hawaiian Airlines Inc.", 
                              "Southwest Airlines Co.")]
  
  # First, Graph of Revenue over Time
  db1b_data[, Carrier.Type := "TBD"]
  db1b_data[Carrier %in% c("American Airlines Inc.", "Delta Air Lines Inc.", "United Air Lines Inc."), 
        Carrier.Type := "Legacy"]
  db1b_data[Carrier %in% c("JetBlue Airways", "Hawaiian Airlines Inc.", "Southwest Airlines Co.", "Alaska Airlines Inc."),
        Carrier.Type := "LCC"]
  db1b_data[Carrier %in% c("Allegiant Air", "Frontier Airlines Inc.", "Spirit Air Lines"),
        Carrier.Type := "ULCC"]
  db1b_data <- db1b_data[Carrier.Type != "TBD",]
  
  db1b_data <- db1b_data[,.(Year, Quarter, Carrier, Carrier.Type, MktFare, Passengers)]; gc();
  
  # Generate data for each Carrier Type in each Quarter, Year
  category.revenue <- db1b_data %>% group_by(Year, Quarter, Carrier.Type) %>%
    summarize(Revenue := sum(Passengers * MktFare, na.rm = TRUE)) %>%
    mutate(Revenue := Revenue / 1000000,
           QuarterDate := 2 + 3 * (Quarter - 1),
           Date := mdy(paste(QuarterDate, "/" , "1", "/", Year, sep = ""))) 

  # Graph
  ggplot(category.revenue, aes(x = Date, y = Revenue, group = Carrier.Type)) +
    geom_line(aes(linetype = Carrier.Type)) + 
    labs(x = "Time", y = "Revenue (Millions USD)") + 
    labs(linetype = "Carrier Type") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2016", "August 2023"))) + 
    scale_y_continuous(expand = c(0, 0))
  ggsave(filename = "05.Figures/Revenue_FirmType.pdf", width = 7, height = 4, units = "in")

  # Now, For Each Subtype of Firm
  firm.revenue <- db1b_data %>% group_by(Year, Quarter, Carrier.Type, Carrier) %>%
    summarize(Revenue := sum(Passengers * MktFare, na.rm = TRUE)) %>%
    mutate(Revenue := Revenue / 1000000,
           QuarterDate := 2 + 3 * (Quarter - 1),
           Date := mdy(paste(QuarterDate, "/" , "1", "/", Year, sep = ""))) %>%
    data.table()
  
  # First, ULCC
  ulcc.revenue <- firm.revenue[Carrier.Type == "ULCC",]
  ggplot(ulcc.revenue, aes(x = Date, y = Revenue, group = Carrier)) +
    geom_line(aes(linetype = Carrier)) + 
    labs(x = "Time", y = "Revenue (Millions USD)") + 
    labs(linetype = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2016", "August 2023"))) + 
    scale_y_continuous(expand = c(0, 0))
  ggsave(filename = "05.Figures/Revenue_ULCC.pdf", width = 7, height = 4, units = "in")
  
  lcc.revenue <- firm.revenue[Carrier.Type == "LCC",]
  ggplot(lcc.revenue, aes(x = Date, y = Revenue, group = Carrier)) +
    geom_line(aes(linetype = Carrier)) + 
    labs(x = "Time", y = "Revenue (Millions USD)") + 
    labs(linetype = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2016", "August 2023"))) + 
    scale_y_continuous(expand = c(0, 0))
  ggsave(filename = "05.Figures/Revenue_LCC.pdf", width = 7, height = 4, units = "in")
  
  legacy.revenue <- firm.revenue[Carrier.Type == "Legacy",]
  ggplot(legacy.revenue, aes(x = Date, y = Revenue, group = Carrier)) +
    geom_line(aes(linetype = Carrier)) + 
    labs(x = "Time", y = "Revenue (Millions USD)") + 
    labs(linetype = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2016", "August 2023"))) + 
    scale_y_continuous(expand = c(0, 0))
  ggsave(filename = "05.Figures/Revenue_Legacy.pdf", width = 7, height = 4, units = "in")
  
  bigFourRevenue <- firm.revenue[Carrier.Type == "Legacy" | Carrier == "Southwest Airlines Co."]
  ggplot(bigFourRevenue, aes(x = Date, y = Revenue, group = Carrier)) +
    geom_line(aes(linetype = Carrier)) + 
    labs(x = "Time", y = "Revenue (Millions USD)") + 
    labs(linetype = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2016", "August 2023"))) + 
    scale_y_continuous(expand = c(0, 0))
  ggsave(filename = "05.Figures/Revenue_Big4.pdf", width = 7, height = 4, units = "in")
  
  spirit_jb_revenue <- firm.revenue[Carrier %in% c("JetBlue Airways",
                                                   "Spirit Air Lines")]
  ggplot(spirit_jb_revenue, aes(x = Date, y = Revenue, group = Carrier)) +
    geom_line(aes(linetype = Carrier)) + 
    labs(x = "Time", y = "Revenue (Millions USD)") + 
    labs(linetype = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0), limits = my(c("January 2016", "August 2023"))) + 
    scale_y_continuous(expand = c(0, 0))
  ggsave(filename = "05.Figures/Revenue_JB_Spirit.pdf", width = 7, height = 4, units = "in")
}

t100_graphs <- function(){
  graph_ridership()
  carrier_group_ridership()
  graph_routes()
  graph_route_ridership()
}

correlation_matrix <- function(data_in = "02.Intermediate/Product_Data.rds"){
  product_data <- readRDS(data_in)
  # First, Core Variables
  product_data.core <- product_data[, .(prices, NonStop, MktMilesFlown, MktMilesFlown_Sq, 
                                        Origin_Firm_Service_Ratio, Extra_Miles, Tourism)]
  round(cor(product_data.core, method = "pearson"), digits = 2)
  
  product_data.exog_interactions <- product_data[,.(MktMiles_NonStop,
                MktMiles_Sq_NonStop,NonStop_Origin_Share,NonStop_ExtraMiles,
                MktMiles_Sq_Interact, MktMiles_Origin_Share,
                MktMiles_Extra, MktMiles_Sq_Origin_Share,MktMiles_Sq_Extra,OriginRatio_Extra)]

  round(cor(product_data.exog_interactions, method = "pearson"), digits = 2)  

  corrplot(cor(product_data.exog_interactions, method = "pearson"), type = "upper")
  
  corrplot(cor(product_data[, .(demand_instruments0,demand_instruments1,demand_instruments2,demand_instruments3,            
                                demand_instruments4, demand_instruments5,demand_instruments6,            
                                demand_instruments7,demand_instruments8,demand_instruments9,            
                                demand_instruments10)], method = "pearson"), type = "upper")
  
  corrplot(cor(product_data[,.(Origin_Hub, MktMiles_OriginHub, MktMilesSq_OriginHub, NonStop_OriginHub,
                               Destination_Hub,MktMiles_DestinationHub, 
                               MktMilesSq_DestinationHub, NonStop_DestinationHub)]), type= "upper")
  
  corrplot(cor(product_data[,.()], method = "pearson"), type = "upper")
}
