db1b_direct_flight_over_time <- function(){
  db1b <- readRDS("02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds");
  
  db1b <- as.data.table(db1b)
  
  if(is.null(db1b$Carrier_Class)){
    db1b$Carrier_Entity <- db1b$RPCarrier
  } 
  
  if(is.null(db1b$Month)){
    db1b$Month <- 0
    db1b[Quarter == 1, Month := 1]
    db1b[Quarter == 2, Month := 4]
    db1b[Quarter == 3, Month := 7]
    db1b[Quarter == 4, Month := 10]
    
  }
  
  # Assign Carriers to Groups 
  db1b$Carrier_Class <- "NA";
  
  # Group Carriers by Type
  # All of these carriers start as Low Cost Carriers
  db1b[Carrier_Entity %in% c("Spirit Air Lines",
                             "Allegiant Air",
                             "Frontier Airlines Inc."), Carrier_Class := "Low Cost"]
  
  # They then transition to be ULCC
  db1b[Carrier_Entity == "Spirit Air Lines" & Year >= 2011, Carrier_Class := "Ultra Low Cost"]
  db1b[Carrier_Entity == "Spirit Air Lines" & Year == 2010 & Month >=4, Carrier_Class := "Ultra Low Cost"]
  db1b[Carrier_Entity == "Allegiant Air" & Year >= 2013, Carrier_Class := "Ultra Low Cost"]
  db1b[Carrier_Entity == "Allegiant Air" & Year == 2012 & Month >=4, Carrier_Class := "Ultra Low Cost"]
  db1b[Carrier_Entity == "Frontier Airlines Inc." & Year >= 2014, Carrier_Class := "Ultra Low Cost"]
  db1b[Carrier_Entity == "Frontier Airlines Inc." & Year == 2013 & Month >= 10, Carrier_Class := "Ultra Low Cost"]
  
  
  db1b[Carrier_Entity %in% c("Virgin America",
                             "JetBlue Airways",
                             "Southwest Airlines Co.",
                             "Alaska Airlines Inc."), Carrier_Class := "Low Cost"]
  db1b[Carrier_Entity %in% c("Delta Air Lines Inc.",
                             "US Airways Inc.", "United Air Lines Inc.",
                             "American Airlines Inc."), Carrier_Class := "Legacy"]
  
  db1b <- db1b %>% filter(Carrier_Class %in% c("Ultra Low Cost", "Low Cost", "Legacy"),
                          Year <= 2022) %>% # Keep only complete years
    group_by(Carrier_Class, Year) %>% summarize(Passengers = n() * 10 / 1000000)
  
  ggplot(db1b, aes(x = Year, y = Passengers, fill = Carrier_Class)) +
    geom_col() +scale_fill_manual(values = c("grey", "black", "darkgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Passenger Enplanements",
                                             fill = "Carrier Type") +
    scale_y_continuous(expand = c(0, 0))
  
  ggsave("05.Figures/Direct_Ridership.pdf", units = "in", height = 4, width = 8)
  
  write.csv(db1b, "03.Output/direct_flights_by_class.csv")
}

db1b_routes <- function(){
  db1b <- readRDS("02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds");
  airlines <- c("Spirit Air Lines", 
                "JetBlue Airways")
  
  db1b$Route <- paste(db1b$Origin, "To", db1b$Dest)
  
  db1b <- db1b %>% filter(RPCarrier %in% airlines)
  
  route_status <- db1b %>% select(Route, Year, Quarter,RPCarrier) %>% 
    unique() %>% group_by(RPCarrier, Year, Quarter) %>% 
    summarize(N_Routes = n()) %>%
    mutate(Quarter = factor(Quarter, levels = c(1, 2, 3, 4),
                            labels = c("January", "April", "July",
                                       "October")),
           Quarter = as.character(Quarter),
           Time = lubridate::my(paste(Quarter, Year, sep = "")))
  
  ggplot(route_status, aes(x = Time, y = N_Routes)) +
    geom_line(aes(color = RPCarrier)) + 
    labs(x = "Time", y = "Number of Routes") + 
    labs(color = "Carrier") + 
    scale_color_grey() +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_x_date(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3250))
  
  ggsave("05.Figures/DB1B/Route_Offerings.pdf", units = "in", 
         width = 8, height = 5)
  
}

firm_ridership <- function(){
  db1b <- readRDS("02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds");
  
  # Keep only major firms 
  db1b <- db1b %>% 
    filter(RPCarrier %in% c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc.",
    "JetBlue Airways", "Alaska Airlines Inc.", "Southwest Airlines Co.", 
    "Virgin America","Delta Air Lines Inc.", "American Airlines Inc.", "United Air Lines Inc."))
  
  db1b <- db1b %>% group_by(Year, Quarter, RPCarrier) %>%
    summarize(N_Passengers = n())
}


big_four_graph <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Condensed.rds",
                           output.ridership = "05.Figures/BigFour_Ridership_Graph.pdf",
                           output.revenue = "05.Figures/BigFour_Revenue_Graph.pdf"){
  db1b <- readRDS(input)
  
  db1b[, BigFour := Carrier %in% c("Delta Air Lines Inc.", 
                                   "United Air Lines Inc.", 
                                   "American Airlines Inc.",
                                   "Southwest Airlines Co.")]
  db1b[, Month := 0]
  db1b[Quarter == 1, Month := 2]
  db1b[Quarter == 2, Month := 5]
  db1b[Quarter == 3, Month := 8]
  db1b[Quarter == 4, Month := 11]
  
  db1b.s <- db1b %>% group_by(Year, Month, BigFour) %>%
    summarize(Passengers = sum(Passengers.Product)) %>%
    mutate(Date := my(paste(Month, Year)))
  
  ggplot(db1b.s, aes(x = Date, y = Passengers, fill = BigFour)) +
    geom_col(position = "fill") + theme_bw() + 
    labs(x = "Time", y = "% of Passenger Boardings") +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(name = "Big Four", values = c("black", "grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  ggsave(output.ridership, units = "in",
         width = 5, height = 3)
  
  db1b.s <- db1b %>% group_by(Year, Month, BigFour) %>%
    summarize(Revenue = sum(Passengers.Product * Avg.Fare, na.rm = TRUE)) %>%
    mutate(Date := my(paste(Month, Year)))
  
  ggplot(db1b.s, aes(x = Date, y = Revenue, fill = BigFour)) +
    geom_col(position = "fill") + theme_bw() + 
    labs(x = "Time", y = "% of Revenue") +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(name = "Big Four", values = c("black", "grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  ggsave(filename = output.revenue, units = "in",
         width = 5, height = 3)
  
}


legacy_southwest_other_graph <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Condensed.rds",
                           output.ridership = "05.Figures/LSO_Ridership_Graph.pdf",
                           output.revenue = "05.Figures/LSO_Revenue_Graph.pdf"){
  db1b <- readRDS(input)
  
  db1b[, Grouping := ""]
  db1b[Carrier %in% c("Delta Air Lines Inc.", 
                        "United Air Lines Inc.", 
                        "American Airlines Inc."), Grouping := "Legacy"]
  db1b[Carrier ==  "Southwest Airlines Co.", Grouping := "Southwest"]
  db1b[! Carrier %in% c("Delta Air Lines Inc.", 
                        "United Air Lines Inc.", 
                        "American Airlines Inc.",
                        "Southwest Airlines Co."), Grouping := "Other LCC"]
  db1b[, Grouping := factor(Grouping, levels = c("Other LCC", "Southwest", "Legacy"))]

  db1b[, Month := 0]
  db1b[Quarter == 1, Month := 2]
  db1b[Quarter == 2, Month := 5]
  db1b[Quarter == 3, Month := 8]
  db1b[Quarter == 4, Month := 11]
  
  db1b.s <- db1b %>% group_by(Year, Month, Grouping) %>%
    summarize(Passengers = sum(Passengers.Product)) %>%
    mutate(Date := my(paste(Month, Year)))
  
  ggplot(db1b.s, aes(x = Date, y = Passengers, fill = Grouping)) +
    geom_col(position = "fill") + theme_bw() + 
    labs(x = "Time", y = "% of Passenger Boardings") +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(name = " ", values = c("black", "darkgrey", "lightgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  ggsave(output.ridership, units = "in",
         width = 5, height = 3)
  
  db1b.s <- db1b %>% group_by(Year, Month, Grouping) %>%
    summarize(Revenue = sum(Passengers.Product * Avg.Fare, na.rm = TRUE)) %>%
    mutate(Date := my(paste(Month, Year)))
  
  ggplot(db1b.s, aes(x = Date, y = Revenue, fill = Grouping)) +
    geom_col(position = "fill") + theme_bw() + 
    labs(x = "Time", y = "% of Revenue") +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(name = " ", values = c("black", "darkgrey", "lightgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  ggsave(filename = output.revenue, units = "in",
         width = 5, height = 3)
}
