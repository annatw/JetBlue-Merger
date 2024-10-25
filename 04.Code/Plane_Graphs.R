plane_class <- function(data){
  # Assign Carriers to Groups 
  data$Carrier_Class <- "NA";
  
  # Group Carriers by Type
  # All of these carriers start as Low Cost Carriers
  data[Carrier %in% c("Spirit Air Lines",
                      "Allegiant Air",
                      "Frontier Airlines Inc."), Carrier_Class := "Low Cost"]
  
  # They then transition to be ULCC
  data[Carrier == "Spirit Air Lines" & Year >= 2011, Carrier_Class := "Ultra Low Cost"]
  data[Carrier == "Spirit Air Lines" & Year == 2010, Carrier_Class := "Ultra Low Cost"]
  data[Carrier == "Allegiant Air" & Year >= 2013, Carrier_Class := "Ultra Low Cost"]
  data[Carrier == "Allegiant Air" & Year == 2012, Carrier_Class := "Ultra Low Cost"]
  data[Carrier == "Frontier Airlines Inc." & Year >= 2014, Carrier_Class := "Ultra Low Cost"]
  data[Carrier == "Frontier Airlines Inc." & Year == 2013, Carrier_Class := "Ultra Low Cost"]
  
  
  data[Carrier %in% c("Virgin America",
                      "JetBlue Airways",
                      "Southwest Airlines Co.",
                      "Alaska Airlines Inc."), Carrier_Class := "Low Cost"]
  data[Carrier %in% c("Delta Air Lines Inc.",
                      "US Airways Inc.", "United Air Lines Inc.",
                      "American Airlines Inc."), Carrier_Class := "Legacy"]
  
  data$Carrier_Class <- factor(data$Carrier_Class, levels = c("Legacy", "Low Cost", "Ultra Low Cost"))
  
  data <- data %>% filter(!is.na(Carrier_Class))
  
  return(data)
}


planes_by_class <- function(input = "02.Intermediate/Fleet_Compilation.rds", target.Planes = "05.Figures/fleet_sizes.pdf",
                            target.Seats = "05.Figures/seat_counts.pdf",
                            target.ratio = "05.Figures/seat_ratios.pdf"){
  planes <- readRDS(input);
  
  planes <- as.data.table(planes);
  
  planes <- plane_class(planes)
  
  plane.census <- planes %>% filter(Carrier_Class != "NA") %>%  group_by(Carrier_Class, Year) %>% summarize(Inventory = n())
  
  ggplot(plane.census, aes(x = Year, y = Inventory, fill = Carrier_Class)) +
    geom_col() +scale_fill_manual(values = c("grey", "black", "darkgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Plane Inventory",
                                             fill = "Carrier Type") +
    scale_y_continuous(expand = c(0, 0))
  
  ggsave(target.Planes, units = "in", height = 3, width = 7)
  
  seat.census <- planes %>% filter(Carrier_Class != "NA") %>% group_by(Carrier_Class, Year) %>% summarize(Seats = sum(NumberSeats) / 1000)
  
  ggplot(seat.census, aes(x = Year, y = Seats, fill = Carrier_Class)) +
    geom_col() +scale_fill_manual(values = c("grey", "black", "darkgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Number of Seats (Thousands)",
                                             fill = "Carrier Type") +
    scale_y_continuous(expand = c(0, 0))
  
  ggsave(target.Seats, units = "in", height = 3, width = 7)
  
  ratios <- planes %>% filter(Carrier_Class != "NA") %>% group_by(Carrier_Class, Year) %>% summarize(Seats = sum(NumberSeats),
                                                                                                     Inventory = n()) %>%
    mutate(ratio = Seats/Inventory)
  
  ggplot(ratios, aes(x = Year, y = ratio, color = Carrier_Class)) +
    geom_line() +scale_color_manual(values = c("grey", "black", "darkgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Average Seats Per Plane",
                                             color = "Carrier Type") +
    scale_y_continuous(expand = c(0, 0), limits = c(120, 200))
  
  ggsave(target.ratio, units = "in", height = 3, width = 7)
}

spirit_planes <- function(input = "02.Intermediate/Fleet_Compilation.rds"){
  planes <- readRDS(input);
  
  planes <- as.data.table(planes);
  
  planes <- planes %>% filter(Carrier == "Spirit Air Lines")
  
  plane.census <- planes %>%  group_by(Year) %>% summarize(Inventory = n())
  
  ggplot(plane.census, aes(x = Year, y = Inventory)) +
    geom_col() + scale_fill_manual(values = c("grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Plane Inventory") +
    scale_y_continuous(expand = c(0, 0), limits = c(0,225))
  
  ggsave("05.Figures/Spirit_Planes.pdf", units = "in", width = 7, height = 3)
  
  seat.census <- planes %>% group_by(Year) %>% summarize(Seats = sum(NumberSeats) / 1000)
  
  ggplot(seat.census, aes(x = Year, y = Seats)) +
    geom_col() +scale_fill_manual(values = c("grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Number of Seats (Thousands)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 37.5))
  
  ggsave("05.Figures/Spirit_Seats.pdf", units = "in", width = 7, height = 3)
  
  seat.ratio <- planes %>% group_by(Year) %>% summarize(Inventory = n(),
                                                        Seats = sum(NumberSeats)) %>%
    mutate(Seat.Ratio = Seats/Inventory)
  
  ggplot(seat.ratio, aes(x = Year, y = Seat.Ratio)) +
    geom_line() +scale_fill_manual(values = c("grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Average Seats Per Plane") +
    scale_y_continuous(expand = c(0, 0), limits = c(120,225))
  
  ggsave("05.Figures/Spirit_Seat_ratio.pdf", units = "in", width = 7, height = 3)
}

jetblue_planes <- function(input = "02.Intermediate/Fleet_Compilation.rds"){
  planes <- readRDS(input);
  
  planes <- as.data.table(planes);
  
  planes <- planes %>% filter(Carrier == "JetBlue Airways")
  
  plane.census <- planes %>%  group_by(Year) %>% summarize(Inventory = n())
  
  ggplot(plane.census, aes(x = Year, y = Inventory)) +
    geom_col() + scale_fill_manual(values = c("grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Plane Inventory") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 325))
  
  ggsave("05.Figures/JetBlue_Planes.pdf", units = "in", width = 7, height = 3)
  
  seat.census <- planes %>% group_by(Year) %>% summarize(Seats = sum(NumberSeats) / 1000)
  
  ggplot(seat.census, aes(x = Year, y = Seats)) +
    geom_col() +scale_fill_manual(values = c("grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Number of Seats (Thousands)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 52.5))
  
  ggsave("05.Figures/JetBlue_Seats.pdf", units = "in", width = 7, height = 3)
  
  seat.ratio <- planes %>% group_by(Year) %>% summarize(Inventory = n(),
                                                        Seats = sum(NumberSeats)) %>%
    mutate(Seat.Ratio = Seats/Inventory)
  
  ggplot(seat.ratio, aes(x = Year, y = Seat.Ratio)) +
    geom_line() +scale_fill_manual(values = c("grey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Average Seats Per Plane") +
    scale_y_continuous(expand = c(0, 0), limits = c(120,175))
  
  ggsave("05.Figures/JetBlue_Seat_Ratio.pdf", units = "in", width = 7, height = 3)
  
}

plane_types <- function(){
  planes <- readRDS("02.Intermediate/Fleet_Compilation.rds");
  planes <- as.data.table(planes)
  planes <- plane_class(planes) # Assign Each Plane to a Carrier Type
  
  jb_spirit <- planes %>% filter(Carrier %in% c("JetBlue Airways", "Spirit Air Lines")) %>%
    group_by(Year, Carrier, NumberSeats, Manufacturer, Model) %>%
    summarize(N = n())
  
  write.csv(jb_spirit, "03.Output/jbSpirit_Fleets.csv")
  
  planes <- planes %>% select(Year, Carrier, Carrier_Class,
                              NumberSeats, Manufacturer) %>%
    unique() %>% group_by(Year, Carrier, Carrier_Class) %>%
    summarize(N = n()) 
  
  write.csv(planes, "03.Output/plane_types_in_use.csv")
  
  plane_industry <- planes %>%
    group_by(Year, Carrier_Class) %>%
    summarize(Avg_Types = mean(N))
  
  ggplot(plane_industry, aes(x = Year, y = Avg_Types, fill = Carrier_Class)) +
    geom_col(position = "dodge") + 
    scale_fill_manual(values = c("grey", "black", "darkgrey")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Year", y = "Average # Plane Models",
                                             fill = "Carrier Type") +
    scale_y_continuous(expand = c(0, 0))
  
  ggsave("05.Figures/average_plane_types_in_use.pdf",
         units = "in", width = 7, height = 3)
}

plane_graphs <- function(){
  planes_by_class(); gc();
  spirit_planes(); gc();
  jetblue_planes(); gc();
  plane_types(); gc()
}
