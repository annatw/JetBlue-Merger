merger_sim_graphs <- function(){
  graph_hhi()
  graph_hhi(pref = "LCC_",
            city_input = "02.Intermediate/HHI_City_LCC_Market.Rds",
            airport_input = "02.Intermediate/HHI_Airport_LCC_Market.Rds")
  market_concentration()
  market_concentration(Type = "LCC_",
        city_input = "02.Intermediate/HHI_City_LCC_Market.Rds",
        airport_input = "02.Intermediate/HHI_Airport_LCC_Market.Rds")
}

graph_hhi <- function(city_input = "02.Intermediate/HHI_City_Market.Rds", 
                      airport_input = "02.Intermediate/HHI_Airport_Market.Rds",
                      pref = NA){
  
  jetBlue_Spirit_Markets_Graph_Relative <- function(data, year, quarter, pref){
    data.par <- data %>% filter(Year == year, Quarter == quarter) %>%
      filter(JetBlue_Prescence == 1, Spirit_Prescence == 1) %>%
      select(Origin, Destination, HHI.Percent.Increase) %>%
      unique();
    
    ggplot(data.par, aes(x = HHI.Percent.Increase)) + 
      geom_histogram(binwidth = 0.025, boundary = 0) +
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = "HHI Percent Increase", y = "Number of Markets") +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0,0))
    
    
    if(is.na(pref)){
      ggsave(filename = paste("05.Figures/CityMarket_HHIPerGraph/", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
      
    } else {
      ggsave(filename = paste("05.Figures/", pref, "CityMarket_HHIPerGraph", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
    }
    
    data.raw <- data %>% filter(Year == year, Quarter == quarter) %>%
      filter(JetBlue_Prescence == 1, Spirit_Prescence == 1) %>%
      select(Origin, Destination, HHI.Increase) %>%
      unique();
    
    ggplot(data.raw, aes(x = HHI.Increase)) + 
      geom_histogram(binwidth = 250, boundary = 0) +
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = "HHI Increase", y = "Number of Markets") +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0,0))
    
    if(is.na(pref)){
      ggsave(filename = paste("05.Figures/CityMarket_HHIWholeGraph/", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
      
    } else {
      ggsave(filename = paste("05.Figures/", pref, "CityMarket_HHIWholeGraph", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
    }
  }
  
  jetBlue_Spirit_Airports_Graph_Relative <- function(data, year, quarter, pref){
    data.par <- data %>% filter(Year == year, Quarter == quarter) %>%
      filter(JetBlue_Prescence == 1, Spirit_Prescence == 1) %>%
      select(Origin, Destination, HHI.Percent.Increase) %>%
      unique();
    
    ggplot(data.par, aes(x = HHI.Percent.Increase)) + 
      geom_histogram(binwidth = 0.025, boundary = 0) +
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = "HHI Percent Increase", y = "Number of Markets") +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0,0))
    
    if(is.na(pref)){
      ggsave(filename = paste("05.Figures/AirportMarket_HHIPerGraph/", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
    } else{
      ggsave(filename = paste("05.Figures/", pref, "AirportMarket_HHIPerGraph/", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
    }
    
    
    data.raw <- data %>% filter(Year == year, Quarter == quarter) %>%
      filter(JetBlue_Prescence == 1, Spirit_Prescence == 1) %>%
      select(Origin, Destination, HHI.Increase) %>%
      unique();
    
    ggplot(data.raw, aes(x = HHI.Increase)) + 
      geom_histogram(binwidth = 250, boundary = 0) +
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = "HHI Increase", y = "Number of Markets") +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0,0))
    
    if(is.na(pref)){
      ggsave(filename = paste("05.Figures/AirportMarket_HHIWholeGraph/", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
    } else {
      ggsave(filename = paste("05.Figures/", pref, "AirportMarket_HHIWholeGraph/", "Y", year, "Q", quarter,
                              ".pdf", sep = ""), units = "in",
             width = 7, height = 5)
    }
    
    
  }
  
  
  city_data <- readRDS(city_input)
  colnames(city_data) <- gsub(pattern = ".City", replacement = "", x = colnames(city_data))
  
  airport_data <- readRDS(airport_input)
  colnames(airport_data) <- gsub(pattern = ".Port", replacement = "", x = colnames(airport_data))
  
  
  for(i in 2016:2022){
    for(j in 1:4){
      jetBlue_Spirit_Markets_Graph_Relative(city_data, year = i,
                                            quarter = j, pref)
      
      jetBlue_Spirit_Airports_Graph_Relative(airport_data, year = i, quarter = j, pref)
    }
    print(i)
  }
  
}

market_concentration <- function(city_input = "02.Intermediate/HHI_City_Market.Rds", 
                                 airport_input = "02.Intermediate/HHI_Airport_Market.Rds",
                                 Type = ""){
  
  # Restrict to Markets that Both Firms Operate In
  city_data <- as.data.table(readRDS(city_input)) %>% 
    filter(JetBlue_Prescence.City == 1, Spirit_Prescence.City == 1,
           Year == 2022, Quarter == 2) %>%
    select(Origin.City, Destination.City, HHI.City.Current, HHI.City.Simulation) %>%
    unique()
  
  airport_data <- as.data.table(readRDS(airport_input)) %>% 
    filter(JetBlue_Prescence.Port == 1, Spirit_Prescence.Port == 1,
           Year == 2022, Quarter == 2) %>%
    select(Origin, Destination, HHI.Port.Current, HHI.Port.Simulation) %>%
    unique()
  
  
  # Based on 2010 Merger Guidelines, Page 19
  city_data$Pre <- "NA"; city_data$Post <- "NA"
  city_data[HHI.City.Current < 1500, Pre := "Unconcentrated"]
  city_data[HHI.City.Current >= 1500, Pre := "Moderately Concentrated"]
  city_data[HHI.City.Current >= 2500, Pre := "Highly Concentrated"]
  city_data[HHI.City.Simulation < 1500, Post := "Unconcentrated"]
  city_data[HHI.City.Simulation >= 1500, Post := "Moderately Concentrated"]
  city_data[HHI.City.Simulation >= 2500, Post := "Highly Concentrated"]
  
  # Now, for Airport Data 
  airport_data$Pre <- "NA"; airport_data$Post <- "NA"
  airport_data[HHI.Port.Current < 1500, Pre := "Unconcentrated"]
  airport_data[HHI.Port.Current >= 1500, Pre := "Moderately Concentrated"]
  airport_data[HHI.Port.Current >= 2500, Pre := "Highly Concentrated"]
  airport_data[HHI.Port.Simulation < 1500, Post := "Unconcentrated"]
  airport_data[HHI.Port.Simulation >= 1500, Post := "Moderately Concentrated"]
  airport_data[HHI.Port.Simulation >= 2500, Post := "Highly Concentrated"]
  
  city_data <- melt(city_data, measure.vars = c("Pre",
                                                "Post"),
                    variable.name = "Period", value.name = "Market_Type")
  
  city_data <- as.data.table(city_data)
  city_data[Period == "Pre", Period := "Pre-Merger"]
  city_data[Period == "Post", Period := "Post-Merger"]
  
  
  ggplot(city_data, aes(x = Market_Type, fill = Period))  +
    geom_bar(position = "dodge") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Market Type", y = "Number of Markets") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 650)) +
    scale_fill_grey()
  
  ggsave(paste("05.Figures/", Type, "City_Concentration_Graph.pdf", sep = ""),
         units = "in", width = 3.5, height = 5)
  
  # Now, airport data
  airport_data <- melt(airport_data, measure.vars = c("Pre",
                                                "Post"),
                    variable.name = "Period", value.name = "Market_Type")
  
  airport_data <- as.data.table(airport_data)
  airport_data[Period == "Pre", Period := "Pre-Merger"]
  airport_data[Period == "Post", Period := "Post-Merger"]
  
  
  ggplot(airport_data, aes(x = Market_Type, fill = Period))  +
    geom_bar(position = "dodge") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Market Type", y = "Number of Markets") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 1300)) +
    scale_fill_grey()
  
  ggsave(paste("05.Figures/", Type, "Airport_Concentration_Graph.pdf", sep = ""),
         units = "in", width = 3.5, height = 5)
}
