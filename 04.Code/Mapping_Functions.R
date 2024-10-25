make_passenger_maps<- function(){
  T100 <- readRDS("02.Intermediate/Compile_T100.Rds")
  passenger_map(airlines = "JetBlue Airways", years = 2012, data = T100, filename = "JetBlue2012.pdf")
  passenger_map(airlines = "JetBlue Airways", years = 2022, data = T100, filename = "JetBlue2022.pdf")
  passenger_map(airlines = "Spirit Air Lines", years = 2012, data = T100, filename = "Spirit2012.pdf")
  passenger_map(airlines = "Spirit Air Lines", years = 2022, data = T100, filename = "Spirit2022.pdf")
  passenger_map(airlines = c("JetBlue Airways", "Spirit Air Lines"),
                years = c(2012), data = T100, filename = "Both_2012.pdf")
  passenger_map(airlines = c("JetBlue Airways", "Spirit Air Lines"),
                years = c(2022), data = T100, filename = "Both_2022.pdf")
  passenger_map(airlines = c("JetBlue Airways"),
                years = c(2012, 2022), data = T100, filename = "JetBlue_Both.pdf")
  passenger_map(airlines = c("Spirit Air Lines"),
                years = c(2012, 2022), data = T100, filename = "Spirit_Both.pdf")
}


passenger_map <- function(airlines, years, data, filename){

  # Local Functions
  # Standardizes the filename for Mainland, Carribean Maps respectively
  prep_name <- function(filename){
    name1 <- paste("05.Figures/Map_Mainland_", filename, sep = "")
    name2 <-paste("05.Figures/Map_PRVI_", filename, sep = "")
    return(c(name1, name2))
  }
  
  # Removes non-mainland observations from the data set.
  make_mainland_data <- function(data){
    data <- data %>% filter(grepl(x = Origin_City, pattern = "PUERTO RICO") == FALSE,
                        grepl(x = Origin_City, pattern = " PR") == FALSE,
                        grepl(x = Origin_City, pattern = "VIRGIN ISLANDS") == FALSE,
                        grepl(x = Origin_City, pattern = " VI") == FALSE,
                        grepl(x = Origin_City, pattern = "ALASKA") == FALSE,
                        grepl(x = Origin_City, pattern = " AK") == FALSE)
    return(data)
  }
  
  # Function that returns Puerto Rico, Virgin Islands Entities
  make_pr_data <- function(data){
    data.pr <- data %>% filter(grepl(x = Origin_City, pattern = "PUERTO RICO") == TRUE | grepl(x = Origin_City, pattern = " PR") == TRUE,
                            grepl(x = Origin_City, pattern = "ALASKA") == FALSE,
                            grepl(x = Origin_City, pattern = " AK") == FALSE)
    data.vi <- data %>% filter(grepl(x = Origin_City, pattern = "VIRGIN ISLANDS") == TRUE | grepl(x = Origin_City, pattern = " VI") == TRUE,
                               grepl(x = Origin_City, pattern = "ALASKA") == FALSE,
                               grepl(x = Origin_City, pattern = " AK") == FALSE)
    data <- rbind(data.pr, data.vi)
    return(data)
  }
  

  # Read in, adjust state file
  state_data <- purrr::quietly(sf::st_read)("01.Input/05.StateBoundaryData/cb_2018_us_state_500k.shp")$result
  state_data <- state_data %>% filter(!STUSPS %in% c("AK", "HI", "GU", "MP", "AS"))
  state_data.mainland <- state_data %>% filter(!STUSPS %in% c("PR", "VI"))
  state_data.PR <- state_data %>% filter(STUSPS %in% c("PR"))
  state_data.mainland <- fortify(state_data.mainland)
  state_data.PR <- fortify(state_data.PR)
  
  T100 <- readRDS("02.Intermediate/Compile_T100.Rds")
  T100 <- T100 %>% filter(Year %in% years,
                          Carrier_Entity %in% airlines)
  
  T100$Carrier_Entity <- as.character(T100$Carrier_Entity)
  
  if(length(airlines) == 2 & length(years) == 2){
    
  } else if(length(airlines) == 1 & length(years) == 2){
    T100_freq <- T100 %>% group_by(Origin_Numeric, Year) %>% 
      select(Origin_Numeric, Year) %>%
      unique() %>% group_by(Origin_Numeric) %>%
      summarize(N = n())
    
    T100 <- merge(T100, T100_freq, 
                  by.x = c("Origin_Numeric"),
                  by.y = c("Origin_Numeric"),
                  all.x = TRUE)
    
    T100 <- T100 %>% mutate(Incidence = paste(Year, N))
    T100 <- as.data.table(T100)
    T100[Incidence == paste(years[1], "1"), Incidence := paste(years[1], "Only")]
    T100[Incidence == paste(years[2], "1"), Incidence := paste(years[2], "Only")]
    T100[Incidence == paste(years[1], "2"), Incidence := paste(years[1], "and", years[2])]
    T100[Incidence == paste(years[2], "2"), Incidence := paste(years[1], "and", years[2])]
  
    T100[,Years := factor(Incidence)]
    
    T100.Mainland <- make_mainland_data(T100)
    
    ggplot(data = state_data.mainland) + 
      geom_sf(fill = "white") + 
      geom_point(T100.Mainland, mapping = aes(x = Origin_lon, y = Origin_lat, color = Years), size = 0.5) +
      scale_color_manual(values = c("black", "cornflowerblue", "burlywood")) + 
      labs(x = "", y = "") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position = "bottom")
    ggsave(prep_name(filename)[1], width = 6, height = 4, units = "in")
    
    # Now for PR, VI
    T100.PR <- make_pr_data(T100)
    ggplot(data = state_data.PR) + 
      geom_sf(fill = "white") + 
      geom_point(T100.PR, mapping = aes(x = Origin_lon, y = Origin_lat, color = Years), size = 0.5) +
      scale_color_manual(values = c("black", "cornflowerblue", "burlywood")) + 
      labs(x = "", y = "") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position = "bottom")
    ggsave(prep_name(filename)[2], width = 6, height = 4, units = "in")
    
    
  } else if(length(years) == 1 & length(airlines) == 2){
    T100_freq <- T100 %>% group_by(Origin_Numeric, Carrier_Entity) %>%
      select(Origin_Numeric, Carrier_Entity) %>%
      unique() %>% group_by(Origin_Numeric) %>%
      summarize(N = n())
    
    T100 <- merge(T100, T100_freq, 
                  by.x = c("Origin_Numeric"),
                  by.y = c("Origin_Numeric"),
                  all.x = TRUE)
    
    T100 <- T100 %>% mutate(Incidence = paste(Carrier_Entity, N))
    T100 <- as.data.table(T100)
    T100[Incidence == paste(airlines[1], "1"), Incidence := paste(airlines[1], "Only")]
    T100[Incidence == paste(airlines[2], "1"), Incidence := paste(airlines[2], "Only")]
    T100[Incidence == paste(airlines[1], "2"), Incidence := paste("Both Firms")]
    T100[Incidence == paste(airlines[2], "2"), Incidence := paste("Both Firms")]
    
    T100[,`Operating Firms` := factor(Incidence)]
    T100.Mainland <- make_mainland_data(T100)
    T100.Mainland$`Operating Firms` <- as.character(T100.Mainland$`Operating Firms`)
    
    ggplot(data = state_data.mainland) + 
      geom_sf(fill = "white") + 
      geom_point(T100.Mainland, mapping = aes(x = Origin_lon, y = Origin_lat, color = `Operating Firms`), size = 0.5) +
     scale_color_manual(values = c("black", "cornflowerblue", "burlywood")) + 
      labs(x = "", y = "") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position = "bottom")
    
    ggsave(prep_name(filename)[1], width = 6, height = 4, units = "in")
    
    # Now for PR, VI
    T100.PR <- make_pr_data(T100)
    ggplot(data = state_data.PR) + 
      geom_sf(fill = "white") + 
      geom_point(T100.PR, mapping = aes(x = Origin_lon, y = Origin_lat, color = `Operating Firms`), size = 0.5) +
      scale_color_manual(values = c("black", "cornflowerblue", "burlywood")) + 
      labs(x = "", y = "") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position = "bottom")
    ggsave(prep_name(filename)[2], width = 6, height = 4, units = "in")
    
  } else{  
    T100.Mainland <- make_mainland_data(T100)
    
    ggplot(data = state_data.mainland) + 
      geom_sf(fill = "white") + 
      geom_point(T100.Mainland, mapping = aes(x = Origin_lon, y = Origin_lat), size = 0.5) +
      labs(x = "", y = "") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(prep_name(filename)[1], width = 6, height = 4, units = "in")
    
    # Now for PR, VI
    T100.PR <- make_pr_data(T100)
    ggplot(data = state_data.PR) + 
      geom_sf(fill = "white") + 
      geom_point(T100.PR, mapping = aes(x = Origin_lon, y = Origin_lat), size = 0.5) +
      scale_color_manual(values = c("black", "cornflowerblue", "burlywood")) + 
      labs(x = "", y = "") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position = "bottom")
    ggsave(prep_name(filename)[2], width = 6, height = 4, units = "in")
  }
}

