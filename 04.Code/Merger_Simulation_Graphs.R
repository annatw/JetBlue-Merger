change_minimum_fare <- function(merger_data = "03.Output/Adv_Merger_Sim_Data.rds",
                                observed_data = "02.Intermediate/Product_Data.rds",
                                graph_out = "05.Figures/Merger_Change_MinimumFare.pdf"){
  merger <- readRDS(merger_data)
  observed <- readRDS(observed_data)
  
  observed[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                               "Dest")]
  observed[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Year", "Quarter", "Origin",
                                                                 "Dest")]
  
  # Compute Costs
  shared_markets <- unique(observed[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
  
  merger <- merger %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(Prices.MinCost = min(Prices.MinCost.Sim) * 100,
              Prices.MeanCost = min(Prices.MeanCost.Sim) * 100,
              Prices.MaxCost = min(Prices.MaxCost.Sim)* 100) %>% as.data.table()
  
  observed <- observed %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(MinPrice = min(prices)* 100) %>% as.data.table()
  
  result <- merge(merger, observed, by = "market_ids")
  
  result[, `Low Cost Merge` := Prices.MinCost - MinPrice]
  result[, `Mean Cost Merge` := Prices.MeanCost - MinPrice]
  result[, `High Cost Merge` := Prices.MaxCost - MinPrice]
  
  result <- result[, .(market_ids, `Low Cost Merge`, `Mean Cost Merge` , `High Cost Merge`)]
  
  result.melt <- melt(result, id.vars = c("market_ids"))
  
  ggplot(data = result.melt, aes(x = value)) + 
    geom_histogram(binwidth = 10, 
                   boundary = 0) +
    facet_grid(rows = vars(variable)) + 
    labs(x = "Change in Minimum Market Price",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)
    
  ggsave(filename = graph_out, 
         units = "in", width = 5, height = 6.5)
}

# Change in Passengers
merger_change_pass_graph <- function(merger_data = "03.Output/Adv_Merger_Sim_Data.rds",
         observed_data = "02.Intermediate/Product_Data.rds",
         graph_out = "05.Figures/Merger_Change_MinimumFare.pdf"){
  sim_data <- readRDS(merger_data)
  observed <- readRDS(observed_data)
  
  # Estimate Number of Passengers in Each Market Following Simulation
  sim_data[, Passengers.Product.Min := Shares.MinCost.Sim * Potential_Passengers]
  sim_data[, Passengers.Product.Mean := Shares.MeanCost.Sim * Potential_Passengers]
  sim_data[, Passengers.Product.Max := Shares.MaxCost.Sim * Potential_Passengers]
  
  # Figure Out Jointly Operating Markets
  observed[, Spirit_Prescence := max(Spirit_Prescence), by = c("market_ids")]
  observed[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("market_ids")]
  shared_markets <- unique(observed[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
  
  shared <- sim_data[market_ids %in% shared_markets,]
  
  # Observed Data: JB + Sp Passengers
  observed.sh <- observed %>% filter(market_ids %in% shared_markets,
                                     Carrier %in% c("Spirit Air Lines",
                                                    "JetBlue Airways")) %>%
    group_by(market_ids) %>%
    summarize(Passengers.Total = sum(Own_Passengers)) %>% as.data.table()
  
  shared <- shared %>% filter(merger_carrier == "JetBlue Airways") %>%
    group_by(market_ids) %>%
    summarize(Simulated.Min.Pass = sum(Passengers.Product.Min),
              Simulated.Max.Pass = sum(Passengers.Product.Max),
              Simulated.Mean.Pass = sum(Passengers.Product.Mean)) %>%
    as.data.table()
  
  merged.data <- merge(shared, observed.sh, by = "market_ids")
  
  merged.data <- merged.data[, Change.Min := Simulated.Min.Pass - Passengers.Total]
  merged.data <- merged.data[, Change.Mean := Simulated.Mean.Pass - Passengers.Total]
  merged.data <- merged.data[, Change.Max := Simulated.Max.Pass - Passengers.Total]
  
  merged.data <- merged.data[, .(market_ids, Change.Min, Change.Mean, Change.Max)]
  colnames(merged.data) <- c("market_ids", "Change Min Cost", "Change Mean Cost",
                             "Change Max Cost")
  
  merged.melt <- melt(merged.data, measure.vars = c("Change Min Cost", "Change Mean Cost",
                                                    "Change Max Cost"))
  
  ggplot(data = merged.melt, aes(x = value)) +
    geom_histogram(binwidth = 500) +
    facet_grid(rows = vars(variable)) +
    labs(x = "Change in Passengers") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)
  ggsave(graph_out, units = "in", height = 8, width = 5)
}

change_spirit_fare_basic <- function(merger_data = "02.Intermediate/Basic_Sim_Product_Data.rds",
                                      figure_out = "05.Figures/Spirit_Fare_Change.pdf"){
  merger <- readRDS(merger_data)
  merger.sp <- merger[Carrier == "Spirit Air Lines",]
  
  ggplot(data = merger.sp, aes(x = price.change)) +
    geom_histogram() 
}
