change_minimum_fare <- function(merger_data.basic = "02.Intermediate/Basic_Sim_Product_Data.rds",
                                merger_data.adv = "03.Output/Adv_Merger_Sim_Data.rds",
                                observed_data = "02.Intermediate/Product_Data.rds",
                                graph_out.internalize = "05.Figures/Merger_Change_MinimumFare_Internalize.pdf",
                                graph_out.BestCase = "05.Figures/Merger_Change_MinimumFare_BestCase.pdf",
                                graph_out.AverageCase = "05.Figures/Merger_Change_MinimumFare_AverageCase.pdf",
                                graph_out.WorstCase = "05.Figures/Merger_Change_MinimumFare_WorstCase.pdf"){
  merger_internalized <- readRDS(merger_data.basic)
  merger <- readRDS(merger_data.adv)
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
  
  merger_internalized <- merger_internalized %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(Prices = min(prices) * 100)
  
  observed <- observed %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(MinPrice = min(prices)* 100) %>% as.data.table()
  
  result <- merge(merger, observed, by = "market_ids")
  result_internalized <- merge(merger_internalized, observed, by = "market_ids") %>% as.data.table()
  
  result[, `Low Cost Merge` := Prices.MinCost - MinPrice]
  result[, `Mean Cost Merge` := Prices.MeanCost - MinPrice]
  result[, `High Cost Merge` := Prices.MaxCost - MinPrice]
  result_internalized[, Difference := Prices - MinPrice]
  result_internalized <- result_internalized[, .(market_ids, Difference)] %>% unique()
  
  
  result <- result[, .(market_ids, `Low Cost Merge`, `Mean Cost Merge` , `High Cost Merge`)] %>% unique()
  
  result.melt <- melt(result, id.vars = c("market_ids")) %>% data.table()
  
  best_case <- result.melt[variable == "Low Cost Merge",]
  average_case <- result.melt[variable == "Mean Cost Merge",]
  worst_case <- result.melt[variable == "High Cost Merge",]
  
  ggplot(data = best_case, aes(x = value)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Minimum Market Price (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)
    
  ggsave(filename = graph_out.BestCase, 
         units = "in", width = 5, height = 3)
  
  ggplot(data = average_case, aes(x = value)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Minimum Market Price (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)
  
  ggsave(filename = graph_out.AverageCase, 
         units = "in", width = 5, height = 3)
  
  ggplot(data = worst_case, aes(x = value)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Minimum Market Price (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)
  
  ggsave(filename = graph_out.WorstCase, 
         units = "in", width = 5, height = 3)
  
  ggplot(data = result_internalized, aes(x = Difference)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Minimum Market Price (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)
  
  ggsave(filename = graph_out.internalize, 
         units = "in", width = 5, height = 3)
}

# Change in Passengers
merger_change_pass_graph <- function(merger_data = "03.Output/Adv_Merger_Sim_Data.rds",
         observed_data = "02.Intermediate/Product_Data.rds",
         graph_out.bestCase = "05.Figures/Merger_Change_Passengers_BestCase.pdf",
         graph_out.averageCase = "05.Figures/Merger_Change_Passengers_AverageCase.pdf",
         graph_out.worstCase = "05.Figures/Merger_Change_Passengers_WorstCase.pdf"){
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
  
  # Observed Data
  observed.sh <- observed %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(Passengers.Total = sum(Own_Passengers)) %>% as.data.table()
  
  shared <- shared %>% 
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
                                                    "Change Max Cost")) %>%
    as.data.table()
  
  ggplot(data = merged.melt[variable == "Change Min Cost",], aes(x = value)) +
    geom_histogram(binwidth = 500) +
    labs(x = "Change in Passengers") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma) +
    scale_x_continuous(limits = c(-100000, 100000))
  ggsave(graph_out.bestCase, units = "in", height = 3, width = 5)
  
  ggplot(data = merged.melt[variable == "Change Mean Cost",], aes(x = value)) +
    geom_histogram(binwidth = 500) +
    labs(x = "Change in Passengers") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma) +
    scale_x_continuous(limits = c(-100000, 100000))
  ggsave(graph_out.averageCase, units = "in", height = 3, width = 5)
  
  
  ggplot(data = merged.melt[variable == "Change Max Cost",], aes(x = value)) +
    geom_histogram(binwidth = 500) +
    labs(x = "Change in Passengers") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma) +
    scale_x_continuous(limits = c(-100000, 100000))
  ggsave(graph_out.worstCase, units = "in", height = 3, width = 5)
}

change_average_fare <- function(merger_data.basic = "02.Intermediate/Basic_Sim_Product_Data.rds",
                                merger_data.adv = "03.Output/Adv_Merger_Sim_Data.rds",
                                observed_data = "02.Intermediate/Product_Data.rds",
                                graph_out.internalize = "05.Figures/Merger_Change_AverageFare_Internalize.pdf",
                                graph_out.BestCase = "05.Figures/Merger_Change_AverageFare_BestCase.pdf",
                                graph_out.AverageCase = "05.Figures/Merger_Change_AverageFare_AverageCase.pdf",
                                graph_out.WorstCase = "05.Figures/Merger_Change_AverageFare_WorstCase.pdf"){
  merger_internalized <- readRDS(merger_data.basic)
  merger <- readRDS(merger_data.adv)
  observed <- readRDS(observed_data)
  
  observed[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                               "Dest")]
  observed[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Year", "Quarter", "Origin",
                                                                 "Dest")]
  
  # Compute Costs
  shared_markets <- unique(observed[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
  
  merger <- merger %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(Prices.MinCost = mean(Prices.MinCost.Sim) * 100,
              Prices.MeanCost = mean(Prices.MeanCost.Sim) * 100,
              Prices.MaxCost = mean(Prices.MaxCost.Sim)* 100,
              Passengers.Mean = sum(Shares.MeanCost.Sim * Potential_Passengers)) %>% as.data.table()
  
  merger_internalized <- merger_internalized %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(Prices = mean(prices) * 100)
  
  observed <- observed %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(MeanPrice = mean(prices)* 100) %>% as.data.table()
  
  result <- merge(merger, observed, by = "market_ids")
  result_internalized <- merge(merger_internalized, observed, by = "market_ids") %>% as.data.table()
  
  result[, `Low Cost Merge` := Prices.MinCost - MeanPrice]
  result[, `Mean Cost Merge` := Prices.MeanCost - MeanPrice]
  result[, `High Cost Merge` := Prices.MaxCost - MeanPrice]
  result_internalized[, Difference := Prices - MeanPrice]
  result_internalized <- result_internalized[, .(market_ids, Difference)] %>% unique()
  
  
  result <- result[, .(market_ids, `Low Cost Merge`, `Mean Cost Merge` , `High Cost Merge`)] %>% unique()
  
  result.melt <- melt(result, id.vars = c("market_ids")) %>% data.table()
  
  best_case <- result.melt[variable == "Low Cost Merge",]
  average_case <- result.melt[variable == "Mean Cost Merge",]
  worst_case <- result.melt[variable == "High Cost Merge",]
  
  ggplot(data = best_case, aes(x = value)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Average Market Fare (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)+
    scale_x_continuous(limits = c(-20, 40))
  
  
  
  ggsave(filename = graph_out.BestCase, 
         units = "in", width = 5, height = 3)
  
  ggplot(data = average_case, aes(x = value)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Average Market Fare (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)+
    scale_x_continuous(limits = c(-20, 40))
  
  ggsave(filename = graph_out.AverageCase, 
         units = "in", width = 5, height = 3)
  
  ggplot(data = worst_case, aes(x = value)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Average Market Fare (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)+
    scale_x_continuous(limits = c(-20, 40))
  
  ggsave(filename = graph_out.WorstCase, 
         units = "in", width = 5, height = 3)
  
  ggplot(data = result_internalized, aes(x = Difference)) + 
    geom_histogram(binwidth = 1, 
                   boundary = 0) +
    labs(x = "Change in Average Market Fare (2017 USD)",
         y = "Count") + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0),
                       labels = comma)+
    scale_x_continuous(limits = c(-20, 40))
  
  ggsave(filename = graph_out.internalize, 
         units = "in", width = 5, height = 3)
  
}

change_spirit_fare_basic <- function(merger_data = "02.Intermediate/Basic_Sim_Product_Data.rds",
                                      figure_out = "05.Figures/Spirit_Fare_Change.pdf"){
  merger <- readRDS(merger_data)
  merger.sp <- merger[Carrier == "Spirit Air Lines",]
  
  ggplot(data = merger.sp, aes(x = price.change)) +
    geom_histogram() 
}
