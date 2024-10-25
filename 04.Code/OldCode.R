merger_results_table <- function(ols_logit_results = "03.Output/ols_logit.pickle",
                                 logit_iv_results = "03.Output/logit_iv.pickle",
                                 nested_logit_iv_results = "03.Output/nested_logit_iv.pickle",
                                 rcl_results = "03.Output/random_coeff_logit_fs_results.pickle",
                                 rcl_nested_results = "03.Output/random_coeff_nested_logit_fs_results.pickle",
                                 product_place = "02.Intermediate/Product_Data.rds",
                                 output_table = "06.Tables/demand_EstimationTable.tex"){
  sd_handle <- function(input){
    return(paste("(", signif(input, digits= 2), ")", sep = ""))
  }
  
  firm_elasticity_row <- function(Row_Name, Chosen_Carrier,
                                  OLS = ols_logit, Logit = logit_iv, 
                                  Nested_Logit = nested_logit_iv, 
                                  RCL = rcl, RCL_Nested = rcl_nested,
                                  Product = product_data){
    # First, find out the markets the given firm operates in
    markets <- unique(Product[Carrier == Chosen_Carrier,]$market_ids)
    # Remove irrelevant markets
    Product <- Product[market_ids %in% markets]; gc(); 
    
    #Product IDS determine order of elasticities within a matrix
    elasticities.ols <- c()
    elasticities.logitIV <- c()
    elasticities.nested_logit <- c()
    elasticities.rcl <- c()
    elasticities.rcl.n <- c()
    
    
    firm_products <- foreach(i = 1:length(markets)) %dopar% {
      m <- markets[i]
      current_products <- Product[market_ids == m,]
      
      ols.e <- data.table(OLS = OLS$extract_diagonals(OLS$compute_elasticities(market_id = m),
                                                      market_id = m))
      liv.e <- data.table(LIV = Logit$extract_diagonals(Logit$compute_elasticities(market_id = m),
                                                        market_id = m))
      nliv.e <- data.table(NLIV = Nested_Logit$extract_diagonals(Nested_Logit$compute_elasticities(market_id = m),
                                                                 market_id = m))
      rcl.e <- data.table(RCL = RCL$extract_diagonals(RCL$compute_elasticities(market_id = m),
                                                      market_id = m))
      rcl.n.e <- data.table(RCL_N = RCL_Nested$extract_diagonals(RCL_Nested$compute_elasticities(market_id = m),
                                                                 market_id = m))
      
      current_products <- cbind(current_products, ols.e, liv.e, nliv.e, 
                                rcl.e, rcl.n.e)
      
      current_products <- current_products[, .(Carrier, market_ids, product_ids, OLS.V1, LIV.V1, NLIV.V1,
                                               RCL.V1, RCL_N.V1)]
      current_products <- current_products[Carrier == Chosen_Carrier,]; gc(); 
      current_products
    }
    
    product_elasticities <- rbindlist(firm_products)
    
    elasticities.ols <- product_elasticities$OLS.V1
    elasticities.logitIV <- product_elasticities$LIV.V1
    elasticities.nested_logit <- product_elasticities$NLIV.V1
    elasticities.rcl <-  product_elasticities$RCL.V1
    elasticities.rcl.n <-product_elasticities$RCL_N.V1
    
    output_row <- c(Row_Name, signif(mean(elasticities.ols), digits = 3),
                    signif(mean(elasticities.logitIV), digits = 3),
                    signif(mean(elasticities.nested_logit), digits = 3),
                    signif(mean(elasticities.rcl), digits = 3),
                    signif(mean(elasticities.rcl.n), digits = 3))
    
    return(output_row)
  }
  
  product_data <- read_rds(product_place)
  ols_logit <- py_load_object(ols_logit_results)
  logit_iv <- py_load_object(logit_iv_results)
  nested_logit_iv <- py_load_object(nested_logit_iv_results)
  rcl <- py_load_object(rcl_results)
  rcl_nested <- py_load_object(rcl_nested_results)
  
  column_names <- c("Variable", "OLS Logit", "IV Logit", "Nest IV Logit", "RC Logit", "Nest RC Logit")  
  prices <- c("Price", signif(ols_logit$beta[1],  digits = 3), 
              signif(logit_iv$beta[1], digits = 3), 
              signif(nested_logit_iv$beta[1], digits = 3),
              signif(rcl$beta[1], digits = 3),
              signif(rcl_nested$beta[1], digits = 3))
  prices.sd <- c("", sd_handle(ols_logit$beta_se[1]), 
                 sd_handle(logit_iv$beta_se[1]),
                 sd_handle(nested_logit_iv$beta_se[1]),
                 sd_handle(rcl$beta_se[1]),
                 sd_handle(rcl_nested$beta_se[1]))
  miles <- c("Miles Flown", signif(ols_logit$beta[2], digits = 3), 
             signif(logit_iv$beta[2], digits = 3), 
             signif(nested_logit_iv$beta[2], digits = 3),
             signif(rcl$beta[2], digits = 3),
             signif(rcl_nested$beta[2], digits = 3))
  miles.sd <- c("", sd_handle(ols_logit$beta_se[2]), sd_handle(logit_iv$beta_se[2]),
                sd_handle(nested_logit_iv$beta_se[2]),
                sd_handle(rcl$beta_se[2]),
                sd_handle(rcl_nested$beta_se[2]))
  miles_sq <- c("Miles Flown\$^2\$", signif(ols_logit$beta[3], digits = 3),
                signif(logit_iv$beta[3], digits = 3),
                signif(nested_logit_iv$beta[3], digits = 3),
                signif(rcl$beta[3], digits = 3),
                signif(rcl_nested$beta[3], digits = 3))
  miles_sq.sd <- c("", sd_handle(ols_logit$beta_se[3]),
                   sd_handle(logit_iv$beta_se[3]),
                   sd_handle(nested_logit_iv$beta_se[3]),
                   sd_handle(rcl$beta_se[3]),
                   sd_handle(rcl_nested$beta_se[3]))
  nonstop <- c("Nonstop", signif(ols_logit$beta[4], digits = 3), 
               signif(logit_iv$beta[4], digits = 3), 
               signif(nested_logit_iv$beta[4], digits = 3),
               signif(rcl$beta[4], digits = 3),
               signif(rcl_nested$beta[4], digits = 3))
  nonstop.sd <- c("", sd_handle(ols_logit$beta_se[4]), sd_handle(logit_iv$beta_se[4]),
                  sd_handle(nested_logit_iv$beta_se[4]),
                  sd_handle(rcl$beta_se[4]),
                  sd_handle(rcl_nested$beta_se[4]))
  origin_ratio <- c("Origin Ratio", signif(ols_logit$beta[5], digits = 3),
                    signif(logit_iv$beta[5], digits = 3), 
                    signif(nested_logit_iv$beta[5], digits = 3),
                    signif(rcl$beta[5], digits = 3),
                    signif(rcl_nested$beta[5], digits = 3))
  origin_ratio.sd <- c("", sd_handle(ols_logit$beta_se[5]),
                       sd_handle(logit_iv$beta_se[5]),
                       sd_handle(nested_logit_iv$beta_se[5]),
                       sd_handle(rcl$beta_se[5]),
                       sd_handle(rcl_nested$beta_se[5]))
  
  rho_estimate <- c("\$\\rho\$", "", "", signif(nested_logit_iv$rho, 3), "", 
                    signif(rcl_nested$rho,3))
  
  # Now, Fixed Effects Rows
  carrier_fe <- c("Carrier", "X", "X", "X", "X", "X")
  market_fe <- c("Time", "X", "X", "X", "X", "X")
  
  # Prep Elasticities
  ols_logit.elasticity <-ols_logit$compute_elasticities(); gc()
  logit_iv.elasticity <- logit_iv$compute_elasticities(); gc()
  nested_logit.elasticity <- nested_logit_iv$compute_elasticities(); gc()
  rcl.elasticity <- rcl$compute_elasticities(); gc()
  rcl_nested.elast <- rcl_nested$compute_elasticities(); gc()
  
  # First, average elasticity, all markets
  elastic.mean <- c("Mean Elasticity", 
                    signif(mean(ols_logit$extract_diagonal_means(ols_logit.elasticity)), digits = 3),
                    signif(mean(logit_iv$extract_diagonal_means(logit_iv.elasticity)), digits = 3),
                    signif(mean(nested_logit_iv$extract_diagonal_means(nested_logit.elasticity)), digits = 3),
                    signif(mean(rcl$extract_diagonal_means(rcl.elasticity)), digits = 3),
                    signif(mean(rcl_nested$extract_diagonal_means(rcl_nested.elast)), digits = 3))
  
  
  
  # spirit.elastic <- firm_elasticity_row(Row_Name = "Spirit Mean Elasticitiy", 
  #                                        Chosen_Carrier = "Spirit Air Lines"); gc();
  # jetblue.elastic <- firm_elasticity_row(Row_Name = "JetBlue Mean Elasticitiy", 
  #                                        Chosen_Carrier = "JetBlue Airways"); gc()
  
  # Summary Rows
  nmarkets <- c("N Markets", rep(x= length(unique(product_data$market_ids)),
                                 times = 5))
  nproducts <- c("N Products", rep(x = length(unique(product_data$product_ids)), 
                                   times = 5))
  
  summaryTable <- rbind(prices, prices.sd, miles, miles.sd,
                        miles_sq, miles_sq.sd, 
                        nonstop, nonstop.sd, 
                        origin_ratio, origin_ratio.sd,
                        rho_estimate,
                        carrier_fe, market_fe, 
                        nmarkets, nproducts,
                        elastic.mean, spirit.elastic, jetblue.elastic)
  colnames(summaryTable) <- column_names
  rownames(summaryTable) <- NULL
  
  kbl(summaryTable,
      format = "latex", 
      escape = FALSE, booktabs = TRUE) %>%
    row_spec(row = 11, hline_after = TRUE) %>%
    pack_rows(group_label = "Controls", 12,13) %>%
    pack_rows(group_label = "Summary Statistics", 14, 18) %>%
    save_kable(file = output_table)
}

{
# Calculate, for each carrier in each time, the overlap
overlap_frame <- c()
for(y in years_allowed){
  for(q in 1:4){
    if(y == 2023 & q == 3){
      break;
    }
    product_data.current <- product_data[Year == y & Quarter == q,]
    # Calculate Number of Major Markets that Major Carriers Overlap In
    delta_markets <- unique(product_data.current[Carrier == "Delta Air Lines Inc.",]$market_ids)
    united_markets <- unique(product_data.current[Carrier == "United Air Lines Inc.",]$market_ids)
    american_markets <- unique(product_data.current[Carrier == "American Airlines Inc.",]$market_ids)
    jetblue_markets <- unique(product_data.current[Carrier == "JetBlue Airways", ]$market_ids)
    alaskan_markets <- unique(product_data.current[Carrier == "Alaska Airlines Inc.",]$market_ids)
    spirit_markets <- unique(product_data.current[Carrier == "Spirit Air Lines",]$market_ids)
    hawaiian_markets <- unique(product_data.current[Carrier == "Hawaiian Airlines Inc.",]$market_ids)
    allegiant_markets <- unique(product_data.current[Carrier == "Allegiant Air",]$market_ids)
    frontier_markets <- unique(product_data.current[Carrier == "Frontier Airlines Inc.", ]$market_ids)
    southwest_markets <- unique(product_data.current[Carrier == "Southwest Airlines Co.",]$market_ids)
    
    for(i in 1:length(unique(product_data.current$Carrier))){
      current_carrier <- unique(product_data.current$Carrier)[i];
      carrier_list <- c("Delta Air Lines Inc.", "United Air Lines Inc.",
                        "American Airlines Inc.", "JetBlue Airways", "Alaska Airlines Inc.", 
                        "Spirit Air Lines", "Hawaiian Airlines Inc.", "Allegiant Air",
                        "Frontier Airlines Inc.")
      current_routes <- unique(product_data.current[Carrier == current_carrier,]$market_ids)
      overlap_1 <- sum(current_routes %in% delta_markets) / length(current_routes)
      overlap_2 <- sum(current_routes %in% united_markets) / length(current_routes)
      overlap_3 <- sum(current_routes %in% american_markets) / length(current_routes)
      overlap_4 <- sum(current_routes %in% jetblue_markets) / length(current_routes)
      overlap_5 <- sum(current_routes %in% alaskan_markets) / length(current_routes)
      overlap_6 <- sum(current_routes %in% spirit_markets) / length(current_routes)
      overlap_7 <- sum(current_routes %in% hawaiian_markets) / length(current_routes)
      overlap_8 <- sum(current_routes %in% allegiant_markets) / length(current_routes)
      overlap_9 <- sum(current_routes %in% frontier_markets) / length(current_routes)
      overlap_10 <- sum(current_routes %in% southwest_markets) / length(current_routes)
      
      new_frame <- data.frame(Carrier = current_carrier, 
                              Year = y,
                              Quarter = q,
                              Delta_Exposure = overlap_1,
                              United_Exposure = overlap_2,
                              American_Exposure = overlap_3,
                              JetBlue_Exposure = overlap_4,
                              Alaskan_Exposure = overlap_5,
                              Spirit_Exposure = overlap_6,
                              Hawaiian_Exposure = overlap_7,
                              Allegiant_Exposure = overlap_8,
                              Frontier_Exposure = overlap_9,
                              Southwest_Exposure = overlap_10)
      
      if(i == 1 & y == years_allowed[1] & q == quarters_allowed[1]){
        overlap_frame <- new_frame;
      } else {
        overlap_frame <- rbind(overlap_frame, new_frame)
      }
    }
  }
}

product_data <- merge(product_data, overlap_frame, 
                      by = c("Carrier", "Year", "Quarter"),
                      all.x = TRUE);
remove(overlap_frame)


# Now, Multiply Each Overlap with Relevant Carrier Prescence Variable
product_data[, Delta_Exposure := Delta_Exposure * Delta_Prescence]
product_data[, United_Exposure := United_Exposure * United_Prescence]
product_data[, American_Exposure := American_Exposure * American_Prescence]
product_data[, Southwest_Exposure := Southwest_Prescence * Southwest_Exposure]
product_data[, JetBlue_Exposure := JetBlue_Prescence * JetBlue_Exposure]
product_data[, Alaskan_Exposure := Alaska_Prescence * Alaskan_Exposure]
product_data[, Spirit_Exposure := Spirit_Prescence * Spirit_Exposure]
product_data[, Hawaiian_Exposure := Hawaiian_Prescence * Hawaiian_Exposure]
product_data[, Frontier_Exposure := Frontier_Prescence * Frontier_Exposure]
product_data[, Allegiant_Exposure := Allegiant_Prescence * Allegiant_Exposure]

}

plot_estimated_hhi <- function(hhi_data,
                               graph_location,
                               mode = "density"){
  hhi_data <- as.data.table(hhi_data)
  colnames(hhi_data) <- "Market HHI"
  if(mode == "density"){
    ggplot(hhi_data, aes(`Market HHI`)) + geom_density() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
  } else if (mode == "histogram"){
    ggplot(hhi_data, aes(`Market HHI`)) + geom_histogram() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
    
  }
  
  ggsave(graph_location, units = "in", width = 5, height = 3)
}


rcl_post_estimation_processing <- function(input_file = "03.Output/random_coeff_logit_results.pickle",
                                           optimal_instrument_results_place = "03.Output/rcl_optimal_instruments.pickle",
                                           cost_graph = "05.Figures/Estimation_Output/rcl_estimated_costs.pdf",
                                           markup_graph = "05.Figures/Estimation_Output/rcl_estimated_markups.pdf",
                                           own_elasticity_graph = "05.Figures/Estimation_Output/rcl_estimated_elasticity.pdf",
                                           product_data_place = "02.Intermediate/Product_Data.rds"){
  rcl_results <- py_load_object(input_file, convert = FALSE)
  
  
  #updated_rcl_results$to_pickle(optimal_instrument_results_place)
  
  
  estimated_costs <- rcl_results$compute_costs()
  
  plot_estimated_costs(cost_data = estimated_costs,
                       graph_location = cost_graph)
  
  plot_estimated_markups(markup_data = rcl_results$compute_markups(costs = estimated_costs),
                         graph_location = markup_graph)
  
  elasticities <- rcl_results$compute_elasticities()
  
  plot_estimated_elasticities(elasticity_data =  rcl_results$extract_diagonal_means(elasticities),
                              graph_location = own_elasticity_graph)
  
}


plot_estimated_costs <- function(cost_data,
                                 graph_location,
                                 mode = "density"){
  
  cost_data_frame <- as.data.frame(py_to_r(cost_data))
  colnames(cost_data_frame) <- "Marginal Cost"
  if(mode == "density"){
    ggplot(cost_data_frame, aes(`Marginal Cost`)) + geom_density() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
  } else if(mode == "histogram"){
    ggplot(cost_data_frame, aes(`Marginal Cost`)) + geom_histogram() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
  }
  ggsave(graph_location, units = "in", width = 5, height = 3)
}

plot_estimated_markups <- function(markup_data,
                                   graph_location,
                                   mode = "density"){
  markups <- as.data.table(py_to_r(markup_data))
  colnames(markups) <- "Markup"
  
  if(mode == "density"){
    ggplot(markups, aes(Markup)) + geom_density() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
  } else if(mode == "histogram"){
    ggplot(markups, aes(Markup)) + geom_histogram() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
    
  }
  
  ggsave(graph_location, units = "in", width = 5, height = 3)
}

plot_estimated_elasticities <- function(elasticity_data, 
                                        graph_location,
                                        mode = "density"){
  own_price_elasticity <- as.data.table(py_to_r(elasticity_data))
  colnames(own_price_elasticity) <- "Own Price Elasticity"
  
  if(mode == "density"){
    ggplot(own_price_elasticity, aes(`Own Price Elasticity`)) + geom_density() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
  } else if(mode == "histogram"){
    ggplot(own_price_elasticity, aes(`Own Price Elasticity`)) + geom_histogram() + 
      theme(panel.background = element_blank(), 
            axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
    
  }
  
  ggsave(graph_location, units = "in", width = 5, height = 3)
}

