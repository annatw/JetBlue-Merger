# Calculate Optimal Instruments
optimal_instrument_results <- logitIV_Results$compute_optimal_instruments()
updated_problem <- optimal_instrument_results$to_problem()
updated_logit_results <- with(pyblp$parallel(24L), updated_problem$solve(sigma = logitIV_Results$sigma,
                                                                         rho = logitIV_Results$rho,
                                                                         rho_bounds = logitIV_Results$rho_bounds))

updated_logit_results$to_pickle(optimal_instrument_results_place)

estimated_costs_optimal <- updated_logit_results$compute_costs()

plot_estimated_costs(cost_data = estimated_costs_optimal,
                     graph_location = paste(file_save_head, "costs", "density.pdf", sep = "_"))

plot_estimated_markups(markup_data = updated_logit_results$compute_markups(costs = estimated_costs_optimal),
                       graph_location = paste(file_save_head, "markups", "density.pdf", sep = "_"))
plot_estimated_markups(markup_data = updated_logit_results$compute_markups(costs = estimated_costs_optimal),
                       graph_location = paste(file_save_head, "markups", "histogram.pdf", sep = "_"),
                       mode = "histogram")


elasticities <- updated_logit_results$compute_elasticities()
elasticities.own.price <- updated_logit_results$extract_diagonal_means(elasticities)


plot_estimated_elasticities(elasticity_data =  elasticities.own.price,
                            graph_location = paste(file_save_head, "elasticities", "density.pdf", sep = "_"))
plot_estimated_elasticities(elasticity_data =  elasticities.own.price,
                            graph_location = paste(file_save_head, "elasticities", "histogram.pdf", sep = "_"),
                            mode = "histogram")


# Now, Merger Simulation Stuff
product_data <- read_rds(product_data_place)
# Identify Each Firm's Corresponding ID 
id_correspondance <- product_data %>% select(firm_ids,
                                             Carrier) %>%
  unique() %>% as.data.table()

# Assign Spirit to JetBlue
spirit_id <- mean(id_correspondance[Carrier == "Spirit Air Lines"]$firm_ids)
jetblue_id <- mean(id_correspondance[Carrier == "JetBlue Airways"]$firm_ids)
product_data[, merger_ids := firm_ids]
product_data[firm_ids == spirit_id, merger_ids := jetblue_id]

new_prices <- updated_logit_results$compute_prices(firm_ids = product_data$merger_ids,
                                                   costs = updated_logit_results$compute_costs())  
new_shares <- updated_logit_results$compute_shares(new_prices)

original_hhi <- py_to_r(updated_logit_results$compute_hhi(
  firm_ids = product_data$firm_ids,
  shares = product_data$shares))

new_hhi <- py_to_r(updated_logit_results$compute_hhi(
  firm_ids = product_data$merger_ids,
  shares = new_shares))

plot_estimated_hhi(hhi_data = original_hhi,
                   graph_location = paste(file_save_head, "hhi_original", "histogram.pdf", sep = "_"),
                   mode = "histogram")

plot_estimated_hhi(hhi_data = original_hhi,
                   graph_location = paste(file_save_head, "hhi_original", "density.pdf", sep = "_"))

plot_estimated_hhi(hhi_data = new_hhi,
                   graph_location = paste(file_save_head, "hhi_simulated", "histogram.pdf", sep = "_"),
                   mode = "histogram")

plot_estimated_hhi(hhi_data = new_hhi,
                   graph_location = paste(file_save_head, "hhi_simulated", "density.pdf", sep = "_"))

# Now, Calculate out the Change in HHI
delta_hhi <- new_hhi - original_hhi

# Remove all markets without change
delta_hhi <- delta_hhi[delta_hhi > 1]

plot_estimated_hhi(hhi_data = delta_hhi,
                   graph_location = paste(file_save_head, "hhi_change", "density.pdf", sep = "_"))

plot_estimated_hhi(hhi_data = delta_hhi,
                   graph_location = paste(file_save_head, "hhi_change", "histogram.pdf", sep = "_"),
                   mode = "histogram")

print(paste("Estimated Mean Own Price Elasticities", mean(py_to_r(elasticities.own.price))))

