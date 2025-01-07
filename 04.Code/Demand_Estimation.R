# instrument reconfigure accepts two arguments: data, instrument_column_names
# The first is a data table styled after product data (the output of merger_sim_data_generate())
# The second is a function of column names which are to be used as instruments by pyblp
# This adjustment allows for the easy creation of the instrument compare table
# using PyBLP (which allows for the use of absorbed fixed effects)
instrument_reconfigure <- function(data,
                                   instrument_column_names,
                                   keep_current = FALSE){
  data <- as.data.table(data)
  non_instruments <- !grepl(x = colnames(data), pattern = "demand_instruments")    
  non_instrument_columns <- data[, ..non_instruments]
  
  if(keep_current == TRUE){
    instrument_column_names <- c(instrument_column_names, 
                                 colnames(data)[grepl(x = colnames(data), pattern = "demand_instruments")])
  }
  
  new_instrument_columns <- data[, ..instrument_column_names]
  
  # Remove duplicate rows
  new_instrument_columns <- t(unique(t(new_instrument_columns)))
  
  # Highly Collinear Instrument Removal 
  
  
  colnames(new_instrument_columns) <- paste("demand_instruments", 0:(ncol(new_instrument_columns)-1), sep = "")
  
  
  return_data <- cbind(non_instrument_columns, new_instrument_columns)
  return(return_data)
}

blp_logit_iv <- function(input_file = "02.Intermediate/Product_Data.rds",
                         output_file = "03.Output/logit_iv.pickle",
                         model = NULL,
                         additional_instruments = NULL,
                         nested = FALSE, keep = TRUE){
  product_data <- read_rds(input_file)
  
  if(!is.null(additional_instruments)){
    product_data <- instrument_reconfigure(data = product_data, instrument_column_names = additional_instruments,
                                           keep_current = keep)
  }
  
  # Not Logit IV
  if(nested == FALSE){
    product_data$nesting_ids <- NULL
  } 
  
  pyblp$options$digits <- 2L
  pyblp$options$verbose <- TRUE
  
  logitIV <- model
  
  problem <- pyblp$Problem(logitIV, product_data)
  if(nested == FALSE){
    logitIV_Results <- problem$solve()
  } else if (nested == TRUE) {
    logitIV_Results <- problem$solve(rho = 0.21,
                                     rho_bounds = tuple(0.01, 0.99),
                                     optimization = pyblp$Optimization('l-bfgs-b', dict('gtol', 1e-8)),
                                     iteration = pyblp$Iteration('squarem'))        #  iteration = pyblp$Iteration('broyden1'))
  }
  
  logitIV_Results$to_pickle(output_file)
}

blp_rcl <- function(input_file = "02.Intermediate/Product_Data.rds",
                    output_file = "03.Output/random_coeff_nested_logit_fs_results.pickle",
                    nonlinearstart = NULL, linearstart = NULL,
                    additional_instruments, linear, nonlinear, 
                    nested = FALSE, optimality = "gradient",
                    precision = 1e-8, se = "clustered"){
  product_data <- readRDS(input_file)
  
  # Format Instruments 
  if(!is.null(additional_instruments)){
    product_data <- instrument_reconfigure(data = product_data, instrument_column_names = additional_instruments,
                                           keep_current = TRUE)
  }
  
  if(nested == FALSE){
    product_data$nesting_ids <- NULL
  }
  
  if(is.null(nonlinearstart)){
  number_non_linear <- 3
  nonlinearstart <- diag(x = 0, ncol = number_non_linear, 
                         nrow = number_non_linear)
    
  nonlinear_lb <- matrix(rep(0, number_non_linear^2), nrow = number_non_linear);
  nonlinear_ub <- nonlinear_lb
  for(i in 1:nrow(nonlinear_ub)){
    nonlinear_lb[i,i] <- 1e-30
    nonlinear_ub[i, i] <- 1000
    nonlinearstart[i,i] <- 1
  }
  }
  
  if(is.null(linearstart)){
    number_linear <- str_count(string = as.character(linear), 
                               pattern = "\\+")
    
    linearstart <- rep(1, times = 24)
  }
  
  pyblp$options$digits <- 2L
  pyblp$options$verbose <- TRUE
  
  if(is.null(linear)){
    linear <- pyblp$Formulation('1 + prices + MktMilesFlown + MktMilesFlown_Sq + NonStop + Origin_Firm_Service_Ratio',
                                absorb = "C(market_ids) + C(Carrier)",
                                absorb_method = "lsmr")
    nonlinear <- pyblp$Formulation("1 + prices + MktMilesFlown + MktMilesFlown_Sq + NonStop + Origin_Firm_Service_Ratio")
  }
  
  # Sigma Matrix controls nonlinear interactions, initial values
  sigma_matrix <- nonlinearstart
  
  # Set up Integration Routine
  # integrate <- pyblp$Integration('halton', size = 10L,
  #                                specification_options = dict("seed" = 413L))
  
  integrate <- pyblp$Integration('halton', size = 250L,
                                 specification_options = dict("seed" = 97L))
  
  
  rcl_problem <- pyblp$Problem(c(linear, nonlinear), product_data, integration = integrate)
  
  
  optimization_parameter <- pyblp$Optimization('l-bfgs-b', dict('gtol', precision))
  
  
  if(nested == TRUE){
    rcl_results <- rcl_problem$solve(sigma = sigma_matrix, 
                                          #  sigma_bounds = tuple(nonlinear_lb, nonlinear_ub),
                                          optimization = optimization_parameter,
                                          check_optimality = optimality,
                                          rho = 0.5,
                                          rho_bounds = c(0.01, 0.99),
                                          shares_bounds = c(1e-200, 0.15),
                                          se_type = se)
  } else {
    rcl_results <- rcl_problem$solve(sigma = sigma_matrix, 
                      optimization = optimization_parameter,
                        check_optimality = "gradient")
  }
  remove(rcl_problem)
  gc(); gc(); 
  py_save_object(object = rcl_results, 
                 filename = output_file)
  gc();
}

rcl_second_step <- function(input, output, 
                            drop_index = NULL,
                            precision = 1e-17,
                            se = "clustered"){
  rcl_step_one <- py_load_object(input)
  instrument_results <- rcl_step_one$compute_optimal_instruments(method = "approximate")
  
  if(!is.null(drop_index)){
    redo_problem <- instrument_results$to_problem()
  } else {
    redo_problem <- instrument_results$to_problem(drop_indices = sequence(drop_index))
  }
  
  new_sigma <- rcl_step_one$sigma
  
  for(i in 1:nrow(new_sigma)){
    if(new_sigma[i,i] == 0){
      new_sigma[i,i] <- 1e-50
    }
  }
  
  new_results <- with(pyblp$parallel(pycores),
                      redo_problem$solve(sigma = new_sigma,
                                         pi = rcl_step_one$pi,
                                         optimization = pyblp$Optimization('l-bfgs-b', dict('gtol', precision)),
                                         method = "2s",
                                         rho = 0.5,
                                         rho_bounds = c(0.01, 0.99),
                                         se_type = se))
  new_results$to_pickle(output)
  gc();
}

starmake <- function(coeff, se){
  q10 <- qnorm(p = 0.95, mean = 0, sd = 1)
  q05 <- qnorm(p = 0.975, mean = 0, sd = 1)
  q01 <- qnorm(p = 0.995, mean = 0, sd = 1)
  
  if(is.nan(se)){
    return(signif(coeff, digits = 3))
  } else if(coeff < 0){
    if(coeff + q01 * se < 0){
      return(paste(signif(coeff, digits = 3), "***", sep = ""))
    } else if(coeff + q05 < 0){
      return(paste(signif(coeff, digits = 3), "**", sep = ""))
    } else if(coeff + q01 < 0){
      return(paste(signif(coeff, digits = 3), "*", sep = ""))
    } else {
      return(signif(coeff, digits = 3))
    }
  } else if (coeff > 0){
    if(coeff - q01 * se > 0){
      return(paste(signif(coeff, digits = 3), "***", sep = ""))
    } else if(coeff - q05 > 0){
      return(paste(signif(coeff, digits = 3), "**", sep = ""))
    } else if(coeff - q01 > 0){
      return(paste(signif(coeff, digits = 3), "*", sep = ""))
    } else {
      return(signif(coeff, digits = 3))
    }
  } else if (coeff == 0){
    return(coeff)
  }
}

sd_format <- function(num){
  return(paste("(", signif(num, digits = 2), ")", sep = ""))
}

export_nested_logit_to_latex <- function(nl_in = "03.Output/nested_logit_iv.pickle",
                                         data_in = "02.Intermediate/Product_Data.rds",
                                         output_table = "06.Tables/NestedLogit_Results.tex"){
  nl <- py_load_object(nl_in)
  nl_data <- readRDS(data_in)
  
  # Beta Rows  
  beta_1 <- c("Prices", "NonStop", "Miles Flown", "Miles Squared")
  beta_2 <- c(starmake(nl$beta[1], nl$beta_se[1]), starmake(nl$beta[2], nl$beta_se[2]),
              starmake(nl$beta[3], nl$beta_se[3]), starmake(nl$beta[4], nl$beta_se[4]))
  beta_3 <- c(paste("(", signif(nl$beta_se[1], digits = 2), ")", sep = ""),
              paste("(", signif(nl$beta_se[2], digits = 2), ")", sep = ""),
              paste("(", signif(nl$beta_se[3], digits = 2), ")", sep = ""),
              paste("(", signif(nl$beta_se[4], digits = 2), ")", sep = ""))
  beta_4 <- c("Origin Ratio", "Extra Miles", "Tourism", "")
  beta_5 <- c(starmake(nl$beta[5], nl$beta_se[5]), starmake(nl$beta[6], nl$beta_se[6]),
              starmake(nl$beta[7], nl$beta_se[7]), "")
  beta_6 <- c(paste("(", signif(nl$beta_se[1], digits = 2), ")", sep = ""),
              paste("(", signif(nl$beta_se[2], digits = 2), ")", sep = ""),
              paste("(", signif(nl$beta_se[3], digits = 2), ")", sep = ""),
              "")
  
  beta_rows <- rbind(beta_1, beta_2, beta_3, beta_4, beta_5, beta_6)
  
  # Nesting Parameter Row:
  nesting <- c("Nesting Parameter:", starmake(nl$rho, nl$rho_se), "SE:", 
               paste("(", signif(nl$rho_se, digits = 2), ")", sep = ""))
  
  nobs <- c("N Products", nrow(nl_data), "N Markets", length(unique(nl_data$Market)))
  elasticity1 <- c("Mean Elasticity:", signif(mean(nl$extract_diagonal_means(nl$compute_elasticities()), digits = 3)), 
                   "Mean Markup:", signif(mean(nl$compute_markups(costs = nl$compute_costs())), digits = 3))
  
  # Now, Estimate Spirit, JetBlue Elasticities
  spirit_markets <-  unique(nl_data[Carrier == "Spirit Air Lines", market_ids])
  jb_markets <-  unique(nl_data[Carrier == "JetBlue Airways", market_ids])
  
  spirit_e <- c()
  jb_e <- c()
  
  for(i in 1:length(spirit_markets)){
    current_matrix <- nl$compute_elasticities(market_id = spirit_markets[i])
    mkt_products <- nl_data[market_ids == spirit_markets[i],]
    spirit_product <- (1:nrow(mkt_products))[mkt_products$Carrier == "Spirit Air Lines"]
    
    for(j in 1:length(spirit_product)){
      spirit_e <- c(spirit_e, current_matrix[spirit_product[j], spirit_product[j]])
    } 
    
    if(i %% 100 == 0){
      gc();
    }
  }
  
  for(i in 1:length(jb_markets)){
    current_matrix <- nl$compute_elasticities(market_id = jb_markets[i])
    mkt_products <- nl_data[market_ids == jb_markets[i],]
    jb_product <- (1:nrow(mkt_products))[mkt_products$Carrier == "JetBlue Airways"]
    
    for(j in 1:length(jb_product)){
      jb_e <- c(jb_e, current_matrix[jb_product[j], jb_product[j]])
    } 
    
    if(i %% 100 == 0){
      gc();
    }
  }
  
  elasticity_means <- c("Spirit Mean E", round(mean(spirit_e), digits = 2), 
                        "JetBlue Mean E", round(mean(jb_e), digits = 2))
  
  sumstat_rows <- rbind(nesting, nobs, elasticity1, elasticity_means)
  
  table_out <- rbind(beta_rows, sumstat_rows)
  
  rownames(table_out) <- NULL
  
  kbl(table_out,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = NULL) %>%
    row_spec(row = 9, hline_after = TRUE) %>%
    row_spec(row = 17, hline_after = TRUE) %>%
    pack_rows(group_label = "Linear Coefficients", 1,6) %>%
    pack_rows(group_label = "Nesting Coefficient", 7,7) %>%
    pack_rows(group_label = "Summary Statistics", 8, 10) %>%
    save_kable(file = output_table)
  
}

export_rcl_output_to_latex <- function(rcl_in = "03.Output/nested_rcl_optimal.pickle",
                                       data_in = "02.Intermediate/Product_Data.rds",
                                       output_table = "06.Tables/RCL_Results.tex"){
  rcl <- py_load_object(rcl_in)
  rcl_data <- readRDS(data_in)
  
  
  sigma1 <- c("", "Price", "Nonstop", "")
  sigma2 <- c("Coefficient", starmake(rcl$sigma[1,1], rcl$sigma_se[1,1]), 
              starmake(rcl$sigma[2,2], rcl$sigma_se[2,2]), 
              "")
  sigma3 <- c("SE", paste("(", signif(rcl$sigma_se[1,1], digits = 3), ")", sep = ""), 
              paste("(", signif(rcl$sigma_se[2,2], digits = 3), ")", sep = ""), 
              "")
  
  sigma_rows <- rbind(sigma1, sigma2, sigma3)
  
  beta1 <- c("Prices", "NonStop", "Miles Flown",  "Miles Squared")
  beta2 <- c(starmake(rcl$beta[1], rcl$beta_se[1]), 
             starmake(rcl$beta[2], rcl$beta_se[2]),
             starmake(rcl$beta[3], rcl$beta_se[3]),
             starmake(rcl$beta[4], rcl$beta_se[4]))
  beta3 <- c(paste("(", signif(rcl$beta_se[1], digits = 2), ")", sep = ""),
             paste("(", signif(rcl$beta_se[2], digits = 2), ")", sep = ""),
             paste("(", signif(rcl$beta_se[3], digits = 2), ")", sep = ""),
             paste("(", signif(rcl$beta_se[4], digits = 2), ")", sep = ""))
  beta4 <- c("Origin Service Ratio", "Extra Miles", "Tourist", "")
  beta5 <- c(starmake(rcl$beta[5], rcl$beta_se[5]),
             starmake(rcl$beta[6], rcl$beta_se[6]), starmake(rcl$beta[7], rcl$beta_se[7]), 
             "")
  beta6 <-  c(paste("(", signif(rcl$beta_se[5], digits = 2), ")", sep = ""),
              paste("(", signif(rcl$beta_se[6], digits = 2), ")", sep = ""),
              paste("(", signif(rcl$beta_se[7], digits = 2), ")", sep = ""), "")
  
  beta_rows <- rbind(beta1, beta2, beta3, beta4, beta5, beta6)
  
  # Now, Statistic Rows
  nesting <- c("Nesting Parameter:", starmake(rcl$rho, rcl$rho_se), "SE:", 
               paste("(", signif(rcl$rho_se, digits = 2), ")", sep = ""))
  nobs <- c("N Products", nrow(rcl_data), "N Markets", length(unique(rcl_data$Market)))
  elasticity1 <- c("Mean Elasticity:", round(mean(rcl$extract_diagonal_means(rcl$compute_elasticities())), digits = 3), 
                   "Mean Markup:", round(mean(rcl$compute_markups(costs = rcl$compute_costs())), digits = 3))
  
  # Now, Estimate Spirit, JetBlue Elasticities
  spirit_markets <-  unique(rcl_data[Carrier == "Spirit Air Lines", market_ids])
  jb_markets <-  unique(rcl_data[Carrier == "JetBlue Airways", market_ids])
  
  spirit_e <- c()
  jb_e <- c()
  
  for(i in 1:length(spirit_markets)){
    current_matrix <- rcl$compute_elasticities(market_id = spirit_markets[i])
    mkt_products <- rcl_data[market_ids == spirit_markets[i],]
    spirit_product <- (1:nrow(mkt_products))[mkt_products$Carrier == "Spirit Air Lines"]
    
    for(j in 1:length(spirit_product)){
      spirit_e <- c(spirit_e, current_matrix[spirit_product[j], spirit_product[j]])
    } 
    
    if(i %% 100 == 0){
      gc();
    }
  }
  
  for(i in 1:length(jb_markets)){
    current_matrix <- rcl$compute_elasticities(market_id = jb_markets[i])
    mkt_products <- rcl_data[market_ids == jb_markets[i],]
    jb_product <- (1:nrow(mkt_products))[mkt_products$Carrier == "JetBlue Airways"]
    
    for(j in 1:length(jb_product)){
      jb_e <- c(jb_e, current_matrix[jb_product[j], jb_product[j]])
    } 
    
    if(i %% 100 == 0){
      gc();
    }
  }
  
  elasticity_means <- c("Spirit Mean E", round(mean(spirit_e), digits = 2), 
                        "JetBlue Mean E", round(mean(jb_e), digits = 2))
  
  sumstat_rows <- rbind(nesting, nobs, elasticity1, elasticity_means)
  
  table_out <- rbind(beta_rows, sigma_rows, sumstat_rows)
  
  rownames(table_out) <- NULL
  
  kbl(table_out,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = NULL) %>%
    row_spec(row = 9, hline_after = TRUE) %>%
    row_spec(row = 17, hline_after = TRUE) %>%
    pack_rows(group_label = "Linear Coefficients", 1,6) %>%
    pack_rows(group_label = "Nonlinear Standard Deviations", 7, 9) %>%
    pack_rows(group_label = "Summary Statistics", 10, 13) %>%
    save_kable(file = output_table)
}

sp_jb_elasticity_mean <- function(rcl, rcl_data){
  # Now, Estimate Spirit, JetBlue Elasticities
  spirit_markets <-  unique(rcl_data[Carrier == "Spirit Air Lines", market_ids])
  jb_markets <-  unique(rcl_data[Carrier == "JetBlue Airways", market_ids])
  
  spirit_e <- c()
  jb_e <- c()
  
  for(i in 1:length(spirit_markets)){
    current_matrix <- rcl$compute_elasticities(market_id = spirit_markets[i])
    mkt_products <- rcl_data[market_ids == spirit_markets[i],]
    spirit_product <- (1:nrow(mkt_products))[mkt_products$Carrier == "Spirit Air Lines"]
    
    for(j in 1:length(spirit_product)){
      spirit_e <- c(spirit_e, current_matrix[spirit_product[j], spirit_product[j]])
    } 
    
    if(i %% 100 == 0){
      gc();
    }
  }
  
  for(i in 1:length(jb_markets)){
    current_matrix <- rcl$compute_elasticities(market_id = jb_markets[i])
    mkt_products <- rcl_data[market_ids == jb_markets[i],]
    jb_product <- (1:nrow(mkt_products))[mkt_products$Carrier == "JetBlue Airways"]
    
    for(j in 1:length(jb_product)){
      jb_e <- c(jb_e, current_matrix[jb_product[j], jb_product[j]])
    } 
    
    if(i %% 100 == 0){
      gc();
    }
  }
  
  return(c(round(mean(spirit_e), digits = 2), 
           round(mean(jb_e), digits = 2)))
}

two_model_make <- function(model_a, model_b, 
                            se_a, se_b,
                               id, label){
  if(dim(model_a)[2] > 1){
    row1 <- c(label, round(model_a[id,id], digits = 3), round(model_b[id,id], digits = 3))
    row2 <- c("", sd_format(se_a[id,id]), sd_format(se_b[id,id]))
  } else{
    row1 <- c(label, round(model_a[id], digits = 3), round(model_b[id], digits = 3))
    row2 <- c("", sd_format(se_a[id]), sd_format(se_b[id]))
  }
  return(rbind(row1, row2))
}

logit_two_period_table <- function(model.post.in = "03.Output/nested_logit_iv.pickle",
                                   product.post.in = "02.Intermediate/Product_Data.rds",
                                   model.pre.in = "03.Output/pre_pandemic_nested_logit_iv.pickle",
                                   product.pre.in = "02.Intermediate/prepandemic.rds",
                                   output = "06.Tables/NestedLogitResults.tex"){
   model.post <- py_load_object(model.post.in)
   product.post <- readRDS(product.post.in)
   model.pre <- py_load_object(model.pre.in)
   product.pre <- readRDS(product.pre.in)
  
  # First Group - Logit Means
  price <- two_model_make(label = "Price", model_a = model.post$beta,
                          model_b = model.pre$beta, se_a = model.post$beta_se,
                          se_b = model.pre$beta_se, id = 1)
  nonstop <- two_model_make(label = "Nonstop", model_a = model.post$beta,
                            model_b = model.pre$beta, se_a = model.post$beta_se,
                            se_b = model.pre$beta_se, id = 2)
  miles <- two_model_make(label = "Miles Flown", model_a = model.post$beta,
                          model_b = model.pre$beta, se_a = model.post$beta_se,
                          se_b = model.pre$beta_se, id = 3)
  miles_sq <- two_model_make(label = "Miles Flown$^2$", model_a = model.post$beta,
                          model_b = model.pre$beta, se_a = model.post$beta_se,
                          se_b = model.pre$beta_se, id = 4)
  serviceRatio <- two_model_make(label = "Origin Prescence", model_a = model.post$beta,
                                 model_b = model.pre$beta, se_a = model.post$beta_se,
                                 se_b = model.pre$beta_se, id = 5)
  extraMiles <- two_model_make(label = "Extra Miles", model_a = model.post$beta,
                               model_b = model.pre$beta, se_a = model.post$beta_se,
                               se_b = model.pre$beta_se, id = 6)
  extraMiles_sq <- two_model_make(label = "Extra Miles$^2$", model_a = model.post$beta,
                               model_b = model.pre$beta, se_a = model.post$beta_se,
                               se_b = model.pre$beta_se, id = 7)
  tourist <- two_model_make(label = "Tourist Route", model_a = model.post$beta,
                            model_b = model.pre$beta, se_a = model.post$beta_se,
                            se_b = model.pre$beta_se, id = 8)
  rho <- two_model_make(label = "Nesting Parameter", model_a = model.post$rho,
                        model_b = model.pre$rho, se_a = model.post$rho_se,
                        se_b = model.pre$rho_se,
                        id = 1)
  
  # Descriptive Statistics
  spirit_jb_elasticity_post <- sp_jb_elasticity_mean(model.post, product.post)
  spirit_jb_elasticity_pre <- sp_jb_elasticity_mean(model.pre, product.pre)
  
  spirit_e <- c("Mean Spirit Elasticity", spirit_jb_elasticity_post[1], spirit_jb_elasticity_pre[1])
  jetblue_e <- c("Mean JetBlue Elasticity", spirit_jb_elasticity_post[2],
                 spirit_jb_elasticity_pre[2])
  elasticity <- c("Mean Elasticity", round(mean(model.post$extract_diagonal_means(model.post$compute_elasticities())), digits = 3), 
                           round(mean(model.pre$extract_diagonal_means(model.pre$compute_elasticities())), digits = 3))
  
  # Summary Rows:
  obs <- c("Observations", nrow(product.post), nrow(product.pre))
  n_products <- c("Products", length(unique(product.post$product_ids)),
                  length(unique(product.pre$product_ids)))
  n_markets <- c("Markets", length(unique(product.post$market_ids)),
                 length(unique(product.pre$market_ids)))
  period <- c("Period", "2021Q2-2023Q2", "2017Q1-2019Q4")
  
  table <- rbind(price, nonstop, miles, miles_sq, serviceRatio, extraMiles,
                 extraMiles_sq, tourist, rho, spirit_e, 
                 jetblue_e, elasticity, obs, n_products, n_markets, period)
  
  kbl(table, row.names = FALSE, format= "latex",
      col.names = c("Variable", "Post-Pandemic", "Pre-Pandemic"),
      booktabs = TRUE, escape = FALSE, linesep = "") %>%
    row_spec(16, hline_after = TRUE) %>%
    save_kable(file = output)
}

logit_two_period_table_present <- function(model.post.in = "03.Output/nested_logit_iv.pickle",
                                         product.post.in = "02.Intermediate/Product_Data.rds",
                                         model.pre.in = "03.Output/pre_pandemic_nested_logit_iv.pickle",
                                         product.pre.in = "02.Intermediate/prepandemic.rds",
                                         output = "06.Tables/Presentation_NestedLogitResults.tex"){
  model.post <- py_load_object(model.post.in)
  product.post <- readRDS(product.post.in)
  model.pre <- py_load_object(model.pre.in)
  product.pre <- readRDS(product.pre.in)
  
  # First Group - Logit Means
  price <- two_model_make(label = "Price", model_a = model.post$beta,
                          model_b = model.pre$beta, se_a = model.post$beta_se,
                          se_b = model.pre$beta_se, id = 1)
  rho <- two_model_make(label = "Nesting Parameter", model_a = model.post$rho,
                        model_b = model.pre$rho, se_a = model.post$rho_se,
                        se_b = model.pre$rho_se,
                        id = 1)
  
  # Descriptive Statistics
  spirit_jb_elasticity_post <- sp_jb_elasticity_mean(model.post, product.post)
  spirit_jb_elasticity_pre <- sp_jb_elasticity_mean(model.pre, product.pre)
  
  spirit_e <- c("Mean Spirit Elasticity", spirit_jb_elasticity_post[1], spirit_jb_elasticity_pre[1])
  jetblue_e <- c("Mean JetBlue Elasticity", spirit_jb_elasticity_post[2],
                 spirit_jb_elasticity_pre[2])
  elasticity <- c("Mean Elasticity", round(mean(model.post$extract_diagonal_means(model.post$compute_elasticities())), digits = 3), 
                  round(mean(model.pre$extract_diagonal_means(model.pre$compute_elasticities())), digits = 3))
  
  # Summary Rows:
  obs <- c("Observations", nrow(product.post), nrow(product.pre))
  n_products <- c("Products", length(unique(product.post$product_ids)),
                  length(unique(product.pre$product_ids)))
  n_markets <- c("Markets", length(unique(product.post$market_ids)),
                 length(unique(product.pre$market_ids)))
  period <- c("Period", "2021Q2-2023Q2", "2017Q1-2019Q4")
  
  table <- rbind(price, rho, spirit_e, 
                 jetblue_e, elasticity, obs, n_products, n_markets, period)
  
  kbl(table, row.names = FALSE, format= "latex",
      col.names = c("Variable", "Post-Pandemic", "Pre-Pandemic"),
      booktabs = TRUE, escape = FALSE, linesep = "") %>%
    row_spec(4, hline_after = TRUE) %>%
    row_spec(7, hline_after = TRUE) %>%
    save_kable(file = output)
}

rcl_two_period_table <- function(post_in = "03.Output/random_coeff_nested_logit_fs_results.pickle",
                                 post_data_in = "02.Intermediate/Product_Data.rds",
                                 pre_in = "03.Output/prepandemic_random_coeff_nested_logit.pickle",
                                 pre_data_in = "02.Intermediate/prepandemic.rds",
                                 output_table = "06.Tables/RCL_Both_Period_Output.tex"){
  model.post <- py_load_object(post_in)
  product_post <- readRDS(post_data_in)
  model.pre <- py_load_object(pre_in)
  product_pre <- readRDS(pre_data_in)
  gc();
  
  # First Group - Logit Means
  price <- two_model_make(label = "Price", model_a = model.post$beta,
                          model_b = model.pre$beta, se_a = model.post$beta_se,
                          se_b = model.pre$beta_se, id = 1)
  nonstop <- two_model_make(label = "Nonstop", model_a = model.post$beta,
                            model_b = model.pre$beta, se_a = model.post$beta_se,
                            se_b = model.pre$beta_se, id = 2)
  miles <- two_model_make(label = "Miles Flown", model_a = model.post$beta,
                          model_b = model.pre$beta, se_a = model.post$beta_se,
                          se_b = model.pre$beta_se, id = 3)
  miles_sq <- two_model_make(label = "Miles Flown$^2$", model_a = model.post$beta,
                             model_b = model.pre$beta, se_a = model.post$beta_se,
                             se_b = model.pre$beta_se, id = 4)
  serviceRatio <- two_model_make(label = "Origin Prescence", model_a = model.post$beta,
                                 model_b = model.pre$beta, se_a = model.post$beta_se,
                                 se_b = model.pre$beta_se, id = 5)
  extraMiles <- two_model_make(label = "Extra Miles", model_a = model.post$beta,
                               model_b = model.pre$beta, se_a = model.post$beta_se,
                               se_b = model.pre$beta_se, id = 6)
  extraMiles_sq <- two_model_make(label = "Extra Miles$^2$", model_a = model.post$beta,
                                  model_b = model.pre$beta, se_a = model.post$beta_se,
                                  se_b = model.pre$beta_se, id = 7)
  tourist <- two_model_make(label = "Tourist Route", model_a = model.post$beta,
                            model_b = model.pre$beta, se_a = model.post$beta_se,
                            se_b = model.pre$beta_se, id = 8)
  rho <- two_model_make(label = "Nesting Parameter", model_a = model.post$rho,
                        model_b = model.pre$rho, se_a = model.post$rho_se,
                        se_b = model.pre$rho_se,
                        id = 1)  
  
  prices.nonlinear <- two_model_make(label = "Price", model_a = model.post$sigma,
                                     model_b = model.pre$sigma, se_a = model.post$sigma_se,
                                     se_b = model.pre$sigma_se, id = 1)
  nonstop.nonlinear <- two_model_make(label = "Nonstop", model_a = model.post$sigma,
                                     model_b = model.pre$sigma, se_a = model.post$sigma_se,
                                     se_b = model.pre$sigma_se, id = 2)
  miles.nonlinear <- two_model_make(label = "Miles Flown", model_a = model.post$sigma,
                                     model_b = model.pre$sigma, se_a = model.post$sigma_se,
                                     se_b = model.pre$sigma_se, id = 3)
  
  
  spirit_jb_elasticity_post <- sp_jb_elasticity_mean(model.post, product_post)
  spirit_jb_elasticity_pre <- sp_jb_elasticity_mean(model.pre, product_pre)
  
  summary_statistics1 <- c("Period", "2017Q1-2019Q4", "2021Q2-2023Q2")
  summary_statistics2 <- c("N Products", nrow(product_post), nrow(product_pre))
  summary_statistics3 <- c("N Markets", length(unique(product_post$market_ids)),
                           length(unique(product_pre$market_ids)))
  summary_statistics4 <- c("Mean Elasticity", round(mean(model.post$extract_diagonal_means(model.post$compute_elasticities())), digits = 3), 
                           round(mean(model.pre$extract_diagonal_means(model.pre$compute_elasticities())), digits = 3))
  summary_statistics5 <- c("Spirit Mean Elasticity", spirit_jb_elasticity_post[1], spirit_jb_elasticity_pre[1])
  summary_statistics6 <- c("JetBlue Mean Elasticity", spirit_jb_elasticity_post[2], spirit_jb_elasticity_pre[2])
  summary_statistics7 <- c("Mean Markup", round(mean(model.post$compute_markups(costs = model.post$compute_costs())), digits = 3),
                           round(mean(model.pre$compute_markups(costs = model.pre$compute_costs())), digits = 3))
  
  table_out <- rbind(price, nonstop, miles, miles_sq, serviceRatio, extraMiles,
                     extraMiles_sq, tourist, prices.nonlinear,
                     nonstop.nonlinear, miles.nonlinear, rho, 
                     summary_statistics1, summary_statistics2,
                     summary_statistics3, summary_statistics4, summary_statistics5,
                     summary_statistics6, summary_statistics7)
  
  rownames(table_out) <- NULL
  colnames(table_out) <- c("Variable", "Post-Pandemic", "Pre-Pandemic")
  table_out <- as.data.frame(table_out)
  
  
  kbl(table_out,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = c("Variable", "Post-Pandemic", "Pre-Pandemic"))  %>%
    row_spec(row = 16, hline_after = TRUE) %>%
    row_spec(row = 22, hline_after = TRUE) %>%
    row_spec(row = 24, hline_after = TRUE) %>%
    row_spec(row = 31, hline_after = TRUE) %>%
    pack_rows(group_label = "Linear Coefficients", 1,16) %>%
    pack_rows(group_label = "Nonlinear Coefficient", 17, 22) %>%
    pack_rows(group_label = "Nesting Coefficient", 23, 24) %>%
    pack_rows(group_label = "Summary Statistics", 25, 31) %>%
    save_kable(file = output_table)
  
  gc();
}

rcl_two_period_table_present <- function(post_in = "03.Output/random_coeff_nested_logit_fs_results.pickle",
                                         post_data_in = "02.Intermediate/Product_Data.rds",
                                         pre_in = "03.Output/prepandemic_random_coeff_nested_logit.pickle",
                                         pre_data_in = "02.Intermediate/prepandemic.rds",
                                         output_table = "06.Tables/RCL_Both_Period_Present.tex"){
  model.post <- py_load_object(post_in)
  product_post <- readRDS(post_data_in)
  model.pre <- py_load_object(pre_in)
  product_pre <- readRDS(pre_data_in)
  gc();
  
  # First Group - Logit Means
  price <- two_model_make(label = "Price", model_a = model.post$beta,
                          model_b = model.pre$beta, se_a = model.post$beta_se,
                          se_b = model.pre$beta_se, id = 1)
  rho <- two_model_make(label = "Nesting Parameter", model_a = model.post$rho,
                        model_b = model.pre$rho, se_a = model.post$rho_se,
                        se_b = model.pre$rho_se,
                        id = 1)  
  
  prices.nonlinear <- two_model_make(label = "Price ($\\sigma$)", model_a = model.post$sigma,
                                     model_b = model.pre$sigma, se_a = model.post$sigma_se,
                                     se_b = model.pre$sigma_se, id = 1)

  
  spirit_jb_elasticity_post <- sp_jb_elasticity_mean(model.post, product_post)
  spirit_jb_elasticity_pre <- sp_jb_elasticity_mean(model.pre, product_pre)
  
  summary_statistics1 <- c("Period", "2017Q1-2019Q4", "2021Q2-2023Q2")
  summary_statistics2 <- c("N Products", nrow(product_post), nrow(product_pre))
  summary_statistics3 <- c("N Markets", length(unique(product_post$market_ids)),
                           length(unique(product_pre$market_ids)))
  summary_statistics4 <- c("Mean Elasticity", round(mean(model.post$extract_diagonal_means(model.post$compute_elasticities())), digits = 3), 
                           round(mean(model.pre$extract_diagonal_means(model.pre$compute_elasticities())), digits = 3))
  summary_statistics5 <- c("Spirit Mean Elasticity", spirit_jb_elasticity_post[1], spirit_jb_elasticity_pre[1])
  summary_statistics6 <- c("JetBlue Mean Elasticity", spirit_jb_elasticity_post[2], spirit_jb_elasticity_pre[2])
  summary_statistics7 <- c("Mean Markup", round(mean(model.post$compute_markups(costs = model.post$compute_costs())), digits = 3),
                           round(mean(model.pre$compute_markups(costs = model.pre$compute_costs())), digits = 3))
  
  table_out <- rbind(price, prices.nonlinear,
                     rho, 
                     summary_statistics1, summary_statistics2,
                     summary_statistics3, summary_statistics4, summary_statistics5,
                     summary_statistics6, summary_statistics7)
  
  rownames(table_out) <- NULL
  colnames(table_out) <- c("Variable", "Post-Pandemic", "Pre-Pandemic")
  table_out <- as.data.frame(table_out)
  
  
  kbl(table_out,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      linesep = "",
      col.names = c("Variable", "Post-Pandemic", "Pre-Pandemic"))  %>%
    row_spec(row = 2, hline_after = TRUE) %>%
    row_spec(row = 4, hline_after = TRUE) %>%
    row_spec(row = 6, hline_after = TRUE) %>%
    save_kable(file = output_table)
  
  gc();
}
