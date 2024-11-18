logit_instrument_compare <- function(input_file = "02.Intermediate/Product_Data.rds",
                                        output_file = "06.Tables/Instrument_Compare_Table.tex"){
  product_data <- readRDS(input_file)
  product_data[, Time := paste(Year, Quarter)]
  
  # Regression 1: Outcome on Price, Miles Flown, Miles Flown Squared, Time Dummy Variables
  regression1 <- lm(Nevo_V_Outcome ~ prices + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                      Origin_Firm_Service_Ratio + Extra_Miles + Tourism + Carrier + Time,
                    data = product_data)
  
  # IV Formulations:
  first_stage_1 <- summary(lm(prices ~ GasMiles +
                                NonStop + MktMilesFlown + MktMilesFlown_Sq +
                                Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data))
  iv_regression1 <- ivreg(Nevo_V_Outcome ~ prices + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio | 
                            GasMiles + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, 
                          data = product_data)
  iv_regression1.summary <- summary(iv_regression1)
  
  first_stage_2 <- summary(lm(prices ~ GasMiles + GasMiles:NonStop + 
                                NonStop + MktMilesFlown + MktMilesFlown_Sq +
                                Extra_Miles + Tourism + Origin_Firm_Service_Ratio +
                                Carrier + Time, data = product_data))
  iv_regression2 <- ivreg(Nevo_V_Outcome ~ prices + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio | 
                            GasMiles + GasMiles:NonStop +
                            NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, 
                          data = product_data)
  iv_regression2.summary <- summary(iv_regression2)
  
  
  first_stage_3 <- summary(lm(prices ~ GasMiles + GasMiles:NonStop + 
                                GasMiles:Extra_Miles +
                                NonStop + MktMilesFlown + MktMilesFlown_Sq +
                                Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data))
  iv_regression3 <- ivreg(Nevo_V_Outcome ~ prices + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio | 
                            GasMiles + GasMiles:NonStop + GasMiles:Extra_Miles +
                            NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data)
  iv_regression3.summary <- summary(iv_regression3)
  
  first_stage_4 <- summary(lm(prices ~ GasMiles + GasMiles + GasMiles:NonStop + GasMiles:Origin_Firm_Service_Ratio + 
                                GasMiles:Extra_Miles + 
                                NonStop + MktMilesFlown + MktMilesFlown_Sq +
                                Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data))
  iv_regression4 <- ivreg(Nevo_V_Outcome ~ prices + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio | 
                            GasMiles + GasMiles:NonStop + GasMiles:Origin_Firm_Service_Ratio + 
                            GasMiles:Extra_Miles +
                            NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data)
  iv_regression4.summary <- summary(iv_regression4)
  
  first_stage_5 <- summary(lm(prices ~ GasMiles + GasMiles + GasMiles:NonStop + GasMiles:Origin_Firm_Service_Ratio + 
                                GasMiles:Origin_Hub + GasMiles:Extra_Miles + 
                                NonStop + MktMilesFlown + MktMilesFlown_Sq +
                                Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data))
  iv_regression5 <- ivreg(Nevo_V_Outcome ~ prices + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio | 
                            GasMiles + GasMiles:NonStop + GasMiles:Origin_Firm_Service_Ratio + 
                            GasMiles:Origin_Hub + GasMiles:Extra_Miles + 
                            NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data)
  iv_regression5.summary <- summary(iv_regression5)
  
  first_stage_6 <- summary(lm(prices ~ GasMiles + GasMiles + GasMiles:NonStop + GasMiles:Origin_Firm_Service_Ratio + 
                                GasMiles:Origin_Hub + GasMiles:Extra_Miles + MktMilesFlown:Extra_Miles +
                                NonStop + MktMilesFlown + MktMilesFlown_Sq +
                                Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data))
  iv_regression6 <- ivreg(Nevo_V_Outcome ~ prices + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio | 
                            GasMiles + GasMiles:NonStop + GasMiles:Origin_Firm_Service_Ratio + 
                            GasMiles:Origin_Hub + GasMiles:Extra_Miles +
                            NonStop:MktMilesFlown + NonStop:MktMilesFlown_Sq + 
                            NonStop:Tourism + NonStop:Origin_Firm_Service_Ratio + 
                            NonStop + MktMilesFlown + MktMilesFlown_Sq +
                            Extra_Miles + Tourism + Carrier + Time + Origin_Firm_Service_Ratio, data = product_data)
  iv_regression6.summary <- summary(iv_regression6)
  
  # Now, Compute Elasticities
  product_data[, Reg1_Elasticity := as.numeric(regression1$coefficients[2]) * prices * (1 - shares)]
  product_data[, IV_Reg1_Elasticity := as.numeric(iv_regression1$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg2_Elasticity := as.numeric(iv_regression2$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg3_Elasticity := as.numeric(iv_regression3$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg4_Elasticity := as.numeric(iv_regression4$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg5_Elasticity := as.numeric(iv_regression5$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg6_Elasticity := as.numeric(iv_regression6$coefficients[2]) * prices * (1- shares)]
  
  models <- list(regression1,iv_regression1, 
                 iv_regression2, iv_regression3, iv_regression4, iv_regression5,
                 iv_regression6)
  headers <- list("OLS" = 1, 
                  "IV" = 2:7)
  
  fit_over_id <- c("", "N/A", round(iv_regression2.summary$diagnostics[3,3], 2),
                   round(iv_regression3.summary$diagnostics[3,3], 2),
                   round(iv_regression4.summary$diagnostics[3,3], 2),
                   round(iv_regression5.summary$diagnostics[3,3], 2),
                   round(iv_regression6.summary$diagnostics[3,3], 2))
  
  id_critical_statistic <- c("N/A", round(1 - c(pchisq(q = iv_regression2.summary$diagnostics[3,3], df = iv_regression1.summary$diagnostics[3,1]),
                                                pchisq(q = iv_regression3.summary$diagnostics[3,3], df = iv_regression1.summary$diagnostics[3,1]),
                                                pchisq(q = iv_regression4.summary$diagnostics[3,3], df = iv_regression4.summary$diagnostics[3,1]),
                                                pchisq(q = iv_regression5.summary$diagnostics[3,3], df = iv_regression5.summary$diagnostics[3,1]),
                                                pchisq(q = iv_regression6.summary$diagnostics[3,3], df = iv_regression6.summary$diagnostics[3,1])),
                                          2))
  
  
  first_stage_r2 <- round(c(summary(regression1)$adj.r.squared, 
                            unname(first_stage_1$adj.r.squared)[1], unname(first_stage_2$adj.r.squared)[1],
                            unname(first_stage_3$adj.r.squared)[1], unname(first_stage_4$adj.r.squared)[1],
                            unname(first_stage_5$adj.r.squared)[1], unname(first_stage_6$adj.r.squared)[1]), 4)
  
  first_stage_f_statistic <- round(c(unname(summary(regression1)$fstatistic)[1],
                                     unname(first_stage_1$fstatistic)[1], unname(first_stage_2$fstatistic)[1], 
                                     unname(first_stage_3$fstatistic)[1], unname(first_stage_4$fstatistic)[1],
                                     unname(first_stage_5$fstatistic)[1], unname(first_stage_6$fstatistic)[1]),
                                   digits = 2)
  
  f_p_1 <- 1 - pf(q = unname(summary(regression1)$fstatistic)[1],
                  df1 = unname(summary(regression1)$fstatistic)[2],
                  df2 = unname(summary(regression1)$fstatistic)[3])
  
  f_p_1_iv <- 1 - pf(q = unname(first_stage_1$fstatistic)[1], df1 = unname(first_stage_1$fstatistic)[2], 
                     df2 = unname(first_stage_1$fstatistic)[3])
  f_p_2_iv <- 1 - pf(q = unname(first_stage_2$fstatistic)[1], df1 = unname(first_stage_2$fstatistic)[2], 
                     df2 = unname(first_stage_2$fstatistic)[3])
  f_p_3_iv <- 1 - pf(q = unname(first_stage_3$fstatistic)[1], df1 = unname(first_stage_3$fstatistic)[2], 
                     df2 = unname(first_stage_3$fstatistic)[3])
  f_p_4_iv <- 1 - pf(q = unname(first_stage_4$fstatistic)[1], df1 = unname(first_stage_4$fstatistic)[2], 
                     df2 = unname(first_stage_4$fstatistic)[3])
  f_p_5_iv <- 1 - pf(q = unname(first_stage_5$fstatistic)[1], df1 = unname(first_stage_5$fstatistic)[2], 
                     df2 = unname(first_stage_5$fstatistic)[3])
  f_p_6_iv <- 1 - pf(q = unname(first_stage_6$fstatistic)[1], df1 = unname(first_stage_6$fstatistic)[2], 
                     df2 = unname(first_stage_6$fstatistic)[3])
  first_stage_f_p <-  round(c(f_p_1, f_p_1_iv, f_p_2_iv, 
                              f_p_3_iv, f_p_4_iv, f_p_5_iv, f_p_6_iv),
                            digits = 4)
  
  wu_hausman <- c("", round(c(iv_regression1.summary$diagnostics[2, 3], iv_regression2.summary$diagnostics[2, 3], 
                              iv_regression3.summary$diagnostics[2, 3], iv_regression4.summary$diagnostics[2, 3],
                              iv_regression5.summary$diagnostics[2, 3], iv_regression6.summary$diagnostics[2, 3]), digits = 2))
  wu_hausman_p <- c("", round(c(iv_regression1.summary$diagnostics[2, 4], iv_regression2.summary$diagnostics[2, 4], 
                                iv_regression3.summary$diagnostics[2, 4], iv_regression4.summary$diagnostics[2, 4],
                                iv_regression5.summary$diagnostics[2, 4], iv_regression6.summary$diagnostics[2, 4]), digits = 5))
  
  
  extra_rows <- list("Added Instrument" = c("", "GasMiles", 
                                            "GM:NonStop", "GM:ExtraMiles", "GM:OriginService", "GM:OriginHub", "Exog. Interact"),
                     # "Time Fixed Effects" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     # "Firm Fixed Effects" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     "Wu-Hausman Test" = wu_hausman,
                     "P-Value" = wu_hausman_p, 
                     "Test of Over Identification" = fit_over_id,
                     "p-value" = c("",  id_critical_statistic),
                     "First Stage F-Statistic:" = first_stage_f_statistic,
                     "p-value" = c(first_stage_f_p), 
                     "First Stage Adj. $R^2$" = as.character(first_stage_r2),
                     "Mean Elasticity" = round(c(mean(product_data$Reg1_Elasticity), 
                                                 mean(product_data$IV_Reg1_Elasticity), mean(product_data$IV_Reg2_Elasticity),
                                                 mean(product_data$IV_Reg3_Elasticity), mean(product_data$IV_Reg4_Elasticity),
                                                 mean(product_data$IV_Reg5_Elasticity), mean(product_data$IV_Reg6_Elasticity)),2),
                     "Median Elasticity" = round(c(median(product_data$Reg1_Elasticity), 
                                                   median(product_data$IV_Reg1_Elasticity), median(product_data$IV_Reg2_Elasticity),
                                                   median(product_data$IV_Reg3_Elasticity), median(product_data$IV_Reg4_Elasticity),
                                                   median(product_data$IV_Reg5_Elasticity), median(product_data$IV_Reg6_Elasticity)),2),
                     "Share Inelastic Products" = round(c(nrow(product_data[abs(Reg1_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg1_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg2_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg3_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg4_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg5_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg6_Elasticity) < 1])/nrow(product_data)), 2),
                     "Share JB Inelastic Products" = 
                       round(c(nrow(product_data[JetBlue == TRUE & abs(Reg1_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg1_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg2_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg3_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg4_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg5_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg6_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE])), 2),
                     "Share SP Inelastic Products" = 
                       round(c(nrow(product_data[Spirit == TRUE & abs(Reg1_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg1_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg2_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg3_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg4_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg5_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg6_Elasticity) < 1])/nrow(product_data[Spirit == TRUE])), 2))
  
  
  omit_coeffs <- paste("(Time", "Intercept", "Carrier", "Other_Carrier)",
                       sep = ")|(")
  texreg(l = models,
         custom.header = headers,
         omit.coef = omit_coeffs,
         center = TRUE,
         include.rsquared = FALSE,
         include.adjrs = FALSE,
         booktabs = TRUE,
         custom.gof.rows = extra_rows,
         use.packages = FALSE,
         table = FALSE,
         file = output_file)
}

instrument_comparison_table <- function(input_file = "02.Intermediate/Product_Data.rds",
                                        output_file = "06.Tables/Instrument_Compare_Table.tex"){
  product_data <- readRDS(input_file)
  product_data[, Time := paste(Year, Quarter)]
  product_data[, Quarter := as.character(Quarter)]
  product_data[, Year := as.character(Year)]
  
  
  # Parts of Formula
  lhs <- "Share_Differences ~ prices + Log_Share_Ratio + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                      Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + Carrier + Time"
  back <- "NonStop + MktMilesFlown + MktMilesFlown_Sq +
                      Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + Carrier + Time"
  rho_instrument <- "Num_Products_In_Market + "
  
  gasmiles <- "GasMiles + GasMiles_Sq + GasMiles_ExtraMiles + GasMiles_NonStop + 
  GasMiles_Origin_Prescence + GasMiles:Extra_Miles_Sq + "
 
  exog_interactions <- "MktMiles_NonStop + MktMiles_Sq_NonStop + MktMiles_Origin_Share + 
  MktMiles_Extra + MktMilesFlown:Extra_Miles_Sq + MktMiles_Sq_Origin_Share + MktMiles_Sq_NonStop + 
  MktMiles_Sq_Extra + NonStop_Origin_Share + NonStop_ExtraMiles +
  OriginRatio_Extra +"
 
  origin_interactions <- "Origin_Hub + MktMiles_OriginHub +
                MktMilesSq_OriginHub + NonStop:Origin_Hub + "
  
  dest_interactions <- "Destination_Hub + MktMiles_DestinationHub +
                    MktMilesSq_DestinationHub + NonStop:Destination_Hub + "
  
  hub_interactions <- "Hub_Endpoint + Hub_End_MktMiles + Hub_End_MktMiles_Sq +" 
  
  # Make Gandhi Instrument Vector
  gandhi_instrument <- ""
  
  max_gandhi <- max(as.numeric(substr(colnames(product_data)[grepl(pattern = "demand_instruments", 
                                                    x = colnames(product_data))],
                      start = 19, stop = 20)))
  for(i in 0:max_gandhi){
    gandhi_instrument <- paste(gandhi_instrument, paste("demand_instruments", i, sep = ""), sep = " + ")
  }
  gandhi_instrument <- paste(gandhi_instrument, "+")
  
  # Regression 1: Outcome on Price, Miles Flown, Miles Flown Squared, Time Dummy Variables
  regression1 <- lm(lhs,
                    data = product_data)
  
  # Regression 2: Instrument with GasMiles
  iv_regression2 <- ivreg(paste(lhs, "|", gasmiles, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression2.summary <- summary(iv_regression2)
  iv_regression2.summary
  
  # Regression 3: Instrument with Origin Interactions
  iv_regression3 <- ivreg(paste(lhs, "|", hub_interactions, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression3.summary <- summary(iv_regression3)
  iv_regression3.summary
  
  # Regression 4: Instrument with Destination Interactions
  iv_regression4 <- ivreg(paste(lhs, "|", hub_interactions, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression4.summary <- summary(iv_regression4)
  iv_regression4.summary
  
  # Regression 5: Origin, GM
  iv_regression5 <- ivreg(paste(lhs, "|", gasmiles, hub_interactions, 
                                rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression5.summary <- summary(iv_regression5)
  iv_regression5.summary
  
  
  
  # Regression 6: Origin, Gandhi
  iv_regression6 <- ivreg(paste(lhs, "|", hub_interactions, 
                                gandhi_instrument,
                                rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression6.summary <- summary(iv_regression6)
  iv_regression6.summary
  
  # Regression 7: Origin, GM, Gandhi
  iv_regression7 <- ivreg(paste(lhs, "|", gasmiles, hub_interactions, 
                                gandhi_instrument,
                                rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression7.summary <- summary(iv_regression7)
  iv_regression7.summary
  
  
  # Regression 8: Origin, Gandhi, Exog Interactions
  iv_regression8 <- ivreg(paste(lhs, "|", hub_interactions, exog_interactions,
                                gandhi_instrument,
                                rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression8.summary <- summary(iv_regression8)
  iv_regression8.summary
  
  # Regression 9: Origin, Dest, Gandhi, Exog
  iv_regression9 <- ivreg(paste(lhs, "|", gasmiles, hub_interactions,
                                exog_interactions,
                                gandhi_instrument,
                                rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression9.summary <- summary(iv_regression9)
  iv_regression9.summary
  
  
  # Now, Compute Elasticities
  product_data[, Reg1_Elasticity := as.numeric(regression1$coefficients[2]) * prices * (1 - shares)]
  product_data[, IV_Reg2_Elasticity := as.numeric(iv_regression2$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg3_Elasticity := as.numeric(iv_regression3$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg4_Elasticity := as.numeric(iv_regression4$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg5_Elasticity := as.numeric(iv_regression5$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg6_Elasticity := as.numeric(iv_regression6$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg7_Elasticity := as.numeric(iv_regression7$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg8_Elasticity := as.numeric(iv_regression8$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg9_Elasticity := as.numeric(iv_regression9$coefficients[2]) * prices * (1- shares)]
  
  wu_hausman_price <- c("", round(c(iv_regression2.summary$diagnostics[3, 3], 
                              iv_regression3.summary$diagnostics[3, 3], iv_regression4.summary$diagnostics[3, 3],
                              iv_regression5.summary$diagnostics[3, 3], iv_regression6.summary$diagnostics[3, 3],
                              iv_regression7.summary$diagnostics[3, 3], iv_regression8.summary$diagnostics[3, 3],
                              iv_regression9.summary$diagnostics[3, 3]), 
                              digits = 3))
  wu_hausman_price_p <- c("", round(c(iv_regression2.summary$diagnostics[3, 4], 
                                iv_regression3.summary$diagnostics[3, 4], iv_regression4.summary$diagnostics[3, 4],
                                iv_regression5.summary$diagnostics[3, 4], iv_regression6.summary$diagnostics[3, 4],
                                iv_regression7.summary$diagnostics[3, 4], iv_regression8.summary$diagnostics[3, 4],
                                iv_regression9.summary$diagnostics[3, 4]), 
                                digits = 3))
  
  
  fit_over_id <- c("", "N/A",
                   round(iv_regression3.summary$diagnostics[4,3], 2),
                   round(iv_regression4.summary$diagnostics[4,3], 2),
                   round(iv_regression5.summary$diagnostics[4,3], 2),
                   round(iv_regression6.summary$diagnostics[4,3], 2),
                   round(iv_regression7.summary$diagnostics[4,3], 2),
                   round(iv_regression8.summary$diagnostics[4,3], 2),
                   round(iv_regression9.summary$diagnostics[4,3], 2))
  sargan_p <- c("", "N/A", 
              round(iv_regression3.summary$diagnostics[4,4], 2),
              round(iv_regression4.summary$diagnostics[4,4], 2),
              round(iv_regression5.summary$diagnostics[4,4], 2),
              round(iv_regression6.summary$diagnostics[4,4], 2),
              round(iv_regression7.summary$diagnostics[4,4], 2),
              round(iv_regression8.summary$diagnostics[4,4], 2),
              round(iv_regression9.summary$diagnostics[4,4], 2))
  
  
  extra_rows <- list("Products in Market" = c("", rep("X", times = 8)),
                     "Gas Instruments" = c("", "X","",
                                           "", "X", "",
                                           "X", "", "X"),
                     "Hub Interactions" = c("", "", "X",
                                               "X", "X", "X",
                                               "X", "X", "X"),
                     "Gandhi Instruments" = c("", "", "", "",
                                              "", "X", "X", "X",
                                              "X"),
                     "Exog Interactions" = c("", "", "", "",
                                             "", "", "", "X",
                                             "X"),
                     "Price Test" = wu_hausman_price,
                     "P-Value" = wu_hausman_price_p, 
                     "Test of Over Identification" = fit_over_id,
                     "p-value" = sargan_p,
                     "Mean Elasticity" = round(c(mean(product_data$Reg1_Elasticity),  mean(product_data$IV_Reg2_Elasticity),
                                                 mean(product_data$IV_Reg3_Elasticity), mean(product_data$IV_Reg4_Elasticity),
                                                 mean(product_data$IV_Reg5_Elasticity), mean(product_data$IV_Reg6_Elasticity),
                                                 mean(product_data$IV_Reg7_Elasticity), mean(product_data$IV_Reg8_Elasticity),
                                                 mean(product_data$IV_Reg9_Elasticity)),2),
                     "Median Elasticity" = round(c(median(product_data$Reg1_Elasticity),  median(product_data$IV_Reg2_Elasticity),
                                                   median(product_data$IV_Reg3_Elasticity), median(product_data$IV_Reg4_Elasticity),
                                                   median(product_data$IV_Reg5_Elasticity), median(product_data$IV_Reg6_Elasticity),
                                                   median(product_data$IV_Reg7_Elasticity), median(product_data$IV_Reg8_Elasticity),
                                                   median(product_data$IV_Reg9_Elasticity)),2),
                     "Share Inelastic Products" = round(c(nrow(product_data[abs(Reg1_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg2_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg3_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg4_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg5_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg6_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg7_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg8_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg9_Elasticity) < 1])/nrow(product_data)), 2),
                     "Share JB Inelastic Products" = 
                       round(c(nrow(product_data[JetBlue == TRUE & abs(Reg1_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg2_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg3_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg4_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg5_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg6_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg7_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg8_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg9_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE])), 2),
                     "Share SP Inelastic Products" = 
                       round(c(nrow(product_data[Spirit == TRUE & abs(Reg1_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg2_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg3_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg4_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg5_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg6_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg7_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg8_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg9_Elasticity) < 1])/nrow(product_data[Spirit == TRUE])), 2))
  models <- list(regression1, 
                 iv_regression2, iv_regression3, iv_regression4, iv_regression5,
                 iv_regression6, iv_regression7, iv_regression8, iv_regression9)
  
  omit_coeffs <- paste("(Time", "Intercept", "Carrier", "Other_Carrier)",
                       sep = ")|(")
  
 coef.map <- list("prices" = "Price",
                          "Log_Share_Ratio" = "Nesting")
  texreg(l = models,
         custom.coef.map = coef.map,
         center = TRUE,
         include.rsquared = FALSE,
         include.adjrs = FALSE,
         booktabs = TRUE,
         custom.gof.rows = extra_rows,
         use.packages = FALSE,
         table = FALSE,
         file = output_file)
  
}

prepan_instrument_comparison_table <- function(input_file = "02.Intermediate/Product_Data.rds",
                                        output_file = "06.Tables/GM_Instrument_Compare_Table.tex"){
  product_data <- readRDS(input_file)
  product_data[, Time := paste(Year, Quarter)]
  product_data[, Quarter := as.character(Quarter)]
  product_data[, Year := as.character(Year)]
  
  # Parts of Formula
  lhs <- "Share_Differences ~ prices + Log_Share_Ratio + NonStop + MktMilesFlown + MktMilesFlown_Sq +
                      Origin_Firm_Service_Ratio + Extra_Miles + Tourism + Carrier + Time"
  back <- "NonStop + MktMilesFlown + MktMilesFlown_Sq +
                      Origin_Firm_Service_Ratio + Extra_Miles + Tourism + Carrier + Time"
  rho_instrument <- c("Num_Products_In_Market + ")
  
  gasmiles_basic <- c("GasMiles + ")
  gasmiles_adv <- "GasMiles:Origin_Firm_Service_Ratio + 
    GasMiles_NonStop + "
  
  exog_interactions <- "NonStop:MktMilesFlown + NonStop:MktMilesFlown_Sq + NonStop:Origin_Firm_Service_Ratio + NonStop:Extra_Miles +
  MktMilesFlown:MktMilesFlown_Sq + MktMilesFlown:Origin_Firm_Service_Ratio + MktMilesFlown:Extra_Miles + MktMilesFlown_Sq:Origin_Firm_Service_Ratio +
  MktMilesFlown_Sq:Extra_Miles + Origin_Firm_Service_Ratio:Extra_Miles + Origin_Firm_Service_Ratio:Extra_Miles_Sq + "
  
  blp_instruments <- "Average_Other_Distance +Average_Other_Distance_Square + Rival_Direct +
  Average_Rival_Origin_Ratio + Average_Rival_Dest_Ratio + "
  
  hub_interactions <- "Hub_Endpoint + Hub_End_MktMiles + Hub_End_MktMiles_Sq +"
  
  gandhi_instrument <- c()
  max_gandhi <- max(as.numeric(substr(colnames(product_data)[grepl(pattern = "demand_instruments", 
                                                                   x = colnames(product_data))],
                                      start = 19, stop = 20)))
  for(i in 0:max_gandhi){
    gandhi_instrument <- paste(gandhi_instrument, paste("demand_instruments", i, sep = ""), sep = " + ")
  }
  gandhi_instrument <- paste(gandhi_instrument, "+")
  
  # Regression 1: Outcome on Price, Miles Flown, Miles Flown Squared, Time Dummy Variables
  regression1 <- lm(lhs,
                    data = product_data)
  
  # Regression 2: Instrument with GasMiles
  iv_regression2 <- ivreg(paste(lhs, "|", gasmiles_basic, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression2.summary <- summary(iv_regression2)
  iv_regression2.summary
  
  # Regression 3: GasMiles Interactions
  iv_regression3 <- ivreg(paste(lhs, "|", gasmiles_basic, gasmiles_adv, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression3.summary <- summary(iv_regression3)
  iv_regression3.summary
  
  # Regression 4: Exogeneous Interactions
  iv_regression4 <- ivreg(paste(lhs, "|", exog_interactions, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression4.summary <- summary(iv_regression4)
  iv_regression4.summary
  
  # Gandhi Instruments
  iv_regression5 <- ivreg(paste(lhs, "|", gandhi_instrument, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression5.summary <- summary(iv_regression5)
  iv_regression5.summary
  
  # Hub Interactions
  iv_regression6 <- ivreg(paste(lhs, "|", hub_interactions, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression6.summary <- summary(iv_regression6)
  iv_regression6.summary
  
  # Hub GasMiles
  iv_regression7 <- ivreg(paste(lhs, "|", gasmiles_basic, gasmiles_adv, hub_interactions, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression7.summary <- summary(iv_regression7)
  iv_regression7.summary
  
  # Hub Gandhi
  iv_regression8 <- ivreg(paste(lhs, "|", gasmiles_basic, gasmiles_adv, gandhi_instrument, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression8.summary <- summary(iv_regression8)
  iv_regression8.summary
  
  # Hub Interactions
  iv_regression9 <- ivreg(paste(lhs, "|", gasmiles_basic, gasmiles_adv, exog_interactions, rho_instrument, back, sep = " "), 
                          data = product_data)
  iv_regression9.summary <- summary(iv_regression9)
  iv_regression9.summary
  
  
  # Now, Compute Elasticities
  product_data[, Reg1_Elasticity := as.numeric(regression1$coefficients[2]) * prices * (1 - shares)]
  product_data[, IV_Reg2_Elasticity := as.numeric(iv_regression2$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg3_Elasticity := as.numeric(iv_regression3$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg4_Elasticity := as.numeric(iv_regression4$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg5_Elasticity := as.numeric(iv_regression5$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg6_Elasticity := as.numeric(iv_regression6$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg7_Elasticity := as.numeric(iv_regression7$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg8_Elasticity := as.numeric(iv_regression8$coefficients[2]) * prices * (1- shares)]
  product_data[, IV_Reg9_Elasticity := as.numeric(iv_regression9$coefficients[2]) * prices * (1- shares)]
  
  wu_hausman_price <- c("", round(c(iv_regression2.summary$diagnostics[3, 3], 
                                    iv_regression3.summary$diagnostics[3, 3], iv_regression4.summary$diagnostics[3, 3],
                                    iv_regression5.summary$diagnostics[3, 3], iv_regression6.summary$diagnostics[3, 3],
                                    iv_regression7.summary$diagnostics[3, 3], iv_regression8.summary$diagnostics[3, 3],
                                    iv_regression9.summary$diagnostics[3, 3]), 
                                  digits = 3))
  wu_hausman_price_p <- c("", round(c(iv_regression2.summary$diagnostics[3, 4], 
                                      iv_regression3.summary$diagnostics[3, 4], iv_regression4.summary$diagnostics[3, 4],
                                      iv_regression5.summary$diagnostics[3, 4], iv_regression6.summary$diagnostics[3, 4],
                                      iv_regression7.summary$diagnostics[3, 4], iv_regression8.summary$diagnostics[3, 4],
                                      iv_regression9.summary$diagnostics[3, 4]), 
                                    digits = 3))
  
  
  fit_over_id <- c("", "N/A",
                   round(iv_regression3.summary$diagnostics[4,3], 2),
                   round(iv_regression4.summary$diagnostics[4,3], 2),
                   round(iv_regression5.summary$diagnostics[4,3], 2),
                   round(iv_regression6.summary$diagnostics[4,3], 2),
                   round(iv_regression7.summary$diagnostics[4,3], 2),
                   round(iv_regression8.summary$diagnostics[4,3], 2),
                   round(iv_regression9.summary$diagnostics[4,3], 2))
  sargan_p <- c("", "N/A", 
                round(iv_regression3.summary$diagnostics[4,4], 2),
                round(iv_regression4.summary$diagnostics[4,4], 2),
                round(iv_regression5.summary$diagnostics[4,4], 2),
                round(iv_regression6.summary$diagnostics[4,4], 2),
                round(iv_regression7.summary$diagnostics[4,4], 2),
                round(iv_regression8.summary$diagnostics[4,4], 2),
                round(iv_regression9.summary$diagnostics[4,4], 2))
  
  
  extra_rows <- list("Added Instrument" = c("", "GasMiles", "Gasmiles+",
                                            "Exog Inter.", "Gandhi", "Hub",
                                            "GasMiles+", "GasMiles+", "GasMiles+"),
                     "Add. Instrument" = c("", "","",
                                           "", "", "",
                                           "Hub", 
                                           "Gandhi", "Exog. Int."),
                     "Price Test" = wu_hausman_price,
                     "P-Value" = wu_hausman_price_p, 
                     "Test of Over Identification" = fit_over_id,
                     "p-value" = sargan_p,
                     "Mean Elasticity" = round(c(mean(product_data$Reg1_Elasticity),  mean(product_data$IV_Reg2_Elasticity),
                                                 mean(product_data$IV_Reg3_Elasticity), mean(product_data$IV_Reg4_Elasticity),
                                                 mean(product_data$IV_Reg5_Elasticity), mean(product_data$IV_Reg6_Elasticity),
                                                 mean(product_data$IV_Reg7_Elasticity), mean(product_data$IV_Reg8_Elasticity),
                                                 mean(product_data$IV_Reg9_Elasticity)),2),
                     "Median Elasticity" = round(c(median(product_data$Reg1_Elasticity),  median(product_data$IV_Reg2_Elasticity),
                                                   median(product_data$IV_Reg3_Elasticity), median(product_data$IV_Reg4_Elasticity),
                                                   median(product_data$IV_Reg5_Elasticity), median(product_data$IV_Reg6_Elasticity),
                                                   median(product_data$IV_Reg7_Elasticity), median(product_data$IV_Reg8_Elasticity),
                                                   median(product_data$IV_Reg9_Elasticity)),2),
                     "Share Inelastic Products" = round(c(nrow(product_data[abs(Reg1_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg2_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg3_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg4_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg5_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg6_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg7_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg8_Elasticity) < 1])/nrow(product_data),
                                                          nrow(product_data[abs(IV_Reg9_Elasticity) < 1])/nrow(product_data)), 2),
                     "Share JB Inelastic Products" = 
                       round(c(nrow(product_data[JetBlue == TRUE & abs(Reg1_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg2_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg3_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg4_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg5_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg6_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg7_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg8_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE]),
                               nrow(product_data[JetBlue == TRUE & abs(IV_Reg9_Elasticity) < 1])/nrow(product_data[JetBlue == TRUE])), 2),
                     "Share SP Inelastic Products" = 
                       round(c(nrow(product_data[Spirit == TRUE & abs(Reg1_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg2_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg3_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg4_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg5_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg6_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg7_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg8_Elasticity) < 1])/nrow(product_data[Spirit == TRUE]),
                               nrow(product_data[Spirit == TRUE & abs(IV_Reg9_Elasticity) < 1])/nrow(product_data[Spirit == TRUE])), 2))
  models <- list(regression1, 
                 iv_regression2, iv_regression3, iv_regression4, iv_regression5,
                 iv_regression6, iv_regression7, iv_regression8, iv_regression9)
  
  omit_coeffs <- paste("(Time", "Intercept", "Carrier", "Other_Carrier)",
                       sep = ")|(")
  
  coef.map <- list("prices" = "Price",
                   "Log_Share_Ratio" = "Nesting")
  texreg(l = models,
         custom.coef.map = coef.map,
         center = TRUE,
         include.rsquared = FALSE,
         include.adjrs = FALSE,
         booktabs = TRUE,
         custom.gof.rows = extra_rows,
         use.packages = FALSE,
         table = FALSE,
         file = output_file)
  
}

