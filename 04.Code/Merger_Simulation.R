merger_sim_data_generate <- function(input_file = "02.Intermediate/DB1B_With_Controls.Rds",
                                     output_file = "02.Intermediate/Product_Data.rds",
                                     years_allowed = 2021:2023,
                                     fast = FALSE){
  # Variables used for grouping rows into market categories
  market_group <- c("Year", "Quarter", "Origin", "Origin_MSA", "Dest", "Destination_MSA")
  
  product_group <- c(market_group, "Carrier", "NonStop")
  
  # First, Generate Product Data
  product_data <- as.data.table(readRDS(input_file))
  
  # Drop Products with Severe Outliers for Distance
  product_data <- product_data[MktMilesFlown < quantile(MktMilesFlown, probs = (0.99))]
  
  # Restrict to sample period
  product_data <- product_data[Year %in% years_allowed, ]; gc();

  if(2021 %in% years_allowed){
    product_data <- product_data[!(Year == 2021 & Quarter < 2),]
  }
  
  # Add Numeric ID for market to help PyBLP
  product_data[, Market := paste(Year, Quarter, Origin, Dest)]
  product_data[, Market := factor(Market)]
  product_data[, Market_ID := as.numeric(Market)]
  
  # Remove All Airports out of the top 100 in Q2 2022
  product_data[, Is.Small := is.na(Origin.Population) | is.na(Destination.Population)]
  small <- unique(product_data[, .(Market_ID, Is.Small, Passengers.Inside.Market)])
  numerator <- nrow(small[Is.Small == TRUE,])
  denominator <- nrow(small) 
  print(paste("Small Markets:", numerator / denominator * 100, " Of Sample"))
  num.passengers <- sum(small[Is.Small == TRUE, Passengers.Inside.Market])
  den.passengers <- sum(small[, Passengers.Inside.Market])
  print(paste("Small Markets Ridership Share:", num.passengers / den.passengers * 100, 
              "Of Sample"))
  
  product_data <- product_data[!is.na(Origin.Population),]
  product_data <- product_data[!is.na(Destination.Population),]
  gc();
  
  
  # Drop Hawaiian, Alaskan Markets
  hi_ak <- c("Kahului-Wailuku-Lahaina, HI", "San Juan-Carolina-Caguas, PR", "Kapaa, HI Mic", "Hilo, HI Mic",
             "Urban Honolulu, HI", "Anchorage, AK")
  product_data <- product_data[!(Origin_MSA %in% hi_ak),]
  product_data <- product_data[!(Destination_MSA %in% hi_ak),]
  gc();
  
  
  product_data[, Potential_Passengers := sqrt(Origin.Population) * sqrt(Destination.Population)]
  product_data[, Share := Passengers.Product / Potential_Passengers]
  
  product_data[, Period := Year + 0.1 * Quarter]
  
  product_data[, Product_Name := paste(Origin, Dest, Carrier, MktMilesFlown, NonStop)]
  product_data[, Product_Name := factor(Product_Name)]
  
  gc(); 
  
  # Remove All Products Previously Completely Excluded from the Sample
  product_data <- product_data[Avg.Fare > 0,]
  product_data <- product_data[!is.na(Avg.Fare),]
  
  product_data[, NonStop := as.numeric(NonStop)]
  
  # Calculate Extra Miles over Minimum in the Market
  product_data[, market_min_miles := min(MktMilesFlown) / 1000, by = market_group]
  product_data[, Extra_Miles := (MktMilesFlown) / 1000 - market_min_miles]
  product_data[, Extra_Miles_Sq := Extra_Miles * Extra_Miles]
  
  # Calculate Counts of Firm Types
  ucc <- c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc.")
  lcc <- c("JetBlue Airways","Southwest Airlines Co.", "Alaska Airlines Inc.")
  leg <- c("Delta Air Lines Inc.", "United Air Lines Inc.", "American Airlines Inc.")
  
  # product_data[, Other_UCC_Count := Spirit_Prescence + Allegiant_Prescence + Frontier_Prescence]
  # product_data[, Other_LCC_Count := Alaska_Prescence + JetBlue_Prescence + Southwest_Prescence]
  # product_data[, Other_Leg_Count := Delta_Prescence + United_Prescence + American_Prescence]
 
  # product_data[, UCC := 0]
  # product_data[Carrier %in% ucc, UCC := 1]
  # product_data[Carrier %in% ucc, Other_UCC_Count := Other_UCC_Count - 1]
  # product_data[Carrier %in% lcc, Other_LCC_Count := Other_LCC_Count - 1]
  # product_data[Carrier %in% leg, Other_Leg_Count := Other_Leg_Count - 1]
  
  product_data[, Market_Passengers := sum(Passengers.Product), by = market_group]
  product_data[, Own_Passengers := sum(Passengers.Product), by = c(market_group, "Carrier")]
  
  # Dummy Variables for Carriers
  product_data[, AmericanAir := as.numeric(Carrier == "American Airlines Inc.")]
  product_data[, Delta := as.numeric(Carrier == "Delta Air Lines Inc.")]
  product_data[, Southwest := as.numeric(Carrier == "Southwest Airlines Co.")]
  product_data[, United := as.numeric(Carrier == "United Air Lines Inc.")]
  product_data[, Spirit := as.numeric(Carrier == "Spirit Air Lines")]
  product_data[, JetBlue := as.numeric(Carrier == "JetBlue Airways")]
  product_data[, Other_Carrier := 1 - AmericanAir - Delta - Southwest - United - Spirit - JetBlue]
  
  # Mkt Miles Flown in Hundreds
  product_data[, MktMilesFlown := MktMilesFlown / 1000]
  
  # Squared Distance Term
  product_data[, MktMilesFlown_Sq := MktMilesFlown * MktMilesFlown]
  
  # Calculate number of products in the market
  product_data[, Num_Products_In_Market := .N, by = market_group]
  product_data[, Other_Products_In_Market := .N - 1, by = market_group]
  product_data[, Num_NonStop_In_Market := sum(NonStop), by = market_group]
  product_data[, Firm_Products_In_Market := .N, by = c(market_group, "Carrier")]
  product_data[, Rival_Products_In_Market := Num_Products_In_Market - Firm_Products_In_Market]
  product_data[, Num_Firms_In_Market := length(unique(Carrier)), by = market_group]
  
  
  # Calculate Average Other Distance in the Market 
  product_data[, Average_Other_Distance := sum(MktMilesFlown), by = market_group]
  product_data[, Average_Other_Distance := (Average_Other_Distance - MktMilesFlown) / (Num_Products_In_Market - 1)]
  
  # Calculate Average Other Products' Distance Square
  product_data[, Average_Other_Distance_Square := sum(MktMilesFlown_Sq), by = market_group]
  product_data[, Average_Other_Distance_Square := (Average_Other_Distance_Square - MktMilesFlown_Sq) / (Num_Products_In_Market - 1)]

  # Calculate Number of Rival Direct Products in the Market
  product_data[, Rival_Direct := sum(NonStop), by = market_group]
  product_data[, Rival_Direct := Rival_Direct - sum(NonStop), by = c(market_group, "Carrier")]
  
  # Calculate Average Rival Firm Origin, Destination Shares
  product_data[, Fractional_Origin_Ratio := Origin_Firm_Service_Ratio / Firm_Products_In_Market ]
  product_data[, Sum_Origin_Ratios := sum(Fractional_Origin_Ratio), by = market_group]
  product_data[, Other_Origin_Ratios := Sum_Origin_Ratios - Origin_Firm_Service_Ratio]
  product_data[, Average_Rival_Origin_Ratio := Other_Origin_Ratios / (Num_Firms_In_Market - 1)]
  product_data[, Fractional_Origin_Ratio := NULL]
  product_data[, Sum_Origin_Ratios := NULL]
  product_data[, Other_Origin_Ratios := NULL]
  
  product_data[, Fractional_Dest_Ratio := Destination_Firm_Service_Ratio / Firm_Products_In_Market]
  product_data[, Sum_Dest_Ratios := sum(Fractional_Dest_Ratio), by = market_group]
  product_data[, Other_Dest_Ratios := Sum_Dest_Ratios - Destination_Firm_Service_Ratio]
  product_data[, Average_Rival_Dest_Ratio := Other_Dest_Ratios / (Num_Firms_In_Market - 1)]
  product_data[, Fractional_Dest_Ratio := NULL]
  product_data[, Sum_Dest_Ratios := NULL]
  product_data[, Other_Dest_Ratios := NULL]

    # Tourism Dummy from Tracing the Woes
  product_data[, Tourism := FALSE]
  product_data[Destination_State == "FL", Tourism := TRUE]
  product_data[Origin_State == "FL", Tourism := TRUE]
  product_data[Origin_MSA == "Las Vegas-Henderson-Paradise, NV", Tourism := TRUE]
  product_data[Destination_MSA == "Las Vegas-Henderson-Paradise, NV", Tourism := TRUE]
  product_data[, Tourism := as.numeric(Tourism)]
  
  # Exogeneous Characteristic Products Interactions
  product_data[, MktMiles_Origin_Share := MktMilesFlown * Origin_Firm_Service_Ratio]
  product_data[, MktMiles_Destination_Share := MktMilesFlown * Destination_Firm_Service_Ratio]
  product_data[, MktMiles_Sq_Origin_Share := MktMilesFlown_Sq * Origin_Firm_Service_Ratio]
  product_data[, MktMiles_Sq_Destination_Share := MktMilesFlown_Sq * Destination_Firm_Service_Ratio]
  product_data[, MktMiles_NonStop := MktMilesFlown * NonStop]
  product_data[, MktMiles_Sq_NonStop := MktMilesFlown_Sq * NonStop]
  product_data[, MktMiles_Tourism := MktMilesFlown * Tourism]
  product_data[, MktMiles_Extra := MktMilesFlown * Extra_Miles]
  product_data[, MKtMiles_Sq_Tourism := MktMilesFlown_Sq * Tourism]
  product_data[, MktMiles_Sq_Extra:= MktMilesFlown_Sq * Extra_Miles ]
  product_data[, NonStop_Origin_Share := NonStop * Origin_Firm_Service_Ratio]
  product_data[, Direct_Destination_Share := NonStop * Destination_Firm_Service_Ratio]
  product_data[, OriginRatio_Extra := Origin_Firm_Service_Ratio * Extra_Miles]
  product_data[, DestinationRation_Extra := Destination_Firm_Service_Ratio * Extra_Miles]
  product_data[, NonStop_LCC := NonStop * (1 - Delta - United - AmericanAir)]
  product_data[, Extra_LCC := Extra_Miles * (1 - Delta - United - AmericanAir)]
  product_data[, Origin_LCC := Origin_Firm_Service_Ratio * (1 - Delta - United - AmericanAir)]
  product_data[, Destination_LCC := Destination_Firm_Service_Ratio * (1 - Delta - United - AmericanAir)]
  product_data[, MktMiles_OriginHub := MktMilesFlown * Origin_Hub]
  product_data[, MktMiles_DestinationHub := MktMilesFlown * Destination_Hub]
  product_data[, MktMilesSq_OriginHub := MktMilesFlown_Sq * Origin_Hub]
  product_data[, MktMilesSq_DestinationHub := MktMilesFlown_Sq * Destination_Hub]
  product_data[, NonStop_OriginHub := NonStop * Origin_Hub]
  product_data[, NonStop_DestinationHub := NonStop * Destination_Hub]
  product_data[, ExtraMiles_OriginHub := Extra_Miles * Origin_Hub]
  product_data[, ExtraMiles_Destination_Hub := Extra_Miles * Destination_Hub]
  product_data[, OriginRatio_OriginHub := Origin_Firm_Service_Ratio * Origin_Hub]
  product_data[, OriginRatio_DestinationHub := Origin_Firm_Service_Ratio * Destination_Hub]
  product_data[, DestinationRatio_OriginHub := Destination_Firm_Service_Ratio * Origin_Hub]
  product_data[, DestinationRatio_DestinationHub := Destination_Firm_Service_Ratio * Destination_Hub]
  product_data[, NonStop_MktSize := NonStop * Potential_Passengers / 100000]
  product_data[, OriginRatio_Size := Origin_Firm_Service_Ratio * Potential_Passengers / 100000]
  product_data[, DestinationRatio_Size := Destination_Firm_Service_Ratio * Potential_Passengers / 100000]
  product_data[, MktMiles_Size := MktMilesFlown * Potential_Passengers / 100000]
  product_data[, NonStop_ExtraMiles := Extra_Miles * NonStop]
  product_data[, MktMiles_Sq_Interact := MktMilesFlown * MktMilesFlown_Sq]
  
  # Add Variable which is 1 if either side is a hub
  product_data[, Hub_Endpoint := Origin_Hub | Destination_Hub]
  product_data[, Hub_End_MktMiles := Hub_Endpoint * MktMilesFlown]
  product_data[, Hub_End_MktMiles_Sq := Hub_Endpoint * MktMilesFlown * MktMilesFlown]
  product_data[, Hub_End_NonStop := Hub_Endpoint * NonStop]

  # Variable for Year-Quarter Fixed Effects
  product_data[, Year_Quarter_Effect := paste(Year, " X ", Quarter)]
  
  # Drop all Monopoly Markets
  # product_data <- product_data[Num_Firms_In_Market > 1,]
  
  # Align Names with PyBLP Requirements
  product_data$prices <- product_data$Avg.Fare;
  product_data$Avg.Fare <- NULL
  
  # Price in 100s
  product_data[, prices := prices / 100]
  # Real prices
  product_data[, prices := prices / (price_index / 100)]
  product_data[, Jet_Fuel_Price := Jet_Fuel_Price / (price_index / 100)]
  product_data[, Jet_Fuel_Square := Jet_Fuel_Price^2]
  
  product_data$market_ids <- product_data$Market_ID
  product_data$Market_ID <- NULL;
  
  product_data$product_ids <- product_data$Product_Name
  
  product_data$firm_ids <- as.numeric(as.factor(product_data$Carrier))
  
  product_data$shares <- product_data$Share
  product_data$Share <- NULL
  
  # Nevo (2001) Table 5 Outcome Variable
  product_data[, Outside_Share := 1 - sum(shares), by = market_group]
  product_data[, Share_Differences := log(shares) - log(Outside_Share)]
  product_data[, Log_Share_Ratio := log(shares / sum(shares)), by = market_group]
  
  # Gas Price Based Supply Shifters
  product_data[, GasMiles := National_Average_JF_Price * MktMilesFlown]
  product_data[, GasMiles_Sq := GasMiles*GasMiles]
  product_data[, GasMiles_Origin_Dest := GasMiles * Origin_Firm_Destinations]
  product_data[, GasMiles_Origin_Prescence := GasMiles * Origin_Firm_Service_Ratio]
  product_data[, GasMiles_Dest_Prescence := GasMiles * Destination_Firm_Service_Ratio]
  product_data[, GasMiles_Dest_Dest := GasMiles * Destination_Firm_Destinations]
  product_data[, GasMiles_ExtraMiles := GasMiles * Extra_Miles]
  product_data[, GasMiles_LCC := GasMiles * (1 - Delta - United - AmericanAir)]
  product_data[, GasMiles_Hub := GasMiles * Destination_Hub]
  product_data[, GasMiles_OriginHub := GasMiles * Origin_Hub]
  product_data[, GasMiles_NonStop := GasMiles * NonStop]
  product_data[, GasMiles_AllHub := GasMiles * Hub_Endpoint]

  # Gas Price Carrier Interactions
  product_data[, GasMiles_Delta := (Carrier == "Delta Air Lines Inc.") * GasMiles]
  product_data[, GasMiles_United := (Carrier == "United Air Lines Inc.") * GasMiles]
  product_data[, GasMiles_American:= (Carrier == "American Airlines Inc.") * GasMiles]
  product_data[, GasMiles_JetBlue := (Carrier == "JetBlue Airways") * GasMiles]
  product_data[, GasMiles_American:= (Carrier == "American Airlines Inc.") * GasMiles]
  product_data[, GasMiles_Spirit := (Carrier == "Spirit Air Lines") * GasMiles]
  product_data[, GasMiles_LCC := (!Carrier %in% c("Delta Air Lines Inc.",
                                                  "United Air Lines Inc.",
                                                  "American Airlines Inc."))]
  
  
  # Hausman, Per Mile Instruments
  product_data[, Product_Per_Mile_Price :=  prices / MktMilesFlown]
  product_data[, Firm_Mkt_Pass := sum(Passengers.Product),
               by =  c("Year", "Quarter", "Origin", "Dest", "Carrier")]
  product_data[, Firm_Per_Mile_Mkt := sum(Product_Per_Mile_Price * Passengers.Product) / Firm_Mkt_Pass, 
               by = c("Year", "Quarter", "Origin", "Dest", "Carrier")]
  product_data.haus <- unique(product_data[, .(Year, Quarter, Origin, Dest, Carrier, Firm_Per_Mile_Mkt,
                                        Firm_Mkt_Pass)])
  product_data.haus[, Firm_Time_Pass := sum(Firm_Mkt_Pass), by = c("Year", "Quarter", "Carrier")]
  product_data.haus[, Firm_Other_Mkt_Per_Mile := (sum(Firm_Per_Mile_Mkt * Firm_Mkt_Pass) - Firm_Per_Mile_Mkt)/
                      (Firm_Time_Pass - Firm_Mkt_Pass),
                    by = c("Year", "Quarter", "Carrier")]
  product_data.haus <- product_data.haus[, .(Year, Quarter, Carrier, Origin, Dest,
                                             Firm_Other_Mkt_Per_Mile)]
  product_data <- merge(product_data, product_data.haus, all.x = TRUE, by = c("Year", "Quarter", "Carrier",
                                                                              "Origin", "Dest"))
  product_data[, Product_Per_Mile_Price := NULL]
  product_data[, Firm_Mkt_Pass := NULL]
  product_data[, Firm_Per_Mile_Mkt := NULL]
  
  # Drop Near Markets
  product_data <- product_data[, MktMiles.Min := min(MktMilesFlown), by = c("Origin", "Dest")]
  product_data <- product_data[MktMiles.Min > 0.150,]
  
  ### Generate Differentiation Instruments ###
  if(!fast){
    gandhi_instruments <- pyblp$build_differentiation_instruments(
      pyblp$Formulation("NonStop + MktMilesFlown + I(MktMilesFlown**2) + Extra_Miles + Origin_Firm_Service_Ratio"),
      product_data,
      interact = F)
  
    gandhi_instruments_R <- as.data.table(py_to_r(gandhi_instruments))
  
    # Remove Instruments with Zero Variance
    variance <- c();
    for(i in 1:ncol(gandhi_instruments_R)){
      variance <- c(variance, var(gandhi_instruments_R[1:nrow(gandhi_instruments_R), ..i]))
    }
    real_variation <- (1:ncol(gandhi_instruments_R))[variance > 0]
    gandhi_instruments_R <- gandhi_instruments_R[, ..real_variation]
  
    # Keep Unique Instruments Only
    gandhi_instruments_R <- transpose(gandhi_instruments_R)
    gandhi_instruments_R <- unique(gandhi_instruments_R)
    gandhi_instruments_R <- transpose(gandhi_instruments_R)
  
    colnames(gandhi_instruments_R) <- paste("demand_instruments", 0:(ncol(gandhi_instruments_R)-1), sep = "")
  
    product_data <- cbind(product_data, gandhi_instruments_R)
  }
  
  
  product_data[, nesting_ids := 1]
  
  product_data[, clustering_ids := as.numeric(as.factor(paste(Origin, Dest)))]
  
  write_rds(product_data, output_file)
  # For Stata
  write_csv(product_data, file = gsub(pattern = ".rds", replacement = ".csv",
                                      x = output_file), na = ".")
}


merger_simulation_basic <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                    data_in = "02.Intermediate/Product_Data.rds",
                                    data_out = "02.Intermediate/Basic_Sim_Product_Data.rds"){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)
  
  data[, merger_carrier := Carrier]
  data[Carrier == "Spirit Air Lines", merger_carrier := "JetBlue Airways"]
  
  data[, merger_ids := firm_ids]
  data[merger_ids == 9, merger_ids := 6] # Spirit -> JB

  data[, cost := model$compute_costs()]
  
  new_prices <- model$compute_prices(firm_ids = data$merger_ids,
                                     costs = data$cost)
  new_shares <- model$compute_shares(new_prices)
  
  data[, new.price := new_prices]
  data[, new.share := new_shares]
  
  data[, price.change := new.price - prices]
  data[, share.change := new.share - shares]
  
  write_rds(data, data_out)
}

merger_results_basic <- function(merger_data = "02.Intermediate/Basic_Sim_Product_Data.rds",
                                 observed_data = "02.Intermediate/Product_Data.rds", 
                                 rcl_file = "03.Output/random_coeff_nested_logit_fs_results.pickle",
                                 table_out = "06.Tables/Merger_Basic.tex"){
  merger <- readRDS(merger_data)
  observed <- readRDS(observed_data)
  rcl <- py_load_object(rcl_file)
  
  # Compute Costs
  observed[, cost := rcl$compute_costs()]

  # First, Identify Markets in Which Both Firms Operated in
  observed[, Spirit_Prescence := max(Spirit_Prescence), by = c("market_ids")]
  observed[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("market_ids")]

  shared_markets <- unique(observed[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
  
  merger_shared <- merger[market_ids %in% shared_markets,]
  merger_shared[, Passengers.Sim := new.share * Potential_Passengers]
  
  observed_shared <- observed[market_ids %in% shared_markets,]
  observed_shared.Sp <- observed_shared[Carrier == "Spirit Air Lines",]
  observed_shared.Jb <- observed_shared[Carrier == "JetBlue Airways",]
  observed_shared.Rv <- observed_shared[!Carrier %in% c("Spirit Air Lines",
                                                        "JetBlue Airways"),]
  
  observed_shared.Sp <- observed_shared.Sp %>% group_by(market_ids) %>%
    summarize(share = sum(shares),
              Passengers = sum(Passengers.Product),
              Price = sum(Passengers.Product * prices) / sum(Passengers.Product),
              Marginal_Cost = sum(Passengers.Product * cost) / sum(Passengers.Product),
              MilesFlown = sum(Passengers.Product * MktMilesFlown) / sum(Passengers.Product),
              Origin_Ratio = sum(Passengers.Product * Origin_Firm_Service_Ratio) / sum(Passengers.Product)) %>%
    as.data.table()
  
  observed_shared.Jb <- observed_shared.Jb %>% group_by(market_ids) %>%
    summarize(share = sum(shares),
              Passengers = sum(Passengers.Product),
              Price = sum(Passengers.Product * prices) / sum(Passengers.Product),
              Marginal_Cost = sum(Passengers.Product * cost) / sum(Passengers.Product),
              MilesFlown = sum(Passengers.Product * MktMilesFlown) / sum(Passengers.Product),
              Origin_Ratio = sum(Passengers.Product * Origin_Firm_Service_Ratio) / sum(Passengers.Product)) %>%
    as.data.table()
  
  observed_shared.Rv <- observed_shared.Rv %>% group_by(market_ids) %>%
    summarize(share = sum(shares),
              Passengers = sum(Passengers.Product),
              Price = sum(Passengers.Product * prices) / sum(Passengers.Product),
              Marginal_Cost = sum(Passengers.Product * cost) / sum(Passengers.Product),
              MilesFlown = sum(Passengers.Product * MktMilesFlown) / sum(Passengers.Product),
              Origin_Ratio = sum(Passengers.Product * Origin_Firm_Service_Ratio) / sum(Passengers.Product)) %>%
    as.data.table()
  
  merger_JB <- merger_shared %>% filter(merger_carrier == "JetBlue Airways",
                                        market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(share = sum(new.share),
              Passengers = sum(Passengers.Sim),
              Price = sum(Passengers.Sim * prices) / sum(Passengers.Sim),
              Marginal_Cost = sum(Passengers.Sim * cost) / sum(Passengers.Sim),
              MilesFlown = sum(Passengers.Sim * MktMilesFlown) / sum(Passengers.Sim),
              Origin_Ratio = sum(Passengers.Sim * Origin_Firm_Service_Ratio) / sum(Passengers.Sim)) %>%
    as.data.table()
  
  merger_Rv <- merger_shared %>% filter(!merger_carrier == "JetBlue Airways",
                                        market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(share = sum(new.share),
              Passengers = sum(Passengers.Sim),
              Price = sum(Passengers.Sim * prices) / sum(Passengers.Sim),
              Marginal_Cost = sum(Passengers.Sim * cost) / sum(Passengers.Sim),
              MilesFlown = sum(Passengers.Sim * MktMilesFlown) / sum(Passengers.Sim),
              Origin_Ratio = sum(Passengers.Sim * Origin_Firm_Service_Ratio) / sum(Passengers.Sim)) %>%
    as.data.table()
  
  # Generate Rows for Tables
  
  pass_min <- c("Minimum", signif(c(min(observed_shared.Sp$Passengers), min(observed_shared.Jb$Passengers),
                                    min(observed_shared.Rv$Passengers), 
                                    min(merger_JB$Passengers), min(merger_Rv$Passengers)),
                                  digits = 3))
  pass_row <- c("Average", signif(c(mean(observed_shared.Sp$Passengers), mean(observed_shared.Jb$Passengers),
                                    mean(observed_shared.Rv$Passengers), 
                                    mean(merger_JB$Passengers), mean(merger_Rv$Passengers)),
                                  digits = 3))
  pass_max <- c("Maximum", signif(c(max(observed_shared.Sp$Passengers), max(observed_shared.Jb$Passengers),
                                    max(observed_shared.Rv$Passengers), 
                                    max(merger_JB$Passengers), max(merger_Rv$Passengers)),
                                  digits = 3))
  
  share_min <- c("Minimum", signif(c(min(observed_shared.Sp$share), min(observed_shared.Jb$share),
                                     min(observed_shared.Rv$share), 
                                     min(merger_JB$share), min(merger_Rv$share)), digits = 3))
  share_row <- c("Average", signif(c(mean(observed_shared.Sp$share), mean(observed_shared.Jb$share),
                                     mean(observed_shared.Rv$share), 
                                     mean(merger_JB$share), mean(merger_Rv$share)), digits = 3))
  share_max <-  c("Maximum", signif(c(max(observed_shared.Sp$share), max(observed_shared.Jb$share),
                                      max(observed_shared.Rv$share), 
                                      max(merger_JB$share), max(merger_Rv$share)), digits = 3))
  
  mc_min <- c("Minimum", signif(c(min(observed_shared.Sp$Marginal_Cost), min(observed_shared.Jb$Marginal_Cost),
                                  min(observed_shared.Rv$Marginal_Cost), 
                                  min(merger_JB$Marginal_Cost),  min(merger_Rv$Marginal_Cost)), digits = 3))
  mc_row <- c("Average", signif(c(mean(observed_shared.Sp$Marginal_Cost), mean(observed_shared.Jb$Marginal_Cost),
                                  mean(observed_shared.Rv$Marginal_Cost), 
                                  mean(merger_JB$Marginal_Cost), mean(merger_Rv$Marginal_Cost)), digits = 3))
  mc_max <- c("Maximum", signif(c(max(observed_shared.Sp$Marginal_Cost), 
                                  max(observed_shared.Jb$Marginal_Cost),
                                  max(observed_shared.Rv$Marginal_Cost), 
                                  max(merger_JB$Marginal_Cost), max(merger_Rv$Marginal_Cost)), digits = 3))
  
  price_min <- c("Minimum", signif(c(min(observed_shared.Sp$Price), min(observed_shared.Jb$Price),
                                     min(observed_shared.Rv$Price), 
                                     min(merger_JB$Price), min(merger_Rv$Price)), digits = 3))
  price_row <- c("Average", signif(c(mean(observed_shared.Sp$Price), mean(observed_shared.Jb$Price),
                                     mean(observed_shared.Rv$Price), 
                                     mean(merger_JB$Price), mean(merger_Rv$Price)), digits = 3))
  price_max <- c("Maximum", signif(c(max(observed_shared.Sp$Price), max(observed_shared.Jb$Price),
                                     max(observed_shared.Rv$Price), 
                                     max(merger_JB$Price), max(merger_Rv$Price)), digits = 3))
  
  miles_min <- c("Minimum", signif(c(min(observed_shared.Sp$MilesFlown),
                                     min(observed_shared.Jb$MilesFlown), min(observed_shared.Rv$MilesFlown),
                                     min(merger_JB$MilesFlown), min(merger_Rv$MilesFlown)), digits = 3))
  miles_row <- c("Average", signif(c(mean(observed_shared.Sp$MilesFlown),
                                     mean(observed_shared.Jb$MilesFlown), mean(observed_shared.Rv$MilesFlown),
                                     mean(merger_JB$MilesFlown), mean(merger_Rv$MilesFlown)), digits = 3))
  miles_max <- c("Maximum", signif(c(max(observed_shared.Sp$MilesFlown),
                                     max(observed_shared.Jb$MilesFlown), max(observed_shared.Rv$MilesFlown),
                                     max(merger_JB$MilesFlown), max(merger_Rv$MilesFlown)), digits = 3))
  
  origin_min <- c("Minimum", signif(c(min(observed_shared.Sp$Origin_Ratio),
                                      min(observed_shared.Jb$Origin_Ratio), min(observed_shared.Rv$Origin_Ratio),
                                      min(merger_JB$Origin_Ratio), min(merger_Rv$Origin_Ratio)),
                                    digits = 3))
  origin_row <- c("Average", signif(c(mean(observed_shared.Sp$Origin_Ratio),
                                      mean(observed_shared.Jb$Origin_Ratio), mean(observed_shared.Rv$Origin_Ratio),
                                      mean(merger_JB$Origin_Ratio), mean(merger_Rv$Origin_Ratio)), digits = 3))
  origin_max <- c("Maximum", signif(c(max(observed_shared.Sp$Origin_Ratio),
                                      max(observed_shared.Jb$Origin_Ratio), max(observed_shared.Rv$Origin_Ratio),
                                      max(merger_JB$Origin_Ratio), max(merger_Rv$Origin_Ratio)), digits = 3))
  
  
  N_Markets <- c("Number of Markets", length(shared_markets), length(shared_markets), length(shared_markets),
                 length(shared_markets), length(shared_markets))
  
  table <- rbind(pass_min, pass_row, pass_max,  share_min, share_row, share_max,
           price_min, price_row, price_max, mc_min,  mc_row, mc_max,
           miles_min, miles_row, miles_max, origin_min, origin_row, origin_max,
           N_Markets)
  
  rownames(table) <- NULL
  
  kbl(table,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = NULL) %>%
    add_header_above(c("Variable" = 1, "Spirit" = 1, "JetBlue" = 1, "Other" = 1,
                       "Merged" = 1, "Other" = 1)) %>%
    add_header_above(c(" " = 1, "Obseved Sample" = 3, "Basic Merger" = 2)) %>%
    pack_rows("Passengers", start_row = 1, end_row = 3) %>%
    pack_rows("Market Share", start_row = 4, end_row = 6) %>%
    pack_rows("Prices", start_row = 7, end_row = 9) %>%
    pack_rows("Marginal Cost", start_row = 10, end_row = 12) %>%
    pack_rows("Miles Flown", start_row = 13, end_row = 15) %>%
    pack_rows("Origin Service Ratio", start_row = 16, end_row = 18) %>%
    row_spec(row = 18, hline_after = TRUE) %>%
    save_kable(file = table_out)
  
}


airport_service_ratios_merger <- function(db1b){
  db1b[, Next.Airport := Dest]
  nonstop_plane <- db1b[, .(Year, Quarter, Origin, Next.Airport,
                            merger_carrier)]
  db1b[, Next.Airport := NULL]
  nonstop_plane <- unique(nonstop_plane)
  
  # Evaluate all destinations available
  # (t = total)
  nonstop_plane_t <- nonstop_plane;
  nonstop_plane_t$merger_carrier <- NULL
  nonstop_plane_t <- unique(nonstop_plane_t)
  nonstop_plane_t[, Destinations.Available := .N, by = c("Year", "Quarter", "Origin")]
  nonstop_plane_t[, Next.Airport := NULL]
  nonstop_plane_t <- unique(nonstop_plane_t)
  
  # Now, For Each Firm, Identify Number of Destinations They Serve
  nonstop_plane[, Firm.Destinations := .N, by = c("merger_carrier", "Year", "Quarter", "Origin")]
  
  # Merge, Calculate Ratio
  nonstop_plane <- merge(nonstop_plane, nonstop_plane_t, by = c("Year", "Quarter", "Origin"),
                         all.x = TRUE)
  nonstop_plane[is.na(Firm.Destinations), Firm.Destinations := 1]
  nonstop_plane[, Firm.Ratio := Firm.Destinations / Destinations.Available * 100]
  
  return(ratio_data)
}


merger_simulation_advanced <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                       data_in = "02.Intermediate/Product_Data.rds",
                                       data_out = "03.Output/Adv_Merger_Sim_Data.rds",
                                       linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
                                       nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
                                       mode = "rcl"){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)
  
  data[, merger_carrier := Carrier]
  data[Carrier == "Spirit Air Lines", merger_carrier := "JetBlue Airways"]
  data[, unobserved := model$xi]
  
  # Compute Costs
  data[, cost := model$compute_costs()]
  
  data.new <- data %>% group_by(merger_carrier, Origin, Dest, Year, Quarter, Year_Quarter_Effect, NonStop, market_ids) %>%
    summarize(NonStopMiles = min(NonStopMiles), 
              MktMilesFlown = min(MktMilesFlown),
              shares = sum(shares),
              prices = mean(prices),
              Tourism = mean(Tourism),
              market_min_miles = min(market_min_miles),
              unobserved = min(unobserved),
              costs.min = min(cost),
              costs.mean = mean(cost),
              costs.max = max(cost),
              original.fare = mean(prices),
              Potential_Passengers = mean(Potential_Passengers)) %>%
    mutate(Extra_Miles = MktMilesFlown - market_min_miles,
           MktMilesFlown_Sq = MktMilesFlown * MktMilesFlown,
           firm_ids = merger_carrier,
           NonStop = as.numeric(NonStop),
           Carrier = merger_carrier,
           UCC = as.numeric(Carrier %in% c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc."))) %>%
    as.data.table()
  
  remove(data); gc();
  
  # Recompute Origin Prescence Variable
  ratio_data <- readRDS("02.Intermediate/Merger_Service_Ratios.Rds")
  
  colnames(ratio_data) <- c("Year", "Quarter", "Carrier", "Origin", 
            "Origin_Firm_Destinations", "Origin_Firm_Service_Ratio")
  data.new <- merge(data.new, ratio_data, by = c("Year", "Quarter", "Carrier", "Origin"),
                      all.x = TRUE);
  
  # Need to remove Spirit Beta Coeff
  beta_vec <- model$beta
  beta_labels <- model$beta_labels
  beta_vec <- beta_vec[!grepl(pattern = "Spirit", x = beta_labels)]
  
  if(mode == "rcl"){
    components <- c(linear, nonlinear) 
  } else {
    components <- linear
  }
  
  simulation <- pyblp$Simulation(product_formulations = components,
                                 product_data = data.new,
                                 beta = beta_vec,
                                 sigma = model$sigma,
                                 integration = pyblp$Integration('product', 9L,
                                            specification_options = dict("seed" = 97L)),
                                 xi = data.new$unobserved)
  
  simulation.min <- simulation$replace_endogenous(costs = data.new$costs.min); gc(); 
  simulation.mean <- simulation$replace_endogenous(costs = data.new$costs.mean); gc();
  simulation.max <- simulation$replace_endogenous(costs = data.new$costs.max); gc()
  
  data.new[, Shares.MinCost.Sim := py_to_r(simulation.min$product_data$shares)]
  data.new[, Shares.MeanCost.Sim := py_to_r(simulation.mean$product_data$shares)]
  data.new[, Shares.MaxCost.Sim := py_to_r(simulation.max$product_data$shares)]
  data.new[, Prices.MinCost.Sim := py_to_r(simulation.min$product_data$prices)]
  data.new[, Prices.MeanCost.Sim := py_to_r(simulation.mean$product_data$prices)]
  data.new[, Prices.MaxCost.Sim := py_to_r(simulation.max$product_data$prices)]
  
  # Add each markets Consumer Surplus
  # markets <- unique(data.new$market_ids)
  # data.new[, CS.Original := 0]
  # data.new[, CS.MinCost.Sim := 0]
  # data.new[, CS.MeanCost.Sim := 0]
  # data.new[, CS.MaxCost.Sim := 0]
  # 
  # for(i in 1:length(markets)){
  #   invisible(data.new[market_ids == markets[i], CS.Original := model$compute_consumer_surpluses(market_id = markets[i])])
  #   invisible(data.new[market_ids == markets[i], CS.MinCost.Sim := py_to_r(simulation.min$compute_consumer_surpluses(market_id = markets[i]))[1,1]])
  #   
  #   if(invisible(nrow(py_to_r(simulation.min$compute_consumer_surpluses(market_id = markets[i]))) > 1)){
  #     print(i);
  #     break;
  #   } else if(invisible(ncol(py_to_r(simulation.min$compute_consumer_surpluses(market_id = markets[i]))) > 1)){
  #     print(i);
  #     break;
  #   }
  #   
  #   invisible(data.new[market_ids == markets[i], CS.MeanCost.Sim := py_to_r(simulation.mean$compute_consumer_surpluses(market_id = markets[i]))[1,1]])
  #  
  #   invisible(data.new[market_ids == markets[i], CS.MaxCost.Sim := py_to_r(simulation.max$compute_consumer_surpluses(market_id = markets[i]))[1,1]])
  #   print(paste("Success: ", i))
  # }
  # gc();
  saveRDS(data.new, file = data_out)
}

merger_results_table <- function(merger_data = "03.Output/Adv_Merger_Sim_Data.rds",
                                 observed_data = "02.Intermediate/Product_Data.rds", 
                                 table_out = "06.Tables/Merger_Results.tex",
                                 rcl = "03.Output/random_coeff_nested_logit_results.pickle"){
  merger <- readRDS(merger_data)
  observed <- readRDS(observed_data)
  rcl <- py_load_object(rcl)
  
  # Compute Costs
  observed[, cost := rcl$compute_costs()]
  
  # First, Identify Markets in Which Both Firms Operated in
  observed[, Spirit_Prescence := max(Spirit_Prescence), by = c("market_ids")]
  observed[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("market_ids")]
  
  
  shared_markets <- unique(observed[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
  
  merger_shared <- merger[market_ids %in% shared_markets,]
  merger_shared[, Passengers.Product.Min := Shares.MinCost.Sim * Potential_Passengers]
  merger_shared[, Passengers.Product.Mean := Shares.MeanCost.Sim * Potential_Passengers]
  merger_shared[, Passengers.Product.Max := Shares.MaxCost.Sim * Potential_Passengers]
  
  
  observed_shared <- observed[market_ids %in% shared_markets,]
  observed_shared.Sp <- observed_shared[Carrier == "Spirit Air Lines",]
  observed_shared.Jb <- observed_shared[Carrier == "JetBlue Airways",]
  observed_shared.Rv <- observed_shared[!Carrier %in% c("Spirit Air Lines",
                                                        "JetBlue Airways"),]
    
  observed_shared.Sp <- observed_shared.Sp %>% group_by(market_ids) %>%
    summarize(share = sum(shares),
              Passengers = sum(Passengers.Product),
              Price = sum(Passengers.Product * prices) / sum(Passengers.Product),
              Marginal_Cost = sum(Passengers.Product * cost) / sum(Passengers.Product),
              MilesFlown = sum(Passengers.Product * MktMilesFlown) / sum(Passengers.Product),
              Origin_Ratio = sum(Passengers.Product * Origin_Firm_Service_Ratio) / sum(Passengers.Product)) %>%
    as.data.table()
  
  
  observed_shared.Jb <- observed_shared.Jb %>% group_by(market_ids) %>%
    summarize(share = sum(shares),
              Passengers = sum(Passengers.Product),
              Price = sum(Passengers.Product * prices) / sum(Passengers.Product),
              Marginal_Cost = sum(Passengers.Product * cost) / sum(Passengers.Product),
              MilesFlown = sum(Passengers.Product * MktMilesFlown) / sum(Passengers.Product),
              Origin_Ratio = sum(Passengers.Product * Origin_Firm_Service_Ratio) / sum(Passengers.Product)) %>%
    as.data.table()
  
  observed_shared.Rv <- observed_shared.Rv %>% group_by(market_ids) %>%
    summarize(share = sum(shares),
              Passengers = sum(Passengers.Product),
              Price = sum(Passengers.Product * prices) / sum(Passengers.Product),
              Marginal_Cost = sum(Passengers.Product * cost) / sum(Passengers.Product),
              MilesFlown = sum(Passengers.Product * MktMilesFlown) / sum(Passengers.Product),
              Origin_Ratio = sum(Passengers.Product * Origin_Firm_Service_Ratio) / sum(Passengers.Product)) %>%
    as.data.table()
  
  merger_JB <- merger_shared %>% filter(merger_carrier == "JetBlue Airways",
                                        market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(share.MinCost = sum(Shares.MinCost.Sim),
              share.MeanCost = sum(Shares.MeanCost.Sim),
              share.MaxCost = sum(Shares.MaxCost.Sim),
              Passengers.MinCost = sum(Passengers.Product.Min),
              Passengers.MeanCost = sum(Passengers.Product.Mean),
              Passengers.MaxCost = sum(Passengers.Product.Max),
              Price.MinCost = sum(Passengers.Product.Min * Prices.MinCost.Sim) / sum(Passengers.Product.Min),
              Price.MeanCost = sum(Passengers.Product.Mean * Prices.MeanCost.Sim) / sum(Passengers.Product.Mean),
              Price.MaxCost = sum(Passengers.Product.Max * Prices.MaxCost.Sim) / sum(Passengers.Product.Max),
              MC_Min = sum(Passengers.Product.Min * costs.min) / sum(Passengers.Product.Min),
              MC_Mean = sum(Passengers.Product.Mean * costs.mean) / sum(Passengers.Product.Mean),
              MC_Max = sum(Passengers.Product.Max * costs.max) / sum(Passengers.Product.Max),
              MilesFlown.Min = sum(Passengers.Product.Min * MktMilesFlown) / sum(Passengers.Product.Min),
              MilesFlown.Mean = sum(Passengers.Product.Mean * MktMilesFlown) / sum(Passengers.Product.Mean),
              MilesFlown.Max = sum(Passengers.Product.Max * MktMilesFlown) / sum(Passengers.Product.Max),
              Extra_Miles.Min = sum(Passengers.Product.Min * Extra_Miles) / sum(Passengers.Product.Min),
              Extra_Miles.Mean = sum(Passengers.Product.Mean * Extra_Miles) / sum(Passengers.Product.Mean),
              Extra_Miles.Max = sum(Passengers.Product.Max * Extra_Miles) / sum(Passengers.Product.Max),
              Origin_Ratio.Min = sum(Passengers.Product.Min * Origin_Firm_Service_Ratio) / sum(Passengers.Product.Min),
              Origin_Ratio.Mean = sum(Passengers.Product.Mean * Origin_Firm_Service_Ratio) / sum(Passengers.Product.Mean),
              Origin_Ratio.Max = sum(Passengers.Product.Max * Origin_Firm_Service_Ratio) / sum(Passengers.Product.Max)) %>%
    as.data.table()
  
  merger_Rv <- merger_shared %>% filter(!merger_carrier == "JetBlue Airways",
                                        market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(share.MinCost = sum(Shares.MinCost.Sim),
              share.MeanCost = sum(Shares.MeanCost.Sim),
              share.MaxCost = sum(Shares.MaxCost.Sim),
              Passengers.MinCost = sum(Passengers.Product.Min),
              Passengers.MeanCost = sum(Passengers.Product.Mean),
              Passengers.MaxCost = sum(Passengers.Product.Max),
              Price.MinCost = sum(Passengers.Product.Min * Prices.MinCost.Sim) / sum(Passengers.Product.Min),
              Price.MeanCost = sum(Passengers.Product.Mean * Prices.MeanCost.Sim) / sum(Passengers.Product.Mean),
              Price.MaxCost = sum(Passengers.Product.Max * Prices.MaxCost.Sim) / sum(Passengers.Product.Max),
              MC_Min = sum(Passengers.Product.Min * costs.min) / sum(Passengers.Product.Min),
              MC_Mean = sum(Passengers.Product.Mean * costs.mean) / sum(Passengers.Product.Mean),
              MC_Max = sum(Passengers.Product.Max * costs.max) / sum(Passengers.Product.Max),
              MilesFlown.Min = sum(Passengers.Product.Min * MktMilesFlown) / sum(Passengers.Product.Min),
              MilesFlown.Mean = sum(Passengers.Product.Mean * MktMilesFlown) / sum(Passengers.Product.Mean),
              MilesFlown.Max = sum(Passengers.Product.Max * MktMilesFlown) / sum(Passengers.Product.Max),
              Extra_Miles.Min = sum(Passengers.Product.Min * Extra_Miles) / sum(Passengers.Product.Min),
              Extra_Miles.Mean = sum(Passengers.Product.Mean * Extra_Miles) / sum(Passengers.Product.Mean),
              Extra_Miles.Max = sum(Passengers.Product.Max * Extra_Miles) / sum(Passengers.Product.Max),
              Origin_Ratio.Min = sum(Passengers.Product.Min * Origin_Firm_Service_Ratio) / sum(Passengers.Product.Min),
              Origin_Ratio.Mean = sum(Passengers.Product.Mean * Origin_Firm_Service_Ratio) / sum(Passengers.Product.Mean),
              Origin_Ratio.Max = sum(Passengers.Product.Max * Origin_Firm_Service_Ratio) / sum(Passengers.Product.Max)) %>%
    as.data.table()
  
  pass_min <- c("Minimum", signif(c(min(observed_shared.Sp$Passengers), min(observed_shared.Jb$Passengers),
                                    min(observed_shared.Rv$Passengers), 
                                    min(merger_JB$Passengers.MinCost), min(merger_Rv$Passengers.MinCost),
                                    min(merger_JB$Passengers.MeanCost), min(merger_Rv$Passengers.MeanCost),
                                    min(merger_JB$Passengers.MaxCost), min(merger_Rv$Passengers.MaxCost)),
                                  digits = 3))
  pass_row <- c("Average", signif(c(mean(observed_shared.Sp$Passengers), mean(observed_shared.Jb$Passengers),
                                              mean(observed_shared.Rv$Passengers), 
                                              mean(merger_JB$Passengers.MinCost), mean(merger_Rv$Passengers.MinCost),
                                              mean(merger_JB$Passengers.MeanCost), mean(merger_Rv$Passengers.MeanCost),
                                              mean(merger_JB$Passengers.MaxCost), mean(merger_Rv$Passengers.MaxCost)),
                                 digits = 3))
  pass_max <- c("Maximum", signif(c(max(observed_shared.Sp$Passengers), max(observed_shared.Jb$Passengers),
                                    max(observed_shared.Rv$Passengers), 
                                    max(merger_JB$Passengers.MinCost), max(merger_Rv$Passengers.MinCost),
                                    max(merger_JB$Passengers.MeanCost), max(merger_Rv$Passengers.MeanCost),
                                    max(merger_JB$Passengers.MaxCost), max(merger_Rv$Passengers.MaxCost)),
                                  digits = 3))
  
  share_min <- c("Minimum", signif(c(min(observed_shared.Sp$share), min(observed_shared.Jb$share),
                                     min(observed_shared.Rv$share), 
                                     min(merger_JB$share.MinCost), min(merger_Rv$share.MinCost),
                                     min(merger_JB$share.MeanCost), min(merger_Rv$share.MeanCost),
                                     min(merger_JB$share.MaxCost), min(merger_Rv$share.MaxCost)), digits = 3))
  share_row <- c("Average", signif(c(mean(observed_shared.Sp$share), mean(observed_shared.Jb$share),
                 mean(observed_shared.Rv$share), 
                 mean(merger_JB$share.MinCost), mean(merger_Rv$share.MinCost),
                 mean(merger_JB$share.MeanCost), mean(merger_Rv$share.MeanCost),
                 mean(merger_JB$share.MaxCost), mean(merger_Rv$share.MaxCost)), digits = 3))
  share_max <-  c("Maximum", signif(c(max(observed_shared.Sp$share), max(observed_shared.Jb$share),
                                      max(observed_shared.Rv$share), 
                                      max(merger_JB$share.MinCost), max(merger_Rv$share.MinCost),
                                      max(merger_JB$share.MeanCost), max(merger_Rv$share.MeanCost),
                                      max(merger_JB$share.MaxCost), max(merger_Rv$share.MaxCost)), digits = 3))
  
  mc_min <- c("Minimum", signif(c(min(observed_shared.Sp$Marginal_Cost), min(observed_shared.Jb$Marginal_Cost),
                                  min(observed_shared.Rv$Marginal_Cost), 
                                  min(merger_JB$MC_Min),  min(merger_Rv$MC_Min),
                                  min(merger_JB$MC_Mean), min(merger_Rv$MC_Mean),
                                  min(merger_JB$MC_Max),  min(merger_Rv$MC_Max)), digits = 3))
  mc_row <- c("Average", signif(c(mean(observed_shared.Sp$Marginal_Cost), mean(observed_shared.Jb$Marginal_Cost),
                 mean(observed_shared.Rv$Marginal_Cost), 
                 mean(merger_JB$MC_Min), mean(merger_Rv$MC_Min),
                 mean(merger_JB$MC_Mean), mean(merger_Rv$MC_Mean),
                 mean(merger_JB$MC_Max), mean(merger_Rv$MC_Max)), digits = 3))
  mc_max <- c("Maximum", signif(c(max(observed_shared.Sp$Marginal_Cost), 
                                  max(observed_shared.Jb$Marginal_Cost),
                                  max(observed_shared.Rv$Marginal_Cost), 
                                  max(merger_JB$MC_Min), max(merger_Rv$MC_Min),
                                  max(merger_JB$MC_Mean), max(merger_Rv$MC_Mean),
                                  max(merger_JB$MC_Max), max(merger_Rv$MC_Max)), digits = 3))
  
  price_min <- c("Minimum", signif(c(min(observed_shared.Sp$Price), min(observed_shared.Jb$Price),
                                     min(observed_shared.Rv$Price), 
                                     min(merger_JB$Price.MinCost), min(merger_Rv$Price.MinCost),
                                     min(merger_JB$Price.MeanCost), min(merger_Rv$Price.MeanCost),
                                     min(merger_JB$Price.MaxCost), min(merger_Rv$Price.MaxCost)), digits = 3))
  price_row <- c("Average", signif(c(mean(observed_shared.Sp$Price), mean(observed_shared.Jb$Price),
              mean(observed_shared.Rv$Price), 
              mean(merger_JB$Price.MinCost), mean(merger_Rv$Price.MinCost),
              mean(merger_JB$Price.MeanCost), mean(merger_Rv$Price.MeanCost),
              mean(merger_JB$Price.MaxCost), mean(merger_Rv$Price.MaxCost)), digits = 3))
  price_max <- c("Maximum", signif(c(max(observed_shared.Sp$Price), max(observed_shared.Jb$Price),
                                     max(observed_shared.Rv$Price), 
                                     max(merger_JB$Price.MinCost), max(merger_Rv$Price.MinCost),
                                     max(merger_JB$Price.MeanCost), max(merger_Rv$Price.MeanCost),
                                     max(merger_JB$Price.MaxCost), max(merger_Rv$Price.MaxCost)), digits = 3))
  
  miles_min <- c("Minimum", signif(c(min(observed_shared.Sp$MilesFlown),
                                     min(observed_shared.Jb$MilesFlown), min(observed_shared.Rv$MilesFlown),
                                     min(merger_JB$MilesFlown.Min), min(merger_Rv$MilesFlown.Min),
                                     min(merger_JB$MilesFlown.Mean), min(merger_Rv$MilesFlown.Mean),
                                     min(merger_JB$MilesFlown.Max), min(merger_Rv$MilesFlown.Max)), digits = 3))
  miles_row <- c("Average", signif(c(mean(observed_shared.Sp$MilesFlown),
             mean(observed_shared.Jb$MilesFlown), mean(observed_shared.Rv$MilesFlown),
             mean(merger_JB$MilesFlown.Min), mean(merger_Rv$MilesFlown.Min),
             mean(merger_JB$MilesFlown.Mean), mean(merger_Rv$MilesFlown.Mean),
             mean(merger_JB$MilesFlown.Max), mean(merger_Rv$MilesFlown.Max)), digits = 3))
  miles_max <- c("Maximum", signif(c(max(observed_shared.Sp$MilesFlown),
                                     max(observed_shared.Jb$MilesFlown), max(observed_shared.Rv$MilesFlown),
                                     max(merger_JB$MilesFlown.Min), max(merger_Rv$MilesFlown.Min),
                                     max(merger_JB$MilesFlown.Mean), max(merger_Rv$MilesFlown.Mean),
                                     max(merger_JB$MilesFlown.Max), max(merger_Rv$MilesFlown.Max)), digits = 3))
  
  origin_min <- c("Minimum", signif(c(min(observed_shared.Sp$Origin_Ratio),
                                      min(observed_shared.Jb$Origin_Ratio), min(observed_shared.Rv$Origin_Ratio),
                                      min(merger_JB$Origin_Ratio.Min), min(merger_Rv$Origin_Ratio.Min),
                                      min(merger_JB$Origin_Ratio.Mean), min(merger_Rv$Origin_Ratio.Mean),
                                      min(merger_JB$Origin_Ratio.Max), min(merger_Rv$Origin_Ratio.Max)), digits = 3))
  origin_row <- c("Average", signif(c(mean(observed_shared.Sp$Origin_Ratio),
             mean(observed_shared.Jb$Origin_Ratio), mean(observed_shared.Rv$Origin_Ratio),
             mean(merger_JB$Origin_Ratio.Min), mean(merger_Rv$Origin_Ratio.Min),
             mean(merger_JB$Origin_Ratio.Mean), mean(merger_Rv$Origin_Ratio.Mean),
             mean(merger_JB$Origin_Ratio.Max), mean(merger_Rv$Origin_Ratio.Max)), digits = 3))
  origin_max <- c("Maximum", signif(c(max(observed_shared.Sp$Origin_Ratio),
                                      max(observed_shared.Jb$Origin_Ratio), max(observed_shared.Rv$Origin_Ratio),
                                      max(merger_JB$Origin_Ratio.Min), max(merger_Rv$Origin_Ratio.Min),
                                      max(merger_JB$Origin_Ratio.Mean), max(merger_Rv$Origin_Ratio.Mean),
                                      max(merger_JB$Origin_Ratio.Max), max(merger_Rv$Origin_Ratio.Max)), digits = 3))
  
  # Estimate Change in Consumer Surplus
  # merger_shared[, ChangeCS.MinCost := CS.MinCost.Sim - CS.Original]
  # merger_shared[, ChangeCS.MeanCost := CS.MeanCost.Sim - CS.Original]
  # merger_shared[, ChangeCS.MaxCost := CS.MaxCost.Sim - CS.Original]
  
  # cs_merger_shared <- unique(merger_shared[, .(market_ids, ChangeCS.MinCost, ChangeCS.MeanCost,
  #                                       ChangeCS.MaxCost)])
  # 
  # CS_Change.min <- c("Minimum", "", "", "", "", round(min(merger_shared$ChangeCS.MinCost), digits = 2), "",
  #                    round(min(merger_shared$ChangeCS.MeanCost), digits = 2),
  #                    "", round(min(merger_shared$ChangeCS.MaxCost), digits = 2))
  # CS_Change.mean <- c("Average", "", "", "", "",
  #                     round(mean(merger_shared$ChangeCS.MinCost), digits = 2), "",
  #                     round(mean(merger_shared$ChangeCS.MeanCost), digits = 2),
  #                    "", round(mean(merger_shared$ChangeCS.MaxCost), digits = 2))
  # CS_Change.max <- c("Maximum", "", "", "", "", round(max(merger_shared$ChangeCS.MinCost), digits = 2),
  #                    "", round(max(merger_shared$ChangeCS.MeanCost), digits = 2),
  #                    "", round(max(merger_shared$ChangeCS.MaxCost), digits = 2))
  # CS_Change.Total <- c("Total", "","","","", round(sum(merger_shared$ChangeCS.MinCost), digits = 2), "", 
  #                      round(sum(merger_shared$ChangeCS.MeanCost), digits = 2),
  #                "", round(sum(merger_shared$ChangeCS.MaxCost), digits = 2))
  #   
  N_Markets <- c("Number of Markets", length(shared_markets), length(shared_markets), length(shared_markets),
                 length(shared_markets), length(shared_markets), length(shared_markets), length(shared_markets),
                 length(shared_markets), length(shared_markets))
  
  table <- rbind(pass_min, pass_row, pass_max, share_min, share_row, share_max,
                 price_min, price_row, price_max, mc_min,  mc_row, mc_max,
                 miles_min, miles_row, miles_max,
                 origin_min, origin_row, origin_max,
                # CS_Change.min, CS_Change.mean, CS_Change.max, CS_Change.Total,
                 N_Markets)
  
  rownames(table) <- NULL
  
  kbl(table,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = NULL) %>%
    add_header_above(c("Variable" = 1, "Spirit" = 1, "JetBlue" = 1, "Other" = 1,
                       "Merged" = 1, "Other" = 1, "Merged" = 1, "Other" = 1,
                       "Merged" = 1, "Other" = 1)) %>%
    add_header_above(c(" " = 1, "Obseved Sample" = 3, "Low-Cost" = 2, 
                      "Mean-Cost" = 2, "High-Cost" = 2)) %>%
    pack_rows("Passengers", start_row = 1, end_row = 3) %>%
    pack_rows("Market Share", start_row = 4, end_row = 6) %>%
    pack_rows("Prices", start_row = 7, end_row = 9) %>%
    pack_rows("Marginal Cost", start_row = 10, end_row = 12) %>%
    pack_rows("Miles Flown", start_row = 13, end_row = 15) %>%
    pack_rows("Origin Service Ratio", start_row = 16, end_row = 18) %>%
   # pack_rows("Change in Consumer Surplus", start_row = 19, end_row = 22) %>%
    row_spec(row = 18, hline_after = TRUE) %>%
    save_kable(file = table_out)
  
}

elasticity_compare_table <- function(postPand_input = "03.Output/random_coeff_nested_logit_results.pickle",
                                     prePand_input = "03.Output/prepandemic_random_coeff_nested_logit.pickle",
                                     table_out = "06.Tables/Elasticity_Compare_Table.tex"){
  postPandemic <- py_load_object(postPand_input);
  prePandemic <- py_load_object(prePand_input);
  
  header <- c("Paper", "Period Analyzed", "Estimated Average Elasticity", "Notes")
  row1 <- c("Berry and Jia (2010)", "1999", "-1.69", "")
  row2 <- c("Berry and Jia (2010)", "2006", "-1.67", "")
  row3 <- c("Gayle (2013)", "2006Q1 - 2006Q4","-4.72", "")
  row4 <- c("Ciliberto and Williams (2014)", "2006Q1 - 2008Q4", "-4.320", "")
  row5 <- c("Ciliberto and Williams (2021)", "2012Q2", "[-7.281, -7.063]", "Median, Exogeneous Entry Model")
  row6 <- c("Ciliberto and Williams (2021)", "2012Q2", "[-4.105, -4.007]", "Median, Endogeneous Entry Model")
  row7 <- c("Turner (2022)", "2000Q3", "-2.107", "")
  row8 <- c("Turner (2022)", "2018Q3", "-4.102", "")
  prePandemic_Row <- c("", "2017Q1 - 2019Q4", round(mean(prePandemic$extract_diagonal_means(prePandemic$compute_elasticities())), digits = 3), "")
  postPandemic_Row <- c("", "2021Q2 - 2023Q2", round(mean(postPandemic$extract_diagonal_means(postPandemic$compute_elasticities())), digits = 3), "")

  table <- rbind(row1, row2, row3, row4, row5, row6, row7, row8,
                 prePandemic_Row, postPandemic_Row)  
  
  rownames(table) <- NULL
  
  kbl(table,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = header) %>%
      pack_rows("Prior Literature", start_row = 1, end_row = 8) %>%
    pack_rows("New Results", start_row = 9, end_row = 10) %>%
    save_kable(file = table_out)
  
}
