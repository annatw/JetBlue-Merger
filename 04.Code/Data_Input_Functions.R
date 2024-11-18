google_conf <<- FALSE
google_api_Key <<- NA # Must be set before running geocoding features

google_startup <- function(){
  if(!google_conf){
    register_google(key = google_api_Key)
    google_conf <<- TRUE
  } 
}

geocode_cities <- function(){
  google_startup()
 t100 <- readRDS("02.Intermediate/Compile_T100.Rds");
 cities <- unique(c(t100$Origin_City, t100$Destination_City));
 cities <- paste(cities, sep = ", ")
 
 geocoded_cities <- ggmap::geocode(location = cities, output = "latlon",
                            source = "google")

  geocoded_cities$original <- cities
  
 write_rds(geocoded_cities, "02.Intermediate/City_Coordinates.rds")
}


clean_codebook <- function(){
  airline_Codebook <- fread("01.Input/03.carrier_decoder.csv")
  
  airline_Codebook[THRU_DATE_SOURCE == "", THRU_DATE_SOURCE := "12/31/2030"]
  airline_Codebook <- airline_Codebook %>% 
    mutate(THRU_DATE_SOURCE = substr(THRU_DATE_SOURCE, start = 1,
                                     stop = 10),
           THRU_DATE_SOURCE = mdy(THRU_DATE_SOURCE)) %>%
    filter(THRU_DATE_SOURCE >= mdy("January 1, 2010"))
  
  # airline_Codebook <- airline_Codebook %>% filter(REGION == "Domestic")
  
  airline_Codebook <- airline_Codebook %>% mutate(CARRIER_NAME = UNIQUE_CARRIER_NAME,
                                                  CARRIER_ENTITY = UNIQUE_CARRIER_ENTITY,
                                                  CARRIER = UNIQUE_CARRIER) %>%
    select(CARRIER, CARRIER_NAME, CARRIER_ENTITY) %>% unique()
  
  write_rds(airline_Codebook, "02.Intermediate/Clean_Codebook.Rds")
}



create_demographic_data <- function(input = "01.Input/06.IPUMS_Population_Data/nhgis0003_ds254_20215_place.csv",
                                    output = "02.Intermediate/Demographic_Data.Rds"){
  census_data <- fread(input)
  
  # Restrict to Relevant Variables
  census_data <- census_data %>% select(STUSAB, STATE, PLACE, AON4E001, AORMM001)
  census_data$Population <- census_data$AON4E001
  census_data$Income.Per_Cap <- census_data$AORMM001
  
  # Change how Place is Listed
  census_data$PLACE <- gsub(pattern = " CDP", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " city and borough", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " borough", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " village", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " municipality", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " metro township", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " city", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " township", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " town", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " communidad", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " zona urbana", replacement = "", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = " \\(balance\\)", replacement = "", x = census_data$PLACE)
  
  
  # Adjust a few names for specificity
  census_data$PLACE <- gsub(pattern = "Nashville\\-Davidson metropolitan government", 
                            replacement = "Nashville", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = "Urban Honolulu", 
                            replacement = "Honolulu", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = "Louisville/Jefferson County metro government", 
                            replacement = "Louisville", x = census_data$PLACE)
  census_data$PLACE <- gsub(pattern = "Boise City", 
                            replacement = "Boise", x = census_data$PLACE)
  census_data$City <- gsub(pattern = "Kailua \\(Hawaii County\\)",
                           replacement = "Kona", x = census_data$City)
  
  census_data$City <- census_data$PLACE
  census_data$PLACE <- NULL
  
  census_data$City <- paste(census_data$City, census_data$STUSAB, sep = ", ")
  
  census_data$City <- gsub(pattern = "Raleigh, NC",
                           replacement = "Raleigh/Durham, NC", x = census_data$City)
  census_data$City <- gsub(pattern = "Sarasota, FL",
                           replacement = "Sarasota/Bradenton, FL", x = census_data$City)
  census_data$City <- gsub(pattern = "West Palm Beach, FL",
                           replacement = "West Palm Beach/Palm Beach, FL", x = census_data$City)
  census_data$City <- gsub(pattern = "Greensboro, NC",
                           replacement = "Greensboro/High Point, NC", x = census_data$City)
  census_data$City <- gsub(pattern = "Lexington-Fayette urban county, KY",
                           replacement = "Lexington, KY", x = census_data$City)
  census_data$City <- gsub(pattern = "Dallas, TX",
                           replacement = "Dallas/Fort Worth, TX", x = census_data$City)
  census_data$City <- gsub(pattern = "Cedar Rapids, IA",
                           replacement = "Cedar Rapids/Iowa City, IA", x = census_data$City)
  census_data$City <- gsub(pattern = "Jackson, MS",
                           replacement = "Jackson/Vicksburg, MS", x = census_data$City)
  census_data$City <- gsub(pattern = "Midland, TX",
                           replacement = "Midland/Odessa, TX", x = census_data$City)
  census_data$City <- gsub(pattern = "Bend, OR",
                           replacement = "Bend/Redmond, OR", x = census_data$City)
  census_data$City <- gsub(pattern = "Mission, TX",
                           replacement = "Mission/McAllen/Edinburg, TX", x = census_data$City)
  census_data$City <- gsub(pattern = "Pasco, WA",
                           replacement = "Pasco/Kennewick/Richland, WA", x = census_data$City)
  census_data$City <- gsub(pattern = "Augusta-Richmond County consolidated government, GA",
                           replacement = "Augusta, GA", x = census_data$City)
  census_data$City <- gsub(pattern = "Allentown, PA",
                           replacement = "Allentown/Bethlehem/Easton, PA", x = census_data$City)
  census_data$City <- gsub(pattern = "Gulfport, MS",
                           replacement = "Gulfport/Biloxi, MS", x = census_data$City)
  census_data$City <- gsub(pattern = "Harlingen, TX",
                           replacement = "Harlingen/San Benito, TX", x = census_data$City)
  census_data$City <- gsub(pattern = "Scranton, PA",
                           replacement = "Scranton/Wilkes-Barre, PA", x = census_data$City)
  census_data$City <- gsub(pattern = "Bismarck, ND",
                           replacement = "Bismarck/Mandan, ND", x = census_data$City)
  census_data$City <- gsub(pattern = "Bristol/Johnson City/Kingsport, TN",
                           replacement = "Bristol/Johnson City/Kingsport, TN", x = census_data$City)
  census_data$City <- gsub(pattern = "Charleston, WV",
                           replacement = "Charleston/Dunbar, WV", x = census_data$City)

  census_data <- census_data %>% select(City, Population, Income.Per_Cap) 
  
  saveRDS(census_data, output)
}

unify_fleet <- function(target = "02.Intermediate/Fleet_Compilation.rds"){
  
  saveRDS(compile, target)
}

# Airbus and Boeing Planes are listed under several names, this standardizes the 
# manufacturers
standardize_Makes <- function(input = "02.Intermediate/Fleet_Compilation.rds",
                              output =  ""){

  
}

clean_fleet <- function(output_file = "02.Intermediate/Fleet_Compilation.rds"){
  file_head <- "01.Input/07.Fleet_Data"
  file_list <- list.files(path = file_head)
  
  compile <- c()
  for(i in 1:length(file_list)){
    current <- fread(paste(file_head,file_list[i], sep = "/"))
    if(i == 1){
      compile <- current
    } else {
      compile <- rbind(compile, current)
    }
    
    gc()
  }
  
  colnames(compile) <- c("Year", "Carrier.Code", "Carrier", "Manufacture_Year", "Carrier.Unique", "Serial_Number", "Tail_Number",
                         "Aircraft_Status", "Operating_Status", "NumberSeats", "Manufacturer", "Aircraft_Type", "Model",
                         "Capacity_Pounds", "Acquire.Date", "Airplane_ID", "Unique_Carrier.Code")
  
  planes <- compile; remove(compile); gc();
  planes <- planes %>% filter(Operating_Status %in% c("Y", "y"))
  
  # Standardize Make
  boeing <- c("BOEING", "THEBOEINGCOMPANY",
              "BoeingCo", "BOEINGCOMPANY",
              "THEBOEINGCO", "TheBoeingCompany",
              "Boeing747-446", "BoeingCo.",
              "boeing", "Boeing","Boeing(McDonnell-Douglas)",
              "MCDONNELL-DOUGLAS", "McDonnelDouglas", "BOEINGCO",
              "DOUGLASAIRCRAFT", "MCDONNELLDOUGLAS")
  
  airbus <- c("AIRBUS", "Airbus", "airbus",
              "AIRBUSINDUSTRIES", "AirbusCompany", "AirbusIndustrie",
              "AirbusIndustries")
  
  planes <- as.data.table(planes);
  
  planes[Manufacturer %in% boeing, Manufacturer := "Boeing"]
  planes[Manufacturer %in% airbus, Manufacturer := "Airbus"]
  
  # Allegiant reports manufacturer incorrectly on some acquired planes in 2020.
  # Frontier reports manufacturer incorrectly on leased planes
  planes[Model %in% c("A320", "A319", "A-320-2NEO",
                      "A-321", "A-320-1/2", "321-211",
                      "320-214", "319-111"), Manufacturer := "Airbus"]
  
  planes[Model %in% c("737-8"), Manufacturer:= "Boeing"]
  
  saveRDS(planes, output)
}

clean_price_index <- function(input = "01.Input/14.price_index.csv",
                              output = "02.Intermediate/price_index.rds"){
  price_data <- fread(input);
  price_data[,Year := year(DATE)] 
  price_data[, Month := month(DATE)]
  price_data[, DATE := NULL]
  price_data[, Quarter := -1]
  price_data[Month == 1, Quarter := 1]
  price_data[Month == 4, Quarter := 2]
  price_data[Month == 7, Quarter := 3]
  price_data[Month == 10, Quarter := 4]
  price_data[,Month := NULL]
  price_data[, price_index := PCECTPI]
  price_data[, PCECTPI := NULL]
  write_rds(price_data, output)
}

comments_handle <- function(input = "01.Input/16.MergerComments/MergerComments.csv",
                            output = "02.Intermediate/CleanedComments.rds"){
  comments <- fread(input)
  
  # A few fields are the same for everyone - drop them.
  comments <- comments[, .(`Document ID`, `Posted Date`, `Is Withdrawn?`,
               `Title`, `State/Province`, `Zip/Postal Code`,
               `Country`, `Government Agency`, `Government Agency Type`,
               `Comment`, `Reason Withdrawn`, `Submitter Representative`)]
  
  # Process Time to be Date
  comments$Date <- ymd(substr(comments$`Posted Date`, start = 1, stop = 10))
  
  # Clean Comments - Remove \n
  comments[, Comment := gsub(pattern = "\n", replacement = " ", x = Comment)]
  comments[, Comment := trimws(Comment)]
  
  # Now, Detect Stance on each Review
  classifier <- transformers$pipeline("zero-shot-classification", 
          model = 'MoritzLaurer/DeBERTa-v3-large-mnli-fever-anli-ling-wanli')
  
  template <- "The author of this comment {} of the merger."
  labels <- c("approves", "disapproves")
  
  # Restrict to only non-blank comments
  comments.check <- comments[Comment != "", .(`Document ID`, Comment)]
  comments.check <- unique(comments.check$Comment)
  
  i <- 1
  while(i < length(comments.check)){
    j <- min(length(comments.check), i + 50)
    current_sample <- comments.check[i:j]
    result <- classifier(current_sample, labels, hypothesis_template = template,
                         multi_label = FALSE)
    
    saveRDS(result, paste("02.Intermediate/StanceDetection/", i, "_through_", j,".rds", sep = ""))
    
    print(paste("Finished", i, "through", j, sep = " "))
    i <- j+1
  }
  
  file_head <- "02.Intermediate/StanceDetection/"
  file_list <- list.files(path = file_head)
  current_file <- c()
  for(i in 1:length(file_list)){
    current_file <- c(current_file,readRDS(paste(file_head, file_list[i], sep = "")))
  }
  
  comments.string <- c()
  stance_high <- c()
  stance_low <- c()
  stance_high.score <- c();
  stance_low.score <- c();
  
  for(j in 1:length(current_file)){
    comments.string <- c(comments.string, as.character(unlist(current_file[[j]][1])))
    stance_high <- c(stance_high, as.character(unlist(current_file[[j]][2]))[1])
    stance_low <- c(stance_low, as.character(unlist(current_file[[j]][2]))[2])
    stance_high.score <- c(stance_high.score, as.character(unlist(current_file[[j]][3]))[1])
    stance_low.score <- c(stance_low.score, as.character(unlist(current_file[[j]][3]))[2])
  }
  
  approve_score <- c();
  disapprove_score <- c();
  
  for(j in 1:length(stance_high)){
    if(stance_high[j] == "approves"){
      approve_score <- c(approve_score, stance_high.score[j])
      disapprove_score <- c(disapprove_score, stance_low.score[j])
    } else {
      approve_score <- c(approve_score, stance_low.score[j])
      disapprove_score <- c(disapprove_score, stance_high.score[j])
    }
  }
  
  table <- data.table(Comment = comments.string,
                      Stance = stance_high,
                      Approve_Score = approve_score,
                      Disapprove_Score = disapprove_score)
  
  comments <- merge(comments, table, by.x = "Comment", by.y = "Comment", all.x = TRUE)
  
  comments$Approve_Score <- as.double(comments$Approve_Score)
  comments$Disapprove_Score <- as.double(comments$Disapprove_Score)
  
  
  # Now, for sentiment score
  sentiment_analyzer <- transformers$pipeline("text-classification",
                "prosusai/finbert", truncation = TRUE)

  result <- c()
  i <- 1
  while(i < length(comments.check)){
    current_sample <- comments.check[i:min(i+49, length(comments.check))]
    result <- c(result, sentiment_analyzer(current_sample))
    print(paste("Finished", i, "through", min(i + 49, length(comments.check)), sep = " "))
    i <- i + 50
  }
  
  sentiments <- c();
  sentiment_probability <- c()
  
  for(i in 1:length(comments.check)){
    sentiments <- c(sentiments, as.character(result[[i]]$label))
    sentiment_probability <- c(sentiment_probability, as.numeric(result[[i]]$score))
  }
  
  sentiment_frame <- data.table(Comment = comments.check,
                                Sentiment = sentiments,
                                Sentiment_Score = sentiment_probability)
  
  comments <- merge(comments, sentiment_frame, by = "Comment", all.x = TRUE)
  
  write_rds(comments, output)
}

clean_income <- function(input = "01.Input/17.MSA_Income/IncomeTable.csv",
                         output = "02.Intermediate/IncomeData.rds"){
  income_data <- fread("01.Input/17.MSA_Income/IncomeTable.csv")
  income_data[, GeoFips := NULL]
  income_data[, LineCode := NULL]
  income_data <- income_data[Description == "Per capita personal income (dollars) 2",]
  income_data$Description <- NULL
  income_data.long <- as.data.table(melt(data = income_data, 
                                         id.vars = c("GeoName")))
  colnames(income_data.long) <- c("MSA", "Year", "Income.PerCap")
  income_data.long[, MSA := trimws(gsub(pattern = "\\(Metropolitan Statistical Area\\)", replacement = "", 
                                        x = MSA))]
  income_data.long[, MSA := trimws(gsub(pattern = "\\*", replacement = "",
                                        x = MSA))]

  # Manually Adjust Various MSA Names to Align with Other MSA Data
  income_data.long[MSA == "Atlanta-Sandy Springs-Alpharetta, GA", 
                   MSA := "Atlanta-Sandy Springs-Roswell, GA"]
  income_data.long[MSA == "Austin-Round Rock-Georgetown, TX",
                   MSA := "Austin-Round Rock, TX"]
  income_data.long[MSA == "Buffalo-Cheektowaga, NY",
                   MSA := "Buffalo-Cheektowaga-Niagara Falls, NY"]
  income_data.long[MSA == "Grand Rapids-Kentwood, MI",
                   MSA := "Grand Rapids-Wyoming, MI"]
  income_data.long[MSA == "Greenville-Anderson, SC",
                   MSA := "Greenville-Anderson-Mauldin, SC"]
  income_data.long[MSA == "Miami-Fort Lauderdale-Pompano Beach, FL",
                   MSA := "Miami-Fort Lauderdale-West Palm Beach, FL"]
  income_data.long[MSA == "Milwaukee-Waukesha, WI",
                   MSA := "Milwaukee-Waukesha-West Allis, WI"]
  income_data.long[MSA == "Phoenix-Mesa-Chandler, AZ",
                   MSA := "Phoenix-Mesa-Scottsdale, AZ"]
  income_data.long[MSA == "Raleigh-Cary, NC",
                   MSA := "Raleigh, NC"]
  income_data.long[MSA == "Sacramento-Roseville-Folsom, CA",
                   MSA := "Sacramento--Roseville--Arden-Arcade, CA"]
  income_data.long[MSA == "San Diego-Chula Vista-Carlsbad, CA",
                   MSA := "San Diego-Carlsbad, CA"]
  income_data.long[MSA == "San Francisco-Oakland-Berkeley, CA",
                   MSA := "San Francisco-Oakland-Hayward, CA"]
  income_data.long[MSA == "Hartford-East Hartford-Middletown, CT",
                   MSA := "Hartford-West Hartford-East Hartford, CT"]
  
  income_data.long[, Year := as.numeric(as.character(Year))]
  
  # Adjust as this is a midyear Income Estimate
  # quarter_frame <- data.table(Year = rep(min(income_data.long$Year):max(income_data.long$Year), 
  #                                    times = 4),
  #                             Quarter = c(rep(1, times = max(income_data.long$Year) - 
  #                                               min(income_data.long$Year) + 1),
  #                                         rep(2, times = max(income_data.long$Year) - 
  #                                               min(income_data.long$Year) + 1),
  #                                         rep(3, times = max(income_data.long$Year) - 
  #                                               min(income_data.long$Year) + 1),
  #                                         rep(4, times = max(income_data.long$Year) - 
  #                                               min(income_data.long$Year) + 1)))
  # 
  # income_data.long <- merge(income_data.long, quarter_frame, by = "Year",
  #                           allow.cartesian = TRUE)
  # income_data.long <- income_data.long[, .(Year, Quarter, MSA, Income.PerCap)]
  # 
  # income_data.long[, Curr_Year_Income_Per_Capita := Income.PerCap]
  # income_data.long[, Year_Quarter := paste(Year, Quarter)]
  # income_data.long <- income_data.long[order(Year_Quarter),]
  saveRDS(income_data.long, output)
}

covid_read_in <- function(input = "01.Input/18.Covid_Cases/Covid_State_Table.csv",
                          output = "02.Intermediate/Covid_State.rds"){
  covid <- fread(input)
  covid[, start_date := mdy(start_date)]
  covid[, Year := year(start_date)]
  covid[, Month := month(start_date)]
  covid[, Quarter := 1 + (Month-1) %/% 3]
  covid[, State := state]
  
  covid <- covid %>% group_by(Year, Quarter, State) %>%
    summarize(Covid_Cases := sum(new_cases, na.rm = TRUE),
              Covid_Deaths := sum(new_deaths, na.rm = TRUE)) %>%
    as.data.table()
  
  write_rds(covid, output)
}

state_pop_handle <- function(output = "02.Intermediate/State_Pop.rds"){
  twenties <- fread("01.Input/19.StatePopulations/NST-EST2023-POP.csv",
                    skip = 2, header = TRUE)
  tens <- fread("01.Input/19.StatePopulations/nst-est2020.csv",
              skip = 2, header = TRUE)
  colnames(tens)[1] <- "Place"
  tens[, Place := gsub(pattern = "\\.", replacement = "",
                       x = Place)]
  tens <- tens[1:52,]
  
  colnames(twenties)[1] <- "Place"
  twenties$V2 <- NULL
  twenties[, Place := gsub(pattern = "\\.", replacement = "",
                           x = Place)]
  twenties <- twenties[1:52,]  
  
  # Merge Frames
  Pop_Frame <- merge(tens, twenties, by = "Place")
  Pop_Frame <- Pop_Frame[Place != "United States"]
  
  # Match States with Abbreviations
  state_codes <- fread("01.Input/20.StateCodes/state.txt")
  state_codes <- state_codes[,.(STUSAB, STATE_NAME)]
  colnames(state_codes) <- c("State", "Place")
  
  Pop_Frame <- merge(Pop_Frame, state_codes, by = "Place")
  Pop_Frame$Place <- NULL;
  
  Pop_Frame.Long <- melt(Pop_Frame, id.vars = "State", value.name = "Population")
  colnames(Pop_Frame.Long) <- c("State", "Year", "Population")
  Pop_Frame.Long <- as.data.table(Pop_Frame.Long)
  Pop_Frame.Long[, Year := as.numeric(as.character(Year))]
  
  Pop_Frame.Long[, Population := gsub(pattern = ",", replacement = "",
                                      x = Population)]
  Pop_Frame.Long[, Population := as.numeric(Population)]
  
  write_rds(Pop_Frame.Long, "02.Intermediate/State_Populations.rds")
}

load_jet_fuel <- function(input = "01.Input/13.Oil_Spot_Data/Jet_Fuel_Spot.csv",
                          output = "02.Intermediate/jet_fuel.rds"){
  oil <- read.csv(input);
  oil <- as.data.table(oil)
  colnames(oil) <- c("Date", "Jet_Fuel_Price")
  
  oil$Date <- mdy(oil$Date)
  oil$Year <- year(oil$Date)
  oil$Month <- as.character(month(oil$Date))
  oil$Quarter <- factor(oil$Month, levels = c(1:12),
                        labels = c(rep(1, 3), rep(2, 3),
                                   rep(3, 3), rep(4, 3)))
  oil <- oil  %>%
    group_by(Year, Quarter) %>% summarize(Jet_Fuel_Price = mean(Jet_Fuel_Price)) %>%
    as.data.table() 
  
  # Put into Real Dollars
  price_index <- readRDS("02.Intermediate/price_index.rds")
  price_index[, Year := as.numeric(Year)]
  price_index[, Quarter := as.numeric(Quarter)]
  oil[, Year := as.numeric(Year)]
  oil[, Quarter := as.numeric(Quarter)]
  
  oil <- merge(oil, price_index, by = c("Year", "Quarter"), all.x = TRUE)
  oil[, Jet_Fuel_Price := Jet_Fuel_Price / (price_index / 100)]
  oil <- oil[, .(Year, Quarter, Jet_Fuel_Price)]
  
  saveRDS(oil, output)
}

load_jetfuel_financial_reports <- function(spot_prices = "02.Intermediate/jet_fuel.rds", 
                                           output = "02.Intermediate/JetFuel_Reports.rds"){
  reported <- c();
  for(i in 2016:2023){
    if(i == 2016){
      reported <- fread("01.Input/23.Schedule P-12(a)/2016.csv")
    } else {
      reported <- rbind(reported, fread(paste("01.Input/23.Schedule P-12(a)/", i, ".csv", sep = "")))
    }
  }
  # Restrict to main carriers
  reported <- reported[CARRIER_NAME %in% c("Southwest Airlines Co.", "Delta Air Lines Inc.",
                                           "American Airlines Inc.", "Alaska Airlines Inc.",
                                           "JetBlue Airways", "Spirit Air Lines",
                                           "Frontier Airlines Inc.", "Allegiant Air",
                                           "United Air Lines Inc."),]
  
  colnames(reported) <- c("Year", "Quarter", "Month", "AirlineID", "UniqueCarrier",
                          "Carrier", "Gallons", "Expenditure")
  reported$AirlineID <- NULL; reported$UniqueCarrier <- NULL
  
  reported <- reported %>% group_by(Year, Quarter, Carrier) %>%
    summarize(Gallons = sum(Gallons),
              Expenditure = sum(Expenditure)) %>% as.data.table()
  
  reported[, Jet_Fuel_Price := Expenditure / Gallons]
  reported[, Year := as.numeric(Year)]
  reported[, Quarter := as.numeric(Quarter)]
  
  reported <- reported[, .(Year, Quarter, Carrier, Jet_Fuel_Price)]
  
  
  national_average <- readRDS(spot_prices)
  colnames(national_average) <- c("Year", "Quarter", "National_Average_JF_Price")
  national_average[, Year := as.numeric(Year)]
  national_average[, Quarter := as.numeric(Quarter)]
  
  reported <- merge(reported, national_average)
  
  
  saveRDS(reported, output)
}

hundred_largest_MSA <- function(input = "01.Input/10.MSA_Population_Data/cbsa-est2023-alldata.csv",
                                output = "03.Output/hundred_largest_MSA.csv"){
  data <- fread(input)
  data <- data[LSAD == "Metropolitan Statistical Area",]
  data <- data[, .(NAME, POPESTIMATE2022, CBSA)]
  data <- data[order(-POPESTIMATE2022),]
  write_csv(data, output)
}

state_income_clean <- function(input = '01.Input/24.State_Income/StateIncomeTable.csv',
                         output = "02.Intermediate/state_income.rds"){
  data <- fread(input)
  data <- data[GeoFips > 0,]
  data <- data[Description %in% c("Population (midperiod, persons) 1",
                                  "Per capita personal income (dollars) 2")]
 
  data[GeoFips == 2000, GeoName := "Alaska"]
  data[GeoFips == 15000, GeoName := "Hawaii"]
  data[, GeoFips := NULL]
  data[, LineCode := NULL]
  data[Description == "Population (midperiod, persons) 1", 
       Description := "Population"]
  data[Description == "Per capita personal income (dollars) 2",
       Description := "Income_PerCapita"]
  
  data.m <- melt(data, id.vars = c("GeoName", "Description"),
                 variable.name = "Period",
                 value.name = "Value") %>% as.data.table()
  data.m[, Year := as.numeric(substr(Period, start = 1, stop = 4))]
  data.m[, Quarter := as.numeric(substr(Period, start = 7, stop = 7))]
  data.m[, Period := NULL]

  data.w <- reshape(data.m, timevar = "Description",
                    idvar = c("GeoName", "Year", "Quarter"),
                    direction = "wide")
  colnames(data.w) <- c("State", "Year", "Quarter", "StatePopulation", "State_Income_PerCapita")

  state_codes <- fread("01.Input/20.StateCodes/state.txt")
  state_codes <- state_codes[, .(STUSAB, STATE_NAME)]
  colnames(state_codes) <- c("Abbr", "State")
  
  data.w[, State := factor(State, levels = state_codes$State, labels = state_codes$Abbr)]
  
  saveRDS(data.w, output)
}
