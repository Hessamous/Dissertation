data_frames_list <- list()
library(dplyr)
library(writexl)
library(readr)
library(zoo)
library(tidyr)
library(forecast)
library(tidyverse)
library(keras)

setwd("~/Downloads/Airline Pax")

#data input
{for (M in 2:35) {
  file_name <- paste0("T_T100D_MARKET_ALL_CARRIER-", M, ".csv")
  
  data_frames_list[[as.character(2025-M)]] <- read_csv(file_name)
}
PAX <- bind_rows(data_frames_list)
}

#initial cleaning
{PAX <- PAX %>%
  filter(DISTANCE !=0)
PAX <- PAX %>%
  filter(PASSENGERS != 0 | FREIGHT != 0 | MAIL != 0)
PAX <- PAX %>%
  filter(CARRIER_GROUP_NEW %in% c(1, 2, 3, 4, 5, 6, 7))
}

#Business Model
{
lcc_carriers <- c("QH", "BE", "DH", "PE (1)", "FF", "SX", "JX (2)", "EE", "B6", "WN", "NK", 
                  "FL", "G4", "XP", "MX", "F9", "7H", "SY", "HP", "VX", "J7", "KN", "KP", 
                  "N7", "NJ", "P9", "QQ", "TZ", "W7", "W9", "WV (1)", "KW", "DH", "6F (1)", 
                  "ML (1)", "YX (1)", "NY", "PS (1)", "PE (1)", "SX", "FF", "U5", "1AQ")
cargo_carriers <- c("2PQ", "ABX", "KH", "PRM", "M6", "5Y", "FX", "KAQ", "N8", "NC", "PO",
                    "WI", "5X", "U7", "KD", "JW", "ER", "PT (1)", "WE", "X6", "TCQ", "PRQ", 
                    "FT", "F2", "GR", "SAQ", "9S", "IKQ", "WO (1)", "BUR", "8C", "KR", "CDQ", 
                    "KLQ", "EZ", "5V", "L2", "1TQ", "CTQ", "JKQ", "GFQ", "PFQ", "9R", "RLQ", 
                    "GG", "TNQ", "FNQ", "ZKQ", "LBQ", "BFQ", "XPQ", "AMQ")

PAX <- PAX %>%
  mutate(Business_Model = case_when(
    UNIQUE_CARRIER %in% cargo_carriers~ "Cargo",
    UNIQUE_CARRIER %in% lcc_carriers ~ "LCC",
    CARRIER_GROUP_NEW == 5 ~ "SCC",
    CARRIER_GROUP_NEW == 6 ~ "Air Taxi",
    TRUE ~ "FSC"
  ))}


Market <- PAX %>%
  left_join(City_Market, by = c("ORIGIN_CITY_MARKET_ID" = "Code")) %>%
  group_by(Market = Description,ORIGIN)%>%
  summarise


#airline PAX
{City_Market <- read_csv("L_CITY_MARKET_ID.csv")
  airline_PAX <- PAX %>%
  left_join(City_Market, by = c("ORIGIN_CITY_MARKET_ID" = "Code")) %>%
  left_join(City_Market, by = c("DEST_CITY_MARKET_ID" = "Code"), suffix = c("_Origin", "_Dest")) %>%
  group_by(UNIQUE_CARRIER_NAME,
           REGION,
           CARRIER_GROUP_NEW,
           Origin = Description_Origin,
           Destination = Description_Dest,
           ORIGIN_STATE_NM,
           ORIGIN_CITY_NAME,
           DEST_STATE_NM,
           DEST_CITY_NAME,
           Date = as.Date(paste(YEAR, MONTH, "01", sep = "-")),
           CLASS,
           Business_Model)%>%
  summarise(Distance = mean(DISTANCE, na.rm = TRUE),
            Passengers = sum(PASSENGERS, na.rm = TRUE),
            Freight = sum(FREIGHT, na.rm = TRUE),
            Mail = sum(MAIL, na.rm = TRUE)
  ) %>%
  mutate(
    Haul = case_when(
      Distance < 311 ~ "Very Short",
      Distance < 932 ~ "Short",
      Distance < 2485 ~ "Medium",
      TRUE ~ "Long"
    )
  )


write.csv(airline_PAX, file = "Airlines_Complete.csv", row.names = FALSE)
}

#market PAX
{market_PAX <- PAX %>%
    left_join(City_Market, by = c("ORIGIN_CITY_MARKET_ID" = "Code")) %>%
    left_join(City_Market, by = c("DEST_CITY_MARKET_ID" = "Code"), suffix = c("_Origin", "_Dest")) %>%
    group_by(
      Route = paste(Description_Origin, " - ", Description_Dest),
      Origin = Description_Origin,
      Destination = Description_Dest,
      Date = as.Date(paste(YEAR, MONTH, "01", sep = "-")),
      Business_Model
    ) %>%
    summarise(
      Distance = mean(DISTANCE, na.rm = TRUE),
      Passengers = sum(PASSENGERS, na.rm = TRUE),
      Freight = sum(FREIGHT, na.rm = TRUE),
      Mail = sum(MAIL, na.rm = TRUE),
      Airlines = n()
    ) %>%
    mutate(
      Haul = case_when(
        Distance < 311 ~ "Very Short",
        Distance < 932 ~ "Short",
        Distance < 2485 ~ "Medium",
        TRUE ~ "Long"
      )
) %>%
    mutate(
      Competition = case_when(
        Airlines < 2 ~ "Autonomy",
        Airlines < 6 ~ "Low",
        Airlines < 11 ~ "Moderate",
        TRUE ~ "High"
      )
)
  write.csv(market_PAX, file = "Routes.csv", row.names = FALSE)
}
market_PAX <- read_csv("Routes.csv")

#dates
all_dates <- seq(as.Date("1990-01-01"), as.Date("2023-09-01"), by = "month")
forecast_dates <- seq(as.Date("2017-01-01"), as.Date("2023-09-01"), by = "month")
fit_dates <- seq(as.Date("1990-01-01"), as.Date("2016-12-01"), by = "month")

#normalization_minmax
{
min <- matrix(NA, nrow = length(business_models), ncol = length(loads), 
              dimnames = list(business_models, loads))
max <- matrix(NA, nrow = length(business_models), ncol = length(loads), 
              dimnames = list(business_models, loads))

# Normalization_minmax
for (business_model in business_models) {
  for (load in loads) {
    min_val <- min(market_PAX[market_PAX$Business_Model == business_model, ][, load], na.rm = TRUE)
    max_val <- max(market_PAX[market_PAX$Business_Model == business_model, ][, load], na.rm = TRUE)
    
    min[business_model, load] <- min_val
    max[business_model, load] <- max_val
  }
}
}
write.csv(max, file = "max.csv", row.names = FALSE)

#data preparation for forecasting
{n_routes = 100
ts_list <- list()
business_models <- c("FSC", "LCC", "Cargo", "SCC", "Air Taxi")
hauls <- c("Very Short", "Short", "Medium", "Long")
haul_df <- data.frame(
 Date = character(),        
 Business_Model = character(),
 Haul = character(),         
 Passengers = numeric(),
 Freight = numeric(),
 Mail = numeric(),
 Mean_Airlines = numeric(),
 Mean_Competition = numeric(),
 Mean_Distance = numeric()
)
min_max_normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

for (business_model in business_models) {
  ts_data <- market_PAX[market_PAX$Business_Model == business_model, ]
  
  # Perform min-max normalization for "Passengers", "Freight", and "Mail" columns
  cols_to_normalize <- c("Passengers", "Freight", "Mail")
  ts_data[cols_to_normalize] <- lapply(ts_data[cols_to_normalize], min_max_normalize)

  selected_routes <- head(arrange(ts_data %>%
                                    group_by(Route) %>%
                                    summarise(Load = sum(Passengers, na.rm = TRUE) + 
                                                sum(Freight, na.rm = TRUE) + 
                                                sum(Mail, na.rm = TRUE)),
                                  desc(Load))$Route, n_routes)
  
#time series of the top 100 routes per business model
  for (route in selected_routes) {
    route_data <- subset(ts_data, Route == route)
    
    merged_data <- merge(data.frame(Date = all_dates), route_data, by = "Date", all.x = TRUE)
    
    merged_data$Origin[is.na(merged_data$Origin)] <- as.character(route_data[1, 2])
    merged_data$Destination[is.na(merged_data$Destination)] <- as.character(route_data[1,3])
    merged_data$Haul[is.na(merged_data$Haul)] <- as.character(route_data[1,11])
    merged_data$Route[is.na(merged_data$Route)] <- route
    merged_data$Business_Model[is.na(merged_data$Business_Model)] <- business_model
    merged_data$Distance[is.na(merged_data$Distance)] <- mean(route_data$Distance)
    merged_data <- mutate(merged_data,
                          Competition = case_when(
                            Competition == "Autonomy" ~ 1,
                            Competition == "Low" ~ 2,
                            Competition == "Moderate" ~ 3,
                            Competition == "High" ~ 4,
                            TRUE ~ 0)
                          )
    merged_data[is.na(merged_data)] <- 0
    
    ts_passengers <- zoo(merged_data$Passengers, order.by = merged_data$Date)
    ts_freight <- zoo(merged_data$Freight, order.by = merged_data$Date)
    ts_mail <- zoo(merged_data$Mail, order.by = merged_data$Date)
    ts_airlines <- zoo(merged_data$Airlines, order.by = merged_data$Date)
    ts_competition <- zoo(merged_data$Competition, order.by = merged_data$Date)
    
    ts_list[[business_model]][["Passengers"]][[route]] <- ts_passengers
    ts_list[[business_model]][["Freight"]][[route]] <- ts_freight
    ts_list[[business_model]][["Mail"]][[route]] <- ts_mail
    ts_list[[business_model]][["Airlines"]][[route]] <- ts_airlines
    ts_list[[business_model]][["Competition"]][[route]] <- ts_competition
    
  }
  #dataframe for keras LSTM
    bm <- ts_data %>% 
      mutate(Competition = case_when(
      Competition == "Autonomy" ~ 1,
      Competition == "Low" ~ 2,
      Competition == "Moderate" ~ 3,
      Competition == "High" ~ 4,
      TRUE ~ 0)) %>% 
      group_by(Date,
               Haul,
               Business_Model)%>%
      summarise(Passengers = sum(Passengers, na.rm = TRUE),
                Freight = sum(Freight, na.rm = TRUE),
                Mail = sum(Mail, na.rm = TRUE),
                Mean_Airlines = mean(Airlines, na.rm = TRUE),
                Mean_Competition = mean(Competition, na.rm = TRUE),
                Mean_Distance = mean(Distance, na.rm = TRUE)
      )
    haul_df <- merge(haul_df, bm, by = c("Date", "Business_Model", "Haul", "Passengers", "Freight", "Mail", "Mean_Airlines", "Mean_Competition", "Mean_Distance"), all = TRUE)
  }
  bm <- expand_grid(Date = all_dates, Business_Model = business_models, Haul = hauls)
  bm$Passengers <- 0
  bm$Freight <- 0
  bm$Mail <- 0
  bm$Mean_Airlines <- 0
  bm$Mean_Competition <- 0
  bm$Mean_Distance <- 0
  haul_df <- merge(haul_df, bm, by = c("Date", "Business_Model", "Haul"), all = TRUE, suffixes = c("",".y"))
  haul_df <- haul_df[, -c(10:15)]
  haul_df[is.na(haul_df)] <- 0}

#haul_df
{business_models <- c("FSC", "LCC", "Cargo", "SCC", "Air Taxi")
haul_df <- data.frame(
  Date = character(),        
  Business_Model = character(),
  Haul = character(),         
  Passengers = numeric(),
  Freight = numeric(),
  Mail = numeric(),
  Mean_Airlines = numeric(),
  Mean_Competition = numeric(),
  Mean_Distance = numeric()
)
for (business_model in business_models) {
  ts_data <- market_PAX[market_PAX$Business_Model == business_model, ] %>% mutate(Competition = case_when(
                                                                                    Competition == "Autonomy" ~ 1,
                                                                                    Competition == "Low" ~ 2,
                                                                                    Competition == "Moderate" ~ 3,
                                                                                    Competition == "High" ~ 4,
                                                                                    TRUE ~ 0))
  
   # Perform min-max normalization for "Passengers", "Freight", and "Mail" columns
    cols_to_normalize <- c("Passengers", "Freight", "Mail")
    ts_data[cols_to_normalize] <- lapply(ts_data[cols_to_normalize], min_max_normalize)
    
    bm <- ts_data %>% 
    group_by(Date,
             Haul,
             Business_Model)%>%
    summarise(Passengers = sum(Passengers, na.rm = TRUE),
              Freight = sum(Freight, na.rm = TRUE),
              Mail = sum(Mail, na.rm = TRUE),
              Mean_Airlines = mean(Airlines, na.rm = TRUE),
              Mean_Competition = mean(Competition ,na.rm = TRUE),
              Mean_Distance = mean(Distance, na.rm = TRUE)
    )
  haul_df <- merge(haul_df, bm, by = c("Date", "Business_Model", "Haul", "Passengers", "Freight", "Mail", "Mean_Airlines", "Mean_Competition", "Mean_Distance"), all = TRUE)
}
bm <- expand_grid(Date = all_dates, Business_Model = business_models, Haul = hauls)
bm$Passengers <- 0
bm$Freight <- 0
bm$Mail <- 0
bm$Mean_Airlines <- 0
bm$Mean_Competition <- 0
bm$Mean_Distance <- 0
haul_df <- merge(haul_df, bm, by = c("Date", "Business_Model", "Haul"), all = TRUE, suffixes = c("",".y"))
haul_df <- haul_df[, -c(10:15)]
haul_df[is.na(haul_df)] <- 0
}
write.csv(haul_df, file = "Haul.csv", row.names = FALSE)
haul_df <- read_csv("Haul.csv")

#train-test splitting, ARIMA/X forecasting, and accuracy measures
{performance_list <- list()
  
  for (i in 1:5) {
    for (j in 1:3) {
      for (k in seq_along(ts_list[[i]][[j]])) {
        ts <- ts_list[[i]][[j]][[k]]
        exog_var <- ts_list[[i]][["Airlines"]][[k]]
        exog_var2 <- ts_list[[i]][["Competition"]][[k]]
        train_set_ts <- window(ts, end = "2016-12-01")
        test_set_ts <- window(ts, start = "2017-01-01")
        train_set_exog <- window(exog_var, end = "2016-12-01")
        test_set_exog <- window(exog_var, start = "2017-01-01")
        train_set_exog2 <- window(exog_var2, end = "2016-12-01")
        test_set_exog2 <- window(exog_var2, start = "2017-01-01")
        
        # Initialize variables
        arimax_mape <- NA
        arimax_rsquared <- NA
        arimax_mape2 <- NA
        arimax_rsquared2 <- NA
        arima_mape <- NA
        arima_rsquared <- NA
        
        # Calculate mean of the test set
        tss <- sum((train_set_ts - mean(train_set_ts))^2)
                
        # Arimax Airlines
        tryCatch({
          arimax_model <- auto.arima(as.numeric(train_set_ts), xreg = as.numeric(train_set_exog))
          arimax_forecasts <- forecast(arimax_model, xreg = as.numeric(test_set_exog), h = 81)
          arimax_rss <- sum(as.numeric(arimax_forecasts[["residuals"]])^2)
          arimax_rsquared <- 1 - (arimax_rss / tss)
          arimax_mape <- accuracy(arimax_forecasts, test_set_ts)["Test set", "MAPE"]
        }, error = function(e) {
          cat("Arimax Airlines error in iteration (i=", i, ", j=", j, ", k=", k, "):", conditionMessage(e), "\n")
        })
        
        # Arima
        tryCatch({
          arima_model <- auto.arima(as.numeric(train_set_ts))
          arima_forecasts <- forecast(arima_model, h = 81)
          arima_rss <- sum(as.numeric(arima_forecasts[["residuals"]])^2)
          arima_rsquared <- 1 - (arima_rss / tss)
          arima_mape <- accuracy(arima_forecasts, test_set_ts)["Test set", "MAPE"]
        }, error = function(e) {
          cat("Arima error in iteration (i=", i, ", j=", j, ", k=", k, "):", conditionMessage(e), "\n")
        })
        
        # Arimax Competition
        tryCatch({
          arimax_model2 <- auto.arima(as.numeric(train_set_ts), xreg = as.numeric(train_set_exog2))
          arimax_forecasts2 <- forecast(arimax_model2, xreg = as.numeric(test_set_exog2), h = 81)
          arimax_rss2 <- sum(as.numeric(arimax_forecasts2[["residuals"]])^2)
          arimax_rsquared2 <- 1 - (arimax_rss2 / tss)
          arimax_mape2 <- accuracy(arimax_forecasts2, test_set_ts)["Test set", "MAPE"]
        }, error = function(e) {
          cat("Arimax Competition error in iteration (i=", i, ", j=", j, ", k=", k, "):", conditionMessage(e), "\n")
        })
        
        # Create rows for the performance table
        r2_row <- c(names(ts_list)[[i]], names(ts_list[[i]])[[j]], names(ts_list[[i]][[j]])[[k]], "R2", arima_rsquared, arimax_rsquared, arimax_rsquared2)
        mape_row <- c(names(ts_list)[[i]], names(ts_list[[i]])[[j]], names(ts_list[[i]][[j]])[[k]], "MAPE", arima_mape, arimax_mape, arimax_mape2)
        
        # Add rows to the performance list
        performance_list[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1]] <- r2_row
        performance_list[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k]] <- mape_row
      }
    }
  }
  
  # Create the performance data frame
  performance <- data.frame(do.call(rbind, performance_list))
  
  # Set column names
  colnames(performance) <- c("Business_Model", "Load", "Route", "Metric", "ARIMA", "ARIMAX_Airlines", "ARIMAX_Competition")


  # Convert columns to numeric
  performance <- performance %>%
    mutate_at(vars(ARIMA, ARIMAX_Airlines, ARIMAX_Competition), as.numeric)
}


#Haul
Route_Haul <- market_PAX %>% group_by(Haul, Route) %>% summarise()

#ARIMA/X analysis
{
  performance <- performance %>%
    filter(!is.infinite(ARIMA) & !is.infinite(ARIMAX_Airlines) & !is.infinite(ARIMAX_Competition) &
         (!is.na(ARIMA) | !is.na(ARIMAX_Airlines) | !is.na(ARIMAX_Competition)))

performance <- left_join(performance, Route_Haul)
      
percentage <- performance %>%
  group_by(Business_Model, Metric, Load, Haul) %>%
  summarise(
    ARIMA_average = mean(ARIMA),
    ARIMAX_Airlines_average = mean(ARIMAX_Airlines, na.rm = TRUE),
    ARIMAX_Competition_average = mean(ARIMAX_Competition, na.rm = TRUE)
    
  )

percentage$Improvement_Airlines <- ifelse(percentage$Metric == "R2",
                                  (percentage$ARIMAX_Airlines_average - percentage$ARIMA_average) * 100 / percentage$ARIMA_average,
                                  (percentage$ARIMA_average - percentage$ARIMAX_Airlines_average) * 100 / percentage$ARIMA_average)
percentage$Improvement_Competition <- ifelse(percentage$Metric == "R2",
                                          (percentage$ARIMAX_Competition_average - percentage$ARIMA_average) * 100 / percentage$ARIMA_average,
                                          (percentage$ARIMA_average - percentage$ARIMAX_Competition_average) * 100 / percentage$ARIMA_average)
R2 <- percentage %>% 
  filter(Metric == "R2")
R2 <- subset(R2, select = -Metric)

MAPE <- percentage %>% 
  filter(Metric == "MAPE")
MAPE <- subset(MAPE, select = -Metric)
}
write.csv(performance, file = "performance.csv", row.names = FALSE)
performance <- read_csv("performance.csv")
write.csv(percentage, file = "percentage.csv", row.names = FALSE)

write.csv(MAPE, file = "MAPE.csv", row.names = FALSE)
write.csv(R2, file = "R2.csv", row.names = FALSE)

business_models <- c("FSC", "LCC", "Cargo", "SCC", "Air Taxi")
hauls <- c("Very Short", "Short", "Medium", "Long")
#ARIMA
{  r2_df <- data.frame(
    Business_Model = character(),
    Load = character(),
    Haul = character(),
    ARIMA = numeric(),
    ARIMAX_Air = numeric(),
    ARIMAX_Comp = numeric()
  )
  
  mape_df <- data.frame(
    Business_Model = character(),
    Load = character(),
    Haul = character(),
    ARIMA = numeric(),
    ARIMAX_Air = numeric(),
    ARIMAX_Comp = numeric()
  )
  
  loads = c("Passengers", "Freight", "Mail")
  
  for (business_model in business_models) {
    for (haul in hauls) {
        subset_data <- haul_df[haul_df$Business_Model == business_model & haul_df$Haul == haul, ]
        
      for (load in loads){
        # Convert relevant columns to numeric
        ts <- zoo(subset_data[[load]], order.by = subset_data$Date)
        exog_var <- zoo(subset_data$Mean_Airlines, order.by = subset_data$Date)
        exog_var2 <- zoo(subset_data$Mean_Competition, order.by = subset_data$Date)
        train_set_ts <- window(ts, end = "2016-12-01")
        test_set_ts <- window(ts, start = "2017-01-01")
        train_set_exog <- window(exog_var, end = "2016-12-01")
        test_set_exog <- window(exog_var, start = "2017-01-01")
        train_set_exog2 <- window(exog_var2, end = "2016-12-01")
        test_set_exog2 <- window(exog_var2, start = "2017-01-01")
        
        # Initialize variables
        arimax_mape <- NA
        arimax_rsquared <- NA
        arimax_mape2 <- NA
        arimax_rsquared2 <- NA
        arima_mape <- NA
        arima_rsquared <- NA
        
        # Calculate mean of the test set
        tss <- sum((train_set_ts - mean(train_set_ts))^2)
        
        # Arimax Airlines
        tryCatch({
          arimax_model <- auto.arima(as.numeric(train_set_ts), xreg = as.numeric(train_set_exog))
          arimax_forecasts <- forecast(arimax_model, xreg = as.numeric(test_set_exog), h = 81)
          arimax_rss <- sum(as.numeric(arimax_forecasts[["residuals"]])^2)
          arimax_rsquared <- 1 - (arimax_rss / tss)
          arimax_mape <- accuracy(arimax_forecasts, test_set_ts)["Test set", "MAPE"]
        }, error = function(e) {
          cat("Arimax Airlines error in iteration (Business Model=", business_model, ", Haul=", haul, ", Load=", load, "):", conditionMessage(e), "\n")
        })
        
        # Arima
        tryCatch({
          arima_model <- auto.arima(as.numeric(train_set_ts))
          arima_forecasts <- forecast(arima_model, h = 81)
          arima_rss <- sum(as.numeric(arima_forecasts[["residuals"]])^2)
          arima_rsquared <- 1 - (arima_rss / tss)
          arima_mape <- accuracy(arima_forecasts, test_set_ts)["Test set", "MAPE"]
        }, error = function(e) {
          cat("Arima error in iteration (Business Model=", business_model, ", Haul=", haul, ", Load=", load, "):", conditionMessage(e), "\n")
        })
        
        # Arimax Competition
        tryCatch({
          arimax_model2 <- auto.arima(as.numeric(train_set_ts), xreg = as.numeric(train_set_exog2))
          arimax_forecasts2 <- forecast(arimax_model2, xreg = as.numeric(test_set_exog2), h = 81)
          arimax_rss2 <- sum(as.numeric(arimax_forecasts2[["residuals"]])^2)
          arimax_rsquared2 <- 1 - (arimax_rss2 / tss)
          arimax_mape2 <- accuracy(arimax_forecasts2, test_set_ts)["Test set", "MAPE"]
        }, error = function(e) {
          cat("Arimax Competition error in iteration (Business Model=", business_model, ", Haul=", haul, ", Load=", load, "):", conditionMessage(e), "\n")
        })
        
        # Save metrics to data frame
        mape_df <- rbind(mape_df, data.frame(
          Business_Model = business_model,
          Load = load,
          Haul = haul,
          ARIMA = arima_mape,
          ARIMAX_Air = arimax_mape,
          ARIMAX_Comp = arimax_mape2
        ))
        
        r2_df <- rbind(r2_df, data.frame(
          Business_Model = business_model,
          Load = load,
          Haul = haul,
          ARIMA = arima_rsquared,
          ARIMAX_Air = arimax_rsquared,
          ARIMAX_Comp = arimax_rsquared2
        ))
      }
    }
  }
}

write.csv(r2_df, file = "ARIMA-R2.csv", row.names = FALSE)
write.csv(mape_df, file = "ARIMA-MAPE.csv", row.names = FALSE)

#After Grid Search
NMSE_AGS <- merge(NMSE, nmse_df, by = c("Business_Model", "Load", "Haul"))
R2_AGS <- merge(R2, r2_df, by = c("Business_Model", "Load", "Haul"))
write.csv(R2_AGS, file = "R2-AGS.csv", row.names = FALSE)
write.csv(NMSE_AGS, file = "NMSE-AGS.csv", row.names = FALSE)

install_keras()

#multivariate LSTM
{
  # Data Preparation
  ts_data <- haul_df[c("Business_Model", "Haul", "Passengers", "Freight", "Mail")]
  
  # Train-test split
  train_size <- floor(0.8 * nrow(ts_data))
  train_set <- ts_data[1:train_size, ]
  test_set <- ts_data[(train_size + 1):nrow(ts_data), ]
  
  # Normalize data if needed
  # train_set[, c("Passengers", "Freight", "Mail")] <- scale(train_set[, c("Passengers", "Freight", "Mail")])
  
  # Multivariate LSTM Model
  create_train_mvlstm_model <- function(train_data) {
    model <- keras_model_sequential() %>%
      layer_lstm(units = 50, input_shape = c(4, 5, 3)) %>%
      layer_dense(units = 3)  # Adjust units based on the number of output variables
    
    model %>% compile(
      loss = "mean_squared_error",
      optimizer = optimizer_adam(),
      metrics = c("mean_absolute_error")
    )
    
    model %>% fit(
      x = array(train_data, dim = c(nrow(train_data), 4, 5, 3)),
      y = matrix(train_data, dim = c(4, 5, 3)),
      epochs = 50,
      batch_size = 32,
      verbose = 0
    )
    
    return(model)
  }
  
  # Create and train LSTM model
  mvlstm_model <- create_train_mvlstm_model(train_set)
  
  # Make predictions
  mvlstm_predictions <- make_lstm_predictions(mvlstm_model, test_set)
  
  # Calculate metrics (adapt these functions for multivariate case)
  nmse <- calculate_nmse(test_set[, c("Passengers", "Freight", "Mail")], mvlstm_predictions)
  r2 <- calculate_rsquared(train_set[, c("Passengers", "Freight", "Mail")], make_lstm_predictions(mvlstm_model, train_set))
}

#GridSearch
{
# Define a function to create and train an LSTM model
create_train_lstm_model <- function(units, batch_size, optimizer, loss, metrics, train_data) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = units, input_shape = c(1, 1)) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = loss,
    optimizer = optimizer,
    metrics = metrics
  )
  
  model %>% fit(
    x = array(train_data, dim = c(length(train_data), 1, 1)),
    y = train_data,
    epochs = 50,
    batch_size = batch_size,
    verbose = 0
  )
  
  return(model)
}

# Grid search parameters
units_values <- c(20, 50, 100)
batch_size_values <- c(16, 32, 64)
optimizer_values <- c("adam", "rmsprop")
loss_values <- c("mean_squared_error", "mean_absolute_error")
metrics_values <- c("mean_absolute_error", "mean_squared_error")

# Data subset for grid search
subset_data <- haul_df[haul_df$Business_Model == "FSC" & haul_df$Haul == "Short", ]
ts_data <- subset_data[["Passengers"]]

# Train-test split
train_size <- floor(0.8 * length(ts_data))
train_set <- ts_data[1:train_size]
test_set <- ts_data[(train_size + 1):length(ts_data)]

# Perform grid search
best_model <- NULL
best_params <- NULL
best_nmse <- Inf

for (units in units_values) {
  for (batch_size in batch_size_values) {
    for (optimizer in optimizer_values) {
      for (loss in loss_values) {
        for (metrics in metrics_values) {
          # Create and train LSTM model
          lstm_model <- create_train_lstm_model(units, batch_size, optimizer, loss, metrics, train_set)
          
          # Make predictions
          lstm_predictions <- make_lstm_predictions(lstm_model, test_set)
          
          # Calculate NMSE
          nmse <- calculate_nmse(test_set, lstm_predictions)
          
          # Check if this is the best model so far
          if (nmse < best_nmse) {
            best_nmse <- nmse
            best_model <- lstm_model
            best_params <- c(units = units, batch_size = batch_size, optimizer = optimizer, loss = loss, metrics = metrics)
          }
        }
      }
    }
  }
}

# Print the best parameters
cat("Best Parameters:\n")
print(best_params)

# Evaluate the best model on the test set
best_predictions <- make_lstm_predictions(best_model, test_set)
best_nmse <- calculate_nmse(test_set, best_predictions)
cat("Best NMSE:", best_nmse, "\n")
}


#Plotting Forecasts
{i <- 3
  j <- 2
  k <- 17
  ts <- ts_list[[i]][[j]][[k]]
  air <- ts_list[[i]][[4]][[k]]
  comp <- ts_list[[i]][[5]][[k]]
  
  train_set_ts <- window(ts, end = "2016-12-01")
  test_set_ts <- window(ts, start = "2017-01-01")
  
  train_set_air <- window(air, end = "2016-12-01")
  test_set_air <- window(air, start = "2017-01-01")
  train_set_comp <- window(comp, end = "2016-12-01")
  test_set_comp <- window(comp, start = "2017-01-01")
  
  arimaxair_model <- auto.arima(as.numeric(train_set_ts), xreg = as.numeric(train_set_air))
  arimaxair_forecast <- forecast(arimaxair_model, xreg = as.numeric(test_set_air), h = 81)$mean
  arimaxcomp_model <- auto.arima(as.numeric(train_set_ts), xreg = as.numeric(train_set_comp))
  arimaxcomp_forecast <- forecast(arimaxcomp_model, xreg = as.numeric(test_set_comp), h = 81)$mean
  
  arima_model <- auto.arima(as.numeric(train_set_ts))
  arima_forecast <- forecast(arima_model, h = 81)$mean
  # Set outer and inner margins
  par(oma = c(0, 0, 0, 0) + 0.1, mar = c(4.5, 1, 2, 0) + 0.1)  
  ylim <- range(ts)
  plot(test_set_ts, type = "l", col = "blue", lwd = 2, xlab= performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 3]], ylim = ylim, cex.lab= 0.6)
  
  # Split the title into multiple lines
  title(main = paste(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 1]], performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 2]], "\n", "Actual vs. Forecast"), cex.main = 0.8)
  lines(forecast_dates, arima_forecast, col = "green", lty = 2)
  lines(forecast_dates, arimaxair_forecast, col = "red", lty = 2)
  lines(forecast_dates, arimaxcomp_forecast, col = "purple", lty = 2)
  legend("bottomleft", c("Actual ", paste("ARIMA Forecast, MAPE=", round(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k, 5]],0)),
                         paste("ARIMAX with Airlines, MAPE=", round(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k, 6]],0)),
                         paste("ARIMAX with Competition MAPE=", round(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k, 7]],0))),
         col = c("blue", "green", "red", "purple"), lty = c(1, 2, 2, 2), cex = 0.4)
}
#Plotting the Fit
{ arimaxair_fitted <- fitted(arimaxair_model)
  arimaxcomp_fitted <- fitted(arimaxcomp_model)
  arima_fitted <- fitted(arima_model)
  
  plot_dates <- time(train_set_ts)
  
  # Set outer and inner margins
  par(oma = c(0, 0, 0, 0) + 0.1, mar = c(4.5, 1, 2, 0) + 0.1)  # Adjust right margin for the legend
  ylim <- range(ts)
  plot(train_set_ts, type = "l", col = "blue", lwd = 2, xlab= performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 3]], ylim = ylim, xlim = c(as.Date("2010-01-01"), as.Date("2016-12-01")), cex.lab= 0.6)
  
  # Split the title into multiple lines
  title(main = paste(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 1]], performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 2]], "\n", "Actual vs. Fitted"), cex.main = 0.8)
  
  lines(plot_dates, arima_fitted, col = "green", lty = 2)
  lines(plot_dates, arimaxair_fitted, col = "red", lty = 2)
  lines(plot_dates, arimaxcomp_fitted, col = "purple", lty = 2)
  
  legend("topright", c("Actual ", paste("ARIMA Forecast, R2=", round(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 5]],2)),
                       paste("ARIMAX with Airlines, R2=", round(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 6]],2)),
                       paste("ARIMAX with Competition R2=", round(performance[[6 * (n_routes) * (i - 1) + 2 * (n_routes) * (j - 1) + 2 * k - 1, 7]],2))),
         col = c("blue", "green", "red", "purple"), lty = c(1, 2, 2, 2), cex = 0.4)
}
