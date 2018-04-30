library("h2o") #Oxdata for Scalable Machine Learning  
h2o.init() # Initialize cluster (local cluster)
library("dplyr") # For manipulating data
library("lubridate") # Dealing with dates
#install.packages("prophet")
library("prophet") #Facebook TS model

setwd("D:\\Mis\\MPS_Project")

#Read daily scores
scores <- read.csv("final.csv") 
#Date format
scores$final_date <- dmy(scores$final_date)
#Aggregate at daily level
scores <- group_by(scores ,final_date ) %>% 
  summarise(mean(Constraining_final) , 
            mean(Harvard_IV_final) , 
            mean(Interesting_final) , 
            mean(Irr_Verb_final) , 
            mean(Litigious_final) , 
            mean(Modal_final) , 
            mean(Negative_final) , 
            mean(Positive_final) ,
            mean(Superfluous_final) ,
            mean(Uncertainty_final) ,
            mean(score) , "count_new" = n()) %>% data.frame()

#Financial index data  
raw_data <- read.csv("D:\\Mis\\MPS_Project\\sectors.csv" , stringsAsFactors =  FALSE)
#Convert to numeric
raw_data[,2:10] <- apply(raw_data[,2:10] , 2 , as.numeric)
#Remove NA
raw_data <- raw_data[complete.cases(raw_data),]
#Change date to the correct type
raw_data$Date <- dmy(raw_data$Date)

#Joining financial data with scores data
agg_data <- left_join(raw_data , scores , by = c("Date" = "final_date"))
#Remove any NA
agg_data_2 <- agg_data[complete.cases(agg_data),]
agg_data_2$week <- week(agg_data_2$Date)
agg_data_2$year <- year(agg_data_2$Date)


#Clean up column names
names(agg_data_2)[11:21] <- c( "Constraining_final"
                    ,"Harvard_IV_final" , "Interesting_final"
                    ,"Irr_Verb_final" , "Litigious_final"
                    ,"Modal_final" , "Negative_final" , "Positive_final"
                    ,"Superfluous_final" , "Uncertainty_final" , "score")

#Loop through each sector
market <- names(agg_data_2)[2:10]

#This is the syntax of non standard evaluation (SE)
#See here 
#https://datascience.blog.wzb.eu/2016/09/27/dynamic-columnvariable-names-with-dplyr-using-standard-evaluation-functions/

#Main list to hold on the aggregation
#Common varaibles

main <- list('mean(Constraining_final)' , 'mean(Harvard_IV_final)'
             ,'mean(Interesting_final)' , 'mean(Irr_Verb_final)'
             ,'mean(Litigious_final)' , 'mean(Modal_final)'
             ,'mean(Negative_final)' , 'mean(Positive_final)'
             ,'mean(Superfluous_final)' ,'mean(Uncertainty_final)'
             ,'mean(score)' , 'mean(count_new)')

acc_store <- matrix(nrow = length(market) , ncol = 4)
raw_table_acc <- list(data.frame("gbm" , "flag" , "nb"))
store_prediction <- data.frame()

###--------------- LOOP THROUGH EACH MARKET ----------------------
###--------------- LOOP THROUGH EACH MARKET ----------------------

pdf("plots_time_series_all3.pdf")
for(i in 1:length(market)){
  #This column will be dynamic
  start_time <- Sys.time()
  main[[13]] <- paste0("mean(" , market[i],")")
  print("fitting")
  print(main[[13]])
  
  agg_data_3 <- agg_data_2 %>%
    group_by(year , week) %>%
    summarise_(.dots = main)
  
  names(agg_data_3) <- c("year" , "week" , "mean.Constraining_final"
                         ,"mean.Harvard_IV_final" , "mean.Interesting_final"
                         ,"mean.Irr_Verb_final" , "mean.Litigious_final"
                         ,"mean.Modal_final" , "mean.Negative_final"
                         ,"mean.Positive_final" , "mean.Superfluous_final"
                         ,"mean.Uncertainty_final" 
                         ,"mean.score" , "mean.count" , "response")
  
  agg_data_3 <- arrange(agg_data_3 , week, year)
  agg_data_3 <- data.frame(agg_data_3)
  #Create lag
  # 
  lag_var <- list('lag(mean.Constraining_final,1)' , 'lag(mean.Harvard_IV_final,1)'
               ,'lag(mean.Interesting_final,1)' , 'lag(mean.Irr_Verb_final,1)'
               ,'lag(mean.Litigious_final,1)' , 'lag(mean.Modal_final,1)'
               ,'lag(mean.Negative_final,1)' , 'lag(mean.Positive_final,1)'
               ,'lag(mean.Superfluous_final,1)' ,'lag(mean.Uncertainty_final,1)'
               ,'lag(mean.score,1)' , 'lag(mean.count,1)')

    
  agg_data_3 <- agg_data_3 %>%
    mutate_(.dots = lag_var)

    #rename lag 
  names(agg_data_3)[16:27] <- c("mean.Constraining_final.lag" , 
                                "mean.Harvard_IV_final.lag" ,
                                "mean.Interesting_final.lag" ,
                                "mean.Irr_Verb_final.lag" ,
                                "mean.Litigious_final.lag" ,
                                "mean.Modal_final.lag" ,
                                "mean.Negative_final.lag",
                                "mean.Positive_final.lag",
                                "mean.Superfluous_final.lag",
                                "mean.Uncertainty_final.lag",
                                "mean.score.lag",
                                "mean.count.lag") 
  #GET WOW (Week over week change )
  agg_data_3$WOW <- (agg_data_3$response/lag(agg_data_3$response,1))-1
  agg_data_3 <- agg_data_3[complete.cases(agg_data_3) ,]
  agg_data_3 <- filter(agg_data_3 , year!= 2012)
  
  agg_data_3_temp <- filter(agg_data_3 , year <= 2015)
  agg_data_3_temp <- filter(agg_data_3 , year != 2015 | week <= 40)
  
  Q1 <- summary(agg_data_3_temp$WOW)[2] #1st Quantile
  Q3 <- summary(agg_data_3_temp$WOW)[5] #3nf Quantile
  #print(summary(agg_data_3_temp$WOW))

  #new column indicating the type of change
  agg_data_3$flag <- ifelse(agg_data_3$WOW <= Q1 , -1 ,
                            ifelse(agg_data_3$WOW <= Q3 , 0 , 1))
  #print(sum(agg_data_3$WOW >= Q3))
  #print(table(agg_data_3$flag)) #Check whether it's balance or not
  #print(table(head(agg_data_3$flag)))
  
  df <- as.h2o(agg_data_3)
  response <- "flag"
  df[[response]] <- as.factor(df[[response]])

  
  predictors <- setdiff(names(df), c(response ,"WOW" ,"year" , "response"
                                      ))
  
  test <- df[df$year == 2016 | df$year == 2017 ,]
  valid <- df[df$year == 2015 & df$week >= 40 ,]  
 
  train <- df[df$year != 2016 & df$year != 2017  ,]
  train <- train[train$year != 2015 | train$week < 40  ,]
  
  #For random split use this
  # splits <- h2o.splitFrame(
  # data = df, 
  # ratios = c(0.7,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  # destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
  # )
    
  #valid <- splits[[2]]
  #test  <- splits[[3]]

#------------- FITTING MODEL --------------
  gbm_params1 <- list(learn_rate = 0.01,
                      ntrees = c(300 ,700) ,
                      sample_rate = c(0.5  , 0.9 , 1),
                      col_sample_rate = c(0.5,   0.9 , 1))
  
  search_criteria <- list(strategy = "RandomDiscrete", max_runtime_secs = 150 , seed = 1)

  # Train and validate a cartesian grid of GBMs
  gbm_grid1 <- h2o.grid("gbm", x = predictors, y = response,
                        training_frame = train,
                        validation_frame = valid,
                        #ntrees = 2000,
                        seed = 1,
                        hyper_params = gbm_params1,
                        search_criteria = search_criteria)
  
  nb <- h2o.naiveBayes(x = predictors , y = response ,
                 training_frame =  train , laplace =  3)
  # Get the grid results, sorted by validation AUC
  gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                               sort_by = "accuracy",
                               decreasing = TRUE)

  # Grab the top GBM model, chosen by validation AUC
  best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
  
  print("GBM with grid search")
  print(h2o.confusionMatrix(h2o.performance(best_gbm1, newdata = test)))
  
  #base line = nb
  print("Naive Bayes Base Line")
  print(h2o.confusionMatrix(h2o.performance(nb, newdata = test))[4,4])
  
  acc <- t(h2o.confusionMatrix(h2o.performance(best_gbm1, newdata = test))[4])
  acc_store[i,] = acc
  #We also need to store this 
  
  acctable <- cbind("gbm" = as.data.frame(predict(best_gbm1 , test))[1] ,
                    "flag" = as.data.frame(test$flag) , 
                    "nb" = as.data.frame(predict(nb , test))[1])
  names(acctable) <- c("gbm" ,"flag" , "nb")

  raw_table_acc[[paste0(market[i])]] <- acctable 
  
  #GET PREDICTION PROBABILITY TO BE USED in the time series model
  pred_final <- h2o.predict( best_gbm1 , df)
  pred_final <- as.data.frame(pred_final)
  
  df_2 <- select(as.data.frame(df),year,week ,response)
  df_2 <- cbind(df_2 , pred_final)
  
  ### TRAINING TIME SERIES MODEL
  time_series_model <- arrange(df_2 , year, week)
  # As part of the model we 
  names(time_series_model)[3] <- "y"
  ds <- seq(as.Date('2013-01-01'), as.Date('2017-12-31'), by = 'w')
  time_series_model$ds <- ds
  #train <= 2015  
  time_series_model_train <- time_series_model[time_series_model$year <= 2015 , ]
  time_series_model_train$cap <- max(tail(time_series_model_train,90)$y) 
  time_series_model_train$floor <- min(tail(time_series_model_train,90)$y) 
  #test >2015
  time_series_model_future <- time_series_model[time_series_model$year > 2015 , ]
  
  # Fit model
  m <- prophet()
  m <- add_regressor(m, 'p.1')
  m <- add_regressor(m, 'p0')
  m <- add_regressor(m , 'p1')
  
  m['cap'] =  quantile(time_series_model_train$y , 0.75)
  m['floor'] = quantile(time_series_model_train$y , 0.25) 
  m['growth'] = 'logistic'
  #m <- prophet(time_series_model_train, growth = 'logistic')
  m <- fit.prophet(m, time_series_model_train)
  
  
  #Create future data frame
  #Need to change this if we want to evaluate of diff
  future <- make_future_dataframe(m, periods = 105 , 
                                  include_history =  TRUE , 
                                  freq =  'week')
  
  future$p.1 <- time_series_model$p.1
  future$p0 <- time_series_model$p0
  future$p1 <- time_series_model$p1
  future['cap'] =  quantile(time_series_model$y , 0.75)
  future['floor'] = quantile(time_series_model$y , 0.25)
  
  
  #For plotting
  m2 <- prophet()
  m2 <- add_regressor(m2, 'p.1')
  m2 <- add_regressor(m2, 'p0')
  m2 <- add_regressor(m2 , 'p1')
  m2['cap'] = max(time_series_model$y)
  m2['floor'] = min(time_series_model$y)
  m2['growth'] = 'logistic'
  time_series_model$cap <-  max(time_series_model$y)
  time_series_model$floor <-  min(time_series_model$y)
  #m <- prophet(time_series_model_train, growth = 'logistic')
  m2 <- fit.prophet(m2, time_series_model)
  
  forecast <- predict(m , future)
  #plot(m2 , forecast)
  
  
  print(plot(m2 , forecast)) #PRINK PREDICTION
  print(prophet_plot_components(m, forecast)) #PRINT DECOMPOSITION
  
  temp <- select(forecast , ds , yhat , yhat_lower , yhat_upper )
  temp$market <- market[i]
  temp$year <- year(temp$ds)
  temp$week <- week(temp$ds)
  
  temp <- left_join(temp ,time_series_model ,
                                by = c("year" = "year" ,"week" = "week"))
  temp <- select(temp , ds.x , yhat,yhat_lower,
                             yhat_upper , market , year , week ,y)
  store_prediction <- rbind(store_prediction , temp)
  
  
  end_time <- Sys.time()
  
  print("Total time for this loop is")

  print(end_time - start_time )
  }
dev.off()
###--------------- END OF LOOP THROUGH EACH MARKET ----------------------
###--------------- END OF LOOP THROUGH EACH MARKET ----------------------

### Get accuracy
acc_store <- data.frame(acc_store)
names(acc_store) <- c("error_decrease" , "error_neutral","error_increase" , "overall_error")

acc_store$overall_acc <- 1-acc_store$overall_error
acc_store <- cbind(market , acc_store)

# get barplot
library("ggplot2")
ggplot(acc_store, aes(x = market, y = overall_acc)) +
  geom_bar(stat = "identity", width =0.5) +   
  geom_text(aes(label = sprintf("%.2f%%", overall_acc * 100)), 
            vjust = -0.25  , size = 3.4) + 
  ggtitle("Overall Accuracy") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14)) 


pred_final <- h2o.predict( best_gbm1 , df)
pred_final <- as.data.frame(pred_final)

df_2 <- select(as.data.frame(df),year,week)
df_2 <- cbind(df_2 , pred_final)

write.csv(df_2 , "multinomial_model.csv")


#COMPARING GBM TO NB --

gbm_acc <- c()
nb_acc <- c()
namess <- c()
for(i in 2:10){
  temp <- raw_table_acc[[i]]
  namess <- c(namess, names(raw_table_acc)[i])  
  correct <- sum(diag(table(temp$gbm,temp$flag)))
  total <- sum((table(temp$gbm,temp$flag)))
  gbm_acc_temp <- correct/total
  gbm_acc <- c(gbm_acc , gbm_acc_temp)
  
  correct_nb <- sum(diag(table(temp$nb,temp$flag)))
  nb_acc_temp <- correct_nb/total
  nb_acc <- c(nb_acc , nb_acc_temp)
}

all_acc <- data.frame(cbind(namess , gbm_acc , nb_acc) ,stringsAsFactors = FALSE)
all_acc$gbm_acc <- as.numeric(all_acc$gbm_acc)
all_acc$nb_acc <- as.numeric(all_acc$nb_acc)



#GET ACCURACY
head(store_prediction)
store_prediction_2 <- store_prediction

#baseline 
filter(store_prediction_2 , year == 2015 & week >= 50) %>% group_by(market) %>% summarise(mean(y)) %>% data.frame() -> baseline
store_prediction_2 <- left_join(store_prediction_2 , baseline , by = c("market" = "market"))
store_prediction_2 <- filter(store_prediction_2 , year >= 2016)

s <- split(store_prediction_2 , store_prediction_2$market)

model <- sapply(s , function(x){ mean(abs(x$yhat- x$y))})
base <- sapply(s , function(x){ mean(abs(x$mean.y.- x$y))})

cbind(model , base) -> acc_ts


write.csv(store_prediction_2 , "data_for_dashboard.csv")
