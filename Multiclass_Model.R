library("h2o") #Oxdata for Scalable Machine Learning  
h2o.init() # Initialize cluster (local cluster)

#To downloan oxdata call the below command

library("dplyr") # For manipulating data
library("lubridate") # Dealing with dates
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

###--------------- LOOP THROUGH EACH MARKET ----------------------
###--------------- LOOP THROUGH EACH MARKET ----------------------

for(i in 1:length(market)){
  #This column will be dynamic
  
  main[[13]] <- paste0("mean(" , market[i],")")
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

  # 
  # lag_var <- list('lag(mean.Constraining_final,52)' , 'lag(mean.Harvard_IV_final,52)'
  #                 ,'lag(mean.Interesting_final,52)' , 'lag(mean.Irr_Verb_final,52)'
  #                 ,'lag(mean.Litigious_final,52)' , 'lag(mean.Modal_final,52)'
  #                 ,'lag(mean.Negative_final,52)' , 'lag(mean.Positive_final,52)'
  #                 ,'lag(mean.Superfluous_final,52)' ,'lag(mean.Uncertainty_final,52)'
  #                 ,'lag(mean.score,52)' , 'lag(mean.count,52)')
  
    
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
  #GET WOW (Week over week change)
  agg_data_3$WOW <- (agg_data_3$response/lag(agg_data_3$response,1))-1
  agg_data_3 <- agg_data_3[complete.cases(agg_data_3) ,]
  
  Q1 <- summary(agg_data_3$WOW)[2] #1st Quantile
  Q3 <- summary(agg_data_3$WOW)[5] #3nf Quantile
  print(summary(agg_data_3$WOW))

#new column indicating the type of change
  agg_data_3$flag <- ifelse(agg_data_3$WOW <= Q1 , -1 ,
                            ifelse(agg_data_3$WOW <= Q3 , 0 , 1))
  print(sum(agg_data_3$WOW >= Q3))
  print(table(agg_data_3$flag)) #Check whether it's balance or not
  #print(table(head(agg_data_3$flag)))
  df <- as.h2o(agg_data_3)
  response <- "flag"
  df[[response]] <- as.factor(df[[response]])

  # predictors <- setdiff(names(df), c(response, "week" ,"WOW" ,"year" , "response"
  #                                    ,"mean.Constraining_final.lag"
  #                                    ,"mean.Harvard_IV_final.lag"
  #                                    ,"mean.Irr_Verb_final.lag"
  #                                    ,"mean.Litigious_final.lag"
  #                                    ,"mean.Modal_final.lag",
  #                                    "mean.Negative_final.lag",
  #                                    "mean.Positive_final.lag"
  #                                    ,"mean.Superfluous_final.lag"
  #                                    ,"mean.Uncertainty_final.lag"
  #                                    ,"mean.score.lag"))

   predictors <- setdiff(names(df), c(response, "week" ,"WOW" ,"year" , "response"
                                      ))
  splits <- h2o.splitFrame(
  data = df, 
  ratios = c(0.7,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
  )
  
  train <- splits[[1]]
  valid <- splits[[2]]
  test  <- splits[[3]]

  gbm <- h2o.gbm(x = predictors, 
                 y = response, 
                 training_frame = train , 
                 validation_frame =  valid , 
                  ntrees = 1000,
                 learn_rate=0.01,
                 col_sample_rate  = 0.8 ,
                 sample_rate = 0.8
              )  #FOR FINDING BEST PARAM
  

  
  print(h2o.confusionMatrix(h2o.performance(gbm, newdata = test)))
  
  acc <- t(h2o.confusionMatrix(h2o.performance(gbm, newdata = test))[4])
  acc_store[i,] = acc

  }
###--------------- END OF LOOP THROUGH EACH MARKET ----------------------
###--------------- END OF LOOP THROUGH EACH MARKET ----------------------


### Get accuracy
acc_store <- data.frame(acc_store)
names(acc_store) <- c("error_decrease" , "error_neutral","error_increase" , "overall_error")

acc_store$overall_acc <- 1-acc_store$overall_error
acc_store <- cbind(market , acc_store)

# 
# barplot(acc_store$overall_acc , main = "Overall accuracy"
#         ,names.arg =  acc_store$market)

# get barplot
library("ggplot2")
ggplot(acc_store, aes(x = market, y = overall_acc)) +
  geom_bar(stat = "identity", width =0.5) +   
  geom_text(aes(label = sprintf("%.2f%%", overall_acc * 100)), 
            vjust = -0.25  , size = 3.4) + 
  ggtitle("Overall Accuracy") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14)) 