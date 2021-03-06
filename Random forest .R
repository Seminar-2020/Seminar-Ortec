## PACKAGES #### 
library(randomForest)
library(MLmetrics)
library(ggplot2)
library(xgboost)
library(gbm)
library(Metrics)
library(neuralnet)
library(caret) #CV
library(lattice)
library (ggplot2)
library(e1071)
library(ranger)
library(keras)
library(h2o)
h2o.init(max_mem_size = "6g")
h2o.removeAll(timeout_secs = 0)
h2o.removeAll()
h2o.ls()
h2o.rm()
h2o.shutdown(
# Random forest appartments
# Remove the variables with only zeros or only 1's 
DF.dum.ap.testx<- DF.dum.ap.test[,-c(2,10,11,12,13,24)]
DF.dum.ap.train<- DF.dum.ap.train_2[,-c(2,10,11,12,13,24)]

#h2o
test.ap <- as.h2o(DF.dum.ap.testx)
train.ap <- as.h2o(DF.dum.ap.train)
list(strategy = "RandomDiscrete", 
     stopping_metric = "mae", 
     stopping_tolerance = 0.0005, stopping_rounds = 5)


RF_params <- list(
  mtries = seq(3,10),
  ntrees = c(100,150,200,250)
  )

rf_grid <- h2o.grid(algorithm = "randomForest", 
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                          "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") , y = 1 , 
                    training_frame = train.ap,
                    grid_id = "rf_grid",
                    nfolds = 5,                           
                    hyper_params = RF_params,
                    search_criteria = search_criteria
                    )
aml <- h2o.automl(      x = c("datescount","WONINGOPP","GARAGE","AGE"
                              ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                              "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                              "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") , y = 1 , 
                        training_frame = train.ap,
                  max_models = 20,
                  seed = 1)
h2o.getGrid("rf_grid", decreasing = F)
rf_model <- h2o.getModel(grid@model_ids[[1]])
predictions_rf <- h2o.predict(rf_model, test.ap[,-1])

RF_app <- randomForest(WAARDE~datescount+WONINGOPP + GARAGE + AGE+
                         BERGING + MONUMENT +  BUURTCODE_08 + BUURTCODE_14 +
                         BUURTCODE_06 + 
                         BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + 
                         BUURTCODE_01 
                       ,DF.dum.ap.train_2,
                       mtry=4, 
                       ntree = 200)
varImpPlot(RF_app)

#Evaluations out of sample 
predictions_rf <-predict(RF_app, DF.dum.ap.testx[,-1])
errors = abs(predictions_rf  - DF.dum.ap.testx[,1])
mean(errors)
mape = 100 * (errors /  DF.dum.ap.testx[,1])
accuracy = 100 - mean(mape)
MAPE(predictions_rf,DF.dum.ap.testx[,1])
MAE(predictions_rf,DF.dum.ap.testx[,1])
mse(DF.dum.ap.testx[,1],predictions_rf)

#Evaluations in sample 
predictions_rf_in<-predict(RF_app, DF.dum.ap.train[,-1])
errors = abs(predictions_rf_in  - DF.dum.ap.train[,1])
mean(errors)
mape = 100 * (errors /  DF.dum.ap.train[,1])
accuracy = 100 - mean(mape)
MAPE(predictions_rf_in,DF.dum.ap.train[,1])
MAE(predictions_rf_in,DF.dum.ap.train[,1])
mse(DF.dum.ap.testx[,1],predictions_rf)

# Random forest non- appartments
# Remove the variables with only zeros or only 1's 
DF.dum.nap.testx<- DF.dum.nap.test[,-c(12,24)]
DF.dum.nap.train<- DF.dum.nap.train_2[,-c(12,24)]

#h2o
test.nap <- as.h2o(DF.dum.nap.testx)
train.nap <- as.h2o(DF.dum.nap.train)

RF_params_nap<- list(
  mtries = seq(3,12),
  ntrees = c(100,200,250)
)

rf_grid_nap<- h2o.grid(algorithm = "randomForest", 
                       x = c("datescount","WONINGOPP","GARAGE","AGE"
                             ,  "WONINGTYPEID_HALF", "WONINGTYPEID_VRIJSTAAND",
                             "BUURTCODE_04","BUURTCODE_06" , "BUURTCODE_05" ,
                             "BUURTCODE_12" , "BUURTCODE_15","KAVOPP") ,  
                       y = 1,
                    training_frame = train.nap,
                    grid_id = "rf_grid_nap",
                    nfolds = 5,                           
                    hyper_params = RF_params,
                    search_criteria = search_criteria
                    
)

h2o.getGrid("rf_grid_nap", decreasing = F)

RF_non_app <- randomForest(WAARDE ~datescount+WONINGOPP + GARAGE  + 
                             AGE+ +WONINGTYPEID_HALF + WONINGTYPEID_VRIJSTAAND+ BUURTCODE_04 + 
                             BUURTCODE_08 +
                             BUURTCODE_06 + BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + KAVOPP 
                            ,DF.dum.nap.train_2,
                            mtry=3, 
                            ntree = 200)
varImpPlot(RF_non_app)

#Evaluations
predictions_rf <-predict(RF_non_app, DF.dum.nap.testx[,-1])
errors = abs(predictions_rf  - DF.dum.nap.testx[,1])
mean(errors)
mape = 100 * (errors /  DF.dum.nap.testx[,1])
accuracy = 100 - mean(mape)
#in sample
predictions_rf <-predict(RF_non_app, DF.dum.nap.train[,-1])
MAPE(predictions_rf,DF.dum.nap.train[,1])
MAE(predictions_rf,DF.dum.nap.train[,1])
mse(DF.dum.nap.testx[,1],predictions_rf)

  

