#### BUNK APPARTMENTS ####
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
#h20
library(h2o)
h2o.init(max_mem_size = "6g")
h2o.removeAll(timeout_secs = 0)
#explain
library(DALEX)
library(IML)

#### RANDOM FOREST ####
#RF - h20
# Remove the variables with only zeros or only 1's 
DF.dum.ap.test <- DF.dum.ap.test[,-c(2,10,11,12,13,24)]
DF.dum.ap.train<- DF.dum.ap.train_2[,-c(2,10,11,12,13,24)]


#tranfrom the dataset into an h2o enivorement 
test.ap <- as.h2o(DF.dum.ap.test)
train.ap <- as.h2o(DF.dum.ap.train)

# Grid search
search_criteria = list(
  strategy = "RandomDiscrete",
  max_runtime_secs=300,
  max_models = 10 
)

rf_param <- list(
                 max_depth = c(20,25,28,30),
                 sample_rate = c(0.7,0.8, 1.0),
                 mtries = c(3,4,5,6,7,8,9),
                 ntrees = c(100,200,150))

rf_grid <- h2o.grid("randomForest",
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                          "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") , y = 1 , 
                    training_frame = train.ap,
                    grid_id = "grid_rf_4",
                    nfolds = 10, 
                    seed = 1234,
                    hyper_params = rf_param,
                    search_criteria = search_criteria,
                    keep_cross_validation_predictions = T
                    
)

#collect the results of the grid search 
rf_rid<-h2o.getGrid("grid_rf_4", decreasing = F)
rf_rid
rf_grid@summary_table[1,]
best_model <- h2o.getModel(rf_grid@model_ids[[1]])
h2o.performance(best_model)

predictions_grid <- h2o.predict(best_model, test.ap[,-1])
MAPE(predictions_grid, test.ap[,1])
MAE(predictions_grid, test.ap[,1])
mse(test.ap[,1],predictions_grid)

# Final RF
rf_grid <- h2o.randomForest(
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                          "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") , y = 1 , 
                    training_frame = train.ap,
                    model_id = "rf_4",
                    nfolds = 10, 
                    seed = 1234,
                    keep_cross_validation_predictions = T,
                    ntrees=96 ,
                    mtries=7,
                    max_depth=25,
                    sample_rate =0.7
                    
)

# final model 
predictions_rf<- h2o.predict(rf_grid, test.ap[,-1])
MAPE(predictions_rf, test.ap[,1])
MAE(predictions_rf, test.ap[,1])
mse(test.ap[,1],predictions_rf)


#### Gradient Boosting Machines  ####
# Grid Search
gmb_param <- list(
  ntrees = c(100,200,500,800),
  max_depth = c(2,6,8,9),
  learn_rate = c(0.023,0.01,0.020)
)
gmb_grid<- h2o.grid("xgboost",
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                          "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") , y = 1 , 
                    training_frame = train.ap,
                    grid_id = "grid_xgboost_2",
                    nfolds = 10, 
                    seed = 1234,
                    hyper_params = gmb_param,
                    search_criteria = search_criteria
)

#collect the results of the grid search 
gbm_grid<-h2o.getGrid("grid_xgboost_2")
gbm_grid
gbm_grid@summary_table[1,]
best_model_gbm <- h2o.getModel(gbm_grid@model_ids[[1]])
h2o.performance(best_model_gbm)

predictions_grid_gbm <- h2o.predict(best_model_gbm, test.ap[,-1])
MAPE(predictions_grid_gbm, test.ap[,1])
MAE(predictions_grid_gbm, test.ap[,1])
mse(test.ap[,1],predictions_grid_gbm)

#final model GBM
gmb_final<- h2o.xgboost(
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                          "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") ,
                    y = 1 , 
                    training_frame = train.ap,
                    model_id="final_xgboost_2",
                    nfolds = 10, 
                    seed = 1234,
                    learn_rate= 0.02,
                    ntrees=800,
                    max_depth=6
                  
)

# Final model 
predictions_final_gbm <- h2o.predict(gmb_final, test.ap[,-1])
MAPE(predictions_final_gbm, test.ap[,1])
MAE(predictions_final_gbm, test.ap[,1])
mse(test.ap[,1],predictions_final_gbm)

# Shap values for GBM
h2o.predict_contributions(gmb_final, test.ap[,-1])

imp.gbm <- FeatureImp$new(gmb_final, loss="mse")


#### Neural Network ####
# GridSearch
dnn_params <- list(
  activation =c("Rectifier","RectifierWithDropout"),
  hidden = list(c(17,17),c(16,16), c(15,15),c(10,10)),
  rho = c( 0.991, 0.992,0.993,0.994),
  epsilon = c(1e-10, 1e-8, 1e-6)
)

dl_grid <- h2o.grid(algorithm = "deeplearning", 
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                          "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") , y = 1 , 
                    training_frame = train.ap,
                    grid_id = "dl_grid_5",
                    nfolds = 10,                           
                    hyper_params = dnn_params,
                    search_criteria = search_criteria,
                    seed = 1234,
                    epochs = 50
)

grid<-h2o.getGrid("dl_grid_5", decreasing = F)
grid
grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]])
predictions_grid <- h2o.predict(best_model, test.ap[,-1])
MAE(predictions_grid, test.ap[,1])
MAPE(predictions_grid, test.ap[,1])

#final model
dnn_h20_ReLU<- h2o.deeplearning(
  x = c("datescount","WONINGOPP","GARAGE","AGE"
        ,"BERGING",  "MONUMENT", "BUURTCODE_08",
        "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
        "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") , y = 1 , 
  training_frame = train.ap,
  model_id = "dnn",
  nfolds = 10,
  hidden = c(16,16), 
  epochs = 50,
  rho = 0.992,
  epsilon = 10e-8,
  activation = "Rectifier",
  seed = 1234)

h2o.performance(dnn_h20_ReLU)

# RelU (17,17), rho = 0.992 MAPE : 0.0131714
# RelU (16,16), rho = 0.992 MAPE : 0.01304838
predictions_Relu <- h2o.predict(dnn_h20_ReLU,test.ap[,-1])
MAE(predictions_Relu,test.ap[,1])
MAPE(predictions_Relu,test.ap[,1])
mse(test.ap[,1],predictions_Relu)

