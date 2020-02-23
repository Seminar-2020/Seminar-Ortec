#### Gradient Boosting Machines  ####
# Appartments 
gmb_param <- list(
  ntrees = rdunif(10, min=100, max = 10000), 
  learn_rate = seq(0.001,1,by =0.1)
)

gmb_grid<- h2o.grid("xgboost",
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,"BERGING",  "MONUMENT", "BUURTCODE_08",
                          "BUURTCODE_14","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15", "BUURTCODE_01") ,
                    y = 1 , 
                    training_frame = train.ap,
                    grid_id = "grid_xgboos",
                    nfolds = 5, 
                    hyper_params = gmb_param,
                    search_criteria = search_criteria
)



#dataset for GBM
DF.dum.ap.train_xgboost_data <-as.matrix( DF.dum.ap.train[,-c(1,3,11,14,18)])
DF.dum.ap.train_xgboost_label <-as.matrix( DF.dum.ap.train[,-c(3,11,14,18)])
DF.dum.ap.test_xgboost <-as.matrix( DF.dum.ap.testx[,-c(3,11,14,18)])

xgboost_R <- xgboost( data = DF.dum.ap.train_xgboost_data  , label =DF.dum.ap.train_xgboost_label[,1],
                      nrounds=800,
                      eta = 0.23,
                      max_depth =6
               )

#Evaluations
predicted = predict(xgboost_R, newdata = DF.dum.ap.test_xgboost[,-1])
MAPE(predicted, DF.dum.ap.test[,1])
MAE(predicted, DF.dum.ap.test[,1])
mse(DF.dum.ap.test[,1],predicted)
errors = abs(predicted  - DF.dum.ap.testx[,1])
mean(errors)
mape = 100 * (errors /  DF.dum.ap.testx[,1])
accuracy = 100 - mean(mape)


# non-appartments
test.nap <- as.h2o(DF.dum.nap.testx)
train.nap <- as.h2o(DF.dum.nap.train)

gmb_param <- list(
  ntrees = rdunif(10, min=100, max = 10000), 
  learn_rate = seq(0.001,1,by =0.1)
)


gmb_grid_nap<- h2o.grid("xgboost",
                    x = c("datescount","WONINGOPP","GARAGE","AGE"
                          ,  "WONINGTYPEID_HALF", "WONINGTYPEID_VRIJSTAAND",
                          "BUURTCODE_04","BUURTCODE_06" , "BUURTCODE_05" ,
                          "BUURTCODE_12" , "BUURTCODE_15","KAVOPP") ,
                    y = 1 , 
                    training_frame = train.nap,
                    grid_id = "grid_xgboost_nap",
                    nfolds = 5, 
                    hyper_params = gmb_param,
                    search_criteria = search_criteria
)

#dataset for GBM
DF.dum.nap.train_xgboost_data <-as.matrix( DF.dum.nap.train[,-c(1,4,6,7,15,17,12,20,19,18)])
DF.dum.nap.train_xgboost_label <-as.matrix( DF.dum.nap.train[,-c(4,6,7,15,17,12,20,19,18)])
DF.dum.nap.test_xgboost <-as.matrix( DF.dum.nap.testx[,-c(4,6,7,15,17,12,20,19,18)])

xgboost_nap_r <- xgboost( data = DF.dum.nap.train_xgboost_data  , label =DF.dum.nap.train_xgboost_label[,1],
                     nrounds=800,
                     eta = 0.023,
                     max_depth =9
)

#Evaluations
predicted = predict(xgboost_nap_r , newdata = DF.dum.nap.test_xgboost[,-1])
MAPE(predicted, DF.dum.nap.test[,1])
MAE(predicted, DF.dum.nap.test[,1])
mse(DF.dum.nap.test[,1],predicted)
errors = abs(predicted  - DF.dum.nap.testx[,1])
mean(errors)
mape = 100 * (errors /  DF.dum.nap.testx[,1])
accuracy = 100 - mean(mape)