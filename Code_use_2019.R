##### RANDOM FOREST ##### 
# packages 
library(randomForest)
library(MLmetrics)
library(Metrics)
library(e1071)

#### APPARTMENTS #### 

# Remove the variables with only zeros or only 1's 
# The dataset now contain log(Y) and the only  attributes used for appartments
# 
DF.AP.Test<- DF.dum.ap.test[,-c(2,4,10,11,12,16,19,13,23,24)]
DF.AP.Train<- DF.dum.ap.train_2[,-c(2,4,10,11,12,16,19,13,23,24)]

set.seed(1234)
RF_app_1 <- randomForest(log(WAARDE)~datescount+WONINGOPP + GARAGE + AGE+
                           BERGING + MONUMENT +  BUURTCODE_08 + BUURTCODE_14 +
                           BUURTCODE_06 + 
                           BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + 
                           BUURTCODE_01,
                         DF.AP.Train,
                         mtry=4, 
                         ntree = 500)

# Different reuslt by running with different seeds for RF_app_1 and RF_app_2
# if a seed is used same results, same predicitions

#Evaluations out of sample 
log.predictions_rf_1 <-predict(RF_app_1,DF.AP.Test)
log.predictions_rf_2 <-predict(RF_app_2,DF.AP.Test)
# Transform the prediciton back to normal values
pred_rf_1 <-  exp(log.predictions_rf_1)
pred_rf_2 <-  exp(log.predictions_rf_2)
results_1<- data.frame(actual =  DF.AP.Test[,1], prediction =pred_rf_1 )
results_2<- data.frame(actual =  DF.AP.Test[,1], prediction =pred_rf_2)

# difference rf_1, rf_2
head(results_1)
head(results_2)

# Metrics 
errors = abs(pred_rf_1- DF.AP.Test[,1])
mean(errors)
mape = 100 * (errors /   DF.AP.Test[,1])
accuracy = 100 - mean(mape)
MAPE(pred_rf_1,DF.AP.Test[,1])
MAE(pred_rf_1,DF.DF.AP.Test[,1])
mse(DF.AP.Test[,1],pred_rf_1)

# Evaluations in sample 
# in-sample predictions
log.predictions_rf_1_in <-predict(RF_app_1,DF.AP.Train)
pred_rf_1_in <-  exp(log.predictions_rf_1_in)
# Metrics
errors = abs(pred_rf_1_in- DF.AP.Train[,1])
mean(errors)
mape = 100 * (errors /  DF.AP.Train[,1])
accuracy = 100 - mean(mape)
MAPE(pred_rf_1_in , DF.AP.Train[,1])
MAE(pred_rf_1_in , DF.AP.Train[,1])
mse( DF.AP.Train[,1],pred_rf_1_in )

#### NON APPARTMENTS #### 
# Remove the variables with only zeros or only 1's 
# The dataset now contain log(Y) and the only  attributes used for appartments
# 
DF.NAP.Test<- DF.dum.nap.test[,-c(4,6,7,11,12,16,18,19,20,24)]
DF.NAP.Train<- DF.dum.nap.train_2[,-c(4,6,7,11,12,16,18,19,20,24)]

set.seed(1234)
RF_non_app <- randomForest(log(WAARDE) ~datescount  + KAVOPP + WONINGOPP + GARAGE  + 
                             AGE+ +WONINGTYPEID_HALF + WONINGTYPEID_VRIJSTAAND+ BUURTCODE_04 + 
                             BUURTCODE_08 +
                             BUURTCODE_06 + BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 
                           ,DF.dum.nap.train_2)


# Global feature attribution
varImpPlot(RF_non_app)
#Evaluations out of sample 
log.predictions_rf_NA <-predict(RF_non_app,DF.NAP.Test[,-1])
# Transform the prediciton back to normal values
pred_rf_NA <-  exp(log.predictions_rf_NA)
results_1<- data.frame(actual =  DF.NAP.Test[,1], prediction = pred_rf_NA)
head(results_1)

# Metrics 
errors = abs(pred_rf_NA - DF.NAP.Test[,1])
mean(errors)
median(errors)
mape = 100 * (errors /   DF.NAP.Test[,1])
accuracy = 100 - mean(mape)
MAPE(pred_rf_NA,DF.NAP.Test[,1])

# Evaluations in sample 
# in-sample predictions
log.predictions_rf_NA_in <-predict(RF_non_app,DF.NAP.Train[,-1])
pred_rf_NA_in <-  exp(log.predictions_rf_NA_in)
# Metrics
errors = abs(pred_rf_NA_in- DF.NAP.Train[,1])
mean(errors)
median(errors)
MAPE(pred_rf_NA_in , DF.NAP.Train[,1])

#### GBM #### 
library(xgboost)
library(gbm)
library(MLmetrics)
library(Metrics)
#### ARPPARMENTS  ####
# DATA for GBM 
DF.AP.Train.DATA <-as.matrix(DF.AP.Train[,-1])
DF.AP.Test.DATA <- as.matrix(DF.AP.Test[,-1])
DF.AP.Train.LABEL <-as.matrix(log(DF.AP.Train[,1]))

set.seed(1234)
xxgboost_1<- xgboost( data = DF.AP.Train.DATA , label =DF.AP.Train.LABEL, nrounds = 800
)

set.seed(1234)
xgboost_2<- xgboost( data = DF.AP.Train.DATA , label =DF.AP.Train.LABEL, nrounds = 800
)

# same results indepent of the seed 
#Evaluations out of sample
log.predict_1 = predict(xgboost_1, newdata = DF.AP.Test.DATA)
log.predict_2= predict(xgboost_1, newdata = DF.AP.Test.DATA)
pred_gbm_1 <- exp(log.predict_1 )
pred_gbm_2 <-  exp(log.predict_2)
results_1<- data.frame(actual =  DF.AP.Test[,1], prediction =pred_gbm_1)
results_2<- data.frame(actual =  DF.AP.Test[,1], prediction =pred_gbm_2)

head(results_1)
head(results_2)

MAPE(pred_gbm_1, DF.AP.Test[,1])
MAE(pred_gbm_1, DF.AP.Test[,1])
errors = abs(pred_gbm_1 - DF.AP.Test[,1])
mean(errors)
mape = 100 * (errors /   DF.AP.Test[,1])
accuracy = 100 - mean(mape)

# Evaluationsin sample in 
log.predict_1_in= predict(xgboost_1, newdata = DF.AP.Train.DATA)
pred_gbm_1_in <- exp(log.predict_1_in)
results_in<- data.frame(actual =  DF.AP.Train[,1], prediction =pred_gbm_1_in)

MAPE(pred_gbm_1_in, DF.AP.Train[,1])
MAPE(pred_gbm_1_in, DF.AP.Train[,1])
errors = abs(pred_gbm_1_in  - DF.AP.Train[,1])
mean(errors)
mape = 100 * (errors /  DF.AP.Train[,1])
accuracy = 100 - mean(mape)

#### NON APPARTMENTS ####
DF.NAP.Train.DATA <-as.matrix(DF.NAP.Train[,-1])
DF.NAP.Test.DATA <- as.matrix(DF.NAP.Test[,-1])
DF.NAP.Train.LABEL <-as.matrix(log(DF.NAP.Train[,1]))

set.seed(1234)
xxgboost_NA<- xgboost( data = DF.NAP.Train.DATA , label =DF.NAP.Train.LABEL, nrounds = 800
)

#Evaluations out of sample
log.predict_1_NA = predict(xxgboost_NA, newdata = DF.NAP.Test.DATA)
pred_gbm_NA <- exp(log.predict_1_NA )
results_NA<- data.frame(actual =  DF.NAP.Test[,1], prediction = pred_gbm_NA)
head(results_NA)

MAPE(pred_gbm_NA, DF.NAP.Test[,1])
errors = abs(pred_gbm_NA - DF.NAP.Test[,1])
mean(errors)
median(errors)
mape = 100 * (errors /   DF.NAP.Test[,1])
accuracy = 100 - mean(mape)

# Evaluationsin sample in 
log.predict_Na_in= predict(xxgboost_NA , newdata = DF.NAP.Train.DATA)
pred_gbm_NA_in <- exp(log.predict_Na_in)
results_in<- data.frame(actual =  DF.NAP.Train[,1], prediction =pred_gbm_NA_in )
head(results_in)

MAPE(pred_gbm_NA_in, DF.NAP.Train[,1])
errors = abs(pred_gbm_NA_in- DF.NAP.Train[,1])
median(errors)


