##### OLS #### 
#### APPARTMENTS ####
lm_ap_fin = lm(log(WAARDE)~ datescount+WONINGOPP+I(WONINGOPP^2) + GARAGE + BERGING + MONUMENT + AGE+ I(AGE^2)+ BUURTCODE_08 + BUURTCODE_14 + BUURTCODE_06 + BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + BUURTCODE_01 , DF.dum.ap.train_2)
log.pred_ols <- predict(lm_ap_fin,DF.AP.Test)
pred_ols <- exp(log.pred_ols)

# Metrics  out - sample
errors = abs(pred_ols - DF.AP.Test[,1])
median(errors)
Mape = 100 * (errors /   DF.AP.Test[,1])
median(Mape)
mean(Mape)

# Metrics in  - sample
log.pred_ols_in<- predict(lm_ap_fin,DF.AP.Train)
pred_ols_in <- exp(log.pred_ols_in)
errors = abs(pred_ols_in - DF.AP.Train[,1])
median(errors)
Mape = 100 * (errors /   DF.AP.Train[,1])
median(Mape)
mean(Mape)

#### NON APPARTMENTS ####
lm_nap_final = lm(log(WAARDE) ~ datescount+WONINGOPP +I(WONINGOPP^2) + GARAGE  + AGE+ I(AGE^2)+WONINGTYPEID_HALF + WONINGTYPEID_VRIJSTAAND+ BUURTCODE_04 + BUURTCODE_08 + BUURTCODE_06 + BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + KAVOPP + I(KAVOPP^2), DF.dum.nap.train_2)
log.pred_ols_NA <- predict(lm_nap_final,DF.NAP.Test)
pred_ols_NA <- exp(log.pred_ols_NA)

# Metrics  out - sample
errors = abs(pred_ols_NA - DF.NAP.Test[,1])
median(errors)
Mape = 100 * (errors /   DF.NAP.Test[,1])
median(Mape)
mean(Mape)

# Metrics in  - sample
log.pred_ols_in_NA<- predict(lm_nap_final,DF.NAP.Train)
pred_ols_in_NA <- exp(log.pred_ols_in_NA)
errors = abs(pred_ols_in_NA - DF.NAP.Train[,1])
median(errors)
Mape = 100 * (errors /   DF.NAP.Train[,1])
median(Mape)
mean(Mape)

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
                         DF.AP.Train)

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
median(errors)
Mape = 100 * (errors /   DF.AP.Test[,1])
median(Mape)
mean(Mape)
MDAPE(DF.AP.Test[,1],pred_rf_1)
accuracy = 100 - median(Mape)


# Evaluations in sample 
# in-sample predictions
log.predictions_rf_1_in <-predict(RF_app_1,DF.AP.Train)
pred_rf_1_in <-  exp(log.predictions_rf_1_in)
# Metrics
errors = abs(pred_rf_1_in - DF.AP.Train[,1])
median(errors)
Mape = 100 * (errors /   DF.AP.Train[,1])
median(Mape)
mean(Mape)
MDAPE(DF.AP.Train[,1],pred_rf_1_in)
100 - median(Mape)

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
errors = abs(pred_rf_NA- DF.NAP.Test[,1])
median(errors)
Mape = 100 * (errors /   DF.NAP.Test[,1])
median(Mape)
mean(Mape)
MDAPE(DF.NAP.Test[,1],pred_rf_NA)
100 - median(Mape)

# Evaluations in sample 
# in-sample predictions
log.predictions_rf_NA_in <-predict(RF_non_app,DF.NAP.Train[,-1])
pred_rf_NA_in <-  exp(log.predictions_rf_NA_in)
# Metrics
errors = abs(pred_rf_NA_in- DF.NAP.Train[,1])
median(errors)
Mape = 100 * (errors /   DF.NAP.Train[,1])
median(Mape)
mean(Mape)

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
results_1_GBM <- data.frame(actual =  DF.AP.Test[,1], prediction.GBM =pred_gbm_1 )
results_2_GBM<- data.frame(actual =  DF.AP.Test[,1], prediction =pred_gbm_2)

head(results_1_GBM)
head(results_2_GBM)
# Metrics 
errors = abs(pred_gbm_1- DF.AP.Test[,1])
median(errors)
Mape = 100 * (errors /   DF.AP.Test[,1])
median(Mape)
mean(Mape)
accuracy = 100 - median(Mape)
# Evaluationsin sample in 
log.predict_1_in= predict(xgboost_1, newdata = DF.AP.Train.DATA)
pred_gbm_1_in <- exp(log.predict_1_in)
results_in<- data.frame(actual =  DF.AP.Train[,1], prediction =pred_gbm_1_in)

errors = abs(pred_gbm_1_in- DF.AP.Train[,1])
median(errors)
Mape = 100 * (errors /  DF.AP.Train[,1])
median(Mape)
mean(Mape)
accuracy = 100 - median(Mape)

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

errors = abs(pred_gbm_NA- DF.NAP.Test[,1])
median(errors)
Mape = 100 * (errors /  DF.NAP.Test[,1])
median(Mape)
mean(Mape)
accuracy = 100 - median(Mape)


# Evaluationsin sample in 
log.predict_Na_in= predict(xxgboost_NA , newdata = DF.NAP.Train.DATA)
pred_gbm_NA_in <- exp(log.predict_Na_in)
results_in<- data.frame(actual =  DF.NAP.Train[,1], prediction =pred_gbm_NA_in )
head(results_in)
errors = abs(pred_gbm_NA_in- DF.NAP.Train[,1])
median(errors)
Mape = 100 * (errors /  DF.NAP.Train[,1])
median(Mape)
mean(Mape)
accuracy = 100 - median(Mape)



#### OVERVIEW #### 
# out of sample t
results_OLS_GBM_RF_AP <- data.frame(actual =  DF.AP.Test[,1], prediction.OLS = pred_ols ,prediction.GBM =pred_gbm_1, prediction.RF = pred_rf_1  )
head(results_OLS_GBM_RF_AP)
diff <-abs(pred_gmb_1 - pred_rf_1)
summary(diff)
head(results_1_GBM_RF_AP )
results_OLS_GBM_RF_NAP <- data.frame(actual =  DF.NAP.Test[,1],prediction.OLS =pred_ols_NA,  prediction.GBM =pred_gbm_NA, prediction.RF =pred_rf_NA  )
head(results_OLS_GBM_RF_NAP)
diff.NA <- abs(pred_gbm_NA -pred_rf_NA )
summary(diff.NA)

