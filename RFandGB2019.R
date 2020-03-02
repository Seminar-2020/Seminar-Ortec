##### RANDOM FOREST ##### 
# packages 
library(randomForest)
library(MLmetrics)
library(Metrics)
library(e1071)

#### APPARTMENTS #### 

# Remove the variables with only zeros or only 1's 
# The dataset now contain log(Y) and the only  attributes used for appartments
DF.AP.Train<- DF.dum.ap.train_2[,-c(2,4,10,11,12,16,19,13,23,24)]
DF.NONAP.Train <- DF.dum.nap.train_2[,-c(4,6,7,11,12,24,20,19,18,16)]

set.seed(1234)
RF_app_1 <- randomForest(log(WAARDE)~datescount+WONINGOPP + GARAGE + AGE+
                           BERGING + MONUMENT +  BUURTCODE_08 + BUURTCODE_14 +
                           BUURTCODE_06 + 
                           BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + 
                           BUURTCODE_01,
                           DF.AP.Train,
                           mtry=4, 
                           ntree = 500)

RF_nonap <- randomForest(log(WAARDE) ~ datescount + WONINGOPP + AGE + GARAGE + KAVOPP
                         + WONINGTYPEID_HALF + WONINGTYPEID_VRIJSTAAND + BUURTCODE_04
                         + BUURTCODE_05 + BUURTCODE_06 + BUURTCODE_08 + BUURTCODE_12
                         + BUURTCODE_15, 
                         DF.NONAP.Train,
                         mtry = 3,
                         ntree = 200)

#Evaluations 2019
log.pred.rf.ap.19 <-predict(RF_app_1,DF.ap.19)
log.pred.rf.nonap.19 <- predict(RF_nonap, DF.nap.19)
# Transform the prediction back to normal values
pred.rf.ap.19 <-  exp(log.pred.rf.ap.19)
pred.rf.nap.19 <- exp(log.pred.rf.nonap.19)
predictions19.ap.rf <- cbind(pred.rf.ap.19, DF.ap.19$INDEX)
predictions19.nap.rf <- cbind(pred.rf.nap.19, DF.nap.19$INDEX)
predict19.RF <- rbind(predictions19.ap.rf,predictions19.nap.rf)
predict19.RF <- predict19.RF[order(predict19.RF[,2], decreasing = FALSE),]
predict19.RF <- predict19.RF[,1] #column with predictions



#### GBM #### 
library(xgboost)
library(gbm)
library(MLmetrics)
library(Metrics)

# DATA for GBM 
DF.AP.Train.DATA <-as.matrix(DF.AP.Train[,-1])
DF.NAP.Train.DATA <- as.matrix(DF.NONAP.Train[,-1])
DF.AP.Train.LABEL <- as.matrix(log(DF.AP.Train[,1]))
DF.NAP.Train.LABEL <- as.matrix(log(DF.NONAP.Train[,1]))

set.seed(1234)
xgboost_AP<- xgboost(data = DF.AP.Train.DATA , label =DF.AP.Train.LABEL, nrounds = 800)

set.seed(1234)
xgboost_NAP<- xgboost(data = DF.NAP.Train.DATA , label =DF.NAP.Train.LABEL, nrounds = 800)

#Evaluations 2019
DF.ap.19.gb <-cbind(DF.ap.19$WONINGOPP, DF.ap.19$GARAGE, DF.ap.19$BERGING, DF.ap.19$MONUMENT,
                    DF.ap.19$datescount, DF.ap.19$AGE, DF.ap.19$BUURTCODE_15, DF.ap.19$BUURTCODE_06,
                    DF.ap.19$BUURTCODE_12, DF.ap.19$BUURTCODE_14, DF.ap.19$BUURTCODE_01,
                    DF.ap.19$BUURTCODE_08, DF.ap.19$BUURTCODE_05)
DF.ap.19.gb <- as.data.frame(DF.ap.19.gb)
names(DF.ap.19.gb) <- names(DF.AP.Train[,-1])
DF.nap.19.gb <- cbind(DF.nap.19$KAVOPP, DF.nap.19$WONINGOPP, DF.nap.19$GARAGE, DF.nap.19$datescount,
                      DF.nap.19$AGE, DF.nap.19$WONINGTYPEID_HALF, DF.nap.19$WONINGTYPEID_VRIJSTAAND,
                      DF.nap.19$BUURTCODE_15, DF.nap.19$BUURTCODE_06, DF.nap.19$BUURTCODE_12,
                      DF.nap.19$BUURTCODE_08, DF.nap.19$BUURTCODE_05, DF.nap.19$BUURTCODE_04)
DF.nap.19.gb <- as.data.frame(DF.nap.19.gb)
names(DF.nap.19.gb) <- names(DF.NONAP.Train[,-1])

log.pred.gb.ap.19 <-predict(xgboost_AP, as.matrix(DF.ap.19.gb))
log.pred.gb.nonap.19 <- predict(xgboost_NAP, as.matrix(DF.nap.19.gb))
# Transform the prediction back to normal values
pred.gb.ap.19 <-  exp(log.pred.gb.ap.19)
pred.gb.nap.19 <- exp(log.pred.gb.nonap.19)
predictions19.ap.gb <- cbind(pred.gb.ap.19, DF.ap.19$INDEX)
predictions19.nap.gb <- cbind(pred.gb.nap.19, DF.nap.19$INDEX)
predict19.GB <- rbind(predictions19.ap.gb,predictions19.nap.gb)
predict19.GB <- predict19.GB[order(predict19.GB[,2], decreasing = FALSE),]
predict19.GB <- predict19.GB[,1] #column with predictions

