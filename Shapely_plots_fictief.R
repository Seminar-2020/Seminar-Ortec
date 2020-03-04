# Libraries
library(shapper)
install_shap()
library(DALEX)

##### SHAPLEY PLOTS FICTIEF #### 
# Appartments 
HUIS_APP_1 <- DF.AP.Train[3,-1]
HUIS_APP_2 <- DF.AP.Train[3,-1]
HUIS_APP_2$WONINGOPP<- 110 

##### RANDOM FOREST Shapley sampling #####
predictor_ap = Predictor$new(RF_app_1, data =  as.data.frame(DF.AP.Train[,-1]))
S_HUIS_1 = Shapley$new(predictor_ap, x.interest = HUIS_APP_1, sample.size = 1000)
S_HUIS_2 = Shapley$new(predictor_ap, x.interest = HUIS_APP_2, sample.size = 1000)
S_HUIS_2
S_HUIS_1
plot(S_HUIS_1)
plot(S_HUIS_2)

#### RANDOM FOREST Kernel shap #### 
# Use individual_variable_effect from shapper
Explainer_RF <- explain(RF_app_1, data = as.data.frame(DF.AP.Train[,-1]), log(DF.AP.Train[,1]) )
K_HUIS_1  = individual_variable_effect(Explainer_RF, HUIS_APP_1 , method = "KernelSHAP", nsamples = 100, D=4)
plot(K_HUIS_1)

#### GBM Shapley sampling #### 
pred = function(model, newdata){
  newData_x = xgb.DMatrix(data.matrix(newdata), missing = NA)
  results <- predict(model, newData_x)
}

predictor_gbm = Predictor$new(xxgboost_1, data =DF.AP.Train[,-1], log(DF.AP.Train[,1]), predict.fun = pred)
S_HUIS_1 = Shapley$new(predictor_gbm , x.interest = HUIS_APP_1, sample.size = 1000)
plot(S_HUIS_1)

#### GBM Kernelshap  #### 
explainer_gbm = explain(xxgboost_1, data = DF.AP.Train[,-1], log(DF.AP.Train[,1]), predict_function = pred) 
K_HUIS_1  = individual_variable_effect(explainer_gbm , HUIS_APP_1 , method = "KernelSHAP", nsamples = 100, D=4)
plot(K_HUIS_1)




