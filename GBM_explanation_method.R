library(shapper)
install_shap()
library(DALEX)

# Feature instance which will be explored 
HUIS_APP_1 <- DF.AP.Train[3,-1]
HUIS_APP_2 <- DF.AP.Train[3,-1]
HUIS_APP_2$WONINGOPP<- 110 

#### GBM Shapley #### 
pred = function(model, newdata){
  newData_x = xgb.DMatrix(data.matrix(newdata), missing = NA)
  results <- predict(model, newData_x)
}

predictor_gbm = Predictor$new(xxgboost_1, data =DF.AP.Train[,-1], log(DF.AP.Train[,1]), predict.fun = pred)
S_HUIS_1 = Shapley$new(predictor_gbm , x.interest = HUIS_APP_1, sample.size = 1000)
plot(S_HUIS_1)

#### GBM Kernel #### 
explainer_gbm = explain(xxgboost_1, data = DF.AP.Train[,-1], log(DF.AP.Train[,1]), predict_function = pred) 
K_HUIS_1  = individual_variable_effect(explainer_gbm , HUIS_APP_1 , method = "KernelSHAP", nsamples = 100, D=4)
plot(K_HUIS_1)

