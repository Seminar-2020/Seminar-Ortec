# Make sure that DF.NAP.Train, DF.NAP.Test, DF.AP.Train, DF.AP.Test are loaded
#### Packages ####
library(xgboost)
library(iml) # for Shapley Sampling
library(shapper) # for Kernel SHAP, python needs to be installed (shapper::install_shap() # make sure you've ran this)
library(DALEX) # needs to be installed for the explainer object of shapper/individual_variable_effect

#### Appartments ####
set.seed(1234)
# xgboost needs matrices as input
xgboost_AP <- xgboost(data = as.matrix(DF.AP.Train[,-c(1,2)]), label =DF.AP.Train[,1], nrounds = 800)

new_obs <- DF.AP.Test[1,-c(1,2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame

# To make xgboost compatible with iml, create the following predict function and predictor object
pred.gb = function(model, newdata){
  newData_x = xgb.DMatrix(data.matrix(newdata), missing = NA)
  results <- predict(model, newData_x)
}

# Shapley Sampling
predictor_XGB_AP = Predictor$new(model = xgboost_AP, 
                                 data = DF.AP.Train[,-c(1,2)], #needs to be df
                                 y = DF.AP.Train[,1], #needs to be df
                                 predict.fun = pred.gb)
shapley_XGB_AP = Shapley$new(predictor_XGB_AP, x.interest = new_obs, sample.size = 1000)

# Kernel SHAP
explainer_XGB_AP = explain(model = xgboost_AP, 
                           data = DF.AP.Train[,-c(1,2)], 
                           y = DF.AP.Train[,1], 
                           predict_function = pred.gb) 
KSHAP_XGB_AP  = individual_variable_effect(x = explainer_XGB_AP, 
                                           new_observation = new_obs, 
                                           method = "KernelSHAP", 
                                           nsamples = 100, D=4)

#### Non-Appartments ####
set.seed(1234)
# xgboost needs matrices as input
xgboost_NAP <- xgboost(data = as.matrix(DF.NAP.Train[,-c(1,2)]), label =DF.NAP.Train[,1], nrounds = 800)
new_obs <- DF.NAP.Test[1,-c(1,2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame

# Shapley Sampling
predictor_XGB_NAP = Predictor$new(model = xgboost_NAP, 
                                  data = DF.NAP.Train[,-c(1,2)], 
                                  y = DF.NAP.Train[,1],
                                  predict.fun = pred.gb) #predictor with training data
shapley_XGB_NAP = Shapley$new(predictor_XGB_NAP, x.interest = new_obs, sample.size = 1000)
# Kernel SHAP
explainer_XGB_NAP = explain(model = xgboost_NAP, 
                           data = DF.NAP.Train[,-c(1,2)], 
                           y = DF.NAP.Train[,1], 
                           predict_function = pred.gb) 
KSHAP_XGB_NAP  = individual_variable_effect(x = explainer_XGB_NAP, 
                                           new_observation = new_obs, 
                                           method = "KernelSHAP", 
                                           nsamples = 100, D=4)
plot(KSHAP_XGB_NAP)
