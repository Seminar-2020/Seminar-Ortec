# Make sure that DF.NAP.Train, DF.NAP.Test, DF.AP.Train, DF.AP.Test are loaded
#### Packages ####
library(randomForest)
library(iml) # for Shapley Sampling
library(shapper) # for Kernel SHAP, python needs to be installed (shapper::install_shap() # make sure you've ran this)
library(DALEX) # needs to be installed for the explainer object of shapper/individual_variable_effect

#### APPARTMENTS ####
set.seed(1234)
RF_app<- randomForest(Price~Datescount+ Surface + Garage + Age+
                    Storage + Monument + Neighbourhood_08 + Neighbourhood_14 +
                    Neighbourhood_06 + 
                    Neighbourhood_05 +  Neighbourhood_12 +  Neighbourhood_15 + 
                    Neighbourhood_01 ,
                  DF.AP.Train)
new_obs <- DF.AP.Test[1,-c(1,2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame

# Shapley Sampling
predictor_RF_AP <- Predictor$new(RF_app, DF.AP.Train[,-c(1,2)]) #predictor with training data
shapley_RF_AP <- Shapley$new(predictor_RF_AP, new_obs, sample.size = 1000) #new observation does not contains y-value
phis_samp <- shapley_RF_AP$results[[2]]

# Kernel SHAP
explainer_RF_AP = explain(model = RF_app, 
                          data = DF.AP.Train[,-c(1,2)], 
                          y = DF.AP.Train[,1]) 
KSHAP_RF_AP  = individual_variable_effect(x = explainer_RF_AP, 
                                          new_observation = new_obs, 
                                          method = "KernelSHAP", 
                                          nsamples = 100,D=4)
phis_kern <- KSHAP_LM_AP$`_attribution_` # absolute values, signs can be retrieved through `_sign_`

#### NON APPARTMENTS ####
set.seed(1234)
RF_non_app <- randomForest(Price ~ Datescount  +SurfLot + Surface + Garage + 
                        Age  + Housetype_semi  + Housetype_det +  Neighbourhood_04 + 
                        Neighbourhood_08+
                        Neighbourhood_06 + Neighbourhood_05 +  Neighbourhood_12 +  Neighbourhood_12
                      ,DF.NAP.Train) 

new_obs <- DF.NAP.Test[1,-c(1,2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame
# Shapley Sampling
predictor_RF_NAP <- Predictor$new(RF_non_app, as.data.frame(DF.NAP.Train[,-c(1,2)])) #predictor with training data (df)
shapley_RF_NAP <- Shapley$new(predictor_RF_NAP, new_obs, sample.size = 1000) #new observation does not contains y-value

# Kernel SHAP
explainer_RF_NAP = explain(model = RF_non_app, 
                          data = DF.NAP.Train[,-c(1,2)], 
                          y = DF.NAP.Train[,1]) 
KSHAP_RF_NAP  = individual_variable_effect(x = explainer_RF_NAP, 
                                          new_observation = new_obs, 
                                          method = "KernelSHAP", 
                                          nsamples = 100,D=4)
