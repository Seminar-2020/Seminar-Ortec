# Make sure that DF.NAP.Train, DF.NAP.Test, DF.AP.Train, DF.AP.Test are loaded
#### Packages ####
library(iml) # for Shapley Sampling
library(shapper) # for Kernel SHAP, python needs to be installed (shapper::install_shap() # make sure you've ran this)
library(DALEX) # needs to be installed for the explainer object of shapper/individual_variable_effect
#### Appartments ####
lm_ap_final <- lm(
  Price ~ Datescount + Surface + I(Surface ^ 2)
  + Garage + Storage + Monument + Age + I(Age ^ 2) + Neighbourhood_08 + Neighbourhood_14 + Neighbourhood_06
  +  Neighbourhood_05 +  Neighbourhood_12 +  Neighbourhood_15 +  Neighbourhood_01,
  DF.AP.Train)

new_obs <- DF.AP.Test[1, -c(1, 2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame
# Shapley Sampling
predictor_LM_AP = Predictor$new(lm_ap_final, DF.AP.Train[, -c(1, 2)]) #predictor with training data (df) without y & WAARDEID
shapley_LM_AP = Shapley$new(predictor_LM_AP, new_obs, sample.size = 1000) #new observation is df and has same dimensions as data in predictor object
# Kernel SHAP
explainer_LM_AP = explain(
  model = lm_ap_final,
  data = DF.AP.Train[, -c(1, 2)],
  y = DF.AP.Train[, 1],
  predict_function = predict.lm
)
KSHAP_LM_AP  = individual_variable_effect(
  x = explainer_LM_AP,
  new_observation = new_obs,
  method = "KernelSHAP",
  nsamples = 100,
  D = 4
)

#### Non-appartments ####
lm_nap_final = lm(
  Price ~ Datescount + Surface + I(Surface ^ 2) + Garage  + Age + I(Age ^
                                                                      2) + Housetype_semi
  + Housetype_det + Neighbourhood_04 + Neighbourhood_08 +
    Neighbourhood_06 + Neighbourhood_05
  + Neighbourhood_12 + Neighbourhood_15  + SurfLot + I(SurfLot ^
                                                         2),
  DF.NAP.Train
)
new_obs <- DF.NAP.Test[1, -c(1, 2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame

# Shapley Sampling
predictor_LM_NAP = Predictor$new(lm_nap_final, DF.NAP.Train[, -c(1, 2)]) #predictor with training data (df) without y & WAARDEID
shapley_LM_NAP = Shapley$new(predictor_LM_NAP, new_obs, sample.size = 1000) #new observation is df and has same dimensions as data in predictor object
# Kernel SHAP
explainer_LM_NAP = explain(
  model = lm_nap_final,
  data = DF.NAP.Train[, -c(1, 2)],
  y = DF.NAP.Train[, 1],
  predict_function = predict.lm
)
KSHAP_LM_NAP  = individual_variable_effect(
  x = explainer_LM_NAP,
  new_observation = new_obs,
  method = "KernelSHAP",
  nsamples = 100,
  D = 4
)
