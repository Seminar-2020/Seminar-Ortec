# Make sure that DF.NAP.Train, DF.NAP.Test, DF.AP.Train, DF.AP.Test are loaded
#### Packages ####
library(neuralnet)
library(iml) # for Shapley Sampling
library(shapper) # for Kernel SHAP, python needs to be installed (shapper::install_shap() # make sure you've ran this)
library(DALEX) # needs to be installed for the explainer object of shapper/individual_variable_effect

#### Appartments ####
set.seed(1234)
# Neural Network model 
# Neural networks require the data to be normalized, the following functions are use to normalise
# and to denormalize respectively the input ant output values
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
denormalize <- function(x_norm, x)
{
  num <- (x_norm* (max(x)- min(x))) + min(x)
}

Normalize.AP.Train = apply(DF.AP.Train, 2, normalize)
Normalize.AP.test = apply(DF.AP.Test, 2, normalize)
Normalize.AP.test  = as.data.frame(Normalize.AP.test)
# The Softplus activation fucntion in used in the neuralnet funciton. 
# Softplus is an approximation of the Relu, rectifier activiation fucntion 
# Compared to Relu, Softplus is smootha dn differentiable, mucht easier and efficient to compute. 
softplus <- function(x) log(1+exp(x))
set.seed(1234) # NN does not end up at the same optimum using the different seeds 
Neuralnet_AP <-  neuralnet(Price~Datescount+ Surface + Garage + Age+
                             Storage + Monument + Neighbourhood_08 + Neighbourhood_14 +
                             Neighbourhood_06 + 
                             Neighbourhood_05 +  Neighbourhood_12 +  Neighbourhood_15 + 
                             Neighbourhood_01 , hidden = 1, stepmax = 1e+10, act.fct = softplus,
                             Normalize.AP.Train )
new_obs <-Normalize.AP.test[1,-c(1,2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame

# Shapley Sampling
predictor_NN_AP = Predictor$new(model = Neuralnet_AP, 
                                 data = as.data.frame(Normalize.AP.Train[,-c(1,2)]), #needs to be df
                                 y =   Normalize.AP.Train[,1] #needs to be df
                                )

shapley_NN_AP = Shapley$new(predictor_NN_AP , x.interest = new_obs, sample.size = 1000)

# Kernel SHAP
explainer_NN_AP = explain(model = Neuralnet_AP, 
                           data = as.data.frame(Normalize.AP.Train[,-c(1,2)]), 
                           y = Normalize.AP.Train[,1]
                          ) 
KSHAP_NN_AP  = individual_variable_effect(x = explainer_NN_AP, 
                                           new_observation = new_obs, 
                                           method = "KernelSHAP", 
                                           nsamples = "auto") # set the kernel shap to auto otherwise no resutls for NN

#### Non-Appartments ####
# Normalize the data for NN
Normalize.NAP.Train = apply(DF.NAP.Train, 2, normalize)
Normalize.NAP.Test  = apply(DF.NAP.Test, 2, normalize)
Normalize.NAP.test  = as.data.frame(Normalize.NAP.Test)

set.seed(1234)
Neuralnet_NAP <- neuralnet(Price ~ Datescount  +SurfLot + Surface + Garage + 
                    Age  + Housetype_semi  + Housetype_det +  Neighbourhood_04 + Neighbourhood_08+
                    Neighbourhood_06 + Neighbourhood_05 +  Neighbourhood_12 +  
                    Neighbourhood_12 , hidden = 1, stepmax = 1e+10, act.fct = softplus,
                    Normalize.NAP.Train)

new_obs <- Normalize.NAP.test[1,-c(1,2)] # it is crucial that the dimension of new obs is equal to training data and is a data frame

# Shapley Sampling
predictor_NN_NAP = Predictor$new(model = Neuralnet_NAP, 
                                  data =  as.data.frame(Normalize.NAP.Train[,-c(1,2)]),  # needs to be df
                                  y = DF.NAP.Train[,1],
                                  )
shapley_NN_NAP = Shapley$new(predictor_NN_NAP, x.interest = new_obs, sample.size = 1000)

# Kernel SHAP
explainer_NN_NAP = explain(model = Neuralnet_NAP, 
                            data = as.data.frame(Normalize.NAP.Train[,-c(1,2)]),   # needs to be df
                            y = DF.NAP.Train[,1], 
                            ) 
KSHAP_NN_NAP  = individual_variable_effect(x = explainer_NN_NAP, 
                                            new_observation = new_obs, 
                                            method = "KernelSHAP", 
                                            nsamples = "auto")
