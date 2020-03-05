# make sure all packages are installed/loaded
install.packages("iml")
install.packages("xgboost")
install.packages("shapper")
install.packages("DALEX")
library(xgboost)
library(iml) # for Shapley Sampling
library(shapper) # for Kernel SHAP, python needs to be installed (shapper::install_shap() # make sure you've ran this)
library(DALEX) # needs to be installed for the explainer object of shapper/individual_variable_effect

# make sure that you have loaded 'Workspace for phi consistency'
# appartments
phi_sampling_gb_ap <- matrix(NA,nrow=1000,ncol=13)
phi_kernel_gb_ap <- matrix(NA,nrow=1000,ncol=26)

for (i in 51:60) {
  shapley_XGB_AP <- Shapley$new(predictor_XGB_AP, x.interest = DF.AP.Test.shuffle[i,], sample.size = 1000)
  phi_sampling_gb_ap[i,] <- shapley_XGB_AP$results[[2]]
  KSHAP_XGB_AP  <- individual_variable_effect(x = explainer_XGB_AP, 
                                              new_observation = DF.AP.Test.shuffle[i,], 
                                              method = "KernelSHAP", 
                                              nsamples = 500) # takes a couple of minutes
  phi_kernel_gb_ap[i,] <- cbind(KSHAP_XGB_AP$`_attribution_`,KSHAP_XGB_AP$`_sign_`)
}

# non-appartments
phi_sampling_gb_nap <- matrix(NA,nrow=1000,ncol=13)
phi_kernel_gb_nap <- matrix(NA,nrow=1000,ncol=26)

for (j in 51:60) {
  shapley_XGB_NAP <- Shapley$new(predictor_XGB_NAP, x.interest = DF.NAP.Test.shuffle[j,], sample.size = 1000)
  phi_sampling_gb_nap[j,] <- shapley_XGB_NAP$results[[2]]
  KSHAP_XGB_NAP  <- individual_variable_effect(x = explainer_XGB_NAP, 
                                               new_observation = DF.NAP.Test.shuffle[j,], 
                                               method = "KernelSHAP", 
                                               nsamples = 500) # takes a couple of minutes
  phi_kernel_gb_nap[j,] <- cbind(KSHAP_XGB_NAP$`_attribution_`,KSHAP_XGB_NAP$`_sign_`)
}
