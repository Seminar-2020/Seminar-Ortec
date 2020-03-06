library(dplyr)
library(ggplot2)
library(gridExtra)
# merge all runs
phi_kernel_ap <- matrix(NA,nrow=100,ncol=13)
phi_kernel_nap <- matrix(NA,nrow=100,ncol=13)
phi_sampling_ap <- matrix(NA,nrow=100,ncol=13)
phi_sampling_nap <- matrix(NA,nrow=100,ncol=13)

# 40 runs fre
phi_kernel_ap[1:40,] <- phi_kernel_gb_ap[1:40,1:13]
phi_kernel_nap[1:40,] <- phi_kernel_gb_nap[1:40,1:13]
phi_sampling_ap[1:40,] <- phi_sampling_gb_ap[1:40,]
phi_sampling_nap[1:40,] <- phi_sampling_gb_nap[1:40,]

# run 40-50 fre
phi_kernel_ap[41:50,] <- phi_kernel_gb_ap[41:50,1:13]
phi_kernel_nap[41:50,] <- phi_kernel_gb_nap[41:50,1:13]
phi_sampling_ap[41:50,] <- phi_sampling_gb_ap[41:50,]
phi_sampling_nap[41:50,] <- phi_sampling_gb_nap[41:50,]

# run 51-60 Tas
phi_kernel_ap[51:60,] <- phi_kernel_gb_ap[51:60,1:13]
phi_kernel_nap[51:60,] <- phi_kernel_gb_nap[51:60,1:13]
phi_sampling_ap[51:60,] <- phi_sampling_gb_ap[51:60,]
phi_sampling_nap[51:60,] <- phi_sampling_gb_nap[51:60,]

# run 60-70 Tas
phi_kernel_ap[61:70,] <- phi_kernel_gb_ap[61:70,1:13]
phi_kernel_nap[61:70,] <- phi_kernel_gb_nap[61:70,1:13]
phi_sampling_ap[61:70,] <- phi_sampling_gb_ap[61:70,]
phi_sampling_nap[61:70,] <- phi_sampling_gb_nap[61:70,]

# put together
# make kernel frames numeric
phi_kernel_ap <- apply(phi_kernel_ap,2,as.numeric)
phi_kernel_nap <- apply(phi_kernel_nap,2,as.numeric)

# make frames for plots
test <- factor(c(rep("bunk",70),rep("non-bunk",70)))
final_df_kernel_surface <- as.data.frame(c(phi_kernel_ap[1:70,1],phi_kernel_nap[1:70,2]))
final_df_kernel_age <- as.data.frame(c(phi_kernel_ap[1:70,6],phi_kernel_nap[1:70,5]))
final_df_sampling_surface <- as.data.frame(c(phi_sampling_ap[1:70,1],phi_sampling_nap[1:70,2]))
final_df_sampling_age <- as.data.frame(c(phi_sampling_ap[1:70,6],phi_sampling_nap[1:70,5]))

final_df_surface <- bind_cols("Kernel_SHAP"=final_df_kernel_surface,"Shapley_sampling"= final_df_sampling_surface,"Type"=test)
names(final_df_surface) <- c("Kernel_SHAP","Shapley_sampling","Type")

final_df_age <- bind_cols("Kernel_SHAP"=final_df_kernel_age,"Shapley_sampling"= final_df_sampling_age,"Type"=test)
names(final_df_age) <- c("Kernel_SHAP","Shapley_sampling","Type")

# make plots
p_surface <- ggplot(final_df_surface, aes(x=Shapley_sampling,y=Kernel_SHAP,color=Type)) +
  geom_point(size=2) +
  geom_abline() +
  ggtitle("Individual effect of Surface")

p_age <- ggplot(final_df_age, aes(x=Shapley_sampling,y=Kernel_SHAP,color=Type)) +
  geom_point(size=2) +
  geom_abline() +
  ggtitle("Individual effect of Age")

grid.arrange(p_surface,p_age,nrow=1)
