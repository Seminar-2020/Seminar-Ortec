# Functions to normalize the data 
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
denormalize <- function(x_norm, x)
{
  num <- (x_norm* (max(x)- min(x))) + min(x)
}

# activation functions
relu <- function(x) sapply(x, function(z) max(0,z))
# approximation of the relu fucntion 
softplus <- function(x) log(1+exp(x))

# Standardize the data
train_data_x <- apply(DF.dum.ap.train[,-c(3,11,14,18)],2, normalize) 
train_data_y<- normalize(DF.dum.ap.train[,1]) 
test_data_y <- normalize(DF.dum.ap.testx[,1])
test_data_x <- apply(DF.dum.ap.testx[,-c(3,11,14,18)],2, normalize) 

# Run the nueral networks using R function, neural net 
NN_Rprop<-neuralnet(WAARDE~datescount+WONINGOPP + GARAGE + AGE+
                BERGING + MONUMENT +  BUURTCODE_08 + BUURTCODE_14 +
                BUURTCODE_06 + 
                BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + 
                BUURTCODE_01 
                ,train_data_x , hidden = 4, threshold = 0.05,        
                stepmax = 1e+10,algorithm = "rprop+", 
                err.fct = "sse", act.fct = softplus,
                ) 
plot(NN_Rprop)
NN_Rprop$result.matrix
nn.results <- compute(NN_Rprop,test_data_x[,-1] )
results <- data.frame(actual = test_data_x[,1], prediction =nn.results$net.result )
head(results)

#predict &denormalize
predictions_NN<-predict(NN_Rprop,test_data_x[,-1])
denorm_pred <- denormalize(predictions_NN,DF.dum.ap.testx[,1])
results <- data.frame(actual = DF.dum.ap.testx[,1], prediction = denorm_pred )
head(results)
# test
MAPE(denorm_pred,DF.dum.ap.testx[,1])
MAE(denorm_pred,DF.dum.ap.testx[,1])
mse(DF.dum.ap.testx[,1],denorm_pred)
# train
denorm_pred <- denormalize(predictions_NN,DF.dum.ap.train[,1])
MAPE(denorm_pred,DF.dum.ap.train[,1])
MAE(denorm_pred,DF.dum.ap.train[,1])
mse(DF.dum.ap.testx[,1],denorm_pred)

errors = abs(denorm_pred  - DF.dum.ap.testx[,1])
mean(errors)
mape = 100 * (errors /  DF.dum.ap.testx[,1])
accuracy = 100 - mean(mape)

#Shapley
predictor = Predictor$new(NN_Rprop, data =  as.data.frame(train_data_x))
shapley = Shapley$new(predictor, x.interest = as.data.frame(test_data_x)[2,], sample.size = 1000)
plot(shapley)
#Kernel Shap 
exp_nn <- explain(NN_Rprop,data =  as.data.frame(train_data_x))
Kernal_shap <- shp(exp_nn, x.interest = as.data.frame(test_data_x)[2,])
plot(Kernelshap)
