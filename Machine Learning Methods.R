### PACKAGES #### 
library(randomForest)
library(MLmetrics)
library(ggplot2)
library(xgboost)
library(gbm)
library(neuralnet)
library(caret) #CV
library(lattice)
library (ggplot2)
library(e1071)
library(ranger)
#h20
library(h2o)
localH2O = h2o.init()
#DNN
library(keras)

#RANDOM FOREST 
# BUNK 
# use data DF.dum.ap.train to train the random forest
# use data DF.dum.ap.test to test the random forest
# use function randomForest() from package "randomForest"
# randomForest: Random forest algorithm for classification and regression 
# ntree: number of trees to grow
# mtry : number or variables randomly sampled as candidates at each split
# nodesize: minimume size of the terminal nodes, large nodesize will lead to smaller trees, default 
# for regression is 5

RF_bunk <- randomForest(WAARDE~.,data=DF.dum.ap.train, nodesize = 5, ntree = 20) 

MAPE(train20$WAARDE,pred_RF) 

# TrainControl: used to apply cross-validation to tune the parameters, mtry, ntrees, 
# number: number of resampling iterations
# Train: sets up a grid of tuning parametes for regression routines, fit each model and calculates
# a resampling based performance measure
# tuneGrid:

#RF -R 
ptm <- proc.time()

control <- trainControl(method='cv', 
                        number=5, search="random")
train(WAARDE~.,
      data=DF.dum.ap.train,
      method = "rf",
      ntee = 5,
      trControl = control)
print(proc.time() - ptm) 

#tunegrid <- expand.grid(.mtry=c(1:15))


#RF - h20
h2o.init(nthreads = 20, max_mem_size = "16g")
test <- as.h2o(DF.dum.ap.test)
train <- as.h2o(DF.dum.ap.train)

#RF parameters 
rf_param <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))


default_rf <- h2o.randomForest(x = 2:24 , y = 1, 
                               training_frame = train, 
                               valdidation_fram test,
                               stopping_rounds = 5, 
                               stopping_tolerance = 0.001, 
                               stopping_metric = "MAE", 
                               mtries = 7,
                               seed = 29,
                               nfolds = 10)


#### Gradient Boosting Machines  ####
# use function xgboost() from packages "xgboost"
# eta - learning rate
# max_depth 
# objective - reg:squarederror
# data - training should be of class xgb.DMatrix 
# use function gbm() from package "gbm"
# distribution = "gaussian" (squared error)
# bag.fraction = 1 --> gebruik alle observaties in de sample

test_GB <- gbm(WAARDE ~ ., distribution = "gaussian", data = train80, n.trees = 100,
               interaction.depth = 1, n.minobsinnode = 0, shrinkage = 0.1, bag.fraction = 1, verbose=T)
pred_GB <- predict(test_GB, train20[-1], n.trees = 100)
MAPE(train20$WAARDE,pred_GB) # 0.01450087 / 0.01449252 (bag frac. = 0.5 or bag frac = 1)
summary(test_GB) # gives relative importance (in values + plot)

# GMB - H20 


#### Neural Network ####
# exclude can be used to prevent using bias neurons: https://stackoverflow.com/questions/40633567/how-to-exclude-bias-neurons-in-neuralnet-in-r
# try with GRPROP instead of BACKPROP 
# normalize data first with max-min normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
val80_s <- cbind(apply(val80[,1:3],2,normalize),val80[,4:ncol(val80)])
#Split the validationset into 80-20 training set
train80_s <- val80_s[train.id, ]
train20_s <- val80_s[-train.id, ]

n <- names(train80_s)
f <- as.formula(paste("WAARDE ~", paste(n[!n %in% "WAARDE"], collapse = " + ")))
test_NN <- neuralnet(f, data = train80_s, hidden = c(2,1), threshold = 0.01, learningrate = 0.1, lifesign = "minimal", algorithm = "backprop", err.fct = "sse", act.fct = "tanh", linear.output=T)
pred_NN <- predict(test_NN, train20_s[-1]) # 
MAPE(train20_s$WAARDE,pred_NN) # 

test_NN_basic <- neuralnet(f, data = train80_s, hidden = c(12), linear.output=T)
pred_NN_basic <- predict(test_NN_basic, train20_s[-1]) # 
MAPE(train20_s$WAARDE,pred_NN_basic) # 0.1134699 for c(2,1), 0.1163395 for c(12)

#NN doet het nu veel minder goed, zou door overfitting kunnen komen?
# toch h2o??
library(h2o)
h2o.init()