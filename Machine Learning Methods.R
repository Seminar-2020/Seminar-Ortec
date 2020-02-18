### PACKAGES ####  #zonder H20
library(randomForest)
library(MLmetrics)
library(ggplot2)
library(xgboost)
library(gbm)
library(neuralnet)

#RANDOM FOREST
# use function randomForest() from package "randomForest"
# RF on the full data set 
test_RF <- randomForest(WAARDE~.,data=train80, ntree = 100, importance = T, do.trace = T) #mtry = p/3
pred_RF <- predict(test_RF, train20[-1])
MAPE(train20$WAARDE,pred_RF) # 0.01357624

#Variable importance plots 
varimp_RF <- importance(test_RF)
varImportance_RF <- data.frame(Variables = row.names(varimp_RF), 
                               Importance = round(varimp_RF[,'%IncMSE'],2)) 
rankImportance <- varImportance_RF %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))
print("Variable importance after second iteration") # why second iteration?
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 

#### Gradient Boosting ####
# use function xgboost() from packages "xgboost" NOG TESTEN
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