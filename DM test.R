# Bunk & Train set
# Random forest versus GB, NN and OLS
NN.f <- NN.AP.train[,2]
GB.f <- GBM.AP.train[,2]
RF.f <- RF.AP.train[,2]
OLS.f <- OLS.AP.train[,2]
y <- OLS.AP.train[,1]

RFvsGB.train.bunk <- DM.test(RF.f, GB.f, y, loss.type = "AE")
RFvsNN.train.bunk <- DM.test(RF.f, NN.f, y, loss.type = "AE")
RFvsOLS.train.bunk <- DM.test(OLS.f, RF.f, y, loss.type = "AE")

#GB versus NN en OLS
GBvsNN.train.bunk <- DM.test(NN.f, GB.f, y, loss.type = "AE")
GBvsOLS.train.bunk <- DM.test(OLS.f, GB.f, y, loss.type = "AE")

#NN versus OLS
NNvsOLS.train.bunk <- DM.test(NN.f, OLS.f, y, loss.type = "AE")

#############################################################
# Non-Bunk & Train set
# Random forest versus GB, NN and OLS
NN.f.NAP <- NN.NAP.train[,2]
GB.f.NAP <- GBM.NAP.train[,2]
RF.f.NAP <- RF.NAP.train[,2]
OLS.f.NAP <- OLS.NAP.train[,2]
y.NAP <- OLS.NAP.train[,1]

RFvsGB.train.nbunk <- DM.test(RF.f.NAP, GB.f.NAP, y.NAP, loss.type = "AE")
RFvsNN.train.nbunk <- DM.test(RF.f.NAP, NN.f.NAP, y.NAP, loss.type = "AE")
RFvsOLS.train.nbunk <- DM.test(OLS.f.NAP, RF.f.NAP, y.NAP, loss.type = "AE")

#GB versus NN en OLS
GBvsNN.train.nbunk <- DM.test(NN.f.NAP, GB.f.NAP, y.NAP, loss.type = "AE")
GBvsOLS.train.nbunk <- DM.test(OLS.f.NAP, GB.f.NAP, y.NAP, loss.type = "AE")

#NN versus OLS
NNvsOLS.train.nbunk <- DM.test(NN.f.NAP, OLS.f.NAP, y.NAP, loss.type = "AE")

##########################################################
# Bunk & Test set
# Random forest versus GB, NN and OLS
NN.f.test <- NN.AP.test[,2]
GB.f.test <- GBM.AP.test[,2]
RF.f.test <- RF.AP.test[,2]
OLS.f.test <- OLS.AP.test[,2]
y.test <- OLS.AP.test[,1]

RFvsGB.test.bunk <- DM.test(RF.f.test, GB.f.test, y.test, loss.type = "AE")
RFvsNN.test.bunk <- DM.test(RF.f.test, NN.f.test, y.test, loss.type = "AE")
RFvsOLS.test.bunk <- DM.test(OLS.f.test, RF.f.test, y.test, loss.type = "AE")

#GB versus NN en OLS
GBvsNN.test.bunk <- DM.test(NN.f.test, GB.f.test, y.test, loss.type = "AE")
GBvsOLS.test.bunk <- DM.test(OLS.f.test, GB.f.test, y.test, loss.type = "AE")

#NN versus OLS
NNvsOLS.test.bunk <- DM.test(NN.f.test, OLS.f.test, y.test, loss.type = "AE")

#########################################################
# Non-Bunk & Test set
# Random forest versus GB, NN and OLS
NN.f.test.NAP <- NN.NAP.test[,2]
GB.f.test.NAP <- GBM.NAP.test[,2]
RF.f.test.NAP <- RF.NAP.test[,2]
OLS.f.test.NAP <- OLS.NAP.test[,2]
y.test.NAP <- OLS.NAP.test[,1]

RFvsGB.test.nbunk <- DM.test(RF.f.test.NAP, GB.f.test.NAP, y.test.NAP, loss.type = "AE")
RFvsNN.test.nbunk <- DM.test(RF.f.test.NAP, NN.f.test.NAP, y.test.NAP, loss.type = "AE")
RFvsOLS.test.nbunk <- DM.test(OLS.f.test.NAP, RF.f.test.NAP, y.test.NAP, loss.type = "AE")

#GB versus NN en OLS
GBvsNN.test.nbunk <- DM.test(NN.f.test.NAP, GB.f.test.NAP, y.test.NAP, loss.type = "AE")
GBvsOLS.test.nbunk <- DM.test(OLS.f.test.NAP, GB.f.test.NAP, y.test.NAP, loss.type = "AE")

#NN versus OLS
NNvsOLS.test.nbunk <- DM.test(NN.f.test.NAP, OLS.f.test.NAP, y.test.NAP, loss.type = "AE")
