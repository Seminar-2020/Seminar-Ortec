# Bunk & Test set
# Random forest versus GB, NN and OLS
#NN.f <- exp(NN[,16])
GB.f <- exp(GBM[,16])
RF.f <- exp(RF[,16])
OLS.f <- exp(OLS[,16])
y <- exp(RF[,1])

RFvsGB.test.bunk <- DM.test(RF.f, GB.f, y, loss.type = "AE")
#RFvsNN.test.bunk <-
RFvsOLS.test.bunk <- DM.test(OLS.f, RF.f, y, loss.type = "AE")

#Bunk & Test set
#GB versus NN en OLS
#GBvsNN.test.bunk
GBvsOLS.test.bunk <- DM.test(OLS.f, GB.f, y, loss.type = "AE")

#Bunk & Test set
#NN versus OLS
#NNvsOLS.test.bunk <- DM.test(NN.f, OLS.f, y, loss.type = "AE")

#Non-bunk & Test
GB.f.NA <- exp(GBM.NA[,16])
RF.f.NA <- exp(RF.NA[,16])
OLS.f.NA <- exp(OLS.NA[,16])
y.NA <- exp(RF.NA[,1])

RFvsGB.test.nonbunk <- DM.test(RF.f.NA, GB.f.NA, y.NA, loss.type = "AE")
#RFvsNN.test.bunk <-
RFvsOLS.test.nonbunk <- DM.test(OLS.f.NA, RF.f.NA, y.NA, loss.type = "AE")

#Bunk & Test set
#GB versus NN en OLS
#GBvsNN.test.bunk
GBvsOLS.test.nonbunk <- DM.test(OLS.f.NA, GB.f.NA, y.NA, loss.type = "AE")

#Bunk & Test set
#NN versus OLS
#NNvsOLS.test.bunk <- DM.test(NN.f, OLS.f, y, loss.type = "AE")


