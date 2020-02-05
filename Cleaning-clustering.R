library(readr)
library(dplyr)
library(ggplot2)
library(raster)
library(sp)

library(latticeExtra)
library(lattice)
library(RColorBrewer)
library(rspatial)
library(robustHD)
library(cluster)
#### DATA ####
dataset <- read_delim("dataset.csv", "|", 
                      +     escape_double = FALSE, trim_ws = TRUE)
##### Filters #### 
#Filter out the ksom house values less than 50.000 
DF  <- dataset %>%  filter(WAARDE > 50000 )
#filter out houses with houseprice > 60mil
DF <- DF %>%  filter(WAARDE < 6000000)
#filter Rotterdam
DF <- DF %>% filter(PC_N %in% (3000:3089))
#filter NA KAV OPP  & extreme large 
DF$KAVOPP[is.na(DF$KAVOPP)] <- 0 
DF <- DF %>% filter(KAVOPP < 9999999)


#filter NA PC_A
DF$PC_A <- as.character(DF$PC_A)
DF$TOEV <- as.character(DF$TOEV)
#filter huisnummer
DF <- DF %>% 
  filter(BOUWJAAR <= 2019)
## dummy variables voor garage etc
DF.C<- DF[,c(6,8,10,11,12,13,14,16,17,18,20,21,22,23,24)]
DF.C <- fastDummies::dummy_cols(DF.C, remove_first_dummy = T)

#DF continous
DF.Continous <- cbind(DF[,c(11,12,13,18)],age)
DF.Continous.normalize <- apply(DF.Continous,2,normalize)


#### LAT & LONG ####
#Lat & Longitutde  plots
par(mfrow = c(1, 3))
hist(DF.C$LAT, col = 'gray')
hist(DF.C$LON, col = 'gray')
plot(DF.C$LON, DF.C$LAT, asp = 1)

#Kmean based on coordinates 
four <-kmeans(DF.C[, c(12,13)], 4, nstart = 20)
clus <- cbind(DF.C[, c(12,13)], clus = four$cluster)
head(clus)
DF.C <- cbind(DF.C,clus = four$cluster)
DF.C <- DF.C[,cbind(10,1,4,5,16,17,18,19,20,21)]

#clustering based on buurtcodes
#feature scaling (0-1)
means <- apply(DF, 2, median)


split <- t(sapply(DF$BUURTCODE, function(x) substring(x, first=c(1,4,6), last=c(4,5,8))))
Buurtcode <-split[,3]


Cluster.data <- cbind(DF.C[,c(2,5,6,7,8,9,12,13,14,15,16,17,18,19)], "buurtcode"=as.numeric(Buurtcode),
                      DF.Continous.normalize)
By_buurtcode <- Cluster.data %>% group_by(buurtcode) #70 buurtcodes
D.Clust<- By_buurtcode %>% 
  summarise(WAARDE = median(WAARDE),
            freq = sum(!is.na(buurtcode)),
            age = median(age), inhoud = median(WONINGINH))

clust <-kmeans(D.Clust[,c(2,5)], 3, nstart = 20)
plot(log(D.Clust$WAARDE), col=clust$cluster)


#### DATES #####  
age <-  function(now, bwj)
{
  age <- (now - bwj)
  return(age)
}
age <- age(2019, DF$BOUWJAAR)
#filter DATUM to dummy 

Dates<- DF %>%
  mutate(DATUM = as.character(as.Date(DF$DATUM, "%d-%m-%Y"),"%Y"))
Dates <- Dates[,"DATUM"]
Dates <- fastDummies::dummy_cols(Dates, remove_first_dummy = T)
Dates <- Dates[,-1]

DF.C <- cbind(DF.C,age, Dates)

####Woningtype####
#woningtype ID merge into 5 factora

DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 9] <- "FLAT" #FLAT
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 10 ] <- "FLAT" #FLAt
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 11 ] <- "FLAT" #FLAt
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 12 ] <- "FLAT" #FLAt
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 13 ] <- "FLAT" #FLAt
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 14 ] <- "FLAT" #FLAt
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 15] <- "FLAT" #FLAt
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 1] <- "VRIJSTAAND"
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 4] <-"VRIJSTAAND"
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 2] <- "HALF"  # HALF TUSSEN 1 BUURTJE 
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 3] <-  "HALF"  # HALF TUSSEN 1 BUURTJE 
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 5] <-  "HALF"  # HALF TUSSEN 1 BUURTJE 
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 6] <- "RIJ" #rijhuis
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 7] <- "RIJ" #rijhuis eind
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 8] <- "RIJ"  #rijhuis eind
DF.C$WONINGTYPEID[DF.C$WONINGTYPEID == 51] <- "PENT" #penthouse 
DF.C$WONINGTYPEID<- as.factor(DF.C$WONINGTYPEID)

#Depend variable analysis
par(mfrow = c(1, 1))
ggplot(DF.C, aes(x=WAARDE))+
  geom_histogram(colour="black", fill="white")+xlim(c(0,3000000))
boxplot(DF.C$WAARDE) 
hist(DF.C$WAARDE)
hist(DF.C$age)
boxplot(DF.C$age)
summary(DF.C$age)


##### Linear Model (SIMPEL)####
# standardize continous variables
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize(DF)

#Continous variables


#depend variable
price = DF$WAARDE
ln.price = log(price)
hist(ln.price)
#basic ols
f1 <- lm(ln.price ~., data= X)
summary(f1)

