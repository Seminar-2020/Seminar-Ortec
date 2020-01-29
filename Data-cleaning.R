library(readr)
library(dplyr)

#### DATA ####
dataset <- read_delim("dataset.csv", "|", 
                      +     escape_double = FALSE, trim_ws = TRUE)

##### Filters #### 
#Filter out the ksom house values less than 50.000 
DF  <- dataset %>%  filter(WAARDE > 50000 )
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

#Plot using 4 clusters
plot(clus$LON, clus$LAT, col = four$cluster, asp = 1,
     pch = four$cluster, main = "four areas",
     xlab = "Longitude",  ylab = "Latitude")
points(four$centers[ ,2], four$centers[ ,1],
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(four$centers[ ,2], four$centers[ ,1], cex = 1.1,
     col = 'black', attributes(four$centers)$dimnames[[1]])

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
