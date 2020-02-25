library(dplyr)
library(readr)
#### DATA ####
# make sure to load Ortec's latest data-set, should be 1091799 observations
dataset <- read_delim("dataset copy.csv", "|", escape_double = FALSE, trim_ws = TRUE)
## Filter  out outlying observations
DF <- dataset
DF <- DF %>% filter(PC_N %in% (3000:3089)) #Rotterdam
DF$KAVOPP[is.na(DF$KAVOPP)] <- 0 #set NA KAVOPP values to zero
age <-  function(now, bwj)
{
  age <- (now - bwj)
  return(age)
}
DF$AGE <- age(2019, DF$BOUWJAAR)
split <- t(sapply(DF$BUURTCODE, function(x) substring(x, first=c(1,4,6), last=c(3,5,8))))
DF$BUURTCODE <-split[,2]
DF$BERGING <- ifelse(DF$INDBERGING_YNI=='Y'|DF$INDBERGING_YNI=='I',1,0)
DF$GARAGE <- ifelse(DF$INDGARAGE_YNI=='Y'|DF$INDGARAGE_YNI=='I',1,0)
DF$MONUMENT <- ifelse(DF$INDMONUMENT_YN=='Y',1,0)

#woningtype ID merge into 5 factors
DF$WONINGTYPEID[DF$WONINGTYPEID == 9] <- "STAPEL" #FLAT
DF$WONINGTYPEID[DF$WONINGTYPEID == 10 ] <- "STAPEL" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 11 ] <- "STAPEL" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 12 ] <- "STAPEL" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 13 ] <- "STAPEL" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 14 ] <- "STAPEL" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 15] <- "STAPEL" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 1] <- "VRIJSTAAND"
DF$WONINGTYPEID[DF$WONINGTYPEID == 4] <- "HALF" #geschakeld
DF$WONINGTYPEID[DF$WONINGTYPEID == 2] <- "HALF"  # HALF
DF$WONINGTYPEID[DF$WONINGTYPEID == 3] <-  "HALF"  # HALF 
DF$WONINGTYPEID[DF$WONINGTYPEID == 5] <-  "HALF"  # HALF 
DF$WONINGTYPEID[DF$WONINGTYPEID == 6] <- "RIJ" #rijhuis
DF$WONINGTYPEID[DF$WONINGTYPEID == 7] <- "RIJ" #rijhuis hoek 
DF$WONINGTYPEID[DF$WONINGTYPEID == 8] <- "RIJ"  #rijhuis eind
DF$WONINGTYPEID[DF$WONINGTYPEID == 51] <- "STAPEL" #penthouse 
DF$WONINGTYPEID<- as.factor(DF$WONINGTYPEID)
 
DF  <- DF %>%  
  filter(WAARDE > 50000 ) %>% #Filter out the ksom house values less than 50.000
  filter(WAARDE < 6000000) %>% #filter out houses with houseprice > 60mil
  filter(KAVOPP < 7000) %>%   #filter extreme large kavOPP 
  filter(!(WONINGTYPEID=='RIJ' & KAVOPP>400)) %>% #filter large KAVOPP values for type 'RIJ'
  filter(BOUWJAAR <= 2019) %>% #filter bouwjaar < 2019
  filter(AGE<=216) %>% # house cannot be built more than 216 years ago
  filter(WONINGID!=3421281) # aberrant observation, not truly "VRIJSTAAND"

dates <- as.data.frame(seq(as.Date("2010-01-01"), as.Date("2018-12-31"), by = "days"))
datescount <- seq(1:nrow(dates))
dates <- cbind(dates, datescount/365) #normalize by dividing through # days/year)
names(dates) = c("DATUM", "datescount")
#create right Date format in all our dataframes
DF$DATUM <- lubridate::dmy(DF$DATUM)
DF <- merge(DF, dates, by="DATUM")

# Select variables to be used 
DF.use <- dplyr::select(DF, "WAARDE","WONINGTYPEID","KAVOPP","WONINGOPP","WONINGINH","GARAGE","BERGING","MONUMENT","datescount","AGE","BUURTCODE")

#### Create numerical DF based on cleaned data ####
DF.dum <- fastDummies::dummy_cols(DF.use, remove_first_dummy = F,remove_selected_columns = T)
DF.dum$WAARDE <- log(DF.dum$WAARDE)

# split data frame based on appartments or not
DF.dum.ap <- filter(DF.dum, WONINGTYPEID_STAPEL == "1")
DF.dum.nap <- filter(DF.dum, WONINGTYPEID_STAPEL != "1")

# create train & test samples (for appartments and non-appartments)
DF.dum.ap.train <- DF.dum.ap[1:18704,] #all appartments sold before 18
DF.dum.ap.test <- DF.dum.ap[18705:21266,] #all appartment sold in 18

DF.dum.nap.train <- DF.dum.nap[1:7608,] #all houses sold before 18
DF.dum.nap.test <- DF.dum.nap[7609:8756,] #all houses sold in 18 

# PAY ATTENTION WHICH VARIABLES TO SELECT FOR WHICH TYPE OF MODEL, DF's still contain all variables
# Tiemen variabele selection split vanaf hier  
# appartments
length_ap = dim(DF.dum.ap.train)[1]/2 #split training sample half-half
set.seed(12346)#DONOTCHANGESEED
sample_ap = c(sample(nrow(DF.dum.ap.train), length_ap, replace = FALSE))
DF.dum.ap.train_1 = DF.dum.ap.train[sample_ap, ]
DF.dum.ap.train_2 = DF.dum.ap.train[-sample_ap,]
DF.dum.ap.test <- DF.dum.ap[18705:21266,] #all appartment sold in 18

# non-appartments
length_nap = dim(DF.dum.nap.train)[1]/2 #split training sample half-half 
set.seed(12345)#DONOTCHANGESEED
sample_nap = c(sample(nrow(DF.dum.nap.train), length_nap, replace = FALSE))
DF.dum.nap.train_1 = DF.dum.nap.train[sample_nap, ]
DF.dum.nap.train_2 = DF.dum.nap.train[-sample_nap,] # use
DF.dum.nap.test <- DF.dum.nap[7609:8756,] #all houses sold in 18
DF.dum.nap.train <- DF.dum.nap[1:7608,] #all houses sold before 18



# Final linear model Appartments  DF.dum.ap.train_2
lm_ap_fin = lm(WAARDE ~ datescount+WONINGOPP+I(WONINGOPP^2) + GARAGE + BERGING + MONUMENT + AGE+ I(AGE^2)+ BUURTCODE_08 + BUURTCODE_14 + BUURTCODE_06 + BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + BUURTCODE_01 , DF.dum.ap.train_2)

# Final linear model non-AppartmentsDF.dum.nap.train_2
lm_nap_final = lm(WAARDE ~ datescount+WONINGOPP +I(WONINGOPP^2) + GARAGE  + AGE+ I(AGE^2)+WONINGTYPEID_HALF + WONINGTYPEID_VRIJSTAAND+ BUURTCODE_04 + BUURTCODE_08 + BUURTCODE_06 + BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + KAVOPP + I(KAVOPP^2), DF.dum.nap.train_2)

# Final selection of variables
#WAARDE ~ datescount+WONINGOPP +I(WONINGOPP^2) + GARAGE  + AGE+ I(AGE^2)+WONINGTYPEID_HALF + WONINGTYPEID_VRIJSTAAND+ BUURTCODE_04 + BUURTCODE_08 + BUURTCODE_06 + BUURTCODE_05 + BUURTCODE_12 + BUURTCODE_15 + KAVOPP + I(KAVOPP^2), DF.dum.nap.train_2 )




