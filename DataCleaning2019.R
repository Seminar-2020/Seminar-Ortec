library(dplyr)
library(readr)

# read csv data file
dataset19 <- read_delim("/Users/Rosa/Documents/Econometrie master/Ortec Finance case study/dataset_2019.csv", "|", escape_double = FALSE, trim_ws = TRUE)

# use only the houses in Rotterdam
DF <- dataset19 %>% filter(PC_N %in% (3000:3089))

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

# woningtype ID merge into 5 factors
DF$WONINGTYPEID[DF$WONINGTYPEID == 9] <- "STAPEL" # flat
DF$WONINGTYPEID[DF$WONINGTYPEID == 10 ] <- "STAPEL" # flat
DF$WONINGTYPEID[DF$WONINGTYPEID == 11 ] <- "STAPEL" # flat
DF$WONINGTYPEID[DF$WONINGTYPEID == 12 ] <- "STAPEL" # flat
DF$WONINGTYPEID[DF$WONINGTYPEID == 13 ] <- "STAPEL" # flat
DF$WONINGTYPEID[DF$WONINGTYPEID == 14 ] <- "STAPEL" # flat
DF$WONINGTYPEID[DF$WONINGTYPEID == 15] <- "STAPEL" # flat
DF$WONINGTYPEID[DF$WONINGTYPEID == 1] <- "VRIJSTAAND" #vrijstaand
DF$WONINGTYPEID[DF$WONINGTYPEID == 4] <- "HALF" # geschakeld
DF$WONINGTYPEID[DF$WONINGTYPEID == 2] <- "HALF"  # HALF
DF$WONINGTYPEID[DF$WONINGTYPEID == 3] <-  "HALF"  # HALF 
DF$WONINGTYPEID[DF$WONINGTYPEID == 5] <-  "HALF"  # HALF 
DF$WONINGTYPEID[DF$WONINGTYPEID == 6] <- "RIJ" # rijhuis
DF$WONINGTYPEID[DF$WONINGTYPEID == 7] <- "RIJ" # rijhuis hoek 
DF$WONINGTYPEID[DF$WONINGTYPEID == 8] <- "RIJ"  # rijhuis eind
DF$WONINGTYPEID[DF$WONINGTYPEID == 51] <- "STAPEL" # penthouse 
DF$WONINGTYPEID<- as.factor(DF$WONINGTYPEID)

# add dates as days
dates <- as.data.frame(seq(as.Date("2010-01-01"), as.Date("2019-12-31"), by = "days")) #begin bij 2010?
datescount <- seq(1:nrow(dates))
dates <- cbind(dates, datescount/365) # normalize by dividing through # days/year)
names(dates) = c("DATUM", "datescount")
#create right Date format in all our dataframes
DF$DATUM <- lubridate::dmy(DF$DATUM)
DF <- merge(DF, dates, by="DATUM")

# Select variables to be used 
DF.use <- dplyr::select(DF,"WONINGTYPEID","KAVOPP","WONINGOPP","WONINGINH","GARAGE","BERGING","MONUMENT","datescount","AGE","BUURTCODE")

#### Create numerical DF based on cleaned data ####
DF.dum <- fastDummies::dummy_cols(DF.use, remove_first_dummy = F,remove_selected_columns = T)

# before splitting the data, add column with 0 non-bunk, 1 bunk
DF.dum$DUM_STAPEL <- ifelse(DF.dum$WONINGTYPEID_STAPEL == "1", 1, 0)

# split data frame based on appartments or not
DF.dum.ap <- filter(DF.dum, WONINGTYPEID_STAPEL == "1")
DF.dum.nap <- filter(DF.dum, WONINGTYPEID_STAPEL != "1")



