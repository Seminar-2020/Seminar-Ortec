library(dplyr)
#### DATA ####
# make sure to load Ortec's latest data-set, should be 1091799 observations
dataset <- read_delim("dataset.csv", "|", escape_double = FALSE, trim_ws = TRUE)
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
DF$WONINGTYPEID[DF$WONINGTYPEID == 9] <- "FLAT" #FLAT
DF$WONINGTYPEID[DF$WONINGTYPEID == 10 ] <- "FLAT" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 11 ] <- "FLAT" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 12 ] <- "FLAT" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 13 ] <- "FLAT" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 14 ] <- "FLAT" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 15] <- "FLAT" #FLAt
DF$WONINGTYPEID[DF$WONINGTYPEID == 1] <- "VRIJSTAAND"
DF$WONINGTYPEID[DF$WONINGTYPEID == 4] <-"VRIJSTAAND"
DF$WONINGTYPEID[DF$WONINGTYPEID == 2] <- "HALF"  # HALF TUSSEN 1 BUURTJE 
DF$WONINGTYPEID[DF$WONINGTYPEID == 3] <-  "HALF"  # HALF TUSSEN 1 BUURTJE 
DF$WONINGTYPEID[DF$WONINGTYPEID == 5] <-  "HALF"  # HALF TUSSEN 1 BUURTJE 
DF$WONINGTYPEID[DF$WONINGTYPEID == 6] <- "RIJ" #rijhuis
DF$WONINGTYPEID[DF$WONINGTYPEID == 7] <- "RIJ" #rijhuis eind
DF$WONINGTYPEID[DF$WONINGTYPEID == 8] <- "RIJ"  #rijhuis eind
DF$WONINGTYPEID[DF$WONINGTYPEID == 51] <- "PENT" #penthouse 
DF$WONINGTYPEID<- as.factor(DF$WONINGTYPEID)
#Filter out the ksom house values less than 50.000 
DF  <- DF %>%  
  filter(WAARDE > 50000 ) %>% 
  filter(WAARDE < 6000000) %>% #filter out houses with houseprice > 60mil
  filter(KAVOPP < 7000) %>%   #filter extreme large kavOPP 
  filter(!(WONINGTYPEID=='RIJ' & KAVOPP>400)) %>% #filter large KAVOPP values for type 'RIJ'
  filter(BOUWJAAR <= 2019) %>% #filter bouwjaar < 2019
  filter(AGE<=216) %>% # house cannot be built more than 216 years ago
  filter(WONINGID!=3421281) # aberrant observation, not truly "VRIJSTAAND"

