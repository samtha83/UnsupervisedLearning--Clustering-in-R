#set path where all files are kept
setwd("C:/Users/Samtha Reddy/Dropbox (Personal)/Copy of Work/R Work/Marketing Mix Model")

#load budget file
media_rawdf <- read.csv("RawData_CPSMediaBudget.csv", header = TRUE) #2806 media records

#load inquiry file
inquiry_rawdf <- read.csv("CPS_InquiryData.csv",header= TRUE,  na.strings = c("", "NA")) #11529 inquiries


dmazip_df <- read.csv("DMA-ZIP.csv", header= TRUE)
#rename the colnames in new df to match the train.data dataframe else predict() will not work
names(dmazip_df)[names(dmazip_df) == "DMA.CODE"] <- "DMA_CODE"
names(dmazip_df)[names(dmazip_df) == "DMA.NAME"] <- "DMA_NAME"



#function to check if packages installed or not and if not install it.
#Then loads all the list of packages, use require to
#check if package installed then only load it or else give a warning .
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#packages to be installed and loaded
packages<-c("sqldf", "gsubfn", "proto", "RSQLite", "dplyr", "caret","lattice","ggplot2","minpack.lm","gridExtra")

check.packages(packages)


# rename the colnames in dataframe with "." as SQL command do not work with "." if in col names

names(media_rawdf)[names(media_rawdf) == "Cost.Digital"] <- "Cost_Digital"
names(media_rawdf)[names(media_rawdf) == "Cost.TV"] <- "Cost_TV"
names(media_rawdf)[names(media_rawdf) == "Cost.Radio"] <- "Cost_Radio"
names(media_rawdf)[names(media_rawdf) == "Cost.Billiboard"] <- "Cost_Billboard"
names(media_rawdf)[names(media_rawdf) == "Cost.Open.House"] <- "Cost_OpenHouse"
names(media_rawdf)[names(media_rawdf) == "Cost.outdoor"] <- "Cost_Outdoor"


# removing international students by removing records where State column = ""
inquiry_rawdf_nonstate_index <- which(is.na(inquiry_rawdf$state))

length(inquiry_rawdf_nonstate_index) #3935 international records

inquiry_rawdf_usstate <- inquiry_rawdf[-inquiry_rawdf_nonstate_index, ] #7594 us state inquiries


#remove spaces in location column in  media file
media_rawdf$Location <- trimws(media_rawdf$Location)


#chnaging name of some locations in media file
media_rawdf$Location[media_rawdf$Location=="Fort Wayne/Huntington/ Kendallville"] <-"Fort Wayne"
media_rawdf$Location[media_rawdf$Location=="Cincy/Northern KY"] <-"Cincinnati"


unique(media_rawdf$Location)# 11 key media locations



#------------------------------------------------------------------------------------

# using subset function - trying to find DMA code for all key 13 locations 
#newdata <- unqiue(subset(dmazip_df, DMA_NAME in ("LOUISVILLE","CINCINNATI"), select=c(DMA_NAME, DMA_CODE)))

#unique(dmazip_df$DMA_CODE[dmazip_df$DMA_NAME=="LOUISVILLE"])
#unique(dmazip_df$DMA_CODE[dmazip_df$DMA_NAME=="CINCINNATI"])
#unique(dmazip_df$DMA_CODE[dmazip_df$DMA_NAME=="EVANSVILLE"])
#unique(dmazip_df$DMA_CODE[dmazip_df$DMA_NAME=="FORT WAYNE"])
#unique(dmazip_df$DMA_CODE[dmazip_df$DMA_NAME=="INDIANAPOLIS"])
#unique(dmazip_df$DMA_CODE[dmazip_df$DMA_NAME=="KOKOMO"])
#unique(dmazip_df$DMA_CODE[dmazip_df$DMA_NAME=="NW IN"])
#----------------------------------------------------------------------------

#fixing inquiries which have no zipcode
inquiry_rawdf_usstate$city <- (trimws(inquiry_rawdf_usstate$city))
inquiry_rawdf_usstate$state <- (trimws(inquiry_rawdf_usstate$state))


#download us inquiries file once and fix it and reload hence to be done only once
#write.csv(inquiry_rawdf_usstate,"inquiry_rawdf_usstate1.csv") 

# fix all missing zipcodes and reload it back , make sure to remove first extra column from csv before loading it.remove the one "Canada" inquiry
inquiry_rawdf_usstate_updated<- read.csv("inquiry_rawdf_usstate.csv",header= TRUE,  na.strings = c("", "NA")) #7593 records
#check 
which(is.na(inquiry_rawdf_usstate_updated$zip))# this should return zero
#DMA Mapping

#create unquie dma-zip dataframe for loolup purposes
lookup_DMA <-unique(dmazip_df)

#adding DMA to inquiry based on DMA-zipcode file
inquiry_rawdf_usstate_updated <- sqldf("select i.*,l.DMA_CODE  from inquiry_rawdf_usstate_updated  i left join lookup_DMA l ON i.zip = l.ZIPCODE ")
str(inquiry_rawdf_usstate_updated )
missing_dma<-subset(inquiry_rawdf_usstate_updated, is.na(DMA_CODE),select=c(city, state, zip)) #  inquiries dont have dma code mapped out, it seems issue with their zipcode- so download fix their zipcodes
#do this only once
#write.csv(inquiry_rawdf_usstate_updated, "inquiry_rawdf_usstate_updated1.csv")

inquiry_rawdf_usstate_updated2<- read.csv("inquiry_rawdf_usstate_updated.csv",header= TRUE,  na.strings = c("", "NA"))
str(inquiry_rawdf_usstate_updated2)
#str(inquiry_rawdf_usstate_updated2)
trimws(inquiry_rawdf_usstate_updated2$zip)
inquiry_rawdf_usstate_updated2<-inquiry_rawdf_usstate_updated2[,1:24]# drop DMA_Code  column #7592 records

#Do re adding DMA to inquiry based on DMA-zipcode file after fixing zipcodes
inquiry_rawdf_usstate_updated2 <- sqldf("select i.*,l.DMA_CODE  from inquiry_rawdf_usstate_updated2  i left join lookup_DMA l ON i.zip = l.ZIPCODE ")#7592 records
missing_dma2<-subset(inquiry_rawdf_usstate_updated2, is.na(DMA_CODE),select=c(city, state, zip, DMA_CODE)) # still 5 records no DMA mapped
#newdata <- unqiue(subset(dmazip_df, DMA_NAME in ("LOUISVILLE","CINCINNATI"), select=c(DMA_NAME, DMA_CODE)))



# using subset function - trying to find DMA code for all key 13 locations 
#newdata <- unqiue(subset(dmazip_df, DMA_NAME in ("LOUISVILLE","CINCINNATI"), select=c(DMA_NAME, DMA_CODE)))
dma_formedia <- data.frame("DMA_City" = c('Louisville','Cincinnati','Evansville','Fort Wayne','Indianapolis','Lafayette','Munster','South Bend',	'Chicago','NW IN','Detroit'), "DMA_CODE" = c('529','515','649','509','527','582','602','588',	'602','602','505'))
media_rawdf<- sqldf("select i.*,l.DMA_Code from media_rawdf i left join dma_formedia  l ON i.location = l.DMA_City ")
#media_rawdf<-media_rawdf[,1:20]
which(is.na(media_rawdf$DMA_CODE))# should return zero

#write.csv(inquiry_rawdf_usstate_updated2,"inquiry_rawdf_usstate_updated2.csv")
# now map inquiries to above key 9 DMA locations 
dma_formedia_lookup <- as.data.frame(unique(dma_formedia$DMA_CODE))
names(dma_formedia_lookup)[names(dma_formedia_lookup) == "unique(dma_formedia$DMA_CODE)"] <- "MEDIA_DMA_CODE"
Inquirydf_media_dma<-sqldf("SELECT i.*, m.MEDIA_DMA_CODE 
                           FROM inquiry_rawdf_usstate_updated2 i left JOIN dma_formedia_lookup  m
                           ON i.DMA_CODE = m.MEDIA_DMA_CODE") #HAS TO BE 7592 records ONLY



length(which(is.na(Inquirydf_media_dma$MEDIA_DMA_CODE)))# 508 inquiries could not be mapped to key 9 DMAs 
#-- downloaded this file and manually changed to map 12 key DMA locations where MEDIA_DMA_CODE = NA, and removed 2016 data + 2017 and its all weeknum less than = 25

#write.csv(Inquirydf_media_dma,"Inquirydf_media_dma1.csv") 

#now reload the fixed file
Inquirydf_media_dma_updated<- read.csv("Inquirydf_media_dma1.csv",header= TRUE,  na.strings = c("", "NA")) #still  82 inquiries could not be mapped to key dma , total rec=3062

#now re do the mapping and create a new column to MEDIA_DMA_CODE2 which has only inquiry dma's mapped to key dma
str(Inquirydf_media_dma_updated)
Inquirydf_media_dma_updated2<-sqldf("SELECT i.*, m.MEDIA_DMA_CODE as MEDIA_DMA_CODE2 
                           FROM Inquirydf_media_dma_updated i left JOIN dma_formedia_lookup  m
                           ON i.DMA_CODE = m.MEDIA_DMA_CODE")
str(Inquirydf_media_dma_updated2)

media_dma_mismatch<-which(is.na(Inquirydf_media_dma_updated2$MEDIA_DMA_CODE2))
length(media_dma_mismatch) #82

Inquirydf_media_dma_updated3 <- Inquirydf_media_dma_updated2[-media_dma_mismatch,] #2980

#dropping on hold records from old digital sheet
#write.csv(media_rawdf,"media_rawdf.csv") --download and remove the on hold records- 764, left - 2806


#media_rawdf_updated <- read.csv("media_rawdf.csv", header = TRUE)

##################################################################################################################################
#grouping all the cost of all mediums by respective week for each year
MediaCostdf_yrwk <- sqldf('select  Year , WeekNum, sum(Cost) as TotalCost, 
                     sum(Cost_Digital) as DigitalCost,
                     sum(Cost_TV) as TVCost, 
                     sum(Cost_Radio) as RadioCost,
                     sum(Cost_Billboard) as BillboardCost,
                     sum(Cost_OpenHouse) as OpenHouseCost,
                     sum(Cost_Outdoor) as OutdoorCost
                     
                     from  media_rawdf group by Year,WeekNum')


#grouping all the inquiries of all mediums by respective week for each year
#Inquirydf_yrwk<-sqldf('select  Year, WeekNum,count(*) as Inquiries from Inquirydf_media_dma_updated2 group by Year,WeekNum') #non matching dma's not removed here as we are mapping to Inquirydf_media_dma_updated2 instead of Inquirydf_media_dma_updated3
Inquirydf_yrwk<-sqldf('select  Year, WeekNum,count(*) as Inquiries from Inquirydf_media_dma_updated2 group by Year,WeekNum') #non matching dma's not removed here as we are mapping to Inquirydf_media_dma_updated2 instead of Inquirydf_media_dma_updated3



#combining inquiries and medium cost for the respective weeks in a single dataframe , in that process we loose some data from cost as inquiry only till june 2018 but budget is till sep 2018

df_yrwk <-sqldf("SELECT m.Year, m.WeekNum , m.TotalCost,m.DigitalCost, m.TVCost,m.RadioCost,m.BillboardCost,
                m.OpenHouseCost, m.OutdoorCost, i.Inquiries
                FROM Inquirydf_yrwk i JOIN MediaCostdf_yrwk m
                ON i.Year = m.Year
                AND i.WeekNum = m.WeekNum")

df_yrwk[is.na(df_yrwk)] <- 0
boxplot(df_yrwk$Inquiries)

#Reallocating budget  data to respective week based on 
#logic budget for week = 50%of budget of that week + 
#25% budget of previous week + 25% budget of previous 2 weeks 

df_yrwk$Estimated_DigitalCost <- df_yrwk$DigitalCost*(0.75)+ lag(df_yrwk$DigitalCost, default = 0 )*(.25) 
df_yrwk$Estimated_BillboardCost<- df_yrwk$BillboardCost*(0.5) + lag(df_yrwk$BillboardCost, default = 0 )*(.25) + lag(df_yrwk$BillboardCost,default = 0, 2 )*(.25)
df_yrwk$Estimated_TVCost <- df_yrwk$TVCost*(0.5) +lag(df_yrwk$TVCost,default = 0 )*(.25)+ lag(df_yrwk$TVCost,default = 0,2 )*(.25)
df_yrwk$Estimated_RadioCost <- df_yrwk$RadioCost*(0.5) + lag(df_yrwk$RadioCost , default = 0)*(.25) + lag(df_yrwk$RadioCost, default = 0,2 )*(.25)
df_yrwk$Estimated_OpenHouseCost<- df_yrwk$OpenHouseCost*(0.5) + lag(df_yrwk$OpenHouseCost , default = 0)*(.25) + lag(df_yrwk$OpenHouseCost, default = 0,2 )*(.25)
df_yrwk$Estimated_OutdoorCost<- df_yrwk$OutdoorCost*(0.5) + lag(df_yrwk$OutdoorCost , default = 0)*(.25) + lag(df_yrwk$OutdoorCost, default = 0,2 )*(.25)

df_yrwk[is.na(df_yrwk)] <- 0



#Reallocating inquiries data to respective week based on logic Inquiry for week = 50%of inquiry of that week + 25% inquiry of previous week + 25% inquiry of previous 2 weeks

df_yrwk$Estimated_Inquiries <- df_yrwk$Inquiries*(0.5) +  lag(df_yrwk$Inquiries,default = 0 )*(.25)+ lag(df_yrwk$Inquiries, 2 , default = 0)*(.25)


head(df_yrwk)
#tail(df_yrwk)
str(df_yrwk)

##############################################################################################################################

###############################################DMA mapped####################################################



MediaCostdf_yrwkdma <- sqldf('select  Year , WeekNum,DMA_CODE, sum(Cost) as TotalCost, 
                             sum(Cost_Digital) as DigitalCost,
                             sum(Cost_TV) as TVCost, 
                             sum(Cost_Radio) as RadioCost,
                             sum(Cost_Billboard) as BillboardCost,
                             sum(Cost_OpenHouse) as OpenHouseCost,
                             sum(Cost_Outdoor) as OutdoorCost
                             from media_rawdf  group by Year,WeekNum,DMA_CODE') # 471 records


#grouping all the inquiries of all mediums by respective week for each year, and each DMA
Inquirydf_yrwkdma<-sqldf('select  Year, WeekNum,MEDIA_DMA_CODE2,count(*) as Inquiries from Inquirydf_media_dma_updated3 group by Year,WeekNum, MEDIA_DMA_CODE2') # 397 record




df_yrwk_dma <-sqldf("SELECT i.Year, i.WeekNum , m.TotalCost,m.DigitalCost, m.TVCost,m.RadioCost,m.BillboardCost,
                   m.OpenHouseCost, m.OutdoorCost, i.Inquiries,i.MEDIA_DMA_CODE2 as inquirydma, m.DMA_CODE as mediadma
                   FROM Inquirydf_yrwkdma i left JOIN MediaCostdf_yrwkdma m
                   ON i.Year = m.Year
                   AND i.WeekNum = m.WeekNum
                   AND i.MEDIA_DMA_CODE2 = m.DMA_CODE")# 397 recds

#head(df_yrwk_dma)
#tail(df_yrwk_dma)
#str(df_yrwk_dma)
df_yrwk_dma[is.na(df_yrwk_dma)] <- 0


# check
sum(df_yrwk_dma$Inquiries) # has to match count of Inquirydf_media_dma_updated3 i.e 2980





#P4 <- ggplot(df_yrwk_dma, aes(x = DigitalCost, y = Inquiries)) +   geom_line(color="red")  

#P5 <- ggplot(df_yrwk_dma, aes(x = TVCost, y = Inquiries)) +   geom_line(color="blue")  

#P6 <- ggplot(df_yrwk_dma, aes(x = RadioCost, y = Inquiries)) +    geom_line(color="green") 

#P7 <- ggplot(df_yrwk_dma, aes(x = BillboardCost, y = Inquiries)) +   geom_line(color="black")  


#grid.arrange(P4,P5,P6,P7,ncol = 2,nrow=2)

#ggplot(df_yrwk_all, aes(x = TotalCost, y = Inquiries)) +   geom_line(color="black")  


#Reallocating budget  data to respective week based on 
#logic budget for week = 50%of budget of that week + 
#25% budget of previous week + 25% budget of previous 2 weeks on data mapped to key DMA


df_yrwk_dma$Estimated_DigitalCost <- df_yrwk_dma$DigitalCost*(0.75)+ lag(df_yrwk_dma$DigitalCost, default = 0 )*(.25) 
df_yrwk_dma$Estimated_BillboardCost<- df_yrwk_dma$BillboardCost*(0.5) + lag(df_yrwk_dma$BillboardCost, default = 0 )*(.25) + lag(df_yrwk_dma$BillboardCost,default = 0, 2 )*(.25)
df_yrwk_dma$Estimated_TVCost <- df_yrwk_dma$TVCost*(0.5) +lag(df_yrwk_dma$TVCost,default = 0 )*(.25)+ lag(df_yrwk_dma$TVCost,default = 0,2 )*(.25)
df_yrwk_dma$Estimated_RadioCost <- df_yrwk_dma$RadioCost*(0.5) + lag(df_yrwk_dma$RadioCost , default = 0)*(.25) + lag(df_yrwk_dma$RadioCost, default = 0,2 )*(.25)
df_yrwk_dma$Estimated_OpenHouseCost<- df_yrwk_dma$OpenHouseCost*(0.5) + lag(df_yrwk_dma$OpenHouseCost , default = 0)*(.25) + lag(df_yrwk_dma$OpenHouseCost, default = 0,2 )*(.25)
df_yrwk_dma$Estimated_OutdoorCost<- df_yrwk_dma$OutdoorCost*(0.5) + lag(df_yrwk_dma$OutdoorCost , default = 0)*(.25) + lag(df_yrwk_dma$OutdoorCost, default = 0,2 )*(.25)


#Reallocating inquiries data to respective week based on logic Inquiry for week = 50%of inquiry of that week + 25% inquiry of previous week + 25% inquiry of previous 2 weeks

df_yrwk_dma$Estimated_Inquiries <- df_yrwk_dma$Inquiries*(0.5) +  lag(df_yrwk_dma$Inquiries,default = 0 )*(.25)+ lag(df_yrwk_dma$Inquiries, 2 , default = 0)*(.25)


#----------------training the model with DMA----------------

score_v4_DMA.01 = list()
scoretrain_v4_DMA.01 = list()
number_v4_DMA.01 <- 0
bestscore_v4_DMA.01 <-100

score_v4_DMA.02 = list()
scoretrain_v4_DMA.02 = list()
number_v4_DMA.02 <- 0
bestscore_v4_DMA.02 <-100

score_v4_DMA.03 = list()
scoretrain_v4_DMA.03 = list()
number_v4_DMA.03 <- 0
bestscore_v4_DMA.03 <-100

score_v4_DMA.04 = list()
scoretrain_v4_DMA.04 = list()
number_v4_DMA.04 <- 0
bestscore_v4_DMA.04 <-100


score_v4_DMA.05 = list()
scoretrain_v4_DMA.05 = list()
number_v4_DMA.05 <- 0
bestscore_v4_DMA.05 <-100
for(i in 1:100)
{
#create indexes for train data
train.index_v4_DMA <- createDataPartition(df_yrwk_dma$Inquiries, p = 0.8, list = FALSE)

#create train data
train.data_v4_DMA  <- df_yrwk_dma[train.index_v4_DMA, ]

#create test data
test.data_v4_DMA<- df_yrwk_dma[-train.index_v4_DMA, ]

#model without adstock and without take changes in media budget/Inquiries

modFitv4_DMA.01<- lm(Inquiries~DigitalCost +  TVCost +RadioCost+ BillboardCost ,  data =train.data_v4_DMA) 
# Make predictions
predictionsv4_DMA.01 <- modFitv4_DMA.01 %>% predict(test.data_v4_DMA)
# Model performance # (a) Prediction error, RMSE
score_v4_DMA.01[i] <- RMSE(predictionsv4_DMA.01, test.data_v4_DMA$Inquiries)

if(as.numeric(score_v4_DMA.01[i]) < as.numeric(bestscore_v4_DMA.01 ))
{
  bestscore_v4_DMA.01  = score_v4_DMA.01[i]
  bestmodel_v4_DMA.01 <- modFitv4_DMA.01
  number_v4_DMA.01 <- i
  predictionsv4_DMA.01_train <- modFitv4_DMA.01 %>% predict(train.data_v4_DMA)
  scoretrain_v4_DMA.01[i]<- RMSE(predictionsv4_DMA.01_train ,  train.data_v4_DMA$Inquiries)
  maev4_DMA.01 <- abs(mean((predictionsv4_DMA.01- test.data_v4_DMA$Inquiries))) #Mean absolute error of test data
}


modFitv4_DMA.02 <-lm(Inquiries~Estimated_DigitalCost + Estimated_BillboardCost + Estimated_TVCost + Estimated_RadioCost ,  data =train.data_v4_DMA)
predictionsv4_DMA.02 <- modFitv4_DMA.02 %>% predict(test.data_v4_DMA)
# Model performance # (a) Prediction error, RMSE
score_v4_DMA.02[i] <- RMSE(predictionsv4_DMA.02, test.data_v4_DMA$Inquiries)
if(as.numeric(score_v4_DMA.02[i]) < as.numeric(bestscore_v4_DMA.02 ))
{
  bestscore_v4_DMA.02  = score_v4_DMA.02[i]
  bestmodel_v4_DMA.02 <- modFitv4_DMA.02
  number_v4_DMA.02 <- i
  predictionsv4_DMA.02_train <- modFitv4_DMA.02 %>% predict(train.data_v4_DMA)
  scoretrain_v4_DMA.02[i]<- RMSE(predictionsv4_DMA.02_train ,  train.data_v4_DMA$Inquiries)
  maev4_DMA.02 <- abs(mean((predictionsv4_DMA.02-test.data_v4_DMA$Inquiries))) #Mean absolute error
}





modFitv4_DMA.03 <-lm(Estimated_Inquiries~DigitalCost + BillboardCost + TVCost +RadioCost,  data =train.data_v4_DMA)
predictionsv4_DMA.03 <- modFitv4_DMA.03 %>% predict(test.data_v4_DMA)
# Model performance # (a) Prediction error, RMSE
score_v4_DMA.03[i] <- RMSE(predictionsv4_DMA.03, test.data_v4_DMA$Inquiries)
if(as.numeric(score_v4_DMA.03[i]) < as.numeric(bestscore_v4_DMA.03))
{
  bestscore_v4_DMA.03  = score_v4_DMA.03[i]
  bestmodel_v4_DMA.03 <- modFitv4_DMA.03
  number_v4_DMA.03 <- i
  predictionsv4_DMA.03_train <- modFitv4_DMA.03 %>% predict(train.data_v4_DMA)
  scoretrain_v4_DMA.03[i]<- RMSE(predictionsv4_DMA.03_train ,  train.data_v4_DMA$Inquiries)
  maev4_DMA.03 <- abs(mean((predictionsv4_DMA.03 - test.data_v4_DMA$Inquiries))) #Mean absolute error
}


modFitv4_DMA.04 <-lm(Estimated_Inquiries~Estimated_DigitalCost + Estimated_BillboardCost + Estimated_TVCost + Estimated_RadioCost ,  data =train.data_v4_DMA)
predictionsv4_DMA.04 <- modFitv4_DMA.04 %>% predict(test.data_v4_DMA)
# Model performance # (a) Prediction error, RMSE
score_v4_DMA.04[i] <- RMSE(predictionsv4_DMA.04, test.data_v4_DMA$Inquiries)
if(as.numeric(score_v4_DMA.04[i]) < as.numeric(bestscore_v4_DMA.04))
{
  bestscore_v4_DMA.04  = score_v4_DMA.04[i]
  bestmodel_v4_DMA.04 <- modFitv4_DMA.04
  number_v4_DMA.04 <- i
  predictionsv4_DMA.04_train <- modFitv4_DMA.04 %>% predict(train.data_v4_DMA)
  scoretrain_v4_DMA.04[i]<- RMSE(predictionsv4_DMA.04_train ,  train.data_v4_DMA$Inquiries)
  maev4_DMA.04 <- abs(mean((predictionsv4_DMA.04- test.data_v4_DMA$Inquiries))) #Mean absolute error
}


modFitv4_DMA.05  <- lm(Inquiries~DigitalCost +  TVCost +RadioCost+ BillboardCost+ factor(inquirydma),data =train.data_v4_DMA)
predictionsv4_DMA.05 <- predict(modFitv4_DMA.05 ,test.data_v4_DMA)
# Model performance # (a) Prediction error, RMSE
score_v4_DMA.05[i] <- RMSE(predictionsv4_DMA.05, test.data_v4_DMA$Inquiries)
if(as.numeric(score_v4_DMA.05[i]) < as.numeric(bestscore_v4_DMA.05))
{
  bestscore_v4_DMA.05  = score_v4_DMA.05[i]
  bestmodel_v4_DMA.05 <- modFitv4_DMA.05
  number_v4_DMA.05 <- i
  predictionsv4_DMA.05_train <- modFitv4_DMA.05 %>% predict(train.data_v4_DMA)
  scoretrain_v4_DMA.05[i]<- RMSE(predictionsv4_DMA.05_train ,  train.data_v4_DMA$Inquiries)
  maev4_DMA.05 <- abs(mean((predictionsv4_DMA.05- test.data_v4_DMA$Inquiries))) #Mean absolute error
}


rm(train.data_v4_DMA)
rm(test.data_v4_DMA)


}


#################################### Analysing each of the 4 models ##################################################################################



###############################################################       modFitv4_DMA.01        ##############################################################################

print(bestscore_v4_DMA.01) 
print(number_v4_DMA.01)
summary(bestmodel_v4_DMA.01)
scoretrain_v4_DMA.01[number_v4_DMA.01]


predictions_full_01 <- predict(bestmodel_v4_DMA.01, df_yrwk_dma)
RMSE(predictions_full_01, df_yrwk_dma$Inquiries)
mean((df_yrwk_dma$Inquiries-predictions_full_01)) #Mean absolute error
###############################################################       modFitv4_DMA.02        ##############################################################################

print(bestscore_v4_DMA.02) 
print(number_v4_DMA.02)
summary(bestmodel_v4_DMA.02)
scoretrain_v4_DMA.02[number_v4_DMA.02]

predictions_full_02 <- predict(bestmodel_v4_DMA.02, df_yrwk_dma)
RMSE(predictions_full_02, df_yrwk_dma$Inquiries)
mean((df_yrwk_dma$Inquiries-predictions_full_02)) #Mean absolute error
###############################################################       modFitv4_DMA.03        ##############################################################################

print(bestscore_v4_DMA.03) 
print(number_v4_DMA.03)
summary(bestmodel_v4_DMA.03)
scoretrain_v4_DMA.03[number_v4_DMA.03]


predictions_full_03 <- predict(bestmodel_v4_DMA.03, df_yrwk_dma)
RMSE(predictions_full_03, df_yrwk_dma$Inquiries)
mean((df_yrwk_dma$Inquiries-predictions_full_03)) #Mean absolute error

###############################################################       modFitv4_DMA.04        ##############################################################################
print(bestscore_v4_DMA.04) 
print(number_v4_DMA.04)
summary(bestmodel_v4_DMA.04)
scoretrain_v4_DMA.04[number_v4_DMA.04]


predictions_full_04 <- predict(bestmodel_v4_DMA.04, df_yrwk_dma)
RMSE(predictions_full_04, df_yrwk_dma$Inquiries)
mean((df_yrwk_dma$Inquiries-predictions_full_04)) #Mean absolute error
mean((df_yrwk_dma$Estimated_Inquiries-predictions_full_04))
##############################################################################################################################################
print(bestscore_v4_DMA.05) 
print(number_v4_DMA.05)
summary(bestmodel_v4_DMA.05)
scoretrain_v4_DMA.04[number_v4_DMA.05]


predictions_full_05 <- predict(bestmodel_v4_DMA.05, df_yrwk_dma)
RMSE(predictions_full_05, df_yrwk_dma$Inquiries)
mean((df_yrwk_dma$Inquiries-predictions_full_05)) #Mean absolute error
#######################################################################################################################################
#using the best model to predict entire dataset

predictions_full <- predict(bestmodel, df_yrwk_dma)
error_per <- sqrt(mean((predictions_full-df_yrwk_dma$Inquiries)^2))
mae <- mean((df_yrwk_dma$Inquiries-predictions_full)) #Mean absolute error
mse <- mean((df_yrwk_dma$Inquiries-predictions_full)^2)
mse_baseline <- mean((df_yrwk_dma$Inquiries-mean(predictions_full))^2)
RMSE(predictions_full,df_yrwk_dma$Inquiries)
r2 <- 1- (mse/mse_baseline) # R² is the ratio between how good our model is vs how good is the naive mean model.The maximum value of R² is 1 but minimum can be minus infinity.
boxplot(df_yrwk_dma$Inquiries)
summary(df_yrwk_dma$Inquiries)





##################################visualization#########################################################################################
comparison_tbl <- data.frame( "Inquiries"=df_yrwk_dma$Inquiries, "predicted" =predictions_full_01, "residual"=(predictions_full-df_yrwk_dma$Inquiries),
                              DigitalCost=df_yrwk_dma$DigitalCost,TVCost=df_yrwk_dma$TVCost, RadioCost=df_yrwk_dma$RadioCost, BillboardCost=df_yrwk_dma$BillboardCost)





# plotting the observerd/actual inquiries
ggplot(comparison_tbl, aes(x = DigitalCost, y = Inquiries)) +  # Set up canvas with outcome variable on y-axis
  geom_point()  # Plot the actual points


ggplot(comparison_tbl, aes(x = DigitalCost, y = Inquiries)) +  # Set up canvas with outcome variable on y-axis
  geom_point() +
geom_point(aes(y = predicted), shape = 2)  # Add the predicted values


#connect  actual data points with their corresponding predicted value using geom_segment():
ggplot(comparison_tbl, aes(x = DigitalCost, y = Inquiries)) +
  geom_smooth(method = "lm", se = FALSE, color = "green") + 
  geom_segment(aes(xend = DigitalCost, yend = predicted), alpha = .2) +# alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 2)+
  theme_bw()  # Add theme for cleaner look

#residuals

ggplot(comparison_tbl, aes(x = DigitalCost, y = Inquiries)) +
  geom_smooth(method = "lm", se = FALSE, color = "green") + 
  geom_segment(aes(xend = DigitalCost, yend = predicted), alpha = .2) +# alpha to fade lines
geom_point(aes(color  = abs(residual))) +  # Alpha mapped to abs(residuals)
  scale_color_gradient2(low = "blue", mid = "orange", high = "red") + 
  guides(color = FALSE) +  # color legend removed

  geom_point(aes(y = predicted), shape = 2) +
  theme_bw()


ggplot(comparison_tbl, aes(x = DigitalCost, y = Inquiries)) +
  geom_segment(aes(xend = DigitalCost, yend = predicted)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 2)


ggplot(comparison_tbl, aes(x = DigitalCost, y = Inquiries)) +
  #geom_segment(aes(xend = DigitalCost, yend = predicted), alpha = .2) +  # Lines to connect points
  geom_point() +  # Points of actual values
  geom_point(aes(y = predicted), shape = 2, color ="red") +  # Points of predicted values
  theme_bw()





ggplot(comparison_tbl, aes(x = DigitalCost, y = Inquiries)) +
  geom_segment(aes(xend = DigitalCost, yend = predicted), alpha = .2) +  # Lines to connect points
  geom_point(aes(color = residual)) +
  scale_color_gradient2(low = "blue", mid = "dark green", high = "red") +
  guides(color = FALSE) +
  #geom_point() +  # Points of actual values
  geom_point(aes(y = predicted), shape = 2, color ="black") +  # Points of predicted values
  theme_bw()















