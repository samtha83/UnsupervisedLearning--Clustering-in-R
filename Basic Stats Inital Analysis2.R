#########################################################################################################

#set path where all files are kept
setwd("C:/Users/Samtha Reddy/Dropbox (Personal)/Copy of Work/R Work/Marketing Mix Model")

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


#load budget file
media_rawdf <- read.csv("RawData_CPSMediaBudget.csv", header = TRUE) #2806 media records, 20 variables

#load inquiry file
inquiry_rawdf <- read.csv("CPS_InquiryData.csv",header= TRUE,  0) #11529 inquiries, 23 variables



# rename the colnames in dataframe with "." as SQL command do not work with "." if in col names

names(media_rawdf)[names(media_rawdf) == "Cost.Digital"] <- "Cost_Digital"
names(media_rawdf)[names(media_rawdf) == "Cost.TV"] <- "Cost_TV"
names(media_rawdf)[names(media_rawdf) == "Cost.Radio"] <- "Cost_Radio"
names(media_rawdf)[names(media_rawdf) == "Cost.Billiboard"] <- "Cost_Billboard"
names(media_rawdf)[names(media_rawdf) == "Cost.Open.House"] <- "Cost_OpenHouse"
names(media_rawdf)[names(media_rawdf) == "Cost.outdoor"] <- "Cost_Outdoor"



#replacing na with 0
#media_rawdf$Cost_Digital[is.na(media_rawdf$Cost_Digital)] <- 0


#media_rawdf$Cost_TV[is.na(media_rawdf$Cost_TV)] <- 0
#media_rawdf$Cost_Radio[is.na(media_rawdf$Cost_Radio)] <- 0
#media_rawdf$Cost_Billboard[is.na(media_rawdf$Cost_Billboard)] <- 0
#media_rawdf$Cost_OpenHouse[is.na(media_rawdf$Cost_OpenHouse)] <- 0
#media_rawdf$Cost_Outdoor[is.na(media_rawdf$Cost_Outdoor)] <- 0



# ------------------Without DMA considerations, not removing any international students ----------------------------------#

#grouping all the cost of all mediums by respective week for each year
MediaCostdf_yrwk <- sqldf('select  Year , WeekNum, sum(Cost) as TotalCost, 
                          sum(Cost_Digital) as DigitalCost,
                          sum(Cost_TV) as TVCost, 
                          sum(Cost_Radio) as RadioCost,
                          sum(Cost_Billboard) as BillboardCost,
                          sum(Cost_OpenHouse) as OpenHouseCost,
                          sum(Cost_Outdoor) as OutdoorCost
                          from  media_rawdf group by Year,WeekNum')


summary(MediaCostdf_yrwk$DigitalCost)
summary(MediaCostdf_yrwk$TVCost)
summary(MediaCostdf_yrwk$RadioCost)
summary(MediaCostdf_yrwk$BillboardCost)
summary(MediaCostdf_yrwk$OpenHouseCost)
summary(MediaCostdf_yrwk$OutdoorCost)
summary(MediaCostdf_yrwk$TotalCost)

#fill all NA's to zero
MediaCostdf_yrwk[is.na(MediaCostdf_yrwk)] <- 0






#grouping all the inquiries of all mediums by respective week for each year
Inquirydf_yrwk_all<-sqldf('select  Year, WeekNum,count(*) as Inquiries from inquiry_rawdf  group by Year,WeekNum') 

mean(Inquirydf_yrwk_all$Inquiries)
summary(Inquirydf_yrwk_all$Inquiries)
#combining inquiries and medium cost for the respective weeks in a single dataframe , in that process we loose some data from cost as inquiry only till june 2018 but budget is till sep 2018

df_yrwk_all <-sqldf("SELECT m.Year, m.WeekNum , m.TotalCost,m.DigitalCost, m.TVCost,m.RadioCost,m.BillboardCost,
                m.OpenHouseCost, m.OutdoorCost, i.Inquiries
                FROM Inquirydf_yrwk_all i JOIN MediaCostdf_yrwk m
                ON i.Year = m.Year
                AND i.WeekNum = m.WeekNum") # 51 obs, 10 variables


summary(df_yrwk_all$Inquiries)

#NOTE- we decide to DROP 25th row #OUTLIEAR
df_yrwk_all <- df_yrwk_all[-1,]


summary(df_yrwk_all$DigitalCost)
summary(df_yrwk_all$TVCost)
summary(df_yrwk_all$RadioCost)
summary(df_yrwk_all$BillboardCost)
summary(df_yrwk_all$OpenHouseCost)
summary(df_yrwk_all$OutdoorCost)
summary(df_yrwk_all$TotalCost)



df_yrwk_all[is.na(df_yrwk_all)] <- 0
#----------general visualizations to understand relations ---

cor(as.numeric(df_yrwk_all$Inquiries), df_yrwk_all$DigitalCost, method ="pearson")
cor(as.numeric(df_yrwk_all$Inquiries), df_yrwk_all$TVCost, method ="pearson")
cor(as.numeric(df_yrwk_all$Inquiries), df_yrwk_all$RadioCost, method ="pearson")
cor(as.numeric(df_yrwk_all$Inquiries), df_yrwk_all$BillboardCost, method ="pearson")

# all methods - pearson, kendall, spearman - don't show any strong relationship 

#df_yrwk_Digital <-sqldf("SELECT m.Year, m.WeekNum , m.TotalCost,m.DigitalCost, i.Inquiries  FROM Inquirydf_yrwk_all i JOIN MediaCostdf_yrwk m  ON i.Year = m.Year    AND i.WeekNum = m.WeekNum  AND m.DigitalCost <> 0 ") # 49 obs, 10 variables 

#df_yrwk_TV <-sqldf("SELECT m.Year, m.WeekNum , m.TotalCost,m.TVCost, i.Inquiries FROM Inquirydf_yrwk_all i JOIN MediaCostdf_yrwk m ON i.Year = m.Year     AND i.WeekNum = m.WeekNum  AND m.TVCost <> 0 ") # 22 obs, 5 variables

#df_yrwk_Radio <-sqldf("SELECT m.Year, m.WeekNum , m.TotalCost,m.RadioCost,   i.Inquiries  FROM Inquirydf_yrwk_all i JOIN MediaCostdf_yrwk m  ON i.Year = m.Year AND i.WeekNum = m.WeekNum AND m.RadioCost <> 0 ") # 22 obs, 5 variables

#df_yrwk_Billboard <-sqldf("SELECT m.Year, m.WeekNum , m.TotalCost,m.BillboardCost, i.Inquiries  FROM Inquirydf_yrwk_all i JOIN MediaCostdf_yrwk m   ON i.Year = m.Year     AND i.WeekNum = m.WeekNum       AND m.BillboardCost <> 0 ")






P1 <- ggplot(df_yrwk_all, aes(x = DigitalCost, y = Inquiries)) +   geom_line(color="red")  

P2 <- ggplot(df_yrwk_all, aes(x = TVCost, y = Inquiries)) +   geom_line(color="blue")  

P3 <- ggplot(df_yrwk_all, aes(x = RadioCost, y = Inquiries)) +    geom_line(color="green") 

P4 <- ggplot(df_yrwk_all, aes(x = BillboardCost, y = Inquiries)) +   geom_line(color="black")  


grid.arrange(P1,P2,P3,P4,ncol = 2,nrow=2)

ggplot(df_yrwk_all, aes(x = TotalCost, y = Inquiries)) +   geom_line(color="black")  




# --------------calculating adstock rate ----------------------------------#



adstock<-function(x,rater=0){
  adstock_val<-as.numeric(stats::filter(x=x,filter=rater,method="recursive"))
  #print(adstock_val)
  return(adstock_val)
}


#----------------running the model----------------------------------------------#


score_v6 = list()
scoretrain_v6 = list()
number_v6 <- 0
bestscore_v6 <-100
bestrate_v6 <- 0

for( i in 1:100)
{
  
  #create indexes for train data
  #create train data
  
  train.index_v6 <- createDataPartition(df_yrwk_all$Inquiries, p = 0.8, list = FALSE)
  train.data_v6  <- df_yrwk_all[train.index_v6, ]
  
  #create test data
  test.data_v6<- df_yrwk_all[-train.index_v6, ]
  
  
  #build model
  
  model_v6_nls <- nlsLM(data= train.data_v6, Inquiries~ b0 + b1 * adstock(DigitalCost,rater) + 
          b2 * adstock(TVCost, rater) +
          b3 * adstock(RadioCost, rater) +
          b4 * adstock(BillboardCost, rater), 
        algorithm = "LM",
        start     = c(b0=1, b1= 1, b2= 1, b3=1, b4=1, rater=0),
        lower     = c(b0=-Inf, b1=-Inf, b2=-Inf,b3=-Inf, b4=-Inf,rater=0),
        upper     = c(b0= Inf, b1= Inf, b2= Inf, b3=Inf, b4=Inf, rater=1))
  
  newrate <- summary(model_v6_nls)$coefficients[6,1]
  #summary(model_v6_nls)
  
  train.data_v6$DigitalCostAd <- adstock(train.data_v6$DigitalCost,newrate)
  train.data_v6$TVCostAd <-adstock(train.data_v6$TVCost,newrate)
  train.data_v6$RadioCostAd <- adstock(train.data_v6$RadioCost, newrate)
  train.data_v6$BillboardCostAd <- adstock(train.data_v6$BillboardCost, newrate)
  train.data_v6[is.na(train.data_v6)] <- 0 
  
  modFit_v6_lm<-lm(Inquiries~DigitalCostAd +
                     TVCostAd +  
                     RadioCostAd  +  
                     BillboardCostAd , 
                      data =train.data_v6)
  
  
  
  #predict(modFit_v5_lm, new_data= train.data_v5, interval = c("confidence"), level = .95)
  #print(summary(model_v5))
  
  #e build the test data
  
  test.data_v6$DigitalCostAd <- adstock(test.data_v6$DigitalCost,newrate)
  test.data_v6$TVCostAd <- adstock(test.data_v6$TVCost,newrate)
  test.data_v6$RadioCostAd <- adstock(test.data_v6$RadioCost,newrate)
  test.data_v6$BillboardCostAd <- adstock(test.data_v6$BillboardCost,newrate)
  test.data_v6[is.na(test.data_v6)] <- 0
  
  
  # Make predictions
  #summary(modFitv4_DMA.08)
  predictionsv6 <-   modFit_v6_lm %>% predict(test.data_v6) 
                            
  # Model performance
  # (a) Prediction error, RMSE
  score_v6[i]=RMSE(predictionsv6, test.data_v6$Inquiries)
  
  
  
  #predictionsv5_train <- model %>% predict(train.data_v4_DMA)
  #scoretrain[i]<- RMSE(predictionsv4_DMA.10_train ,  train.data_v4_DMA$Inquiries)
  if(as.numeric(score_v6[i]) < as.numeric(bestscore_v6))
  {
    bestscore_v6 = score_v6[i]
    bestmodel <- modFit_v6_lm
    number_v6 <- i
    bestrate_v6<- newrate
    
    predictionsv6_train <- modFit_v6_lm %>% predict(train.data_v6)
    scoretrain_v6[i] <- RMSE(predictionsv6_train , train.data_v6$Inquiries)
    maev6_test<- abs(mean((predictionsv6-test.data_v6$Inquiries))) #Mean absolute error
  }
  
  rm(train.data_v6)
  rm(test.data_v6)
}



print(score_v6[number_v6])
print(scoretrain_v6[number_v6])
print(number_v6)
print(bestscore_v6)
print(bestrate_v6)

summary(bestmodel)
#final model



 

#based on best model and bestrate we got we see only digital cost is the best related to inquiries and recreate the model with Adstock digital cost

df_yrwk_all_new <- df_yrwk_all
df_yrwk_all_new$DigitalCostAd <- adstock(df_yrwk_all_new$DigitalCost,bestrate_v6)
df_yrwk_all_new$TVCostAd <-adstock(df_yrwk_all_new$TVCost,bestrate_v6)
df_yrwk_all_new$RadioCostAd <- adstock(df_yrwk_all_new$RadioCost, bestrate_v6)
df_yrwk_all_new$BillboardCostAd <- adstock(df_yrwk_all_new$BillboardCost, bestrate_v6)
df_yrwk_all_new[is.na(df_yrwk_all_new)] <- 0 

predictionsv6_full <- predict(bestmodel,df_yrwk_all_new)
RMSE(predictionsv6_full , df_yrwk_all_new$Inquiries)
mae_v6_full <- abs(mean(predictionsv6_full - df_yrwk_all_new$Inquiries))


# using lm method adstock on digital only  
  
model_final_v6.1<- lm(Inquiries~DigitalCostAd, data=df_yrwk_all_new)
summary(model_final_v6.1)
confint(model_final_v6.1)
predictions_full_v6.1 <-predict(model_final_v6.1, df_yrwk_all_new )

RMSE(predictions_full_v6.1 , df_yrwk_all_new$Inquiries)
mea_full_v6.1 <- abs(mean(predictions_full_v6.1 -  df_yrwk_all_new$Inquiries))


##based on best model and bestrate we got we drop radio cost, and drop billboardCost as it is not signifcant as high p value  and recreate the model with Adstock digital cost
# using lm method adstock on digital, TV only  



model_final_v6.2<- lm(Inquiries~DigitalCostAd + TVCostAd , data=df_yrwk_all_new)
summary(model_final_v6.2)
confint(model_final_v6.2)
predictions_full_v6.2 <-predict(model_final_v6.2, df_yrwk_all_new )

RMSE(predictions_full_v6.2 , df_yrwk_all_new$Inquiries)
mea_full_v6.2 <- abs(mean(predictions_full_v6.2 -  df_yrwk_all_new$Inquiries))
#========================================

par(mfrow = c(2, 2))
plot_bestmodel <- plot(bestmodel)
plot_model_final1 <- plot(model_final1)
plot_model_final2 <- plot(model_final2)

grid.arrange(P4,P5,P6,P7,ncol = 2,nrow=2)
#varaiance data # The variance is a numerical measure of how the data values is dispersed around the mean. 
sd(df_yrwk_all$DigitalCost) 
sd(df_yrwk_all_new$DigitalCostAd)

sd(df_yrwk_all$TVCost) 
sd(df_yrwk_all_new$TVCostAd)

sd(df_yrwk_all$BillboardCost) 
sd(df_yrwk_all_new$DigitalCostAd)

sd(df_yrwk_all$Inquiries)
sd(predictionsv5_full1)
sd(predictionsv5_full2)

##################using nls without adstock############################

model_v7_nls <- nlsLM(data= df_yrwk_all, Inquiries~ b0 + b1 * DigitalCost + 
                        b2 * TVCost +
                        b3 * RadioCost +
                        b4 * BillboardCost, 
                      algorithm = "LM",
                      start     = c(b0=1, b1= 1, b2= 1, b3=1, b4=1),
                      lower     = c(b0=-Inf, b1=-Inf, b2=-Inf,b3=-Inf, b4=-Inf),
                      upper     = c(b0= Inf, b1= Inf, b2= Inf, b3=Inf, b4=Inf))


summary(model_v7_nls)
predictionsv7_full <- predict(model_v7_nls,df_yrwk_all)
RMSE(predictionsv7_full , df_yrwk_all$Inquiries)
mae_v7_full <- abs(mean(predictionsv7_full - df_yrwk_all$Inquiries))

model_v8_nls <- nlsLM(data= df_yrwk_all, Inquiries~ b0 + b1 * DigitalCost ,

                      algorithm = "LM",
                      start     = c(b0=1, b1= 1 ),
                      lower     = c(b0=-Inf, b1=-Inf),
                      upper     = c(b0= Inf, b1= Inf))


summary(model_v8_nls)

predictionsv8_full <- predict(model_v8_nls,df_yrwk_all)
RMSE(predictionsv8_full , df_yrwk_all$Inquiries)
mae_v8_full <- abs(mean(predictionsv8_full - df_yrwk_all$Inquiries))


model_v9_lm <- lm(data= df_yrwk_all, Inquiries~ DigitalCost + TVCost + RadioCost + BillboardCost)


summary(model_v9_lm)

predictionsv9_full <- predict(model_v9_lm,df_yrwk_all)
RMSE(predictionsv9_full , df_yrwk_all$Inquiries)
mae_v9_full <- abs(mean(predictionsv9_full - df_yrwk_all$Inquiries))





model_v10_lm <- lm(data= df_yrwk_all, Inquiries~ DigitalCost )


summary(model_v10_lm)

predictionsv10_full <- predict(model_v10_lm,df_yrwk_all)
RMSE(predictionsv10_full , df_yrwk_all$Inquiries)
mae_v10_full <- abs(mean(predictionsv10_full - df_yrwk_all$Inquiries))


#######################