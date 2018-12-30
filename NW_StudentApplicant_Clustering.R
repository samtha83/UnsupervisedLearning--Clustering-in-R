
install.packages("stringr")#extract the year part
install.packages("lubridate")#extract the year
install.packages("dplyr")
install.packages("pmml")
install.packages("rattle")
library(pmml)
library(stringr)
library(lubridate)
library(dplyr)
library(rattle)

#set the working directory
#setwd("C:/Users/Samtha Reddy/Dropbox (Personal)/Copy of Work/ClusterAnalysis_7-06-2018")
setwd("C:/Users/Samtha Reddy/Dropbox (Converge Consulting)/Converge Consulting/Client/Northwestern/NW Analytics 18-05/MEM Personas Incl Apps 2018-07")
nwmem_df <- read.csv(file = "NW_FinalDataLoad.csv", header = TRUE, sep = ",")

#View(nwmem_df)
#Age At Term calculated in excel


#Create Binary factor for "U.S Citizen or not"
uscitizen <- which(nwmem_df$Country.of.Citizenship=="USA")

nwmem_df$BinaryUSCitizen <- 0 #So all rows including with column value blanks get 0
nwmem_df$BinaryUSCitizen[uscitizen] <- 1

#check if all USA citizen have value 1
#nwmem_df$Country.of.Citizenship[nwmem_df$BinaryUSCitizen==1]

# make gpa as numeric
nwmem_df$Undergrad.GPA <- as.numeric(as.character(nwmem_df$Undergrad.GPA))
# Warning message: NAs introduced by coercion 

nwmem_df$Undergrad.GPA.scale <- as.numeric(as.character(nwmem_df$Undergrad.GPA.scale))
#Warning message: NAs introduced by coercion

# create percentage for gpa/gpa scale

nwmem_df$GPAPercent <- nwmem_df$Undergrad.GPA /nwmem_df$Undergrad.GPA.scale

# Calculate GPA for 4 point scale
nwmem_df$GPA4pt <- nwmem_df$GPAPercent*4

# prep new column with lowest rank

nwmem_df$TitleLevel <- 1

# create Title column for this specific data file
nwmem_df$Title <- as.character(nwmem_df$Current.Employment.1..Job.Title)

#pull out index of titles


chief <- which(grepl("chief", nwmem_df$Title, ignore.case = T))
ceo<- which(grepl("cEO", nwmem_df$Title, ignore.case = T))
president<- which(grepl("president", nwmem_df$Title, ignore.case = T))
Cofounder<- which(grepl("Co founder", nwmem_df$Title, ignore.case = T))# I added this 

coo<- which(grepl("coo", nwmem_df$Title, ignore.case = T))
cfo<- which(grepl("cfo", nwmem_df$Title, ignore.case = T))

vicepresident<- which(grepl("vice president", nwmem_df$Title, ignore.case = T))
vp<- which(grepl("vp", nwmem_df$Title, ignore.case = T))
attorney<- which(grepl("attorney", nwmem_df$Title, ignore.case = T))
director<- which(grepl("director", nwmem_df$Title, ignore.case = T))
vice <- which(grepl("vice", nwmem_df$Title, ignore.case = T))
controller<- which(grepl("contoller", nwmem_df$Title, ignore.case = T))
head<- which(grepl("head", nwmem_df$Title, ignore.case = T))

engineer<- which(grepl("engineer", nwmem_df$Title, ignore.case = T))
lead<- which(grepl("lead", nwmem_df$Title, ignore.case = T))
senior <- which(grepl("senior", nwmem_df$Title, ignore.case = T))
sr <- which(grepl("sr", nwmem_df$Title, ignore.case = T))

specialist<- which(grepl("specialist", nwmem_df$Title, ignore.case = T))
analyst<- which(grepl("analyst", nwmem_df$Title, ignore.case = T))
supervisor<- which(grepl("supervisor", nwmem_df$Title, ignore.case = T))
manager<- which(grepl("manager", nwmem_df$Title, ignore.case = T))

# fill in appropriate rank for the record:

# 5: chief, ceo or president
nwmem_df$TitleLevel[chief] <- 5
nwmem_df$TitleLevel[ceo] <- 5
nwmem_df$TitleLevel[president] <- 5
nwmem_df$TitleLevel[coo] <- 5
nwmem_df$TitleLevel[cfo] <- 5
nwmem_df$TitleLevel[Cofounder] <- 5

# 4: vp or vicepresident
nwmem_df$TitleLevel[vp] <- 4
nwmem_df$TitleLevel[vicepresident] <- 4
nwmem_df$TitleLevel[vice] <- 4

# 3: attorney, director, specialist, Manager
nwmem_df$TitleLevel[attorney] <- 3
nwmem_df$TitleLevel[director] <- 3
nwmem_df$TitleLevel[specialist] <- 3
nwmem_df$TitleLevel[manager] <- 3
nwmem_df$TitleLevel[controller] <- 3
nwmem_df$TitleLevel[head] <- 3

# 2: analyst, Supervisor
nwmem_df$TitleLevel[analyst] <- 2
nwmem_df$TitleLevel[supervisor] <- 2
nwmem_df$TitleLevel[engineer] <- 2
nwmem_df$TitleLevel[lead] <- 2
nwmem_df$TitleLevel[senior] <- 2
nwmem_df$TitleLevel[sr] <- 2

# 1: everything else



# this field used in clustering hence changing it to numeric from factor
nwmem_df$No.of..years.of.work.experience.after.undergraduate.degree. <- as.numeric(as.character(nwmem_df$No.of..years.of.work.experience.after.undergraduate.degree.))

#Cleansing
#1. Checking if any na's in AgeatTerm,

#which(is.na(nwmem_df$Age))#3 records


#---------------------------------------------

#---- spliting dataset by program type
ft <-subset(nwmem_df, nwmem_df$Program.Binary==1)
pt <-subset(nwmem_df, nwmem_df$Program.Binary==0)

# ---
write.csv(nwmem_df, file = "NWStudentApplicantUpdatedinR.csv",row.names=FALSE)

write.csv(ft, file = "NWStudentApplicantFullTimeUpdatedinR.csv",row.names=FALSE)
write.csv(pt, file = "NWStudentApplicantParTimeUpdatedinR.csv",row.names=FALSE)


# do clustering in Rattle


rattle()

#reloaded the file we got from Rattle which has cluster information of each record
#before loading the csv fix-> racial.makeup column remove <U+0095> ctl+H
nwmem_df_cl <- read.csv(file = "NWStudentApplicantUpdatedinR_train_score_all.csv", header = TRUE, sep = ",")


#%of full time in each cluster
tapply(nwmem_df_cl$Program.Binary, nwmem_df_cl$kmeans, mean)

#% of student and applicant in each cluster
tapply(nwmem_df_cl$Type, nwmem_df_cl$kmeans, summary)


# Checking geography

#default all are level 3- all other us states
nwmem_df_cl$GeoLevel <- 3


nwmem_df_cl$US.State <- as.character(nwmem_df_cl$US.State)

international<-which(is.na(nwmem_df_cl$US.State))
nwmem_df_cl$GeoLevel[international]<-5

coast<-which(nwmem_df_cl$US.State %in% c('Maine','ME',	'New Hampshire ','NH',	'Massachusetts','MA',	'Rhode Island','RI',	'Connecticut','CT',	'New York','NY',	'New Jersey','NJ',	'Delaware','DE',	'Maryland','MD',	'Virginia','VA',	'North Carolina','NC',	'South Carolina','SC',	'Georgia','GA',	'Florida.','FL',	'California ','CA',	'Oregon','OR',	'Washington','WA',	'Alaska','AK',	'Hawaii','HI'))
nwmem_df_cl$GeoLevel[coast]<-4

border <- which(nwmem_df_cl$US.State %in% c('Wisconsin','WI','Michigan','MI','Indiana','IN','Kentucky','KY','Missouri','MO','Iowa','IA','Tennessee','TN'))  
nwmem_df_cl$GeoLevel[border]<-2

IL <- which(nwmem_df_cl$US.State %in% c('IL','IL - Illinois','IL.','Illinois','Illionis'))
nwmem_df_cl$GeoLevel[IL]<-1


#% of geo in each cluster
nwmem_df_cl$GeoLevel<-as.factor(nwmem_df_cl$GeoLevel)

tapply(nwmem_df_cl$GeoLevel, nwmem_df_cl$kmeans, summary)




#-----------------5 Clusters Analysis------------------------------#
#reloaded the file we got from Rattle for 5 clusters which has cluster information of each record
#before loading the csv fix-> racial.makeup column remove <U+0095> ctl+H
setwd("C:/Users/Samtha Reddy/Dropbox (Personal)/Copy of Work/ClusterAnalysis_7-06-2018")

nwmem_df_5cl <- read.csv(file = "NWStudentApplicantUpdatedinR_train_score_all_5Clusters.csv", header = TRUE, sep = ",")

#% of student and applicant in each cluster
tapply(nwmem_df_5cl$Type, nwmem_df_5cl$kmeans, summary)

nwmem_df_cl_students <- nwmem_df_cl[nwmem_df_cl$Type =="student", ]
tapply(nwmem_df_cl_students$GeoLevel, nwmem_df_cl_students$kmeans, summary)

