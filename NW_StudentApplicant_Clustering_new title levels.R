
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

  nwmem_df$TitleLevel <- 0

# create Title column for this specific data file
 
nwmem_df$Title <- as.character((nwmem_df$Current.Employment.1..Job.Title))

#pull out index of titles



chief <- which(grepl("chief", nwmem_df$Title, ignore.case = T))
ceo<- which(grepl("cEO", nwmem_df$Title, ignore.case = T))
president<- which(grepl("president", nwmem_df$Title, ignore.case = T))
founder<- which(grepl("found", nwmem_df$Title, ignore.case = T))
coo<- which(grepl("coo", nwmem_df$Title, ignore.case = T))
cfo<- which(grepl("cfo", nwmem_df$Title, ignore.case = T))
chair<- which(grepl("chair", nwmem_df$Title, ignore.case = T))
managing<- which(grepl("managing", nwmem_df$Title, ignore.case = T))
partner<- which(grepl("partner", nwmem_df$Title, ignore.case = T))


vice<- which(grepl("vice", nwmem_df$Title, ignore.case = T))
vp<- which(grepl("vp", nwmem_df$Title, ignore.case = T))
#attorney<- which(grepl("attorney", nwmem_df$Title, ignore.case = T))
#controller<- which(grepl("contoller", nwmem_df$Title, ignore.case = T))
head<- which(grepl("head", nwmem_df$Title, ignore.case = T))


dire<- which(grepl("dir", nwmem_df$Title, ignore.case = T))
fellow<- which(grepl("fellow", nwmem_df$Title, ignore.case = T))


lead<- which(grepl("lead", nwmem_df$Title, ignore.case = T))
senior <- which(grepl("senior", nwmem_df$Title, ignore.case = T))
sr <- which(grepl("sr", nwmem_df$Title, ignore.case = T))
sup<- which(grepl("sup", nwmem_df$Title, ignore.case = T))
manage<- which(grepl("manage", nwmem_df$Title, ignore.case = T))
mang<- which(grepl("mang", nwmem_df$Title, ignore.case = T))
consultant<- which(grepl("consultant", nwmem_df$Title, ignore.case = T))
office<- which(grepl("office", nwmem_df$Title, ignore.case = T))
teach<- which(grepl("teach", nwmem_df$Title, ignore.case = T))






engineer<- which(grepl("engineer", nwmem_df$Title, ignore.case = T))
specialist<- which(grepl("specialist", nwmem_df$Title, ignore.case = T))
anal<- which(grepl("anal", nwmem_df$Title, ignore.case = T))
staff<- which(grepl("staff", nwmem_df$Title, ignore.case = T))
ass<- which(grepl("ass", nwmem_df$Title, ignore.case = T))
tech<- which(grepl("tech", nwmem_df$Title, ignore.case = T))
research<- which(grepl("research", nwmem_df$Title, ignore.case = T))
dev<- which(grepl("dev", nwmem_df$Title, ignore.case = T))



intern<- which(grepl("intern", nwmem_df$Title, ignore.case = T))
student<- which(grepl("student", nwmem_df$Title, ignore.case = T))
train<- which(grepl("train", nwmem_df$Title, ignore.case = T))




# fill in appropriate rank for the record:

# 6: chief, ceo or president
nwmem_df$TitleLevel[chief] <- 6
nwmem_df$TitleLevel[ceo] <- 6
nwmem_df$TitleLevel[president] <- 6
nwmem_df$TitleLevel[coo] <- 6
nwmem_df$TitleLevel[cfo] <- 6
nwmem_df$TitleLevel[founder] <- 6
nwmem_df$TitleLevel[chair] <- 6
nwmem_df$TitleLevel[managing] <- 6
nwmem_df$TitleLevel[partner] <- 6



# 5: vp or vicepresident
nwmem_df$TitleLevel[vice] <- 5
nwmem_df$TitleLevel[vp] <- 5
nwmem_df$TitleLevel[head] <- 5

# 4: Dir,Fellow
nwmem_df$TitleLevel[dire] <- 4
nwmem_df$TitleLevel[fellow] <- 4

# 3: Manager, lead, supervisor

nwmem_df$TitleLevel[manage] <- 3
nwmem_df$TitleLevel[mang] <- 3
nwmem_df$TitleLevel[sup] <- 3
nwmem_df$TitleLevel[lead] <- 3
nwmem_df$TitleLevel[senior] <- 3
nwmem_df$TitleLevel[sr] <- 3
nwmem_df$TitleLevel[consultant] <- 3
nwmem_df$TitleLevel[office] <- 3
nwmem_df$TitleLevel[teach] <- 3

# 2: analyst, engineer

nwmem_df$TitleLevel[engineer] <- 2
nwmem_df$TitleLevel[specialist] <- 2
nwmem_df$TitleLevel[anal] <- 2
nwmem_df$TitleLevel[staff] <- 2
nwmem_df$TitleLevel[ass] <- 2
nwmem_df$TitleLevel[tech] <- 2
nwmem_df$TitleLevel[research] <- 2
nwmem_df$TitleLevel[dev] <- 2


nwmem_df$TitleLevel[intern] <- 1
nwmem_df$TitleLevel[student]<- 1
nwmem_df$TitleLevel[train] <- 1


# 0: everything else



# this field used in clustering hence changing it to numeric from factor
nwmem_df$No.of..years.of.work.experience.after.undergraduate.degree. <- as.numeric(as.character(nwmem_df$No.of..years.of.work.experience.after.undergraduate.degree.))

#Cleansing
#1. Checking if any na's in AgeatTerm,

#which(is.na(nwmem_df$Age))#3 records



rattle()

write.csv(nwmem_df, file = "NWStudentApplicantUpdatedinR_newtitlelvls.csv",row.names=FALSE)



head()







