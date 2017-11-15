library(ggplot2)
library(dplyr)
library(ggmap)
library(stringr)
library(lubridate)

crime=read.csv("crime_w_CTs20171102103130.csv")
head(crime)

library("readxl")
homeless=read_xls("homless count 2015-2017.xls")
head(homeless)

# homeless population

ggplot(homeless,aes(x=factor(Year),y=Population,fill=Category))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Year")+
  ggtitle("Number of Homeless People 2015-2017")+
  geom_text(aes(label = Population),size=3, hjust=0.6, vjust=3, position = position_dodge(0.9))+
  theme_bw()


# Date occurred
date_occ = as.data.frame(str_split_fixed(crime$DATE.OCCURRED, '/', 3))
crime["date_occ_year"]=date_occ[,3]
crime["date_occ_month"]=date_occ[,1]
crime["date_occ_day"]=date_occ[,2]
head(crime)

ggplot(crime,aes(x=date_occ_month))+
  geom_histogram(stat="count",color="blue",fill="lightblue")+
  xlab("Month Occurred")+
  ggtitle("Month of Crime Occurred")


# Difftime between occurred and reported
crime$DATE.OCCURRED2=as.POSIXct(crime$DATE.OCCURRED,format="%m/%d/%Y",origin="PDT")
crime$DATE.REPORTED2=as.POSIXct(crime$DATE.REPORTED,format="%m/%d/%Y",origin="PDT")

crime$difftime=difftime(crime$DATE.REPORTED2,crime$DATE.OCCURRED2,units="days")

ggplot(crime,aes(as.factor(difftime)))+
  geom_histogram(stat="count")

# Hour
library(dplyr)
library(tidyr)
crime = crime %>% 
  separate(col = "TIME.OCCURRED", into = c("H", "M"), sep = -3)
head(crime)
crime$H=ordered(crime$H, levels=c(1:24))

ggplot(crime,aes(as.factor(H)))+
  geom_histogram(stat="count",fill="lightblue")+
  geom_line(stat="count",group=1,adjust=5,color="blue")+
  ggtitle("Hour Distribution of the Crime")+
  xlab("Hour")+
  theme_bw()

# August has the most crimes
# Noon and night are the peak time


# Demographics of victims

#victim age
ggplot(crime,aes(x=VICTIM.AGE))+
  geom_histogram(aes(y=..density..), fill="lightblue",binwidth=3)+
  geom_density(aes(y=..density..),color="blue")+
  ggtitle("Age Distribution of the Victims")+
  xlab("Age of Victim")+
  theme_bw()


# victim sex
crime_sex=crime[crime$VICTIM.SEX!="X",]
ggplot(crime_sex,aes(x=VICTIM.SEX),color=VICTIM.SEX)+
  geom_histogram(stat="count",fill="lightblue",color="blue")+
  xlab("Gender")+
  ggtitle("Gender of the Victims")+
  theme_bw()

# victim descent
crime$VICTIM.DESCENT = factor(ifelse(crime$VICTIM.DESCENT == "B", "Black",
                              ifelse(crime$VICTIM.DESCENT == "H", "Hispanic", 
                              ifelse(crime$VICTIM.DESCENT=="W","White",
                                     ifelse(crime$VICTIM.DESCENT=="A","Asian","Others")))))

crime$VICTIM.DESCENT=ordered(crime$VICTIM.DESCENT, levels=c("Asian","White","Black","Hispanic","Others"))
ggplot(crime,aes(x=VICTIM.DESCENT))+
  geom_histogram(stat="count",fill="lightblue",color="blue")+
  xlab("Descent of the Victims")+
  ggtitle("Descent Distribution of the Victims")+
  theme_bw()


# category of the crime
crime %>% 
  group_by(CRIME.CODE.DESCRIPTION) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:10) %>%
ggplot(aes(x=reorder(CRIME.CODE.DESCRIPTION,count),y=count))+
  geom_bar(stat="identity",fill="lightblue",color="blue")+
  coord_flip()+
  geom_text(aes(label=count),size=6, hjust=1, vjust=0.8)+
  xlab("Crime Category")+
  ggtitle("Top 10 Crime Category")+
  theme_bw()


# Geography 
la_map = get_map("LosAngeles", zoom = 10)

ggmap(la_map) +
  geom_point(data=crime,
             aes(x=LONGITUDE,
                 y=LATITUDE,
                 color=AREA.ID))+
  ggtitle("Crime distribution in LA County")




