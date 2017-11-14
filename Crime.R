library(ggplot2)
library(dplyr)
library(ggmap)
library(stringr)

crime=read.csv("crime_w_CTs20171102103130.csv")
head(crime)

library("readxl")
homeless=read_xls("homless count 2015-2017.xls")
head(homeless)

# homeless population
ggplot(homeless,aes(Unshelter,))+
  geom_histogram()

ggplot(homeless, aes(x = Unshelter, y = ..density..)) +
  geom_histogram(fill = "lightblue", color = "white", size = 0.2) +
  geom_line(stat="density")

?read.csv



table(is.na(crime))
colSums(is.na(crime))

la_map = get_map("LosAngeles", zoom = 10)

# Date occurred
date_occ = as.data.frame(str_split_fixed(crime$DATE.OCCURRED, '/', 3))
crime["date_occ_year"]=date_occ[,3]
crime["date_occ_month"]=date_occ[,1]
crime["date_occ_day"]=date_occ[,2]
head(crime)

ggplot(crime,aes(x=date_occ_month))+
  geom_histogram(stat="count")


# August has the most crimes


# Demographics of victims
ggplot(crime,aes(x=VICTIM.AGE))+
  geom_histogram()

ggplot(crime,aes(x=VICTIM.SEX))+
  geom_histogram(stat="count")

ggplot(crime,aes(x=VICTIM.DESCENT))+
  geom_histogram(stat="count")

# geographical distribution
ggplot(crime,
       aes(x=LONGITUDE,
           y=LATITUDE,
           color=AREA.ID))+
  geom_point()+
  theme_void()




# severity of the crime
ggmap(la_map) +
  geom_point(data=crime,
             aes(x=LONGITUDE,
                 y=LATITUDE,
                 color=AREA.ID))


scale_fill_gradient(low="white",high="darkred")+
  theme_void()
