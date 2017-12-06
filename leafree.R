library(dplyr)
library(lubridate)
library(ggmap)
library(stringr)
library(tidyr)

la_map = get_map("LosAngeles", zoom = 10) 

crime = read.csv("raw_data/Crime__Homeless_Victim_8_16_-_8_17.csv")

summary(crime)
dim(crime)

class(crime$Date.Reported)

crime$Date.Reported = mdy(crime$Date.Reported)
crime$Date.Occurred = mdy(crime$Date.Occurred)

class(crime$Time.Occurred)

crime$Location = gsub("[(*)]", "", crime$Location)

crime = crime %>% 
  separate(col = "Time.Occurred", into = c("H", "M"), sep = -3) %>% 
  separate(col = "Location", into = c ("latitude","longitude"),sep = ",")


crime$latitude = as.numeric(crime$latitude)
crime$longitude = as.numeric(crime$longitude)

ggmap(la_map) + 
  stat_density2d(data = crime, 
                 aes(x = longitude, y = latitude, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "darkred") + 
  facet_wrap(~H) + 
  theme(legend.position = 'none')

