library(dplyr)
library(lubridate)
library(ggmap)
library(stringr)
library(tidyr)

la_map = get_map("LosAngeles", zoom = 10) 

Crime = read.csv("raw_data/Crime__Homeless_Victim_8_16_-_8_17.csv")

summary(Crime)
dim(Crime)

class(Crime$Date.Reported)

Crime$Date.Reported = mdy(Crime$Date.Reported)
Crime$Date.Occurred = mdy(Crime$Date.Occurred)

class(Crime$Time.Occurred)

Crime$Time.Occurred = hm(Crime$Time.Occurred)
?substr
Crime$Location = gsub("[(*)]", "", Crime$Location)

Crime = Crime %>% 
  separate(col = "Time.Occurred", into = c("H", "M"), sep = -3) %>% 
  separate(col = "Location", into = c ("latitude","longitude"),sep = ",")

ggmap(la_map) + 
  stat_density2d(data = Crime, 
                 aes(x = longitude, y = latitude, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "darkred") + 
  facet_wrap(~H) + 
  theme(legend.position = 'none')

