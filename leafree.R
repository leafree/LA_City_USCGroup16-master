library(dplyr)
library(lubridate)
library(ggmap)

la_map = get_map("LosAngeles", zoom = 10) 

Crime = read.csv("raw_data/Crime__Homeless_Victim_8_16_-_8_17.csv")


