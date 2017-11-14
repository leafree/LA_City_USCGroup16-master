library(dplyr)
library(lubridate)
library(ggmap)

la_map = get_map("LosAngeles", zoom = 10) 

data_311 = read.csv("la_city_independent_analysis/311_calls_w_CTs20171102103144.csv")

data_311$CREATEDDATE = mdy_hm(data_311$CREATEDDATE)
data_311$SERVICEDATE = mdy_hm(data_311$SERVICEDATE)

data = data_311 %>%
  select(CREATEDDATE, SERVICEDATE, LONGITUDE, LATITUDE, CT10) %>%
  mutate(CREATEDMONTH = month(CREATEDDATE, label = T , abbr = T)) %>%
  mutate(RESPONSE_DAYS = (SERVICEDATE - CREATEDDATE)/1440) %>%
  mutate(RESPONSE_LEVEL = ifelse(RESPONSE_DAYS <= 3, "1-3 days", 
                           ifelse(RESPONSE_DAYS <= 7,"4-7 days",
                                  ifelse(RESPONSE_DAYS <= 10,"8-10 days",
                                         ifelse(RESPONSE_DAYS <= 20, "11-20 days", 
                                                ifelse(RESPONSE_DAYS > 20, "21 days or above", 0 )))))) %>%
  filter(!is.na(RESPONSE_LEVEL)) %>%
  filter(CREATEDMONTH!= "Nov")


zlevels_res = levels(factor(data$RESPONSE_LEVEL))
data$RESPONSE_LEVEL = factor(data$RESPONSE_LEVEL,
                             levels_res[c(1,4,5,2,3)])

###########data2
data2 = data_311 %>%
  select(CREATEDDATE, SERVICEDATE, LONGITUDE, LATITUDE, CT10) %>%
  mutate(CREATEDMONTH = month(CREATEDDATE, label = T , abbr = T)) %>%
  mutate(CREATEDQUARTER = ifelse(CREATEDMONTH %in% c("Jan","Feb","Mar"),"Q1",
                                 ifelse(CREATEDMONTH %in% c("Apr","May","Jun"),"Q2",
                                        ifelse(CREATEDMONTH %in% c("Jul","Aug","Sep"),"Q3", NA)))) %>%
  mutate(RESPONSE_DAYS = (SERVICEDATE - CREATEDDATE)/1440) %>%
  mutate(RESPONSE_LEVEL = ifelse(RESPONSE_DAYS <= 10, "1-10 days", 
                                 ifelse(RESPONSE_DAYS <= 20, "11-20 days", 
                                        ifelse(RESPONSE_DAYS > 20, "21 days or above", 0 )))) %>%
  mutate(SERVICE_WDAY = wday(SERVICEDATE, label = T, abbr = T))%>% 
  filter(!is.na(RESPONSE_LEVEL)) %>%
  filter(!is.na(CREATEDQUARTER)) %>%
  filter(CREATEDMONTH!= "Nov")


###### requests created count by month
ggmap(la_map) +
    stat_density2d(data = data2, 
                   aes(x = LONGITUDE, y = LATITUDE,
                       fill = ..level..,
                       alpha = ..level..),
                   geom = "polygon") +
    scale_fill_gradient(low = "pink", high = "steelblue") +
    facet_wrap(~CREATEDMONTH)+
    theme(legend.position = "none")+
    labs(x = "", y = "", 
       title = "Requests Created By Month")
###### service dilivered by weekday
ggmap(la_map) +
  stat_density2d(data = data2, 
                 aes(x = LONGITUDE, y = LATITUDE,
                     fill = SERVICE_WDAY,
                     alpha = ..level..),
                 geom = "polygon") +
#  scale_fill_gradient(low = "pink", high = "steelblue") +
  facet_wrap(~SERVICE_WDAY)+
  theme(legend.position = "none")+
  labs(x = "", y = "", 
       title = "Service dilivered by weekday")
########response rate
ggmap(la_map) +
  stat_density2d(data = data2, 
                 aes(x = LONGITUDE, y = LATITUDE,
                     fill = RESPONSE_LEVEL,
                     alpha = ..level..),
                 geom = "polygon") +
  facet_wrap(CREATEDQUARTER~RESPONSE_LEVEL)+
#  theme(legend.position = "none")+
  labs(x = "", y = "", 
       title = "Service efficiency",
       subtitle = "by XX")

#scale_fill_discrete()
#  scale_fill_gradient(low = "pink", high = "steelblue") 
  

