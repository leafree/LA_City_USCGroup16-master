library(stringr)
library(ggplot2)
library(dplyr)
library(ggmap)

fun_words = read.csv("shelters_function_keywords.csv")
shelters = read.csv("la_city_independent_analysis/shelters_w_CTs20171102103122.csv")

fun_matrix = matrix(0,
                    nrow=dim(shelters)[1],
                    ncol=dim(fun_words)[1])
colnames(fun_matrix)= fun_words$shelter_function


for(i in 1:dim(fun_words)[1]){
  for(j in 2:dim(fun_words)[2]){
    if(as.character(fun_words[i,j]) == ""){
      j = dim(fun_words)[2]
    }else
    {
    fun_matrix[,i] =fun_matrix[,i]+str_detect(shelters$DESCRIPTION,
                               pattern = 
                                 as.character(fun_words[i,j]))
    }
  }
}
fun_matrix[fun_matrix > 0] = 1

shelters_fun = data.frame(shelters$NAME,
                          shelters$LONGITUDE,
                          shelters$LATITUDE,
                          shelters$CITY,
                          fun_matrix)
rm(fun_matrix,fun_words,i,j)
## find function matrix for each shelters
?get_map

la_map = get_map("LosAngeles", zoom = 9)

ggmap(la_map) +
  geom_point(data=shelters_fun,
             aes(x=shelters.LONGITUDE,
                 y=shelters.LATITUDE,
                 color = shelters.CITY,
                 size = 1))

#the shelters seem evenly located around LA

ggmap(la_map) + 
  stat_density2d(data = shelters_fun, 
                 aes(x=shelters.LONGITUDE,
                     y=shelters.LATITUDE,
                     fill = ..level.., 
                     alpha = ..level..), 
                 geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "darkred") + 
  theme(legend.position = 'none')

#questions remained in this image

fun_plot = function()

shelters_financial = shelters_fun %>% 
  filter(financial_assistance == 1)

ggmap(la_map) +
  geom_point(data = shelters_financial,
             aes(x=shelters.LONGITUDE,
                 y=shelters.LATITUDE,
                 color= shelters.CITY,
                 size =1))
# 