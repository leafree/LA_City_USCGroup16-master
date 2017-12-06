library(stringr)
library(ggplot2)
library(dplyr)
library(ggmap)

fun_words = read.csv("shelters_function_keywords.csv")
shelters = read.csv("la_city_independent_analysis/shelters_w_CTs20171102103122.csv")
## find function matrix for each shelters
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
                          shelters$ORG_NAME,
                          shelters$ADDRLN1,
                          shelters$PHONES,
                          fun_matrix)
rm(fun_matrix,fun_words,i,j)
?write.csv
write.csv(shelters_fun,file = "shelters_fun.csv")

##count the number of shelters to provide different function, and reture an image
shelters_stat = data.frame(colnames(shelters_fun)[5:23],
                           colSums(shelters_fun[5:23]))
colnames(shelters_stat)[1]="shelter_function"
colnames(shelters_stat)[2]="count"

ggplot(shelters_stat,aes(x = reorder(shelter_function,count), 
                         y = count)) +
  geom_bar(stat = "identity",fill = "steelblue") +
  coord_flip()+
  ggtitle("Number of Shelters to Provide Particular Services")+
  xlab("Services")+
  ylab("Count")+
  geom_text(aes(label = count), 
            position = position_stack(vjust = 0.8)) 
##the shelters seem evenly located around LA

la_map = get_map("LosAngeles", zoom = 9)

ggmap(la_map) +
  geom_point(data=shelters_fun,
             aes(x=shelters.LONGITUDE,
                 y=shelters.LATITUDE,
                 color = shelters.CITY,
                 size = 1))

#questions remained in this image

ggmap(la_map) + 
  stat_density2d(data = shelters_fun, 
                 aes(x=shelters.LONGITUDE,
                     y=shelters.LATITUDE,
                     fill = ..level.., 
                     alpha = ..level..), 
                 geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "darkred") + 
  theme(legend.position = 'none')

#shelters provides financial assistant

shelters_financial = shelters_fun %>% 
  filter(financial_assistance == 1)


ggmap(la_map) +
  geom_point(data = shelters_financial,
             aes(x=shelters.LONGITUDE,
                 y=shelters.LATITUDE,
                 color= shelters.CITY),
             size =3)

rm(shelters_financial)