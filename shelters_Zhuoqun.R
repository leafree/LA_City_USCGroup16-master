fun_words = read.csv("shelters_function_keywords.csv")
shelters = read.csv("la_city_independent_analysis/shelters_w_CTs20171102103122.csv")

fun_matrix = matrix(0,nrow=as.numeric(dim(shelters)[1]),
                    ncol=as.numeric(dim(fun_words)[1]))
colnames(fun_matrix)= fun_words$shelter_function
library(stringr)
for(i in 1:dim(fun_words)[1]){
  for(j in 2:dim(fun_words)[2]){
    if(as.character(fun_words[i,j]) == ""){
      j = dim(fun_words)[2]
    }else
    {
    fun_matrix[,i] =str_detect(shelters$DESCRIPTION,
                               pattern = 
                                 as.character(fun_words[i,j]))
    }
  }
}
## find function matrix for each shelters

