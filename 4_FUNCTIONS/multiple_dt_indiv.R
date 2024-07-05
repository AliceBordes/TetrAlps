#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

######################################################################

# Description:

# to obtain a list of data.frame for each individual, from the raw data frame
# containing all animal positions

######################################################################

multiple_dt_indiv<-function(data,pattern)
{
  data <- as.data.frame(data)
  pattern<-as.character(pattern)
  dt.list<-list()
  
  indx <- grepl(pattern, colnames(data)) # saving the index of the column of the data frame containing the pattern
  vect.animal.names<-as.vector(unlist(unique(data[indx])))
  
  for(i in 1:length(vect.animal.names))  # for i from 1 to the number of named bird
  {

    dt.list[[i]]<-data %>% filter(data[indx] == vect.animal.names[i])

  }
  names(dt.list)<-vect.animal.names

  return(dt.list)
}


