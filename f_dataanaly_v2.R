#functions for data analysis


################################################################################################################################
#1. Regression analysis
################################################################################################################################

#Creating Data for Regression analysis for n Datasets
f_constructﬂdataﬂreg <- function(data_X,data_Y){
  "Returns a dataframe for regression analysis with the structure X;Y;
  from two dataframes or matrices of same length.
  
  Keyword arguments:
  dataX -- the data for x of the regression (dataframe or matrix)
  dataY -- the data for y of the regression (dataframe or matrix)
  "
  
  #check if same length otherwise return specific errors
  if(length(data_X) != length(data_Y)){
    return(warning('data is not of same length'))
  }
  
  #construct for two dataframes
  else if(is.data.frame(data_X) == TRUE & is.data.frame(data_Y) == TRUE){
    assign("data_reg", merge(data_X,data_Y))
    return(data_reg)
  }
  
  #construct for two matrices
  else if((is.matrix(data_X) == TRUE || is.vector(data_X) == TRUE)  & (is.matrix(data_Y) == TRUE || is.vector(data_Y) == TRUE)){
    assign("data_reg", as.data.frame(matrix (c(data_X,data_Y),length(data_X))))
    return(data_reg)
  }
  
  #error if data is not of the same type
  else{
    return(warning('data X or data Y are not of the desired datatype or not the same datatype'))
  }
}

#create n regessiondatasets in a list for different y data with the same x data
f_constructﬂlistﬂdataﬂreg <- function(data_X,data_Y,names_dataY=c(),topic="",enumerate=FALSE){
  "Returns a list of N dataframes for regression analysis with the structure X;Y;
  from one dataframe or matrix X and differen dataframes or matrices Y all of same length
  (all X and Y have to be of the same type)
  The listelements are named after the names of the Y dataframes or can be named using a topic and enumerating them
  (is false by default)
  
  
  Keyword arguments:
  dataX -- the data for x of the regression (dataframe or matrix)
  names.dataY -- the names of the dataframes for y of the regression (vector of strings)
  topic -- giving the listelements topicnames (string)
  enumerate -- enumerating the listelemt givn the elemetns names of $topic'i' (boolian)
  "
  
  list_reg <- list()
  
  for(i in 1:ncol(data_Y)){
    Lelem <- f_constructﬂdataﬂreg(data_X,data_Y[,i])
    list_reg[[length(list_reg)+1]] = Lelem
  }
  
  list_reg <- f_nameﬂlist(list_reg,names_dataY,topic,enumerate,ncol(data_Y))
  
  return(list_reg)
}

#Gives the result of regression analysis for a list of X,Y dat afor regression 
f_createﬂlistﬂregression <- function(list_data,b_save=FALSE,name_table="",name_datadirectory="",name_maindirectory = name_directoryﬂmain){
  "Returns a dataframe of linear regression coefficients and there fiterrors (using the lm function) for a list of datasets for
  regression analysis that can be printend to csv.

  Keyword arguments:
  list_data -- the list of datasets for regression (list of dataframes or matrices)
  b_save -- if the result should be saved to csv (boolian)
  name_table -- giving the saved file a name (string)
  name_datadirectory -- directoryname where to save (string)
  name_maindirectory -- five the maindirectory so you automatically go back there
  "
  
  data_result <- data.frame(I = c(0), deltaI = c(0),S = c(0),deltaS = c(0))
  
  for(i in 1:length(list_data)){
    temp <- lm(V2~V1, data=list_data[[i]])
    temp2 <- summary(temp)
    
    col <- data.frame(temp2$coefficients[1,1],temp2$coefficients[1,2],temp2$coefficients[2,1],temp2$coefficients[2,2])
    data_result[names(list_data)[i],] <- col
  }
  
  data_result <- data_result[-1,]
  
  if(b_save == TRUE){
    #nametable has to include .csv
    setwd(name_datadirectory)
    write.csv(data_result, name_table)
   setwd(name_maindirectory) 
  }
  
  return(data_result)
}

################################################################################################################################
#2. Polarisation analysis
################################################################################################################################

#Function for the analysis of the polarisation between prior and posterior
f_resultﬂpolratio <- function(data_X,data_Y){
  "Returns a numeric that gives the ratio of the polarisation between data_x and data_Y

  Keyword arguments:
  data_X -- dataframe with 1 entry with the ratings for the prior
  data_Y -- dataframe with 1 entry with the ratings for the posterior
  "
  
  if(length(data_X) != length(data_Y)){
    return(warning("length of data to compare for polarisation does not match"))
  }
  
  else{
    
    temp = c(1:length(data_X))
    
    for (i in 1:length(data_X)) {
      if(abs(data_Y[i]) > abs(data_X[i])){
        temp[i] = 1
      }
      else{
        temp[i] = 0
      }
    }
    
    res_pol = sum(temp)/length(data_X)
    return(res_pol)
    
  }
}

#create a list of multiple polratios (Grad der polarisation) for high/low scorers and 2 topics
f_polratioresult <- function(list_dataﬂtopic1,list_dataﬂtopic2,b_save=FALSE,name_table="",name_datadirectory="",name_maindirectory = name_directoryﬂmain){
  "Returns a dataframe of polarisations and their guasserrors that can be printend to csv.
  
  Keyword arguments:
  list_dataﬂtopic1 -- the list of data to calculate polarisation from for the 1. topic
  list_dataﬂtopic2 -- the list of data to calculate polarisation from for the 2. topic
  b_save -- if the result should be saved to csv (boolian)
  name_table -- giving the saved file a name (string)
  name_datadirectory -- directoryname where to save (string)
  name_maindirectory -- five the maindirectory so you automatically go back there
  "
  
  result_postpriorﬂpol = data.frame(Pol = c(), error = c())
  for (i in 1:length(list_dataﬂtopic1)){
    
    tmp1 <- f_resultﬂpolratio(list_dataﬂtopic1[[i]][,1],list_dataﬂtopic1[[i]][,2])
    tmp2 <- f_resultﬂpolratio(list_dataﬂtopic2[[i]][,1],list_dataﬂtopic2[[i]][,2])
    
    result_postpriorﬂpol = rbind(result_postpriorﬂpol,data.frame(Pol = tmp1, error = 1/sqrt(length(list_dataﬂtopic1[[i]]))*tmp1))
    result_postpriorﬂpol = rbind(result_postpriorﬂpol,data.frame(Pol = tmp2, error = 1/sqrt(length(list_dataﬂtopic2[[i]]))*tmp2))
    
  }
  
  #das hier is etwas sketchy
  rownames(result_postpriorﬂpol) <- c("P1topic1", "P1topic2","P2topic1", "P2topic2")
  
  if(b_save == TRUE){
    #nametable has to include .csv
    setwd(name_datadirectory)
    write.csv(result_postpriorﬂpol, name_table)
    setwd(name_maindirectory) 
  }
  
  return(result_postpriorﬂpol)
  
}

#Function for the analysis of the strength of polarisation between prior and posterior
f_resultstrengthpolratio <- function(data_X,data_Y){
  "Returns a numeric that gives the ratio of the strength of polarisation between data_x and data_Y

  Keyword arguments:
  data_X -- dataframe with 1 entry with the ratings for the prior
  data_Y -- dataframe with 1 entry with the ratings for the posterior
  "
  
  if(length(data_X) != length(data_Y)){
    return(warning("length of data to compare for polarisation does not match"))
  }
  
  else{
    
    temp = c(1:length(data_X))
    
    for (i in 1:length(data_X)) {
      temp [i] = abs(data_Y[i] - data_X[i])
    }
    
    res_pol = sum(temp)/length(data_X)
    return(res_pol)
    
  }
}

#create a lsit of muultiple strength of pol values (St‰rke der Polarisation) for high/low scorers and 2 topics
f_strengthpolratioresult <- function(list_dataﬂtopic1,list_dataﬂtopic2,b_save=FALSE,name_table="",name_datadirectory="",name_maindirectory = name_directoryﬂmain){
  "Returns a dataframe of strength of polarisations and their guasserrors that can be printend to csv.
  
  Keyword arguments:
  list_dataﬂtopic1 -- the list of data to calculate polarisation from for the 1. topic
  list_dataﬂtopic2 -- the list of data to calculate polarisation from for the 2. topic
  b_save -- if the result should be saved to csv (boolian)
  name_table -- giving the saved file a name (string)
  name_datadirectory -- directoryname where to save (string)
  name_maindirectory -- five the maindirectory so you automatically go back there
  "
  
  result_postpriorﬂpol = data.frame(PolS = c(), error = c())
  for (i in 1:length(list_dataﬂtopic1)){
    
    tmp1 <- f_resultstrengthpolratio(list_dataﬂtopic1[[i]][,1],list_dataﬂtopic1[[i]][,2])
    tmp2 <- f_resultstrengthpolratio(list_dataﬂtopic2[[i]][,1],list_dataﬂtopic2[[i]][,2])
    
    result_postpriorﬂpol = rbind(result_postpriorﬂpol,data.frame(Pol = tmp1, error = abs(1/sqrt(length(list_dataﬂtopic1[[i]]))*tmp1)))
    result_postpriorﬂpol = rbind(result_postpriorﬂpol,data.frame(Pol = tmp2, error = abs(1/sqrt(length(list_dataﬂtopic2[[i]]))*tmp2)))
    
  }
  
  #das hier is etwas sketchy
  rownames(result_postpriorﬂpol) <- c("P1topic1", "P1topic2","P2topic1", "P2topic2")
  
  if(b_save == TRUE){
    #nametable has to include .csv
    setwd(name_datadirectory)
    write.csv(result_postpriorﬂpol, name_table)
    setwd(name_maindirectory) 
  }
  
  return(result_postpriorﬂpol)
  
}

################################################################################################################################
#3. quadratic fit
################################################################################################################################

#Gives the result of quadratic fit for a list of X,Y data
f_createﬂlistﬂquadraticfit <- function(list_data,b_save=FALSE,name_table="",name_datadirectory="",name_maindirectory = name_directoryﬂmain){
  "Returns a dataframe of quadratic coefficients and there fiterrors (using the lm function) for a list of datasets for
  delta analysis that can be printend to csv.
  
  Keyword arguments:
  list_data -- the list of datasets for regression (list of dataframes or matrices)
  b_save -- if the result should be saved to csv (boolian)
  name_table -- giving the saved file a name (string)
  name_datadirectory -- directoryname where to save (string)
  name_maindirectory -- five the maindirectory so you automatically go back there
  "
  
  data_result <- data.frame(p0 = c(0), deltap0 = c(0),p1 = c(0), deltap1 = c(0),p2 = c(0),deltap2 = c(0))
  
  
  
  for(i in 1:length(list_data)){
    
    #clean data
    list_data[[i]][,2][which(is.nan(list_data[[i]][,2]))] = NA
    list_data[[i]][,2][which(list_data[[i]][,2]==Inf)] = NA
    
    #create quadratic coeff
    P_i <- list_data[[i]][,1]
    P_2 <- P_i^2
    
    temp <- lm(delta_i~P_i + P_2, data=list_data[[i]], na.action = na.exclude)
    temp2 <- summary(temp)
    
    col <- data.frame(temp2$coefficients[1,1],temp2$coefficients[1,2],temp2$coefficients[2,1],temp2$coefficients[2,2],temp2$coefficients[3,1],temp2$coefficients[3,2])
    data_result[names(list_data)[i],] <- col
  }
  
  data_result <- data_result[-1,]
  
  if(b_save == TRUE){
    #nametable has to include .csv
    setwd(name_datadirectory)
    write.csv(data_result, name_table)
   setwd(name_maindirectory) 
  }
  
  return(data_result)
}

################################################################################################################################
#4. Misc analysis
################################################################################################################################

#calcualting stdabw sigma
f_stdabw <- function(data_X) {
  "calculates the stadabw for a row of a dataframe x

  Keyword arguments:
  data_X -- row of a dataframe to calculate the standardabw from
  "
  n=length(data_X) ; sqrt(var(data_X) * (n-1) / n)
}
