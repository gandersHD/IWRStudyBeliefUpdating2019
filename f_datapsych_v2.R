## Packages ----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(reshape2)
library(rjags)
library(coda)
library(lattice)
library(broom)
library(ggpubr)
library(ggmcmc)

#functions for creation of psychological datasets

#creates a list of n WMC scores for a dataset of n persons having participated in a visual span task
f_createﬂlistWMC <- function(data_SpanTask){
  "Returns a dataframe with n entries containing the Partial Storage Scores from a visual span task with 2*2 (expample) +2*3 + 2*4 + 2*5) design
  aditionally it contains the original data of the visual span task

  partial storage score,  is the sum of items recalled in the correct serial
  position, regardless of whether the entire trial was recalled correctly;

  Keyword arguments:
  data_SpanTask -- gives the data from the visual span task
  "
  
  FL_Data <- data_SpanTask
  
  ######### Correct = 1, Wrong answer = 0 ##################################
  ############## Sample_a  #################################################
  
  FL_Data$Sample_a_Sym1 <-integer(nrow(FL_Data))
  FL_Data$Sample_a_Sym2 <-integer(nrow(FL_Data))
  
  
  FL_Data <- transform(FL_Data, Sample_a_Sym1 = ifelse(VS35== 2, 1,
                                                       Sample_a_Sym1 ))
  FL_Data <- transform(FL_Data, Sample_a_Sym2 = ifelse(VS07== 2, 1,
                                                       Sample_a_Sym2 ))
  
  FL_Data$Sample_a_Ans <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, Sample_a_Ans = ifelse((VS06  == 2 &
                                                         VS06_07 == 2 & VS06_16 == 2), 1, Sample_a_Ans))
  
  ############## Sample_b  #################################################
  
  FL_Data$Sample_b_Sym1 <- integer(nrow(FL_Data))
  FL_Data$Sample_b_Sym2 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, Sample_b_Sym1 = ifelse(VS08== 1, 1,
                                                       Sample_b_Sym1 ))
  FL_Data <- transform(FL_Data, Sample_b_Sym2 = ifelse(VS09== 2, 1,
                                                       Sample_b_Sym2 ))
  
  FL_Data$Sample_b_Ans  <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, Sample_b_Ans = ifelse((VS36 == 2 & VS36_01
                                                       == 2 & VS36_10 == 2), 1, Sample_b_Ans))
  
  ############## Sample_c  #################################################
  
  FL_Data$Sample_c_Sym1  <- integer(nrow(FL_Data))
  FL_Data$Sample_c_Sym2  <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, Sample_c_Sym1 = ifelse(VS70 == 2, 1, 
                                                       Sample_c_Sym1))
  FL_Data <- transform(FL_Data, Sample_c_Sym2 = ifelse(VS72 == 2, 1, 
                                                       Sample_c_Sym2))
  
  FL_Data$Sample_c_Ans  <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, Sample_c_Ans = ifelse((VS74  == 2 &
                                                         VS74_07 == 2 & VS74_16 == 2), 1, Sample_c_Ans))
  
  
  ############## Sample_d  #################################################
  
  FL_Data$Sample_d_Sym1 <- integer(nrow(FL_Data))
  FL_Data$Sample_d_Sym2 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, Sample_d_Sym1 = ifelse(VS77== 1, 1,
                                                       Sample_d_Sym1 ))
  FL_Data <- transform(FL_Data, Sample_d_Sym2 = ifelse(VS79== 2, 1,
                                                       Sample_d_Sym2 ))
  
  FL_Data$Sample_d_Ans  <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, Sample_d_Ans = ifelse((VS81 == 2 & VS81_01
                                                       == 2 & VS81_10 == 2), 1, Sample_d_Ans))
  
  
  ############## VSpanA_a  #################################################
  
  FL_Data$VSpanA_a_Sym1 <- integer(nrow(FL_Data))
  FL_Data$VSpanA_a_Sym2 <- integer(nrow(FL_Data))
  FL_Data$VSpanA_a_Sym3 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanA_a_Sym1= ifelse(VS04 == 1, 1,
                                                      VSpanA_a_Sym1))
  FL_Data <- transform(FL_Data, VSpanA_a_Sym2= ifelse(VS12 == 1,1,
                                                      VSpanA_a_Sym2))
  FL_Data <- transform(FL_Data, VSpanA_a_Sym3= ifelse(VS13 == 2, 1,
                                                      VSpanA_a_Sym3))
  
  FL_Data$VSpanA_a_Ans <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanA_a_Ans= ifelse((VS37 == 3 & VS37_07 
                                                      == 2 & VS37_08  == 2 & VS37_14 == 2),1, VSpanA_a_Ans))
  
  
  ############## VSpanA_b  #################################################
  
  FL_Data$VSpanA_b_Sym1 <- integer(nrow(FL_Data))
  FL_Data$VSpanA_b_Sym2 <- integer(nrow(FL_Data))
  FL_Data$VSpanA_b_Sym3 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanA_b_Sym1= ifelse(VS14 ==  2, 1,
                                                      VSpanA_b_Sym1))
  FL_Data <- transform(FL_Data, VSpanA_b_Sym2= ifelse(VS15 ==  2, 1,
                                                      VSpanA_b_Sym2))
  FL_Data <- transform(FL_Data, VSpanA_b_Sym3= ifelse(VS16 ==  2, 1,
                                                      VSpanA_b_Sym3))
  
  FL_Data$VSpanA_b_Ans <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanA_b_Ans= ifelse((VS38 ==  3 & VS38_03
                                                      ==  2  & VS38_04 ==  2  & VS38_11 ==  2 ),1, VSpanA_b_Ans))
  
  
  ############## VSpanB_a  #################################################
  
  FL_Data$VSpanB_a_Sym1 <- integer(nrow(FL_Data))
  FL_Data$VSpanB_a_Sym2 <- integer(nrow(FL_Data))
  FL_Data$VSpanB_a_Sym3 <- integer(nrow(FL_Data))
  FL_Data$VSpanB_a_Sym4 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanB_a_Sym1= ifelse(VS17 ==  1,1,
                                                      VSpanB_a_Sym1))
  FL_Data <- transform(FL_Data, VSpanB_a_Sym2= ifelse(VS18 == 
                                                        2,1,VSpanB_a_Sym2))
  FL_Data <- transform(FL_Data, VSpanB_a_Sym3= ifelse(VS19 ==  1,1,
                                                      VSpanB_a_Sym3))
  FL_Data <- transform(FL_Data, VSpanB_a_Sym4= ifelse(VS20 ==  1,1,
                                                      VSpanB_a_Sym4))
  
  FL_Data$VSpanB_a_Ans <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanB_a_Ans= ifelse((VS40 ==  4 & VS40_03
                                                      ==  2  & VS40_15 ==  2  & VS40_09 ==  2  & VS40_12 == 
                                                        2 ),1, VSpanB_a_Ans))
  
  ############## VSpanB_b  #################################################
  
  FL_Data$VSpanB_b_Sym1 <- integer(nrow(FL_Data))
  FL_Data$VSpanB_b_Sym2 <- integer(nrow(FL_Data))
  FL_Data$VSpanB_b_Sym3 <- integer(nrow(FL_Data))
  FL_Data$VSpanB_b_Sym4 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanB_b_Sym1= ifelse(VS21 ==  2,1,
                                                      VSpanB_b_Sym1))
  FL_Data <- transform(FL_Data, VSpanB_b_Sym2= ifelse(VS22 ==  2,1,
                                                      VSpanB_b_Sym2))
  FL_Data <- transform(FL_Data, VSpanB_b_Sym3= ifelse(VS23 ==  1,1,
                                                      VSpanB_b_Sym3))
  FL_Data <- transform(FL_Data, VSpanB_b_Sym4= ifelse(VS24 ==  2,1,
                                                      VSpanB_b_Sym4))
  
  FL_Data$VSpanB_b_Ans <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanB_b_Ans= ifelse((VS39 ==  4 & VS39_05
                                                      ==  2  & VS39_01 ==  2  & VS39_16 ==  2  & VS39_07 == 
                                                        2 ),1, VSpanB_b_Ans))
  
  
  ############## VSpanC_a  #################################################
  
  FL_Data$VSpanC_a_Sym1 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_a_Sym2 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_a_Sym3 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_a_Sym4 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_a_Sym5 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanC_a_Sym1= ifelse(VS25 ==  2,1,
                                                      VSpanC_a_Sym1))
  FL_Data <- transform(FL_Data, VSpanC_a_Sym2= ifelse(VS26 ==  2,1,
                                                      VSpanC_a_Sym2))
  FL_Data <- transform(FL_Data, VSpanC_a_Sym3= ifelse(VS27 ==  2,1,
                                                      VSpanC_a_Sym3))
  FL_Data <- transform(FL_Data, VSpanC_a_Sym4= ifelse(VS28 ==  1,1,
                                                      VSpanC_a_Sym4))
  FL_Data <- transform(FL_Data, VSpanC_a_Sym5= ifelse(VS29 ==  1,1,
                                                      VSpanC_a_Sym5))
  
  FL_Data$VSpanC_a_Ans <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanC_a_Ans= ifelse((VS41 ==  5 & VS41_06
                                                      ==  2  & VS41_11 ==  2  & VS41_09 ==  2  & VS41_10 == 
                                                        2  & VS41_07 ==  2 ),1, VSpanC_a_Ans))
  
  
  ############## VSpanC_b  #################################################
  
  FL_Data$VSpanC_b_Sym1 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_b_Sym2 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_b_Sym3 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_b_Sym4 <- integer(nrow(FL_Data))
  FL_Data$VSpanC_b_Sym5 <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanC_b_Sym1= ifelse(VS30 ==  1,1,
                                                      VSpanC_b_Sym1))
  FL_Data <- transform(FL_Data, VSpanC_b_Sym2= ifelse(VS31 ==  2,1,
                                                      VSpanC_b_Sym2))
  FL_Data <- transform(FL_Data, VSpanC_b_Sym3= ifelse(VS32 ==  2,1,
                                                      VSpanC_b_Sym3))
  FL_Data <- transform(FL_Data, VSpanC_b_Sym4= ifelse(VS33 ==  2,1,
                                                      VSpanC_b_Sym4))
  FL_Data <- transform(FL_Data, VSpanC_b_Sym5= ifelse(VS34 ==  1,1,
                                                      VSpanC_b_Sym5))
  
  FL_Data$VSpanC_b_Ans <- integer(nrow(FL_Data))
  
  FL_Data <- transform(FL_Data, VSpanC_b_Ans= ifelse((VS42 ==  5 & VS42_13
                                                      ==  2  & VS42_16 ==  2  & VS42_02 ==  2  & VS42_15 == 
                                                        2  & VS42_06 ==  2 ),1, VSpanC_b_Ans))
  
  ####### Final Scores ######################################################
  
  ###### Own Partial Storage Score ##########################################
  #                    Max score = 6                                        #
  
  ###### Waehle Bedingung ohne Reihenfolge aus ZU02 = 1 #####################
  
  FL_Data$SampleOwnPartialStorageScore <- integer(nrow(FL_Data))
  
  FL_Data$SampleOwnPartialStorageScore <- (FL_Data$Sample_a_Ans +
                                             FL_Data$Sample_b_Ans)
  
  FL_Data$OwnPartialStorageScore <- integer(nrow(FL_Data)) 
  
  FL_Data$OwnPartialStorageScore <- (FL_Data$VSpanA_a_Ans +
                                       FL_Data$VSpanA_b_Ans + FL_Data$VSpanB_a_Ans + FL_Data$VSpanB_b_Ans +
                                       FL_Data$VSpanC_a_Ans+ FL_Data$VSpanC_b_Ans)
  
  return(FL_Data)
}

#creates a list of n accuraccy error scores for a dataset of n persons having participated in a visual span task
f_createﬂlistAE <- function(data_SpanTask){
  "Returns a dataframe with n entries containing the Acurracy Error Scores from a visual span task with 2*2 (expample) +2*3 + 2*4 + 2*5) design
  aditionally it contains the original data of the visual span task
  !Warning this needs  a df created by f_createlistWMC to function as it needs data created by that function

  accuracy errors, which are the number of processing problems that were answered incorrectly (note that processing errors = speed errors +                             accuracy errors).
  aditionally it contains the original data of the visual span task

  Keyword arguments:
  data_SpanTask -- gives the data from the visual span task
  "
  FL_Data <- data_SpanTask
  
  FL_Data$AccuracyErrorsOwnPartialScore <- (24-(FL_Data$VSpanA_a_Sym1+
                                                  FL_Data$VSpanA_a_Sym2+ FL_Data$VSpanA_a_Sym3+ FL_Data$VSpanA_b_Sym1+
                                                  FL_Data$VSpanA_b_Sym2+ FL_Data$VSpanA_b_Sym3+ FL_Data$VSpanB_a_Sym1+
                                                  FL_Data$VSpanB_a_Sym2+ FL_Data$VSpanB_a_Sym3+ FL_Data$VSpanB_a_Sym4+
                                                  FL_Data$VSpanB_b_Sym1+
                                                  FL_Data$VSpanB_b_Sym2+
                                                  FL_Data$VSpanB_b_Sym3+ FL_Data$VSpanB_b_Sym4+ FL_Data$VSpanC_a_Sym1+
                                                  FL_Data$VSpanC_a_Sym2+ FL_Data$VSpanC_a_Sym3+ FL_Data$VSpanC_a_Sym4+
                                                  FL_Data$VSpanC_a_Sym5+ FL_Data$VSpanC_b_Sym1+ FL_Data$VSpanC_b_Sym2+
                                                  FL_Data$VSpanC_b_Sym3+ FL_Data$VSpanC_b_Sym4+ FL_Data$VSpanC_b_Sym5))
  
  return(FL_Data)
}

#creates a list of the big5 for a VP
f_createﬂlistbig5 <- function(data_questionaire,vector_1,vector_2,vector_3,vector_4,vector_5){
  "Returns a list with a dataframe for each BIg5 factor with an entry for each participant

  Keyword arguments:
  data__questionaire -- gives the data from the questionaire that is used to measure th Big5 (df)
  vector_1 -- gives the info on which questions belong to N and the polarisation of those questions (vector)
  vector_2 -- gives the info on which questions belong to E and the polarisation of those questions (vector)
  vector_3 -- gives the info on which questions belong to O and the polarisation of those questions (vector)
  vector_4 -- gives the info on which questions belong to A and the polarisation of those questions (vector)
  vector_5 -- gives the info on which questions belong to C and the polarisation of those questions (vector)
  "
  
  
  
  #vectors are in the order: N,E,O,A,C
  v_1 <- abs(vector_1)
  v_2 <- abs(vector_2)
  v_3 <- abs(vector_3)
  v_4 <- abs(vector_4)
  v_5 <- abs(vector_5)
  
  list_big5 <- list()
  
  for (i in 1:5) {
    temp <- data_questionaire[,eval(parse(text = (paste0("v_",i))))]
    
    for (j in 1:length(eval(parse(text = (paste0("vector_",i)))))) {
      if(eval(parse(text = (paste0("vector_",i))))[j] < 0){
        temp[,j] <- 6 - temp[,j]
      }
    }
    
    temp2 <- data.frame(mean = c(), std = c())
    
    for (k in 1:nrow(data_questionaire)){
      col_temp2 <- data.frame(mean = mean(unlist(temp[k,])),std = f_stdabw(unlist(temp[k,])))
      temp2 <- rbind(temp2,col_temp2)
    }
    
    Lelem <- temp2
    list_big5[[length(list_big5)+1]] = Lelem
    
  }
  
  return(list_big5)
  
}

#function to create a list of personal coefficient with delta score for fit of delta parameter  
f_createﬂlistdeltai <- function(data_inter, list_coeff,index_coeff,data_prior,data_P){
  "Returns a dataframe of the indivudal scores of the personal variable and the calculated personal delta score

  Keyword arguments:
  data_inter -- gives the personal scores on the interpretations (vector)
  data_prior -- gives the personal scores on the prior (vector)
  list_coeff -- gives the resultlist of the original alpha and gamma parameters (dataframe)
  index_coeff -- gives the row in the resultslist where to find the right coefficients (number)
  data_P -- gives the individual scores on the personal variables (vector)
  "
  
  data_result <- data.frame(P_i = c(0),delta_i = c(0))
  
  if(length(data_inter) == length(data_P)){
      for(i in 1:length(data_inter)){
        temp <- ((data_inter[i] - list_coeff[index_coeff,1] - list_coeff[index_coeff,3] * data_prior[i]) * (data_P[i])**(-1))
      
        col <- data.frame(data_P[i],temp)
        data_result[i,] <- col
      }
    
    data_result <- data_result[-1,]
  }
  
  else{
    for(i in 1:length(data_P)){
      temp <- (data_inter[i] - list_coeff[index_coeff,1] - list_coeff[index_coeff,3] * data_prior[i]) * (data_P[i])**(-1)
      
      col <- data.frame(data_P[i],temp)
      data_result[i,] <- col
    }
    
    for(i in 1:length(data_P)){
      temp <- ((data_inter[i+length(data_P)] - list_coeff[index_coeff,1] - list_coeff[index_coeff,3] * data_prior[i]) * (data_P[i])**(-1))
    
      col <- data.frame(data_P[i],temp)
      data_result[i,] <- col
    }
    
    data_result <- data_result[-1,]
  }

  return(data_result)
}

#creates a list of n dataframes of x and y data by saving multiple instances of f_createlist_deltai and cleanes their entries
f_createﬂcleanlistdeltai <- function(data_inter, list_coeff,index_coeff,data_prior,data_P, number_entries){
  "Returns a list of the indivudal scores of the personal variable and the calculated personal delta score for multiple zsf

  Keyword arguments:
  data_inter -- gives the personal scores on the interpretations (vector)
  data_prior -- gives the personal scores on the prior (vector)
  list_coeff -- gives the resultlist of the original alpha and gamma parameters (dataframe)
  index_coeff -- gives the row in the resultslist where to find the right coefficients (number)
  data_P -- gives the individual scores on the personal variables (vector)
  number_entries -- gives the number of dataframes to be included in the list (number)
  "
  
  list_result <- list()
  
  #create
  for (i in 1:number_entries) {
    list_result[[i]] <- f_createﬂlistdeltai(data_inter[[i]][,2], list_coeff,index_coeff,data_prior[[i]][,1],data_P)
  }
  
  #clean
  for (i in 1:number_entries) {
    list_result[[i]][,2][which(is.nan(list_result[[i]][,2]))] = NA
    list_result[[i]][,2][which(list_result[[i]][,2]==Inf)] = NA
    list_result[[i]][,2][which(list_result[[i]][,2]==-Inf)] = NA
  }
  
  #Warning: naming works only for 3 entries
  list_result <- f_nameﬂlist(list_result,c("pro","con","neu"),3)

  return(list_result)
}

#creates a list of n entries with the meta-d values of each participant
f_createﬂlistmetad <- function(nR_S1, nR_S2, number_VP){
  "returns a vector of all meta-d' values for each participant
  Warning: runnung takes some time

  Keyword arguments:
  numberVP -- gives the  number of persons to calculate the meta-d for (number)
  "
  
  data_metad <- c()
  
  for (i in 1:number_VP) {
    numPart = i # number of participant we want to have a look at
    
    ## Individual meta_d function ------------------------------------------------------
    source("Function_metad_indiv.R")
    output <- metad_indiv(nR_S1 = nR_S1[,numPart], nR_S2 = nR_S2[,numPart])
    
    ## Model output ------------------------------------------------------------
    
    # Mean values 
    Value <- summary(output)
    stat <- data.frame(mean = Value[["statistics"]][, "Mean"])
    stat %<>%
      rownames_to_column(var = "name")
    
    data_metad[i] = stat[11,2]
  }
  
  return(data_metad)
  
}
