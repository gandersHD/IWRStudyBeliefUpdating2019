#Auswertung Hauptstuide v2.0

####################################################################################################################################################################
#1. Header:                                                                                                                                                        #
####################################################################################################################################################################


#1.1 Libraries ########################################################################################################################

library(readxl) #Exelfiles einlesen
library(psych) # Functions for statistics in psychology
#all others are for metad and showing off graphs
library(tidyverse)
library(magrittr)
library(reshape2)
library(rjags)
library(coda)
library(lattice)
library(broom)
library(ggpubr)
library(ggmcmc)

#1.2 global functions########################################################################################################################

source("f_datamod_v2.R")
source("f_dataanaly_v2.R")
source("f_datapsych_v2.R")

#1.3 config ########################################################################################################################

source("config Hauptstudie3_v1.R")

####################################################################################################################################################################

####################################################################################################################################################################
#2. Daten einlesen:                                                                                                                                               #
####################################################################################################################################################################

#2.1 Gesamtdaten ###################################################################################################################################################

list_cutdata <- f_readﬂdataset(name_directoryﬂmain,name_datafileﬂmain,list_topicsﬂmain,names_topicsﬂmain)

#2.2 Data for the 2 topics of the summaries ########################################################################################################################

#create datasets out of the veg and nanocategory

#name the list giving the cuts for the different topics
list_topicsﬂbelief <- f_nameﬂlist(list_topicsﬂbelief,names_topicsﬂbelief)

#topic 1
list_dataﬂtopic1 <- f_createﬂlisttopic(list_cutdata[[1]],list_topicsﬂbelief,vector_topicﬂmodifiersﬂbelief)

#topic 2
list_dataﬂtopic2 <- f_createﬂlisttopic(list_cutdata[[2]],list_topicsﬂbelief,vector_topicﬂmodifiersﬂbelief)

#2.3 Data for demographics ########################################################################################################################

#gender
data_condﬂgender = list_cutdata[[5]][,4]
#education
data_condﬂedu = list_cutdata[[5]][,5]
#age
data_age = list_cutdata[[5]][,7]

#2.4 Big 5 ##########################################################################################################################################################

list_big5 <- f_createﬂlistbig5(list_cutdata[[4]],v_N,v_E,v_O,v_A,v_C)

#2.5 WMC ####################################################################################################################################################

data_WMC = f_createﬂlistWMC(list_cutdata[[7]])

#2.7 Confidence ####################################################################################################################################################

#2.7.1 Grundonfidence
#mittlere Confience ¸ber alle Zsf des Themas f¸r jedes Individuum

#Nanotechnologie
data_confﬂtopic1 <- data.frame(G = c(0), eG = c(0))

for (i in 1:nrow(list_dataﬂtopic1[[3]])) {
  Grundconfcol <- data.frame(mean(unlist(list_dataﬂtopic1[[3]][i,])),
                             f_stdabw(unlist(list_dataﬂtopic1[[3]][i,])))
  data_confﬂtopic1[paste0("VP",i),] <- Grundconfcol
}
data_confﬂtopic1 <- data_confﬂtopic1[-1,]

#vegetarismus
data_confﬂtopic2 <- data.frame(G = c(0), eG = c(0))

for (i in 1:nrow(list_dataﬂtopic2[[3]])) {
  Grundconfcol <- data.frame(mean(unlist(list_dataﬂtopic2[[3]][i,])),
                             f_stdabw(unlist(list_dataﬂtopic2[[3]][i,])))
  data_confﬂtopic2[paste0("VP",i),] <- Grundconfcol
}
data_confﬂtopic2 <- data_confﬂtopic1[-1,]

#2.7.2 meta-d'

#####################################

# Example of meta d calculation for individual subject and
# exemple of trace plots and posterior distribution plots
# using the Function_metad_indiv.R
# AM 2018
#
# Modified by Nadia Said and Gerrit Anders 2019
# nadia.said@iwr.uni-heidelberg.de
# gerrit.anders@stud.uni-heidelberg.de

#####################################

# nR_S1 and nR_S2 should be two vectors
# model output is a large mcmc list and two vectors for d1 and c1

# OUTPUTS
# nR_S1, nR_S2
# these are vectors containing the total number of responses in
# each response category, conditional on presentation of S1 and S2.
#
# e.g. if nR_S1 = [100 50 20 10 5 1], then when stimulus S1 was
# presented, the subject had the following response counts:
# responded S1, rating=3 : 100 times
# responded S1, rating=2 : 50 times
# responded S1, rating=1 : 20 times
# responded S2, rating=1 : 10 times
# responded S2, rating=2 : 5 times
# responded S2, rating=3 : 1 time
#
# The ordering of response / rating counts for S2 should be the same as it
# is for S1. e.g. if nR_S2 = [3 7 8 12 27 89], then when stimulus S2 was
# presented, the subject had the following response counts:
# responded S1, rating=3 : 3 times
# responded S1, rating=2 : 7 times
# responded S1, rating=1 : 8 times
# responded S2, rating=1 : 12 times
# responded S2, rating=2 : 27 times
# responded S2, rating=3 : 89 times


###############################################################################################

#                                     Prepare Data                                            #

###############################################################################################

#### read in data #####

Data_confﬂtopic1 <- list_dataﬂtopic1[[3]]

# Set parameters

ConfidenceLevel = 6 #(0.5 - 1.0)
NumParticpants  = 164

############# Initialize Data Frame ########################

nR_S11 <- data.frame(matrix(ncol = NumParticpants, nrow = 2*ConfidenceLevel))
colnames(nR_S11) <- paste0("p", c(1:NumParticpants))

nR_S21 <- data.frame(matrix(ncol = NumParticpants, nrow = 2*ConfidenceLevel))
colnames(nR_S21) <- paste0("p", c(1:NumParticpants))


# Split Dataset 

#Warning: das hier muss noch verallgemeinert werden

# Stimulus S1 was presented/ True Statements: 
# Cases in which S1 was presented & whether or not participants did answer correctly + 
# participants confidence in their answer      

# Richtige (Pro) Statements in Bezug auf ClimateChange waren: 1,2 (topic1) and 1,2 (topic2)

S1 = data.frame(list_dataﬂtopic1[[2]][,1],list_dataﬂtopic1[[2]][,2],list_dataﬂtopic2[[2]][,1],list_dataﬂtopic2[[2]][,2],
                list_dataﬂtopic1[[3]][,1],list_dataﬂtopic1[[3]][,2],list_dataﬂtopic2[[3]][,1],list_dataﬂtopic2[[3]][,2])

# Stimulus S2 was presented/ False (Con) Statements:
# Cases in which S2 was presented & whether or not participants did answer correctly + 
# participants confidence in their answer   

# Falsche Statements in Bezug auf ClimateChange waren: 3,4 (topic1) and 3,4 (topic2)

S2 = data.frame(list_dataﬂtopic1[[2]][,3],list_dataﬂtopic1[[2]][,4],list_dataﬂtopic2[[2]][,3],list_dataﬂtopic2[[2]][,4],
                list_dataﬂtopic1[[3]][,3],list_dataﬂtopic1[[3]][,4],list_dataﬂtopic2[[3]][,3],list_dataﬂtopic2[[3]][,4])


###################### Prepare Data #############################################

#Warning: Hier fehlt ein allgemeines Kriterium
# S1 = TRUE, Confidence level = 6 (0.5 - 1.0)

for (i in 1:NumParticpants){ 
  nR_S1 <- rep(0, 12)
  for (n in 1:4){
    if (S1[i,n] > 3){
      
      if (S1[i,n+4] == 6){
        nR_S1[1] = nR_S1[1]+1
      }
      
      if (S1[i,n+4] == 5){
        nR_S1[2] = nR_S1[2]+1
      }
      if (S1[i,n+4] == 4){
        nR_S1[3] = nR_S1[3]+1
      }
      if (S1[i,n+4] == 3){
        nR_S1[4] = nR_S1[4]+1
      }
      if (S1[i,n+4] == 2){
        nR_S1[5] = nR_S1[5]+1
      }
      if (S1[i,n+4] == 1){
        nR_S1[6] = nR_S1[6]+1
      }
    }
    if (S1[i,n] < 3){
      
      if (S1[i,n+4] == 6){
        nR_S1[12] = nR_S1[12]+1
      }
      if (S1[i,n+4] == 5){
        nR_S1[11] = nR_S1[11]+1
      }
      if (S1[i,n+4] == 4){
        nR_S1[10] = nR_S1[10]+1
      }
      if (S1[i,n+4] == 3){
        nR_S1[9] = nR_S1[9]+1
      }
      if (S1[i,n+4] == 2){
        nR_S1[8] = nR_S1[8]+1
      }
      if (S1[i,n+4] == 1){
        nR_S1[7] = nR_S1[7]+1
      }
    }
  }
  nR_S11[i] <- nR_S1
}

#S2 = FALSE

for (i in 1:NumParticpants){ 
  nR_S2 <- rep(0, 12)
  for (n in 1:4){
    if (S2[i,n] < -3){
      
      if (S2[i,n+4] == 6){
        nR_S2[12] = nR_S2[12]+1
      }
      
      if (S2[i,n+4] == 5){
        nR_S2[11] = nR_S2[11]+1
      }
      if (S2[i,n+4] == 4){
        nR_S2[10] = nR_S2[10]+1
      }
      if (S2[i,n+4] == 3){
        nR_S2[9] = nR_S2[9]+1
      }
      if (S2[i,n+4] == 2){
        nR_S2[5] = nR_S2[5]+1
      }
      if (S2[i,n+4] == 1){
        nR_S2[8] = nR_S2[8]+1
      }
    }
    
    if (S2[i,n] > -3){
      
      if (S2[i,n+4] == 6){
        nR_S2[1] = nR_S2[1]+1
      }
      if (S2[i,n+4] == 5){
        nR_S2[2] = nR_S2[2]+1
      }
      if (S2[i,n+4] == 4){
        nR_S2[3] = nR_S2[3]+1
      }
      if (S2[i,n+4] == 3){
        nR_S2[4] = nR_S2[4]+1
      }
      if (S2[i,n+4] == 2){
        nR_S2[5] = nR_S2[5]+1
      }
      if (S2[i,n+4] == 1){
        nR_S2[6] = nR_S2[6]+1
      }
    }
  }
  nR_S21[i] <- nR_S2
}

nR_S1 <- nR_S11
nR_S2 <- nR_S21

#Warning: wenn man das hier ausf¸hrt dauert es ne weile
data_metad <- f_createﬂlistmetad(nR_S1,nR_S2,NumParticpants)

####################################################################################################################################################################
#3. Daten f¸r analyse vorbereiten:                                                                                                                                 #
####################################################################################################################################################################

#3.1 data for belief updating ######################################################################################################################################

#es werden zwei Arten der Regression betrachtet:
#im 1. Fall wird jede Zusammenfassung einzeln betrachtet.
#im 2. Fall werden Pos/Con/Neu zusammengefasst.

#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1 = list_dataﬂtopic1[[1]][,1]
data_Yﬂtopic1ﬂcase1 = list_dataﬂtopic1[[2]]

data_Xﬂtopic2ﬂcase1 = list_dataﬂtopic2[[1]][,1]
data_Yﬂtopic2ﬂcase1 = list_dataﬂtopic2[[2]]

list_dataregﬂtopic1ﬂcase1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1,data_Yﬂtopic1ﬂcase1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1,data_Yﬂtopic2ﬂcase1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.2 data for belief with demographics #############################################################################################################################

#3.2.1 Geschlecht

#Trennung nach Geschlecht (1=weiblich, 2=m‰nnlich)

#Nanotechnologie
list_data1ﬂtopic1ﬂgender <- f_data_conditioner(list_dataﬂtopic1[[1]],N_Condﬂgender,"gender",b_continous = FALSE,data_condﬂgender, condition =  conditionﬂgender)
list_data2ﬂtopic1ﬂgender <- f_data_conditioner(list_dataﬂtopic1[[2]],N_Condﬂgender,"gender",b_continous = FALSE,data_condﬂgender, condition =  conditionﬂgender)
list_data3ﬂtopic1ﬂgender <- f_data_conditioner(list_dataﬂtopic1[[3]],N_Condﬂgender,"gender",b_continous = FALSE,data_condﬂgender, condition =  conditionﬂgender)

#vegetarismus
list_data1ﬂtopic2ﬂgender <- f_data_conditioner(list_dataﬂtopic2[[1]],N_Condﬂgender,"gender",b_continous = FALSE,data_condﬂgender, condition =  conditionﬂgender)
list_data2ﬂtopic2ﬂgender <- f_data_conditioner(list_dataﬂtopic2[[2]],N_Condﬂgender,"gender",b_continous = FALSE,data_condﬂgender, condition =  conditionﬂgender)
list_data3ﬂtopic2ﬂgender <- f_data_conditioner(list_dataﬂtopic2[[3]],N_Condﬂgender,"gender",b_continous = FALSE,data_condﬂgender, condition =  conditionﬂgender)

#Erstellung der Regressionsdaten

#m‰nnlich
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂgender1 = list_data1ﬂtopic1ﬂgender[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂgender1 = list_data2ﬂtopic1ﬂgender[[1]]

data_Xﬂtopic2ﬂcase1ﬂgender1 = list_data1ﬂtopic2ﬂgender[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂgender1 = list_data2ﬂtopic2ﬂgender[[1]]

list_dataregﬂtopic1ﬂcase1ﬂgender1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂgender1,data_Yﬂtopic1ﬂcase1ﬂgender1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂgender1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂgender1,data_Yﬂtopic2ﬂcase1ﬂgender1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂgender1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂgender1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂgender1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂgender1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#weiblich
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂgender2 = list_data1ﬂtopic1ﬂgender[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂgender2 = list_data2ﬂtopic1ﬂgender[[2]]

data_Xﬂtopic2ﬂcase1ﬂgender2 = list_data1ﬂtopic2ﬂgender[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂgender2 = list_data2ﬂtopic2ﬂgender[[2]]

list_dataregﬂtopic1ﬂcase1ﬂgender2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂgender2,data_Yﬂtopic1ﬂcase1ﬂgender2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂgender2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂgender2,data_Yﬂtopic2ﬂcase1ﬂgender2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂgender2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂgender2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂgender2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂgender2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)


#3.2.2 Education

#Trennung nach Bildungsgrad (1,2,3=non-academic 4,5=academic)

#Nanotechnologie
list_data1ﬂtopic1ﬂedu <- f_data_conditioner(list_dataﬂtopic1[[1]],N_Condeﬂedu,"education",b_continous = FALSE,data_condﬂedu, condition =  conditionﬂedu)
list_data2ﬂtopic1ﬂedu <- f_data_conditioner(list_dataﬂtopic1[[2]],N_Condeﬂedu,"education",b_continous = FALSE,data_condﬂedu, condition =  conditionﬂedu)
list_data3ﬂtopic1ﬂedu <- f_data_conditioner(list_dataﬂtopic1[[3]],N_Condeﬂedu,"education",b_continous = FALSE,data_condﬂedu, condition =  conditionﬂedu)

#vegetarismus
list_data1ﬂtopic2ﬂedu <- f_data_conditioner(list_dataﬂtopic2[[1]],N_Condeﬂedu,"education",b_continous = FALSE,data_condﬂedu, condition =  conditionﬂedu)
list_data2ﬂtopic2ﬂedu <- f_data_conditioner(list_dataﬂtopic2[[2]],N_Condeﬂedu,"education",b_continous = FALSE,data_condﬂedu, condition =  conditionﬂedu)
list_data3ﬂtopic2ﬂedu <- f_data_conditioner(list_dataﬂtopic2[[3]],N_Condeﬂedu,"education",b_continous = FALSE,data_condﬂedu, condition =  conditionﬂedu)

#Erstellung der Regressionsdaten

#academic
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂedu1 = list_data1ﬂtopic1ﬂedu[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂedu1 = list_data2ﬂtopic1ﬂedu[[1]]

data_Xﬂtopic2ﬂcase1ﬂedu1 = list_data1ﬂtopic2ﬂedu[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂedu1 = list_data2ﬂtopic2ﬂedu[[1]]

list_dataregﬂtopic1ﬂcase1ﬂedu1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂedu1,data_Yﬂtopic1ﬂcase1ﬂedu1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂedu1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂedu1,data_Yﬂtopic2ﬂcase1ﬂedu1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂedu1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂedu1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂedu1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂedu1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#non-academic
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂedu2 = list_data1ﬂtopic1ﬂedu[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂedu2 = list_data2ﬂtopic1ﬂedu[[2]]

data_Xﬂtopic2ﬂcase1ﬂedu2 = list_data1ﬂtopic2ﬂedu[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂedu2 = list_data2ﬂtopic2ﬂedu[[2]]

list_dataregﬂtopic1ﬂcase1ﬂedu2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂedu2,data_Yﬂtopic1ﬂcase1ﬂedu2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂedu2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂedu2,data_Yﬂtopic2ﬂcase1ﬂedu2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂedu2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂedu2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂedu2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂedu2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.2.3 age 

#Infos zum Alter
N_medianﬂage = median(data_age[[1]])
N_minﬂage = min(data_age[[1]])
N_maxﬂage = max(data_age[[1]])

#Trennung nach ALter (Trennung an N_medianﬂage)

#Nanotechnologie
list_data1ﬂtopic1ﬂage <- f_data_conditioner(list_dataﬂtopic1[[1]],N_Condﬂage,"age",b_continous = TRUE,data_age, condition =  N_medianﬂage)
list_data2ﬂtopic1ﬂage <- f_data_conditioner(list_dataﬂtopic1[[2]],N_Condﬂage,"age",b_continous = TRUE,data_age, condition =  N_medianﬂage)
list_data3ﬂtopic1ﬂage <- f_data_conditioner(list_dataﬂtopic1[[3]],N_Condﬂage,"age",b_continous = TRUE,data_age, condition =  N_medianﬂage)

#vegetarismus
list_data1ﬂtopic2ﬂage <- f_data_conditioner(list_dataﬂtopic2[[1]],N_Condﬂage,"age",b_continous = TRUE,data_age, condition =  N_medianﬂage)
list_data2ﬂtopic2ﬂage <- f_data_conditioner(list_dataﬂtopic2[[2]],N_Condﬂage,"age",b_continous = TRUE,data_age, condition =  N_medianﬂage)
list_data3ﬂtopic2ﬂage <- f_data_conditioner(list_dataﬂtopic2[[3]],N_Condﬂage,"age",b_continous = TRUE,data_age, condition =  N_medianﬂage)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂage1 = list_data1ﬂtopic1ﬂage[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂage1 = list_data2ﬂtopic1ﬂage[[1]]

data_Xﬂtopic2ﬂcase1ﬂage1 = list_data1ﬂtopic2ﬂage[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂage1 = list_data2ﬂtopic2ﬂage[[1]]

list_dataregﬂtopic1ﬂcase1ﬂage1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂage1,data_Yﬂtopic1ﬂcase1ﬂage1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂage1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂage1,data_Yﬂtopic2ﬂcase1ﬂage1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂage1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂage1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂage1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂage1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂage2 = list_data1ﬂtopic1ﬂage[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂage2 = list_data2ﬂtopic1ﬂage[[2]]

data_Xﬂtopic2ﬂcase1ﬂage2 = list_data1ﬂtopic2ﬂage[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂage2 = list_data2ﬂtopic2ﬂage[[2]]

list_dataregﬂtopic1ﬂcase1ﬂage2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂage2,data_Yﬂtopic1ﬂcase1ﬂage2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂage2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂage2,data_Yﬂtopic2ﬂcase1ﬂage2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂage2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂage2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂage2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂage2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5 Belief updating und personenbezogene Faktoren ####################################################################################################################################################

#3.5.1 Regression

#3.5.1.1 Regression nach WMC

#Infos zum WMC
N_medianﬂWMC = median(data_WMC[[263]])
N_CondﬂWMC = 2

#Trennung nach WMC (Trennung an N_medianﬂWMC)

#Nanotechnologie
list_data1ﬂtopic1ﬂWMC <- f_data_conditioner(list_dataﬂtopic1[[1]],N_CondﬂWMC,"WMC",b_continous = TRUE,data_WMC[[263]], condition =  N_medianﬂWMC)
list_data2ﬂtopic1ﬂWMC <- f_data_conditioner(list_dataﬂtopic1[[2]],N_CondﬂWMC,"WMC",b_continous = TRUE,data_WMC[[263]], condition =  N_medianﬂWMC)
list_data3ﬂtopic1ﬂWMC <- f_data_conditioner(list_dataﬂtopic1[[3]],N_CondﬂWMC,"WMC",b_continous = TRUE,data_WMC[[263]], condition =  N_medianﬂWMC)

#vegetarismus
list_data1ﬂtopic2ﬂWMC <- f_data_conditioner(list_dataﬂtopic2[[1]],N_CondﬂWMC,"WMC",b_continous = TRUE,data_WMC[[263]], condition =  N_medianﬂWMC)
list_data2ﬂtopic2ﬂWMC <- f_data_conditioner(list_dataﬂtopic2[[2]],N_CondﬂWMC,"WMC",b_continous = TRUE,data_WMC[[263]], condition =  N_medianﬂWMC)
list_data3ﬂtopic2ﬂWMC <- f_data_conditioner(list_dataﬂtopic2[[3]],N_CondﬂWMC,"WMC",b_continous = TRUE,data_WMC[[263]], condition =  N_medianﬂWMC)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂWMC1 = list_data1ﬂtopic1ﬂWMC[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂWMC1 = list_data2ﬂtopic1ﬂWMC[[1]]

data_Xﬂtopic2ﬂcase1ﬂWMC1 = list_data1ﬂtopic2ﬂWMC[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂWMC1 = list_data2ﬂtopic2ﬂWMC[[1]]

list_dataregﬂtopic1ﬂcase1ﬂWMC1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂWMC1,data_Yﬂtopic1ﬂcase1ﬂWMC1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂWMC1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂWMC1,data_Yﬂtopic2ﬂcase1ﬂWMC1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂWMC1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂWMC1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂWMC1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂWMC1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂWMC2 = list_data1ﬂtopic1ﬂWMC[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂWMC2 = list_data2ﬂtopic1ﬂWMC[[2]]

data_Xﬂtopic2ﬂcase1ﬂWMC2 = list_data1ﬂtopic2ﬂWMC[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂWMC2 = list_data2ﬂtopic2ﬂWMC[[2]]

list_dataregﬂtopic1ﬂcase1ﬂWMC2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂWMC2,data_Yﬂtopic1ﬂcase1ﬂWMC2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂWMC2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂWMC2,data_Yﬂtopic2ﬂcase1ﬂWMC2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂWMC2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂWMC2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂWMC2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂWMC2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5.1.2 Regression nach N

#Infos zum N
N_medianﬂN = median(list_big5[[1]][,1])
N_CondﬂN = 2

#Trennung nach WMC (Trennung an N_medianﬂN)

#Nanotechnologie
list_data1ﬂtopic1ﬂN <- f_data_conditioner(list_dataﬂtopic1[[1]],N_CondﬂN,"N",b_continous = TRUE,list_big5[[1]][,1], condition =  N_medianﬂN)
list_data2ﬂtopic1ﬂN <- f_data_conditioner(list_dataﬂtopic1[[2]],N_CondﬂN,"N",b_continous = TRUE,list_big5[[1]][,1], condition =  N_medianﬂN)
list_data3ﬂtopic1ﬂN <- f_data_conditioner(list_dataﬂtopic1[[3]],N_CondﬂN,"N",b_continous = TRUE,list_big5[[1]][,1], condition =  N_medianﬂN)

#vegetarismus
list_data1ﬂtopic2ﬂN <- f_data_conditioner(list_dataﬂtopic2[[1]],N_CondﬂN,"N",b_continous = TRUE,list_big5[[1]][,1], condition =  N_medianﬂN)
list_data2ﬂtopic2ﬂN <- f_data_conditioner(list_dataﬂtopic2[[2]],N_CondﬂN,"N",b_continous = TRUE,list_big5[[1]][,1], condition =  N_medianﬂN)
list_data3ﬂtopic2ﬂN <- f_data_conditioner(list_dataﬂtopic2[[3]],N_CondﬂN,"N",b_continous = TRUE,list_big5[[1]][,1], condition =  N_medianﬂN)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂN1 = list_data1ﬂtopic1ﬂN[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂN1 = list_data2ﬂtopic1ﬂN[[1]]

data_Xﬂtopic2ﬂcase1ﬂN1 = list_data1ﬂtopic2ﬂN[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂN1 = list_data2ﬂtopic2ﬂN[[1]]

list_dataregﬂtopic1ﬂcase1ﬂN1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂN1,data_Yﬂtopic1ﬂcase1ﬂN1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂN1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂN1,data_Yﬂtopic2ﬂcase1ﬂN1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂN1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂN1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂN1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂN1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂN2 = list_data1ﬂtopic1ﬂN[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂN2 = list_data2ﬂtopic1ﬂN[[2]]

data_Xﬂtopic2ﬂcase1ﬂN2 = list_data1ﬂtopic2ﬂN[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂN2 = list_data2ﬂtopic2ﬂN[[2]]

list_dataregﬂtopic1ﬂcase1ﬂN2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂN2,data_Yﬂtopic1ﬂcase1ﬂN2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂN2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂN2,data_Yﬂtopic2ﬂcase1ﬂN2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂN2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂN2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂN2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂN2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5.1.3 Regression nach E

#Infos zum E
N_medianﬂE = median(list_big5[[2]][,1])
N_CondﬂE = 2

#Trennung nach E (Trennung an N_medianﬂE)

#Nanotechnologie
list_data1ﬂtopic1ﬂE <- f_data_conditioner(list_dataﬂtopic1[[1]],N_CondﬂE,"E",b_continous = TRUE,list_big5[[2]][,1], condition =  N_medianﬂE)
list_data2ﬂtopic1ﬂE <- f_data_conditioner(list_dataﬂtopic1[[2]],N_CondﬂE,"E",b_continous = TRUE,list_big5[[2]][,1], condition =  N_medianﬂE)
list_data3ﬂtopic1ﬂE <- f_data_conditioner(list_dataﬂtopic1[[3]],N_CondﬂE,"E",b_continous = TRUE,list_big5[[2]][,1], condition =  N_medianﬂE)

#vegetarismus
list_data1ﬂtopic2ﬂE <- f_data_conditioner(list_dataﬂtopic2[[1]],N_CondﬂE,"E",b_continous = TRUE,list_big5[[2]][,1], condition =  N_medianﬂE)
list_data2ﬂtopic2ﬂE <- f_data_conditioner(list_dataﬂtopic2[[2]],N_CondﬂE,"E",b_continous = TRUE,list_big5[[2]][,1], condition =  N_medianﬂE)
list_data3ﬂtopic2ﬂE <- f_data_conditioner(list_dataﬂtopic2[[3]],N_CondﬂE,"E",b_continous = TRUE,list_big5[[2]][,1], condition =  N_medianﬂE)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂE1 = list_data1ﬂtopic1ﬂE[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂE1 = list_data2ﬂtopic1ﬂE[[1]]

data_Xﬂtopic2ﬂcase1ﬂE1 = list_data1ﬂtopic2ﬂE[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂE1 = list_data2ﬂtopic2ﬂE[[1]]

list_dataregﬂtopic1ﬂcase1ﬂE1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂE1,data_Yﬂtopic1ﬂcase1ﬂE1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂE1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂE1,data_Yﬂtopic2ﬂcase1ﬂE1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂE1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂE1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂE1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂE1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂE2 = list_data1ﬂtopic1ﬂE[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂE2 = list_data2ﬂtopic1ﬂE[[2]]

data_Xﬂtopic2ﬂcase1ﬂE2 = list_data1ﬂtopic2ﬂE[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂE2 = list_data2ﬂtopic2ﬂE[[2]]

list_dataregﬂtopic1ﬂcase1ﬂE2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂE2,data_Yﬂtopic1ﬂcase1ﬂE2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂE2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂE2,data_Yﬂtopic2ﬂcase1ﬂE2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂE2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂE2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂE2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂE2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5.1.4 Regression nach O

#Infos zum O
N_medianﬂO = median(list_big5[[3]][,1])
N_CondﬂO = 2

#Trennung nach O (Trennung an N_medianﬂO)

#Nanotechnologie
list_data1ﬂtopic1ﬂO <- f_data_conditioner(list_dataﬂtopic1[[1]],N_CondﬂO,"O",b_continous = TRUE,list_big5[[3]][,1], condition =  N_medianﬂO)
list_data2ﬂtopic1ﬂO <- f_data_conditioner(list_dataﬂtopic1[[2]],N_CondﬂO,"O",b_continous = TRUE,list_big5[[3]][,1], condition =  N_medianﬂO)
list_data3ﬂtopic1ﬂO <- f_data_conditioner(list_dataﬂtopic1[[3]],N_CondﬂO,"O",b_continous = TRUE,list_big5[[3]][,1], condition =  N_medianﬂO)

#vegetarismus
list_data1ﬂtopic2ﬂO <- f_data_conditioner(list_dataﬂtopic2[[1]],N_CondﬂO,"O",b_continous = TRUE,list_big5[[3]][,1], condition =  N_medianﬂO)
list_data2ﬂtopic2ﬂO <- f_data_conditioner(list_dataﬂtopic2[[2]],N_CondﬂO,"O",b_continous = TRUE,list_big5[[3]][,1], condition =  N_medianﬂO)
list_data3ﬂtopic2ﬂO <- f_data_conditioner(list_dataﬂtopic2[[3]],N_CondﬂO,"O",b_continous = TRUE,list_big5[[3]][,1], condition =  N_medianﬂO)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂO1 = list_data1ﬂtopic1ﬂO[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂO1 = list_data2ﬂtopic1ﬂO[[1]]

data_Xﬂtopic2ﬂcase1ﬂO1 = list_data1ﬂtopic2ﬂO[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂO1 = list_data2ﬂtopic2ﬂO[[1]]

list_dataregﬂtopic1ﬂcase1ﬂO1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂO1,data_Yﬂtopic1ﬂcase1ﬂO1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂO1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂO1,data_Yﬂtopic2ﬂcase1ﬂO1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂO1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂO1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂO1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂO1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂO2 = list_data1ﬂtopic1ﬂO[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂO2 = list_data2ﬂtopic1ﬂO[[2]]

data_Xﬂtopic2ﬂcase1ﬂO2 = list_data1ﬂtopic2ﬂO[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂO2 = list_data2ﬂtopic2ﬂO[[2]]

list_dataregﬂtopic1ﬂcase1ﬂO2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂO2,data_Yﬂtopic1ﬂcase1ﬂO2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂO2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂO2,data_Yﬂtopic2ﬂcase1ﬂO2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂO2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂO2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂO2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂO2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5.1.5 Regression nach A

#Infos zum A
N_medianﬂA = median(list_big5[[4]][,1])
N_CondﬂA = 2

#Trennung nach A (Trennung an N_medianﬂA)

#Nanotechnologie
list_data1ﬂtopic1ﬂA <- f_data_conditioner(list_dataﬂtopic1[[1]],N_CondﬂA,"A",b_continous = TRUE,list_big5[[4]][,1], condition =  N_medianﬂA)
list_data2ﬂtopic1ﬂA <- f_data_conditioner(list_dataﬂtopic1[[2]],N_CondﬂA,"A",b_continous = TRUE,list_big5[[4]][,1], condition =  N_medianﬂA)
list_data3ﬂtopic1ﬂA <- f_data_conditioner(list_dataﬂtopic1[[3]],N_CondﬂA,"A",b_continous = TRUE,list_big5[[4]][,1], condition =  N_medianﬂA)

#vegetarismus
list_data1ﬂtopic2ﬂA <- f_data_conditioner(list_dataﬂtopic2[[1]],N_CondﬂA,"A",b_continous = TRUE,list_big5[[4]][,1], condition =  N_medianﬂA)
list_data2ﬂtopic2ﬂA <- f_data_conditioner(list_dataﬂtopic2[[2]],N_CondﬂA,"A",b_continous = TRUE,list_big5[[4]][,1], condition =  N_medianﬂA)
list_data3ﬂtopic2ﬂA <- f_data_conditioner(list_dataﬂtopic2[[3]],N_CondﬂA,"A",b_continous = TRUE,list_big5[[4]][,1], condition =  N_medianﬂA)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂA1 = list_data1ﬂtopic1ﬂA[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂA1 = list_data2ﬂtopic1ﬂA[[1]]

data_Xﬂtopic2ﬂcase1ﬂA1 = list_data1ﬂtopic2ﬂA[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂA1 = list_data2ﬂtopic2ﬂA[[1]]

list_dataregﬂtopic1ﬂcase1ﬂA1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂA1,data_Yﬂtopic1ﬂcase1ﬂA1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂA1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂA1,data_Yﬂtopic2ﬂcase1ﬂA1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂA1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂA1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂA1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂA1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂA2 = list_data1ﬂtopic1ﬂA[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂA2 = list_data2ﬂtopic1ﬂA[[2]]

data_Xﬂtopic2ﬂcase1ﬂA2 = list_data1ﬂtopic2ﬂA[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂA2 = list_data2ﬂtopic2ﬂA[[2]]

list_dataregﬂtopic1ﬂcase1ﬂA2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂA2,data_Yﬂtopic1ﬂcase1ﬂA2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂA2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂA2,data_Yﬂtopic2ﬂcase1ﬂA2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂA2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂA2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂA2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂA2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5.1.6 Regression nach C

#Infos zum C
N_medianﬂC = median(list_big5[[5]][,1])
N_CondﬂC = 2

#Trennung nach C (Trennung an N_medianﬂC)

#Nanotechnologie
list_data1ﬂtopic1ﬂC <- f_data_conditioner(list_dataﬂtopic1[[1]],N_CondﬂC,"C",b_continous = TRUE,list_big5[[5]][,1], condition =  N_medianﬂC)
list_data2ﬂtopic1ﬂC <- f_data_conditioner(list_dataﬂtopic1[[2]],N_CondﬂC,"C",b_continous = TRUE,list_big5[[5]][,1], condition =  N_medianﬂC)
list_data3ﬂtopic1ﬂC <- f_data_conditioner(list_dataﬂtopic1[[3]],N_CondﬂC,"C",b_continous = TRUE,list_big5[[5]][,1], condition =  N_medianﬂC)

#vegetarismus
list_data1ﬂtopic2ﬂC <- f_data_conditioner(list_dataﬂtopic2[[1]],N_CondﬂC,"C",b_continous = TRUE,list_big5[[5]][,1], condition =  N_medianﬂC)
list_data2ﬂtopic2ﬂC <- f_data_conditioner(list_dataﬂtopic2[[2]],N_CondﬂC,"C",b_continous = TRUE,list_big5[[5]][,1], condition =  N_medianﬂC)
list_data3ﬂtopic2ﬂC <- f_data_conditioner(list_dataﬂtopic2[[3]],N_CondﬂC,"C",b_continous = TRUE,list_big5[[5]][,1], condition =  N_medianﬂC)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂC1 = list_data1ﬂtopic1ﬂC[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂC1 = list_data2ﬂtopic1ﬂC[[1]]

data_Xﬂtopic2ﬂcase1ﬂC1 = list_data1ﬂtopic2ﬂC[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂC1 = list_data2ﬂtopic2ﬂC[[1]]

list_dataregﬂtopic1ﬂcase1ﬂC1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂC1,data_Yﬂtopic1ﬂcase1ﬂC1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂC1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂC1,data_Yﬂtopic2ﬂcase1ﬂC1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂC1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂC1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂC1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂC1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂC2 = list_data1ﬂtopic1ﬂC[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂC2 = list_data2ﬂtopic1ﬂC[[2]]

data_Xﬂtopic2ﬂcase1ﬂC2 = list_data1ﬂtopic2ﬂC[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂC2 = list_data2ﬂtopic2ﬂC[[2]]

list_dataregﬂtopic1ﬂcase1ﬂC2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂC2,data_Yﬂtopic1ﬂcase1ﬂC2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂC2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂC2,data_Yﬂtopic2ﬂcase1ﬂC2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂC2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂC2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂC2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂC2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5.1.7 Regression nach Gconf

#Infos zum Gconf
N_medianﬂGconfﬂtopic1 = median(data_confﬂtopic1[,1])
N_medianﬂGconfﬂtopic2 = median(data_confﬂtopic2[,1])
N_CondﬂGconf = 2

#Trennung nach Gconf (Trennung an N_medianﬂGconf)

#Nanotechnologie
list_data1ﬂtopic1ﬂGconf <- f_data_conditioner(list_dataﬂtopic1[[1]],N_CondﬂGconf,"Gconf",b_continous = TRUE,data_confﬂtopic1[,1], condition =  N_medianﬂGconfﬂtopic1)
list_data2ﬂtopic1ﬂGconf <- f_data_conditioner(list_dataﬂtopic1[[2]],N_CondﬂGconf,"Gconf",b_continous = TRUE,data_confﬂtopic1[,1], condition =  N_medianﬂGconfﬂtopic1)
list_data3ﬂtopic1ﬂGconf <- f_data_conditioner(list_dataﬂtopic1[[3]],N_CondﬂGconf,"Gconf",b_continous = TRUE,data_confﬂtopic1[,1], condition =  N_medianﬂGconfﬂtopic1)

#vegetarismus
list_data1ﬂtopic2ﬂGconf <- f_data_conditioner(list_dataﬂtopic2[[1]],N_CondﬂGconf,"Gconf",b_continous = TRUE,data_confﬂtopic2[,1], condition =  N_medianﬂGconfﬂtopic2)
list_data2ﬂtopic2ﬂGconf <- f_data_conditioner(list_dataﬂtopic2[[2]],N_CondﬂGconf,"Gconf",b_continous = TRUE,data_confﬂtopic2[,1], condition =  N_medianﬂGconfﬂtopic2)
list_data3ﬂtopic2ﬂGconf <- f_data_conditioner(list_dataﬂtopic2[[3]],N_CondﬂGconf,"Gconf",b_continous = TRUE,data_confﬂtopic2[,1], condition =  N_medianﬂGconfﬂtopic2)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂGconf1 = list_data1ﬂtopic1ﬂGconf[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂGconf1 = list_data2ﬂtopic1ﬂGconf[[1]]

data_Xﬂtopic2ﬂcase1ﬂGconf1 = list_data1ﬂtopic2ﬂGconf[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂGconf1 = list_data2ﬂtopic2ﬂGconf[[1]]

list_dataregﬂtopic1ﬂcase1ﬂGconf1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂGconf1,data_Yﬂtopic1ﬂcase1ﬂGconf1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂGconf1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂGconf1,data_Yﬂtopic2ﬂcase1ﬂGconf1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂGconf1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂGconf1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂGconf1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂGconf1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂGconf2 = list_data1ﬂtopic1ﬂGconf[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂGconf2 = list_data2ﬂtopic1ﬂGconf[[2]]

data_Xﬂtopic2ﬂcase1ﬂGconf2 = list_data1ﬂtopic2ﬂGconf[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂGconf2 = list_data2ﬂtopic2ﬂGconf[[2]]

list_dataregﬂtopic1ﬂcase1ﬂGconf2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂGconf2,data_Yﬂtopic1ﬂcase1ﬂGconf2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂGconf2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂGconf2,data_Yﬂtopic2ﬂcase1ﬂGconf2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂGconf2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂGconf2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂGconf2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂGconf2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#3.5.1.8 Regression nach metad

#Infos zum metad
N_medianﬂmetad = median(data_metad)
N_Condﬂmetad = 2

#Trennung nach metad (Trennung an N_medianﬂmetad)

#Nanotechnologie
list_data1ﬂtopic1ﬂmetad <- f_data_conditioner(list_dataﬂtopic1[[1]],N_Condﬂmetad,"metad",b_continous = TRUE,data_metad, condition =  N_medianﬂmetad)
list_data2ﬂtopic1ﬂmetad <- f_data_conditioner(list_dataﬂtopic1[[2]],N_Condﬂmetad,"metad",b_continous = TRUE,data_metad, condition =  N_medianﬂmetad)
list_data3ﬂtopic1ﬂmetad <- f_data_conditioner(list_dataﬂtopic1[[3]],N_Condﬂmetad,"metad",b_continous = TRUE,data_metad, condition =  N_medianﬂmetad)

#vegetarismus
list_data1ﬂtopic2ﬂmetad <- f_data_conditioner(list_dataﬂtopic2[[1]],N_Condﬂmetad,"metad",b_continous = TRUE,data_metad, condition =  N_medianﬂmetad)
list_data2ﬂtopic2ﬂmetad <- f_data_conditioner(list_dataﬂtopic2[[2]],N_Condﬂmetad,"metad",b_continous = TRUE,data_metad, condition =  N_medianﬂmetad)
list_data3ﬂtopic2ﬂmetad <- f_data_conditioner(list_dataﬂtopic2[[3]],N_Condﬂmetad,"metad",b_continous = TRUE,data_metad, condition =  N_medianﬂmetad)

#Erstellung der Regressionsdaten

#below median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂmetad1 = list_data1ﬂtopic1ﬂmetad[[1]][,1]
data_Yﬂtopic1ﬂcase1ﬂmetad1 = list_data2ﬂtopic1ﬂmetad[[1]]

data_Xﬂtopic2ﬂcase1ﬂmetad1 = list_data1ﬂtopic2ﬂmetad[[1]][,1]
data_Yﬂtopic2ﬂcase1ﬂmetad1 = list_data2ﬂtopic2ﬂmetad[[1]]

list_dataregﬂtopic1ﬂcase1ﬂmetad1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂmetad1,data_Yﬂtopic1ﬂcase1ﬂmetad1,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂmetad1 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂmetad1,data_Yﬂtopic2ﬂcase1ﬂmetad1,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂmetad1 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂmetad1,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂmetad1 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂmetad1,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)

#above median
#1. Fall
#Die Daten werden in eine Prior(x)-Interpretation(y) Tabelle f¸r jede VP geordnet 

data_Xﬂtopic1ﬂcase1ﬂmetad2 = list_data1ﬂtopic1ﬂmetad[[2]][,1]
data_Yﬂtopic1ﬂcase1ﬂmetad2 = list_data2ﬂtopic1ﬂmetad[[2]]

data_Xﬂtopic2ﬂcase1ﬂmetad2 = list_data1ﬂtopic2ﬂmetad[[2]][,1]
data_Yﬂtopic2ﬂcase1ﬂmetad2 = list_data2ﬂtopic2ﬂmetad[[2]]

list_dataregﬂtopic1ﬂcase1ﬂmetad2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic1ﬂcase1ﬂmetad2,data_Yﬂtopic1ﬂcase1ﬂmetad2,enumerate=TRUE,topic="Zsf")
list_dataregﬂtopic2ﬂcase1ﬂmetad2 <- f_constructﬂlistﬂdataﬂreg(data_Xﬂtopic2ﬂcase1ﬂmetad2,data_Yﬂtopic2ﬂcase1ﬂmetad2,enumerate=TRUE,topic="Zsf")

#2. Fall
#Wir schreiben die Daten aus den 2 passenden Zusammenfasungen an einander

list_dataregﬂtopic1ﬂcase2ﬂmetad2 <- f_bindingﬂdata(list_dataregﬂtopic1ﬂcase1ﬂmetad2,vector_beliefﬂcategoriesﬂtopic1,names_newlistelem = names_beliefﬂcategories)
list_dataregﬂtopic2ﬂcase2ﬂmetad2 <- f_bindingﬂdata(list_dataregﬂtopic2ﬂcase1ﬂmetad2,vector_beliefﬂcategoriesﬂtopic2,names_newlistelem = names_beliefﬂcategories)


####################################################################################################################################################################
#4. Analyse:                                                                                                                                 #
####################################################################################################################################################################

#4.1 Regressionanalysis
data_regﬂtopic1ﬂcase1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1,b_save=TRUE,name_table="reg_topic1_case1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1,b_save=TRUE,name_table="reg_topic2_case1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2,b_save=TRUE,name_table="reg_topic1_case2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2,b_save=TRUE,name_table="reg_topic2_case2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)


#4.2 Regression mit interaktion (demographie)

#4.2.1 Geschlecht

#m‰nnlich
data_regﬂtopic1ﬂcase1ﬂgender1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂgender1,b_save=TRUE,name_table="reg_topic1_case1_gender1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂgender1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂgender1,b_save=TRUE,name_table="reg_topic2_case1_gender1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂgender1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂgender1,b_save=TRUE,name_table="reg_topic1_case2_gender1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂgender1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂgender1,b_save=TRUE,name_table="reg_topic2_case2_gender1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#weiblich
data_regﬂtopic1ﬂcase1ﬂgender2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂgender2,b_save=TRUE,name_table="reg_topic1_case1_gender2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂgender2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂgender2,b_save=TRUE,name_table="reg_topic2_case1_gender2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂgender2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂgender2,b_save=TRUE,name_table="reg_topic1_case2_gender2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂgender2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂgender2,b_save=TRUE,name_table="reg_topic2_case2_gender2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)


#4.2.2 education

#academic
data_regﬂtopic1ﬂcase1ﬂedu1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂedu1,b_save=TRUE,name_table="reg_topic1_case1_edu1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂedu1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂedu1,b_save=TRUE,name_table="reg_topic2_case1_edu1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂedu1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂedu1,b_save=TRUE,name_table="reg_topic1_case2_edu1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂedu1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂedu1,b_save=TRUE,name_table="reg_topic2_case2_edu1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)


#non-academic
data_regﬂtopic1ﬂcase1ﬂedu2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂedu2,b_save=TRUE,name_table="reg_topic1_case1_edu2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂedu2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂedu2,b_save=TRUE,name_table="reg_topic2_case1_edu2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂedu2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂedu2,b_save=TRUE,name_table="reg_topic1_case2_edu2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂedu2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂedu2,b_save=TRUE,name_table="reg_topic2_case2_edu2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.2.3 age 

#below median
data_regﬂtopic1ﬂcase1ﬂage1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂage1,b_save=TRUE,name_table="reg_topic1_case1_age1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂage1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂage1,b_save=TRUE,name_table="reg_topic2_case1_age1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂage1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂage1,b_save=TRUE,name_table="reg_topic1_case2_age1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂage1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂage1,b_save=TRUE,name_table="reg_topic2_case2_age1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#above median
data_regﬂtopic1ﬂcase1ﬂage2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂage2,b_save=TRUE,name_table="reg_topic1_case1_age2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂage2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂage2,b_save=TRUE,name_table="reg_topic2_case1_age2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂage2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂage2,b_save=TRUE,name_table="reg_topic1_case2_age2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂage2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂage2,b_save=TRUE,name_table="reg_topic2_case2_age2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)


#4.3 post-prior Vergleich

#4.3.1 Kolmogorov-Smirnov f¸r gleiche Verteilungen
#testung einseitig auf less, da sich ja die varianz erhˆhen soll

results_postpriorﬂkvﬂtopic1 <- ks.test(list_dataﬂtopic1[[1]][,1],list_dataﬂtopic1[[1]][,2], alternative = "less", exact = TRUE)
results_postpriorﬂkvﬂtopic2 <- ks.test(list_dataﬂtopic2[[1]][,1],list_dataﬂtopic2[[1]][,2], alternative = "less", exact = TRUE)

#4.3.2 Varianzverh‰ltnistest )(f-test)

result_postpriorﬂvarﬂtopic1 <- var.test(list_dataﬂtopic1[[1]][,1],list_dataﬂtopic1[[1]][,2])
result_postpriorﬂvarﬂtopic2 <- var.test(list_dataﬂtopic2[[1]][,1],list_dataﬂtopic2[[1]][,2])

#4.3.3 individueller Post-Prior-Vergleich

#wir bestimmen f¸r jedes Idividuum ob |Post_i| > |Prior_i| und bestimmen den polarisationsanteil

result_postpriorﬂpolﬂtopic1 <- f_resultﬂpolratio(list_dataﬂtopic1[[1]][,1],list_dataﬂtopic1[[1]][,2])
result_postpriorﬂpolﬂtopic2 <- f_resultﬂpolratio(list_dataﬂtopic2[[1]][,1],list_dataﬂtopic2[[1]][,2])

#4.4 WMC

#4.4.1 Untersuchung EInfluss scrollen

#datenmit scrollen
data_scroll = list_cutdata[[6]] #2 = yes, 1 = no
#now 1 = yes, 0 = no
data_scroll = transform(data_scroll, SC01 = ifelse(list_cutdata[[6]][,1] == 1,0, SC01))
data_scroll = transform(data_scroll, SC01 = ifelse(list_cutdata[[6]][,1] == 2,1, SC01))


#Anteil an leuten die scrollen mussten
result_scrollratio = (sum(data_scroll)/165)

# Diagramm
Filename = "WMC_Scrolling.pdf"
filepath_Scrolling = paste(name_directoryﬂdata,"/",Filename, sep = "")

pdf(file = filepath_Scrolling)
hist(data_scroll[[1]],br=c(0,0.5,1),right=F,freq=F, main= "Frequency of participants who had to scroll", xlim =c(0,1), xlab = "yes = 1, no = 0", ylab = "Freq", col = "blue")
dev.off()

#condition data based on scrolling yes/no
data_WMCﬂno <- subset(data_WMC, list_cutdata[[6]][,1] == 1)
data_WMCﬂyes <- subset(data_WMC, list_cutdata[[6]][,1] == 2)

#descriptive statistics on the sets
describeBy(data_WMC,data_scroll)

length(data_WMCﬂno[[263]])
length(data_WMCﬂyes[[263]])

f_stdabw(data_WMCﬂno[[263]])
f_stdabw(data_WMCﬂyes[[263]])

shapiro.test(data_WMCﬂyes[[263]])
shapiro.test(data_WMCﬂno[[263]])

t.test(data_WMCﬂyes[[263]],
       data_WMCﬂno[[263]])

wilcox.test(data_WMCﬂyes[[263]],
            data_WMCﬂno[[263]], correct = F)

#4.4.2 Untersuchung Acurracy Error

data_AE <- f_createﬂlistAE(data_WMC)

N_meanﬂAccuraryError = mean(data_AE$AccuracyErrorsOwnPartialScore)
N_stdevﬂAccuraryError = f_stdabw(data_AE$AccuracyErrorsOwnPartialScore)

# Diagramm
Filename_AE = "WMC_AccuracyErrors.pdf"
filepath_AE = paste(name_directoryﬂdata,"/",Filename_AE, sep = "")

pdf(file = filepath_AE)
hist(data_AE$AccuracyErrorsOwnPartialScore,br=c(0,0.75,1.5,2),right=F,freq=F, main= "Frequency of Accuracy Errors", xlim =c(0,2), xlab = "Accuracy Errors", ylab = "Freq", col = "blue")
dev.off()

#4.4.3 Descriptive Statistic WMC

N_meanﬂWMC = mean(data_WMC$OwnPartialStorageScore)
N_stdevﬂWMC = f_stdabw(data_WMC$OwnPartialStorageScore)

#4.4.4 SPeichern der WMC und AE Werte

#WMC
setwd(name_directoryﬂdata)
write.csv(data_WMC$OwnPartialStorageScore, "WMC_Scores.csv")
setwd(name_directoryﬂmain) 

#AE
setwd(name_directoryﬂdata)
write.csv(data_AE$AccuracyErrorsOwnPartialScore, "WMC_AEScores.csv")
setwd(name_directoryﬂmain) 

#4.5 Big5

#aggregated data of all VP for big5
result_Big5 <- data.frame(Score = c(), Error = c())
result_Big5 <- rbind(result_Big5,data.frame(Score = mean(list_big5[[1]][,1]),Error = f_stdabw(list_big5[[1]][,1])))
result_Big5 <- rbind(result_Big5,data.frame(Score = mean(list_big5[[2]][,1]),Error = f_stdabw(list_big5[[2]][,1])))
result_Big5 <- rbind(result_Big5,data.frame(Score = mean(list_big5[[3]][,1]),Error = f_stdabw(list_big5[[3]][,1])))
result_Big5 <- rbind(result_Big5,data.frame(Score = mean(list_big5[[4]][,1]),Error = f_stdabw(list_big5[[4]][,1])))
result_Big5 <- rbind(result_Big5,data.frame(Score = mean(list_big5[[5]][,1]),Error = f_stdabw(list_big5[[5]][,1])))
row.names(result_Big5) <- (c("N","E","O","A","C"))

#save the aggregated data
setwd(name_directoryﬂdata)
write.csv(result_Big5, "Big5_Scores.csv")
setwd(name_directoryﬂmain) 

#4.6 Confidence

#4.6.1 Grundconfidence
#Grundconfidence f¸r alle Zusammenfassungen gesamt ¸ber alle VP

#umrechnung score in %:
#0,4 + 0,1 *x = y

confﬂtopic1 = 0.4 + 0.1 * mean(unlist(data_confﬂtopic1[,1]))
econfﬂtopic1 = 0.1 * f_stdabw(unlist(data_confﬂtopic1[,1]))

confﬂtopic2 = 0.4 + 0.1 * mean(unlist(data_confﬂtopic2[,1]))
econfﬂtopic2 = 0.1 * f_stdabw(unlist(data_confﬂtopic2[,1]))

result_Conf <- data.frame(confﬂtopic1 = confﬂtopic1, econfﬂtopic1 = econfﬂtopic1, confﬂtopic2 = confﬂtopic2, econfﬂtopic2 = econfﬂtopic2)

#4.7 Belief updating und personenbezogene Faktoren

#4.7.1 Regression mit Interaktion (personenbezogen)

#4.7.1.1 WMC

#low scorer
data_regﬂtopic1ﬂcase1ﬂWMC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂWMC1,b_save=TRUE,name_table="reg_topic1_case1_WMC1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂWMC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂWMC1,b_save=TRUE,name_table="reg_topic2_case1_WMC1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂWMC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂWMC1,b_save=TRUE,name_table="reg_topic1_case2_WMC1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂWMC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂWMC1,b_save=TRUE,name_table="reg_topic2_case2_WMC1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂWMC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂWMC2,b_save=TRUE,name_table="reg_topic1_case1_WMC2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂWMC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂWMC2,b_save=TRUE,name_table="reg_topic2_case1_WMC2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂWMC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂWMC2,b_save=TRUE,name_table="reg_topic1_case2_WMC2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂWMC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂWMC2,b_save=TRUE,name_table="reg_topic2_case2_WMC2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.1.2 N

#low scorer
data_regﬂtopic1ﬂcase1ﬂN1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂN1,b_save=TRUE,name_table="reg_topic1_case1_N1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂN1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂN1,b_save=TRUE,name_table="reg_topic2_case1_N1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂN1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂN1,b_save=TRUE,name_table="reg_topic1_case2_N1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂN1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂN1,b_save=TRUE,name_table="reg_topic2_case2_N1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂN2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂN2,b_save=TRUE,name_table="reg_topic1_case1_N2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂN2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂN2,b_save=TRUE,name_table="reg_topic2_case1_N2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂN2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂN2,b_save=TRUE,name_table="reg_topic1_case2_N2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂN2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂN2,b_save=TRUE,name_table="reg_topic2_case2_N2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.1.3 E

#low scorer
data_regﬂtopic1ﬂcase1ﬂE1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂE1,b_save=TRUE,name_table="reg_topic1_case1_E1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂE1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂE1,b_save=TRUE,name_table="reg_topic2_case1_E1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂE1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂE1,b_save=TRUE,name_table="reg_topic1_case2_E1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂE1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂE1,b_save=TRUE,name_table="reg_topic2_case2_E1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂE2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂE2,b_save=TRUE,name_table="reg_topic1_case1_E2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂE2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂE2,b_save=TRUE,name_table="reg_topic2_case1_E2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂE2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂE2,b_save=TRUE,name_table="reg_topic1_case2_E2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂE2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂE2,b_save=TRUE,name_table="reg_topic2_case2_E2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.1.4 O

#low scorer
data_regﬂtopic1ﬂcase1ﬂO1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂO1,b_save=TRUE,name_table="reg_topic1_case1_O1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂO1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂO1,b_save=TRUE,name_table="reg_topic2_case1_O1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂO1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂO1,b_save=TRUE,name_table="reg_topic1_case2_O1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂO1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂO1,b_save=TRUE,name_table="reg_topic2_case2_O1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂO2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂO2,b_save=TRUE,name_table="reg_topic1_case1_O2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂO2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂO2,b_save=TRUE,name_table="reg_topic2_case1_O2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂO2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂO2,b_save=TRUE,name_table="reg_topic1_case2_O2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂO2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂO2,b_save=TRUE,name_table="reg_topic2_case2_O2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.1.5 A

#low scorer
data_regﬂtopic1ﬂcase1ﬂA1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂA1,b_save=TRUE,name_table="reg_topic1_case1_A1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂA1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂA1,b_save=TRUE,name_table="reg_topic2_case1_A1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂA1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂA1,b_save=TRUE,name_table="reg_topic1_case2_A1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂA1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂA1,b_save=TRUE,name_table="reg_topic2_case2_A1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂA2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂA2,b_save=TRUE,name_table="reg_topic1_case1_A2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂA2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂA2,b_save=TRUE,name_table="reg_topic2_case1_A2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂA2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂA2,b_save=TRUE,name_table="reg_topic1_case2_A2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂA2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂA2,b_save=TRUE,name_table="reg_topic2_case2_A2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.1.6 C

#low scorer
data_regﬂtopic1ﬂcase1ﬂC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂC1,b_save=TRUE,name_table="reg_topic1_case1_C1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂC1,b_save=TRUE,name_table="reg_topic2_case1_C1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂC1,b_save=TRUE,name_table="reg_topic1_case2_C1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂC1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂC1,b_save=TRUE,name_table="reg_topic2_case2_C1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂC2,b_save=TRUE,name_table="reg_topic1_case1_C2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂC2,b_save=TRUE,name_table="reg_topic2_case1_C2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂC2,b_save=TRUE,name_table="reg_topic1_case2_C2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂC2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂC2,b_save=TRUE,name_table="reg_topic2_case2_C2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.1.7 Gconf

#low scorer
data_regﬂtopic1ﬂcase1ﬂGconf1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂGconf1,b_save=TRUE,name_table="reg_topic1_case1_Gconf1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂGconf1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂGconf1,b_save=TRUE,name_table="reg_topic2_case1_Gconf1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂGconf1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂGconf1,b_save=TRUE,name_table="reg_topic1_case2_Gconf1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂGconf1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂGconf1,b_save=TRUE,name_table="reg_topic2_case2_Gconf1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂGconf2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂGconf2,b_save=TRUE,name_table="reg_topic1_case1_Gconf2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂGconf2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂGconf2,b_save=TRUE,name_table="reg_topic2_case1_Gconf2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂGconf2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂGconf2,b_save=TRUE,name_table="reg_topic1_case2_Gconf2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂGconf2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂGconf2,b_save=TRUE,name_table="reg_topic2_case2_Gconf2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.1.8 Meta-d

#low scorer
data_regﬂtopic1ﬂcase1ﬂmetad1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂmetad1,b_save=TRUE,name_table="reg_topic1_case1_metad1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂmetad1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂmetad1,b_save=TRUE,name_table="reg_topic2_case1_metad1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂmetad1 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂmetad1,b_save=TRUE,name_table="reg_topic1_case2_metad1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂmetad1 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂmetad1,b_save=TRUE,name_table="reg_topic2_case2_metad1.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#high scorer
data_regﬂtopic1ﬂcase1ﬂmetad2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase1ﬂmetad2,b_save=TRUE,name_table="reg_topic1_case1_metad2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase1ﬂmetad2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase1ﬂmetad2,b_save=TRUE,name_table="reg_topic2_case1_metad2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

data_regﬂtopic1ﬂcase2ﬂmetad2 <- f_createﬂlistﬂregression(list_dataregﬂtopic1ﬂcase2ﬂmetad2,b_save=TRUE,name_table="reg_topic1_case2_metad2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_regﬂtopic2ﬂcase2ﬂmetad2 <- f_createﬂlistﬂregression(list_dataregﬂtopic2ﬂcase2ﬂmetad2,b_save=TRUE,name_table="reg_topic2_case2_metad2.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)


#4.7.2 Bestimmung des Fitparamters delta

#4.7.2.1 Vorbereiten der Daten f¸r delta
#f¸r jeden personenbezogenen Faktor benˆtigen wir die erstellung einer delta_i P_i tabelle
#dazu muss zun‰chst delta_i f¸r jede Person bestimmt werden (f¸r jedes einzele P)

#kann erst jetzt erfolgen, da regressionsdaten benˆtigt werden

#delta_i = (interpret_i - alpha - gamma *prior_i)/P_i

#4.7.2.1.1 WMC

list_deltaiﬂtopic1ﬂWMC <- f_createﬂcleanlistdeltai(list_dataregﬂtopic1ﬂcase2,data_regﬂtopic1ﬂcase2,1,list_dataregﬂtopic1ﬂcase2,data_WMC[[263]],3)
list_deltaiﬂtopic2ﬂWMC <- f_createﬂcleanlistdeltai(list_dataregﬂtopic2ﬂcase2,data_regﬂtopic2ﬂcase2,1,list_dataregﬂtopic2ﬂcase2,data_WMC[[263]],3)

#4.7.2.1.2 O

list_deltaiﬂtopic1ﬂO <- f_createﬂcleanlistdeltai(list_dataregﬂtopic1ﬂcase2,data_regﬂtopic1ﬂcase2,1,list_dataregﬂtopic1ﬂcase2,list_big5[[3]][,1],3)
list_deltaiﬂtopic2ﬂO <- f_createﬂcleanlistdeltai(list_dataregﬂtopic2ﬂcase2,data_regﬂtopic2ﬂcase2,1,list_dataregﬂtopic2ﬂcase2,list_big5[[3]][,1],3)

#4.7.2.1.3 A

list_deltaiﬂtopic1ﬂA <- f_createﬂcleanlistdeltai(list_dataregﬂtopic1ﬂcase2,data_regﬂtopic1ﬂcase2,1,list_dataregﬂtopic1ﬂcase2,list_big5[[4]][,1],3)
list_deltaiﬂtopic2ﬂA <- f_createﬂcleanlistdeltai(list_dataregﬂtopic2ﬂcase2,data_regﬂtopic2ﬂcase2,1,list_dataregﬂtopic2ﬂcase2,list_big5[[4]][,1],3)

#4.7.2.1.4 Grundconf

list_deltaiﬂtopic1ﬂGconf <- f_createﬂcleanlistdeltai(list_dataregﬂtopic1ﬂcase2,data_regﬂtopic1ﬂcase2,1,list_dataregﬂtopic1ﬂcase2,data_confﬂtopic1[,1],3)
list_deltaiﬂtopic2ﬂGconf <- f_createﬂcleanlistdeltai(list_dataregﬂtopic2ﬂcase2,data_regﬂtopic2ﬂcase2,1,list_dataregﬂtopic2ﬂcase2,data_confﬂtopic2[,1],3)

#4.7.2.1.5 meta-d'
list_deltaiﬂtopic1ﬂmetad <- f_createﬂcleanlistdeltai(list_dataregﬂtopic1ﬂcase2,data_regﬂtopic1ﬂcase2,1,list_dataregﬂtopic1ﬂcase2,data_metad,3)
list_deltaiﬂtopic2ﬂmetad <- f_createﬂcleanlistdeltai(list_dataregﬂtopic2ﬂcase2,data_regﬂtopic2ﬂcase2,1,list_dataregﬂtopic2ﬂcase2,data_metad,3)


#4.7.2.2 Fit delta

#4.7.2.2.1 WMC
data_deltaﬂtopic1ﬂWMC <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic1ﬂWMC,b_save=TRUE,name_table="delta_topic1_WMC.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_deltaﬂtopic2ﬂWMC <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic2ﬂWMC,b_save=TRUE,name_table="delta_topic2_WMC.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.2.2.2 O
data_deltaﬂtopic1ﬂO <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic1ﬂO,b_save=TRUE,name_table="delta_topic1_O.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_deltaﬂtopic2ﬂO <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic2ﬂO,b_save=TRUE,name_table="delta_topic2_O.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.2.2.3 A
data_deltaﬂtopic1ﬂA <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic1ﬂA,b_save=TRUE,name_table="delta_topic1_A.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_deltaﬂtopic2ﬂA <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic2ﬂA,b_save=TRUE,name_table="delta_topic2_A.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.2.2.4 Grundconf
data_deltaﬂtopic1ﬂGconf <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic1ﬂGconf,b_save=TRUE,name_table="delta_topic1_Gconf.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_deltaﬂtopic2ﬂGconf <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic2ﬂGconf,b_save=TRUE,name_table="delta_topic2_Gconf.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.2.2.5 metad
data_deltaﬂtopic1ﬂmetad <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic1ﬂmetad,b_save=TRUE,name_table="delta_topic1_metad.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_deltaﬂtopic2ﬂmetad <- f_createﬂlistﬂquadraticfit(list_deltaiﬂtopic2ﬂmetad,b_save=TRUE,name_table="delta_topic2_metad.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.3 post-prior (Grad der Polarisation)

data_GpolﬂWMC <- f_polratioresult(list_data1ﬂtopic1ﬂWMC,list_data1ﬂtopic2ﬂWMC,b_save=TRUE,name_table="Gpol_WMC.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_GpolﬂN <- f_polratioresult(list_data1ﬂtopic1ﬂN,list_data1ﬂtopic2ﬂN,b_save=TRUE,name_table="Gpol_N.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_GpolﬂE <- f_polratioresult(list_data1ﬂtopic1ﬂE,list_data1ﬂtopic2ﬂE,b_save=TRUE,name_table="Gpol_E.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_GpolﬂO <- f_polratioresult(list_data1ﬂtopic1ﬂO,list_data1ﬂtopic2ﬂO,b_save=TRUE,name_table="Gpol_O.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_GpolﬂA <- f_polratioresult(list_data1ﬂtopic1ﬂA,list_data1ﬂtopic2ﬂA,b_save=TRUE,name_table="Gpol_A.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_GpolﬂC <- f_polratioresult(list_data1ﬂtopic1ﬂN,list_data1ﬂtopic2ﬂC,b_save=TRUE,name_table="Gpol_C.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_GpolﬂGconf <- f_polratioresult(list_data1ﬂtopic1ﬂGconf,list_data1ﬂtopic2ﬂGconf,b_save=TRUE,name_table="Gpol_Gconf.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_Gpolﬂmetad <- f_polratioresult(list_data1ﬂtopic1ﬂmetad,list_data1ﬂtopic2ﬂmetad,b_save=TRUE,name_table="Gpol_metad.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)

#4.7.3 post-prior2 (St‰rke der Polarisation)

data_SpolﬂWMC <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂWMC,list_data1ﬂtopic2ﬂWMC,b_save=TRUE,name_table="Spol_WMC.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_SpolﬂN <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂN,list_data1ﬂtopic2ﬂN,b_save=TRUE,name_table="Spol_N.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_SpolﬂE <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂE,list_data1ﬂtopic2ﬂE,b_save=TRUE,name_table="Spol_E.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_SpolﬂO <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂO,list_data1ﬂtopic2ﬂO,b_save=TRUE,name_table="Spol_O.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_SpolﬂA <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂA,list_data1ﬂtopic2ﬂA,b_save=TRUE,name_table="Spol_A.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_SpolﬂC <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂN,list_data1ﬂtopic2ﬂC,b_save=TRUE,name_table="Spol_C.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_SpolﬂGconf <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂGconf,list_data1ﬂtopic2ﬂGconf,b_save=TRUE,name_table="Spol_Gconf.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
data_Spolﬂmetad <- f_strengthpolratioresult(list_data1ﬂtopic1ﬂmetad,list_data1ﬂtopic2ﬂmetad,b_save=TRUE,name_table="Spol_metad.csv",name_datadirectory=name_directoryﬂdata,name_maindirectory = name_directoryﬂmain)
