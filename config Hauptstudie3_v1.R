#config

####################################################################################################################################################################
#1. config of data                                                                                                                                                 #
####################################################################################################################################################################

#1.1 configuration of generaldata ##################################################################################################################################

#directory of studydata
name_directory�main = "E:/Studium/Psychologie/Bachelorarbeit/Auswertung"
#name of the file of the studydata
name_datafile�main = "data_IWR-Study2018_2019-05-10_15-02_clean2.xlsx"
#list that gives the vectors where the studydata should be cut
list_topics�main = list(c(7:26),c(27:46),c(47:52),c(53:82),c(83:89),90,c(91:309))
#names of the topics in which the studydata is cut
names_topics�main = c("nano","veg","glu","big5","dem","scro","vis")
#directory of results
name_directory�data = "E:/Studium/Psychologie/Bachelorarbeit/ErgebnisseHauptstudie"


#1.2 configuration of data for beliefupdating ######################################################################################################################

#name the categories in which the data of the summaries is cut (default: initial and final answer, answers for summaries, confidence for summaries)
names_topics�belief = c("IA","ZSF","Conf")
names_belief�categories = c('Pos','Con','Neu')

#the following contains the rows coresponding to each topic you have to define as many vectors as length(names_topics�summaries)
#which rows store the data on inital and final question for each topic
vector_topic�priorpost = c(1,20)
#which rows store the data on each summary for each topic
vector_topic�summaries = c(2,5,8,11,14,17)
#which rows store the confidence on each summary for each topic
vector_topic�confidence = c(4,7,10,13,16,19)

#adds the topicvectors into a list
#if you added a vector simpy add its name here
list_topics�belief = list(vector_topic�priorpost,vector_topic�summaries,vector_topic�confidence)

#range of the question for prior, posterior and summaries
numeric_topic�range = 8

#vector that gives the numeric that is substracted from the data for each category
#if you added a vector add the modifier for it
vector_topic�modifiers�belief = c(numeric_topic�range,numeric_topic�range,0)

#vector with the info which summaries belong to which category
vector_belief�categories�topic1 = c("1,2","3,4","5,6")
vector_belief�categories�topic2 = c("1,2","3,4","5,6")

#1.3 configuration of data for belief updating with dempgraphics ###################################################################################################
#das hier funktioniert so net und muss erst nach main ausgef�hrt werden bitte rerun

#1.3.1 gender
N_Cond�gender = 2
condition�gender = c("1","2")

#1.3.2 education
N_Conde�edu = 2
condition�edu = c("4,5","1,2,3")

#1.3.3 age
N_Cond�age = 2

#1.4 Big 5
#give the info which question belongs to which part of the big5 
#plese give polarisation using a - sign

v_N = c(3,8,12,16,21,26)
v_E = c(1,4,13,17,18,27)
v_O = c(-5,9,-14,22,-23,29)
v_A = c(6,10,15,19,-24,30)
v_C = c(2,7,11,20,25,-28)

