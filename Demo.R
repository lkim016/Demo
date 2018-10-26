### Lori Kim
## 

setwd("/Volumes/LEXAR/DATA SCIENCE/Nov 5 Demo")

install.packages("maps")
install.packages("ggplot2") 
library(ggplot2)
library(maps)
library(dplyr)
library(stringr)

# variables for data files
gusa = map_data("state") # graphical map

dis.Ind = read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv")
#* need to put chci.csv back in "Project 2/ACS_DP02 data"
chci = read.csv("chci.csv")

# clean up disease question by obesity
obesity = dis.Ind %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Overweight or obesity")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" ))

# get the 50 states
# take out USA, District of Columbia, Guam, 
states = as.data.frame(state.name)
colnames(states) = c("LocationDesc")

# join state
obesity = left_join(states, obesity, by ="LocationDesc")

# plot
plot = ggplot(filter(obesity, LocationDesc %in% c("California", "Alabama")),
       aes(x=YearStart, y=DataValue, color=LocationDesc)) + geom_point()
plot



####
plot + geom_point() + geom_line(aes(y = pred.sc), color="red")

colnames(disInd)
colnames(chci)

# checking data frequency
dValUn = as.data.frame(table(disInd$DataValueUnit))
dValTy = as.data.frame(table(disInd$DataValueType))
dVal = as.data.frame(table(disInd$DataValue))
question = as.data.frame(table(disInd$Question))
gusa = as.data.frame(table(gusa$reg))
strat = as.data.frame(table())

# data4 = as.data.frame(table(disInd$DataValueAlt)) // same as DataValue


# only get the data that is DataValueType = Age-adjusted Prevalence / only get Question = obesity
resp = disInd %>% select()
dataClean = as.data.frame(table(disInd$response))

# years: 2011-2016
# is the crude rate a general rate of the dataset? and is the Age-adjusted prevalence a standard deviation?
# story: 

# analyze the data
# organize the data by year and states (50 states)
# only need chci16

healthData = disINd %>% select(YearStart, YearEnd, LocationAbbr,Topic, Question,DataValueUnit, DataValueType,DataValue )




















# ask about glasses blue light emission filter protection