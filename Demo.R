### Lori Kim & Cindy So
## 

setwd("/Volumes/LEXAR/DATA SCIENCE/Demo")
setwd("/Users/munchbook/Desktop/Cindy Class")

#install.packages("maps")
#install.packages("ggplot2") 
install.packages("googleVis")
library(ggplot2)
library(maps)
library(dplyr)
library(stringr)
library(googleVis)
library(readxl)

# variables for data files
gusa = map_data("state") # graphical map
chron.dis = read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv", stringsAsFactors = FALSE)
nutri = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

#chci = read_excel("CHCI_state.xlsx", skip = 1)

# clean up disease question by obesity
obesity = chron.dis %>%
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

# control variable
filtered.states = c("California", "Alabama", "New York", "Florida", "Texas")
filtered.year = as.data.frame(c(2011:2016))
colnames(filtered.year) = c("year")

#plot v2
obesity.stat = filter(obesity, LocationDesc %in% filtered.states)
ggplot(data=obesity.stat, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Obesity over the Years")

# leisure - getting leisure stats
leisure = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "No leisure-time physical activity among adults aged >= 18 years")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter(LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# leisure - plot
ggplot(data=leisure, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("No Leisure Time But Engaging in Physical Activity")

# poverty - getting poverty stats
poverty = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Poverty")) %>%
  filter (!str_detect(Question, "Poverty among women aged 18-44 years")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# poverty - plot
poverty = filter(poverty, LocationDesc %in% filtered.states)
ggplot(data=poverty, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("poverty")

# hs - getting hs stats
hs = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "High school completion among adults aged 18-24 years")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# high school - plot
ggplot(data=hs, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("High School Completion")

  
  # adults with diabetes
  obesity.dia = chron.dis %>%
    select(-one_of(c("YearEnd"))) %>%
    filter( str_detect(Question, "Prevalence of diagnosed diabetes")) %>%
    filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
    filter(str_detect(Stratification1, "Overall" )) %>%
    filter (LocationDesc %in% states$LocationDesc) %>%
    filter( LocationDesc %in% filtered.states) %>%
    filter( YearStart %in% filtered.year$year)

  # adults with diabetes - plot
  obesity.dia = filter(obesity.dia, LocationDesc %in% filtered.states)
  ggplot(data=obesity.dia, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
    geom_line() +
    geom_point() +
    xlab("Years") +
    ylab("Percent") + 
    ggtitle("Adults with Diabetes")
  
# clean up data more LocationDesc, YearStart, DataValue for adult obesity
obesity.stat = obesity.stat %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
leisure = leisure %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
poverty = poverty %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
hs = hs %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))

# merged adult data
colmerge = c("LocationDesc", "YearStart")

adult.ob = left_join(obesity.stat, leisure, by = colmerge)
adult.ob = left_join(adult.ob, poverty, by = colmerge)
adult.ob = left_join(adult.ob, hs, by = colmerge)


# renaming columns for adultobesity
colnames(adult.ob) = c("states", "year", "obesity.over.18","leisure", "poverty","over.18.hs.grad")

# adult obesity: changing the data value type from char to num
for(a in 3:ncol(adult.ob)) {
  adult.ob[,a] = as.numeric(adult.ob[,a])
}

# doing the linear for adult obesity
lin.fit = lm(obesity.over.18 ~. -year, data = adult.ob) # Mississippi, West Virginia, Alabama
summary(lin.fit)

  # doing the linear for hs obesity
  hs.lin.fit = lm(hs.obesity ~ ., data = hs.ob, na.action=na.exclude)
  summary(hs.lin.fit)

# Google Vis plot - added
library(datasets)
  
# standaradizing obesity for the map
obesity$DataValue = as.numeric(obesity$DataValue)
mean(obesity$DataValue)
sd(obesity$DataValue)
?sd

sc = scale(obesity$DataValue) # trying to get a standardized measurement
obesity$DataValue = sc[,1]


str(obesity)
#obesity$DataValue = as.numeric(obesity$DataValue)
G3 = gvisGeoChart(filter(obesity, YearStart %in% 2016), 
                  locationvar = "LocationDesc", 
                  colorvar = "DataValue",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               width=800, height=600))
plot(G3)












### TO DO: MAKE INTERACTIVE DYNAMIC GRAPH

hist(adult.ob$obesity.over.18)
hist(adult.ob$leisure)
hist(adult.ob$poverty)
hist(adult.ob$over.18.hs.grad)

ques = as.data.frame(table(nutri$Question))
ques1 = as.data.frame(table(chron.dis$Question))

#### filtering code
topic = as.data.frame(table(chron.dis$Topic))
question = as.data.frame(table(chron.dis$Question))

fil = nutri %>%
  filter(str_detect(Question, "adults who engage in muscle-strengthening activities"))
fil2 = as.data.frame(table(fil))

#### important variables = YearStart, LocationDesc, Data_Value
n = as.data.frame(table(nutri$Question))



### shiny
## reference: https://shiny.rstudio.com/articles/plot-interaction-advanced.html














