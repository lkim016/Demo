### Lori Kim
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

dis.Ind = read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv", stringsAsFactors = FALSE)
chci = read_excel("CHCI_state.xlsx", skip = 1)

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

# control variable - added
filtered.states = c("California", "Alabama", "New York", "Florida", "Texas")
filtered.year = as.data.frame(c(2011:2016))
colnames(filtered.year) = c("year")

#plot v2 - added
obesity.stat = filter(obesity, LocationDesc %in% filtered.states)
ggplot(data=obesity.stat, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Obesity over the Years")

# leisure - getting leisure stats - added
leisure = dis.Ind %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "No leisure-time physical activity among adults aged >= 18 years")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter(LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# leisure - plot - added
ggplot(data=leisure, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("No Leisure Time")

# soda - getting soda stats - added
soda = dis.Ind %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Soda consumption among high school students")) %>%
  filter( str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# soda - plot - added
ggplot(data=soda, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Soda consumption among HS students")

# poverty - getting poverty stats - added
poverty = dis.Ind %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Poverty")) %>%
  filter (!str_detect(Question, "Poverty among women aged 18-44 years")) %>%
  filter( str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# poverty - plot - added
poverty = filter(poverty, LocationDesc %in% filtered.states)
ggplot(data=poverty, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("poverty")

# hs - getting hs stats - added
hs = dis.Ind %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "High school completion among adults aged 18-24 years")) %>%
  filter( str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# high school - plot - added
ggplot(data=hs, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("High School Completion")

# computer use in hs - getting hs stats - added
comp.hs = dis.Ind %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Computer use among high school students")) %>%
  filter( str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter (LocationDesc %in% states$LocationDesc) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year$year)

# computer use in comp.hs - plot - added
ggplot(data=comp.hs, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("High School Computer Use")

# Google Vis plot - added
library(datasets)

str(obesity)
obesity$DataValue = as.numeric(obesity$DataValue)
G3 = gvisGeoChart(filter(obesity, YearStart %in% 2016), 
                  locationvar = "LocationDesc", 
                  colorvar = "DataValue",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               width=800, height=600))
plot(G3)

# clean up data more LocationDesc, YearStart, DataValue
obesity.stat = obesity.stat %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
leisure = leisure %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
comp.hs = comp.hs %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
poverty = poverty %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
soda = soda %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
hs = hs %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))

colmerge = c("LocationDesc", "YearStart")

final.ob = merge(obesity.stat, leisure, by = colmerge)
final.ob = left_join(final.ob, comp.hs, by = colmerge)
final.ob = left_join(final.ob, poverty, by = colmerge)
final.ob = left_join(final.ob, soda, by = colmerge)
final.ob = left_join(final.ob, hs, by = colmerge)

colnames(final.ob) = c("states", "year", "obesity.over.18","leisure", "hs.comp.use", "poverty","hs.soda.intake","over.18.hs.grad")

is.na(final.ob) = NA





