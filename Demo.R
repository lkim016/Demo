### Lori Kim & Cindy So
## 

setwd("/Volumes/LEXAR/DATA SCIENCE/Demo")
setwd("/Users/munchbook/Desktop/Cindy Class")

#install.packages("maps")
#install.packages("ggplot2") 
#install.packages("googleVis")
#install.packages("shinydashboard")
library(shinydashboard)
library(ggplot2)
library(maps)
library(dplyr)
library(stringr)
library(googleVis)
library(readxl)
library(shiny)
library(datasets)

# THIS IS FOR OBESITY OVER 18

# variables for data files
gusa = map_data("state") # graphical map
chron.dis = read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv", stringsAsFactors = FALSE)
nutri = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# changing data values to numeric and colnames
chron.dis$DataValue = as.numeric(chron.dis$DataValue)
nutri$Data_Value = as.numeric(nutri$Data_Value)
names(nutri)[11] = "DataValue" # changing nutri file Data_Value to Datavalue

#chci = read_excel("CHCI_state.xlsx", skip = 1)

# get the 50 states
# take out USA, District of Columbia, Guam, 
filtered.states = state.name
#colnames(states) = c("LocationDesc")

# join state
#obesity = left_join(states, obesity, by ="LocationDesc")

# plot
#plot = ggplot(filter(obesity, LocationDesc %in% c("California", "Alabama")),
#              aes(x=YearStart, y=DataValue, color=LocationDesc)) + geom_point()
#plot

# control variable
# filtered.states = c("California", "Alabama", "New York", "Florida", "Texas", "Mississippi", "West Virginia")
filtered.six.year = c(2011:2016)
#filtered.year = c(2011, 2013, 2015)

# obesity - getting obesity stats
obesity = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Overweight or obesity")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter(LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% (filtered.six.year))

# obesity - plot
ggplot(data=obesity, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
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
  filter(LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.six.year)

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
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.six.year)

# poverty - plot
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
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.six.year)

# high school - plot
ggplot(data=hs, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("High School Completion")


# adults with diabetes - adult diabetes stats
diabetes = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Prevalence of diagnosed diabetes")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.six.year)

# adults with diabetes - plot
ggplot(data=diabetes, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Adults with Diabetes")

# vegetable consumption - vegetable consumption < one time daily stats
vegetable = nutri %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "adults who engage in muscle-strengthening activities")) %>%
  filter(str_detect(Stratification1, "Total" )) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.year)

# vegetable consumption - plot
ggplot(data=vegetable, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Vegetable Consumption")

# fitness 300 mins / wk - 5 hrs/wk moderate-intense aerobic stats
fitness = nutri %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "at least 300")) %>%
  filter(str_detect(Stratification1, "Total" )) %>%
  filter( LocationDesc %in% filtered.states) %>%
  filter( YearStart %in% filtered.six.year)

# fitness 300 mins / wk - plot
ggplot(data=fitness, aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("5 hrs/wk moderate-intense aerobic...")


# clean up data more LocationDesc, YearStart, DataValue for adult obesity
obesity = obesity %>% filter( YearStart %in% (filtered.year))

obesity = obesity %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
leisure = leisure %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
poverty = poverty %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
hs = hs %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
diabetes = diabetes %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
vegetable = vegetable %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
fitness = fitness %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))

# merged adult data
colmerge = c("LocationDesc", "YearStart")

adult.ob = left_join(obesity, leisure, by = colmerge)
adult.ob = left_join(adult.ob, poverty, by = colmerge)
adult.ob = left_join(adult.ob, diabetes, by = colmerge)
adult.ob = left_join(adult.ob, hs, by = colmerge)
adult.ob = left_join(adult.ob, vegetable, by = colmerge)
adult.ob = left_join(adult.ob, fitness, by = colmerge)


# renaming columns for adultobesity
colnames(adult.ob) = c("states", "year", "obesity", "leisure", "poverty","hs.grad","diabetes","vegetable","fitness")
#colnames(adult.ob) = c("states", "year", "obesity", "leisure", "poverty","hs.grad","diabetes")
str(adult.ob)

# doing the linear for adult obesity
# lin.fit = lm(obesity ~. -diabetes-year-states-vegetable, data = adult.ob) 
# summary(lin.fit)

lin.fit = lm(filter(obesity, YearStart %in% 2016) ~. -diabetes-year-states-vegetable, data = adult.ob) 
summary(lin.fit)

# tfe1=plm(obesity~leisure+poverty+hs.grad+diabetes+vegetable+fitness, index=c("states","year"), model="within", data = adult.ob)
# summary(tfe1)
# tfe2=plm(obesity~leisure+poverty+hs.grad+diabetes+factor(year), index=c("states","year"), model="within", data = adult.ob)
# summary(tfe2)
# pFtest(tfe1,lin.fit)


########## Google Vis plot & Shiny - added
# filtering again for mappin
obs = filter(obesity, YearStart %in% 2015)
lei = filter(leisure, YearStart %in% 2015)
pov = filter(poverty, YearStart %in% 2015)
hs.grad = filter(hs, YearStart %in% 2015)
dia = filter(diabetes, YearStart %in% 2015)


# obesity
G1 = gvisGeoChart(obs,
             locationvar = "LocationDesc", 
             colorvar = "DataValue",
             options=list(region="US", 
                          displayMode="regions", 
                          resolution="provinces",
                          width=800, height=600))

# leisure
G2 = gvisGeoChart(lei, 
                  locationvar = "LocationDesc", 
                  colorvar = "DataValue",
                  options=list(title = "Leisure",
                               region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               width=800, height=600))

# poverty
G3 = gvisGeoChart(pov, 
                  locationvar = "LocationDesc", 
                  colorvar = "DataValue",
                  options=list(title = "Leisure",
                               region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               width=800, height=600))

# hs
G4 = gvisGeoChart(hs.grad, 
                  locationvar = "LocationDesc", 
                  colorvar = "DataValue",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               width=800, height=600))

# diabetes
G5 = gvisGeoChart(dia, 
                  locationvar = "LocationDesc", 
                  colorvar = "DataValue",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               width=800, height=600))

J = gvisMotionChart(adult.ob, idvar="states", timevar="year", xvar = "hs.grad", yvar="obesity",
                     options=list(width=700, height=600))


# Shiny App
dashHead = dashboardHeader(title = "Menu")
sideBar = dashboardSidebar()
dashBoard = dashboardBody(
  h2("Obesity"),
    htmlOutput("obesity"),
  h2("Leisure"),
    htmlOutput("leisure"),
  h2("Poverty"),
    htmlOutput("poverty"),
  h2("Over 18 High School Graduates"),
    htmlOutput("high"),
  h2("Diabetes"),
    htmlOutput("dia"),
  h2("Motion Chart: Obesity vs ..."),
    htmlOutput("adultOb")
)

ui <- dashboardPage(dashHead,sideBar, dashBoard)

server = function(input, output) {
  
  output$obesity <- renderGvis({
    map <- G1
  })
  output$leisure <- renderGvis({
    map <- G2
  })
  output$poverty <- renderGvis({
    map <- G3
  })
  output$high <- renderGvis({
    map <- G4
  })
  output$dia <- renderGvis({
    map <- G5
  })
  output$adultOb <- renderGvis({
    map <- J
  })
}

shinyApp(ui, server)





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
## reference:
# 1. https://shiny.rstudio.com/articles/plot-interaction-advanced.html
# 2. https://shiny.rstudio.com/reference/shiny/0.14/shinyApp.html
# 3. https://shiny.rstudio.com/articles/html-tags.html
# 4. https://magesblog.com/post/2013-02-26-first-steps-of-using-googlevis-on-shiny/





