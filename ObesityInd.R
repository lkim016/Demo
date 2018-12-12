### Lori Kim & Cindy So
## 

setwd("/Volumes/LEXAR/DATA SCIENCE/Demo/Demo-master")
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
library(plm)

# THIS IS FOR OBESITY OVER 18

# variables for data files
#gusa = map_data("state") # graphical map
chron.dis = read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv", stringsAsFactors = FALSE)
nutri = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")

# changing data values to numeric and colnames
# https://catalog.data.gov/dataset/u-s-chronic-disease-indicators-cdi
chron.dis$DataValue = as.numeric(chron.dis$DataValue)
nutri$Data_Value = as.numeric(nutri$Data_Value)
names(nutri)[11] = "DataValue" # changing nutri file Data_Value to Datavalue

# DATASETS - FAST FOOD - 2014
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/
fastfoodtemp = read_excel("DataDownload.xlsx", sheet = "RESTAURANTS")
fastfoodtemp = fastfoodtemp[,c(2,4,5)]
colnames(fastfoodtemp) = c("state", "ff09","ff14")
# getting the sum of each state by year
fastfoodtemp = fastfoodtemp %>% group_by(state) %>% summarise_all(funs(sum))

#merge
fastfood = fastfoodtemp[, c("state", "ff14")]
fastfood$year = 2014
colnames(fastfood) = c("state", "dataValue","year")

# DATASETS - FAST FOOD - 2011
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/
fastfoodtemp = read_excel("February2014.xlsx", sheet = "RESTAURANTS")
fastfoodtemp = fastfoodtemp[,c(2,4,5)]
colnames(fastfoodtemp) = c("state", "ff07","ff11")
# getting the sum of each state by year
fastfoodtemp = fastfoodtemp %>% group_by(state) %>% summarise_all(funs(sum))
#merge
fastfoodtemp = fastfoodtemp[, c("state", "ff11")]
fastfoodtemp$year = 2011
colnames(fastfoodtemp) = c("state", "dataValue","year")
# DATASETS - FAST FOOD - merge
fastfood = rbind(fastfood, fastfoodtemp)

# DATASETS - FAST FOOD - 2012
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/
fastfoodtemp = read_excel("August2015.xlsx", sheet = "RESTAURANTS")
fastfoodtemp = fastfoodtemp[,c(2,4,5)]
colnames(fastfoodtemp) = c("state", "ff07","ff12")
# getting the sum of each state by year
fastfoodtemp = fastfoodtemp %>% group_by(state) %>% summarise_all(funs(sum))
#merge
fastfoodtemp = fastfoodtemp[, c("state", "ff12")]
fastfoodtemp$year = 2012
colnames(fastfoodtemp) = c("state", "dataValue","year")
# DATASETS - FAST FOOD - merge
fastfood = rbind(fastfood, fastfoodtemp)

# ChCI
chci = read_excel("CHCI_state.xlsx", skip = 1)
chci = chci[, c("Geography", "2011", "2012", "2013")]
colnames(chci) = c("state", "y2011", "y2012", "y2013")
# 2011
chcitemp = chci[, c("state", "y2011")]
chcitemp = cbind(chcitemp, rep(2011, nrow(chcitemp)))
colnames(chcitemp) = c("LocationDesc", "chci", "YearStart")
chci2 = chcitemp
# 2012
chcitemp = chci[, c("state", "y2012")]
chcitemp = cbind(chcitemp, rep(2012, nrow(chcitemp)))
colnames(chcitemp) = c("LocationDesc", "chci", "YearStart")
chci2 = rbind(chci2,chcitemp)
# 2013
chcitemp = chci[, c("state", "y2013")]
chcitemp = cbind(chcitemp, rep(2013, nrow(chcitemp)))
colnames(chcitemp) = c("LocationDesc", "chci", "YearStart")
chci2 = rbind(chci2,chcitemp)

# States control variable
filtered.states.full = state.name
filtered.states.selected = c("Colorado", "Mississippi", "West Virginia", "Arkansas", "North Dakota","South Carolina","Arizona")

# Year control variable
filtered.six.year = c(2011:2016)
#filtered.year = c(2011, 2013, 2015)

# obesity - getting obesity stats
obesity = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Overweight or obesity")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter(LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% (filtered.six.year))

# obesity - plot
ggplot(data=filter(obesity, LocationDesc %in% filtered.states.selected), aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Obesity over the Years")

# inactivity - getting inactivity stats
inactivity = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "No leisure-time physical activity among adults aged >= 18 years")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter(LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)

# poverty - getting poverty stats
poverty = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Poverty")) %>%
  filter (!str_detect(Question, "Poverty among women aged 18-44 years")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter( LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)

# adults with diabetes - adult diabetes stats
diabetes = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Prevalence of diagnosed diabetes")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter( LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)

# fitness 300 mins / wk - 5 hrs/wk moderate-intense aerobic stats
fitness = nutri %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "at least 300")) %>%
  filter(str_detect(Stratification1, "Total" )) %>%
  filter( LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)


# chci - filter
chci2 = chci2 %>%
  filter( LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)

# chci - plot
ggplot(data=filter(chci2, LocationDesc %in% filtered.states.selected), aes(x=YearStart, y=chci, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("") + 
  ggtitle("CHCI")

# fast food - data cleanup
fastfood$states = state.name[match(fastfood$state,state.abb)]
fastfood = fastfood %>%
  filter( states %in% filtered.states.full) %>%
  filter( year %in% filtered.six.year)
fastfood = fastfood %>% select(one_of(c("states", "year", "dataValue")))


# clean up data more LocationDesc, YearStart, DataValue for adult obesity
obesity = obesity %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
inactivity = inactivity %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
poverty = poverty %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
diabetes = diabetes %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
#fitness = fitness %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
colnames(fastfood) = c("LocationDesc", "YearStart", "DataValue")

# merged adult data
colmerge = c("LocationDesc", "YearStart")

adult.ob = left_join(obesity, inactivity, by = colmerge)
adult.ob = left_join(adult.ob, poverty, by = colmerge)
adult.ob = left_join(adult.ob, diabetes, by = colmerge)
adult.ob = left_join(adult.ob, chci2, by = colmerge)
adult.ob = left_join(adult.ob, fastfood, by = colmerge)
#adult.ob = left_join(adult.ob, fitness, by = colmerge)

# renaming columns for adultobesity

colnames(adult.ob) = c("state", "year", "obesity", "inactivity", "poverty","diabetes", "chci", "fastfood")
# Linear Regression
#adult.ob2 = filter(filter(adult.ob, year %in% 2012), state %in% filtered.states.selected)
#adult.ob2 = filter(adult.ob, year %in% 2014)
#adult.ob2 = adult.ob
#lin.fit = lm(obesity~. -diabetes-year-state, data = adult.ob2) 
#lin.fit = lm(obesity~inactivity+poverty+chci+fastfood^2+diabetes, data = adult.ob2) 
#lin.fit = lm(obesity~inactivity+poverty+chci+log(fastfood)+diabetes, data = adult.ob2) 
#summary(lin.fit)

#sols=lm(obesity~inactivity+poverty+chci+fastfood, data=adult.ob2)
#summary(sols)
#tfe1=plm(obesity~inactivity+poverty+chci+log(fastfood)+diabetes, index=c("state","year"), model="within", data=adult.ob2)
#summary(tfe1)

# Boxplot of Obesity in 2011 & 2016
# avg.ob = adult.ob %>% filter( year %in% c(2011, 2016))


########## Google Vis plot & Shiny - added
# obesity

G1 = gvisGeoChart(filter(adult.ob, year %in% 2011),
                  locationvar = "state", 
                  colorvar = "obesity",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               colorAxis="{colors:[\'#87CEEB\', \'#BE2625\']}",
                               width=600, height=600))

G2 = gvisGeoChart(filter(adult.ob, year %in% 2016),
                  locationvar = "state", 
                  colorvar = "obesity",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               colorAxis="{colors:[\'#87CEEB\', \'#BE2625\']}",
                               width=600, height=600))


J = gvisMotionChart(adult.ob, idvar="state", timevar="year", xvar = "diabetes", yvar="obesity",
                    options=list(width=700, height=600))

# Shiny App
dashHead = dashboardHeader(title = "Obesity: 2011-2016")

sideBar = dashboardSidebar(
  disable = TRUE
  #collapsed = TRUE,
  #sidebarMenu(
  #menuItem("Map 2011 & 2016", tabName = "map"),
  #menuItem("Dynamic Graph", tabName = "dynamic")
  #)
)

body = dashboardBody(
  #tabItems(
  #tabItem(tabName = "map",
  fluidRow(
    column( width = 6,
            h2("Obesity in 2011"),
            htmlOutput("obesity11") ),
    column( width = 6,
            h2("Obesity in 2016"),
            htmlOutput("obesity16")
            )
    ),
  
  fluidRow(
    column(8, align = "center", offset = 2,
      h2("Box Plot: Average Obesity"),
      plotOutput("box")
    )
  ),
  
  #tabItem(tabName = "dynamic",
  fluidRow(
    tags$div(style = "text-align: center",
      h2("Dynamic Interactive Graph: Obesity vs ..."),
      htmlOutput("adultOb")
    )
  )
  #)
  #)
)

ui <- dashboardPage(dashHead,sideBar, body)

server = function(input, output) {
  
  output$obesity11 <- renderGvis({
    map <- G1
  })
  output$obesity16 <- renderGvis({
    map <- G2
  })
  output$box <- renderPlot({
      boxplot(obesity~year,data=adult.ob, main="Average Obesity in 2011 & 2016", 
                      xlab="Years", ylab="Average Percent Population of Obesity")
  })
  output$adultOb <- renderGvis({
    map <- J
  })
}

shinyApp(ui, server)
