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
#gusa = map_data("state") # graphical map
chron.dis = read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv", stringsAsFactors = FALSE)
nutri = read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv")
gym = read.csv("open-gym.csv")

# changing data values to numeric and colnames
chron.dis$DataValue = as.numeric(chron.dis$DataValue)
nutri$Data_Value = as.numeric(nutri$Data_Value)
names(nutri)[11] = "DataValue" # changing nutri file Data_Value to Datavalue
chci = read_excel("CHCI_state.xlsx", skip = 1)
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



n <- chci$geography

# transpose all but the first column (name)
df.aree <- as.data.frame(t(df.aree[,-1]))
colnames(df.aree) <- n
df.aree$myfactor <- factor(row.names(df.aree))

str(df.aree) # Check the column types

# States control variable
filtered.states.full = state.name
filtered.states.selected = c("Mississippi", "West Virginia", "Arkansas", "North Dakota","South Carolina","Arizona")

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

# inactivity - plot
ggplot(data=filter(inactivity, LocationDesc %in% filtered.states.selected), aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Inactivity")

# poverty - getting poverty stats
poverty = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Poverty")) %>%
  filter (!str_detect(Question, "Poverty among women aged 18-44 years")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter( LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)

# poverty - plot
ggplot(data=filter(poverty, LocationDesc %in% filtered.states.selected), aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("poverty")

# adults with diabetes - adult diabetes stats
diabetes = chron.dis %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "Prevalence of diagnosed diabetes")) %>%
  filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
  filter(str_detect(Stratification1, "Overall" )) %>%
  filter( LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)

# adults with diabetes - plot
ggplot(data=filter(diabetes, LocationDesc  %in% filtered.states.selected), aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("Adults with Diabetes")

# fitness 300 mins / wk - 5 hrs/wk moderate-intense aerobic stats
fitness = nutri %>%
  select(-one_of(c("YearEnd"))) %>%
  filter( str_detect(Question, "at least 300")) %>%
  filter(str_detect(Stratification1, "Total" )) %>%
  filter( LocationDesc %in% filtered.states.full) %>%
  filter( YearStart %in% filtered.six.year)

# fitness 300 mins / wk - plot
ggplot(data=filter(fitness, LocationDesc %in% filtered.states.selected), aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
  geom_line() +
  geom_point() +
  xlab("Years") +
  ylab("Percent") + 
  ggtitle("5 hrs/wk moderate-intense aerobic...")


# clean up data more LocationDesc, YearStart, DataValue for adult obesity

obesity = obesity %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
inactivity = inactivity %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
poverty = poverty %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
diabetes = diabetes %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))
#fitness = fitness %>% select (one_of(c("LocationDesc","YearStart", "DataValue")))

# merged adult data
colmerge = c("LocationDesc", "YearStart")

adult.ob = left_join(obesity, inactivity, by = colmerge)
adult.ob = left_join(adult.ob, poverty, by = colmerge)
adult.ob = left_join(adult.ob, diabetes, by = colmerge)
adult.ob = left_join(adult.ob, chci2, by = colmerge)
#adult.ob = left_join(adult.ob, fitness, by = colmerge)


# renaming columns for adultobesity
colnames(adult.ob) = c("state", "year", "obesity", "inactivity", "poverty","diabetes", "chci")


# Linear Regression - 2013 has stat significance
adult.ob2 = filter(filter(adult.ob, year %in% 2013), state %in% filtered.states.selected)
adult.ob2 = filter(adult.ob, year %in% 2013)
#lin.fit = lm(obesity~. -diabetes-year-state, data = adult.ob2) 
lin.fit = lm(obesity~inactivity+poverty+chci, data = adult.ob2) 
summary(lin.fit)

########## Google Vis plot & Shiny - added
# obesity
G1 = gvisGeoChart(filter(adult.ob, year %in% 2011),
                  locationvar = "state", 
                  colorvar = "obesity",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               colorAxis="{colors:[\'#87CEEB\', \'#BE2625\']}",
                               width=800, height=600))

G2 = gvisGeoChart(filter(adult.ob, year %in% 2016),
                  locationvar = "state", 
                  colorvar = "obesity",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               colorAxis="{colors:[\'#87CEEB\', \'#BE2625\']}",
                               width=800, height=600))


J = gvisMotionChart(adult.ob, idvar="state", timevar="year", xvar = "inactivity", yvar="obesity",
                    options=list(width=700, height=600))


# Shiny App
dashHead = dashboardHeader(title = "Menu")
sideBar = dashboardSidebar()
dashBoard = dashboardBody(
  h2("Obesity in 2011"),
  htmlOutput("obesity11"),
  h2("Obesity in 2016"),
  htmlOutput("obesity16"),
  h2("Obesity for Selected States"),
  plotOutput("obesityline", height="500px", width="600px"),
  h2("Motion Chart: Obesity vs ..."),
  htmlOutput("adultOb")
)

ui <- dashboardPage(dashHead,sideBar, dashBoard)

server = function(input, output) {
  
  output$obesity11 <- renderGvis({
    map <- G1
  })
  output$obesity16 <- renderGvis({
    map <- G2
  })
  output$obesityline <- renderPlot({ 
    reactive({
      map <- ggplot(data=filter(obesity, "LocationDesc" %in% filtered.states.selected), aes(x=YearStart, y=DataValue, group=LocationDesc, colour=LocationDesc)) +
        geom_line() +
        geom_point() +
        xlab("Years") +
        ylab("Percent") + 
        ggtitle("Obesity over the Years")
    })
  })
  output$adultOb <- renderGvis({
    map <- J
  })
  # output$lm <- textOutput({
  #   o <- summary(lin.fit)
  # })
}

shinyApp(ui, server)



### shiny
## reference:
# 1. https://shiny.rstudio.com/articles/plot-interaction-advanced.html
# 2. https://shiny.rstudio.com/reference/shiny/0.14/shinyApp.html
# 3. https://shiny.rstudio.com/articles/html-tags.html
# 4. https://magesblog.com/post/2013-02-26-first-steps-of-using-googlevis-on-shiny/




# FAST FOOD
res = read_excel("DataDownload.xls", sheet = "RESTAURANTS")
res = health[,c(2,4,5)]
colnames(health) = c("state", "ff09","ff14")

# getting the sum of each state by year
res = res %>% group_by(state) %>% summarise_each(funs(sum))


# DATASETS
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/