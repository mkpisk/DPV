library("plm")

pop_data <- read.csv("pop_Data.csv", dec = ".")
gdp_data <- read.csv("gdp_Data.csv", dec = ".")

pop_data <- head(pop_data, - 5) 
colnames(pop_data) <- c("Year", 
                        "Year.Code", 
                        "Country.Name", 
                        "Country.Code",
                        "Birth.rate",
                        "Life.expectancy.total",
                        "Life.expectancy.male",
                        "Life.expectancy.female",
                        "Population.%growth",
                        "Population.female",
                        "Population.male",
                        "Population.total",
                        "Population.%male",
                        "Population.%female",
                        "Net.migration")
pop_data = pdata.frame(pop_data,c("Country.Name", "Year"))
pop_data = subset(pop_data, select = -c(Year.Code))

gdp_data <- head(gdp_data, - 5) 
colnames(gdp_data) <- c("Country.Name", 
                        "Country.Code",
                        "Year", 
                        "Year.Code", 
                        "GDP")
gdp_data = pdata.frame(gdp_data,c("Country.Name", "Year"))

mergeddata <- merge(pop_data,gdp_data,by=c("Country.Name","Year"))
datafinal = subset(mergeddata, select = -c(Year.Code, Country.Code.y))
colnames(datafinal) <- c("Country.Name",
                         "Year", 
                         "Country.Code",
                         "Birth.rate",
                         "Life.expectancy.total",
                         "Life.expectancy.male",
                         "Life.expectancy.female",
                         "Population.%growth",
                         "Population.female",
                         "Population.male",
                         "Population.total",
                         "Population.%male",
                         "Population.%female",
                         "Net.migration",
                         "GDP")
str(datafinal)

  #watch out for the 2021 year... we do not have the observations there
summary(datafinal$Year)
levels(datafinal$Year)
datafinal[,4:15] <- sapply(datafinal[, 4:15], as.numeric)
datafinal$Country.Code <- as.factor(datafinal$Country.Code)

str(datafinal)
summary(datafinal[,3:15])

summary(datafinal$Country.Name)
levels(datafinal$Country.Name)

#region separation
data_regions <- subset(datafinal, Country.Name == c("Middle East & North Africa",
                                                    "Sub-Saharan Africa",
                                                    "Europe & Central Asia",
                                                    "East Asia & Pacific",
                                                    "South Asia",
                                                    "Latin America & Caribbean",
                                                    "North America"))
data_migration <- subset(datafinal, !is.na(datafinal$Net.migration[]))









#visualization but we need proper datasets so do not use yet
library(ggplot2)

#popgrowth by birthrate
ggplot(data=datafinal_num, aes(x=Birth.rate, y=Population.percentgrowth))+
  geom_point()+
  geom_smooth() 

#popgrowth by lifeexpectancy
ggplot(data=datafinal_num)+
  geom_point(mapping = aes(x=Life.expectancy.total, y=Population.percentgrowth))+
  facet_wrap(~Year, nrow = 3)

#popgrowth by netmigration
ggplot(data=datafinal_num, aes(x=Net.migration, y=Population.percentgrowth))+
  geom_point()
  

#popgrowth by gdp (not numeric yet)
ggplot(data=datafinal_num, aes(x=GDP, y=Population.percentgrowth))+
  geom_point()
  


#birthrate by lifeexpectancy
ggplot(data=datafinal_num, aes(x=Life.expectancy.total, y=Birth.rate))+
  geom_point(aes(color = Year))
  


#line chart
ggplot(data_regions, aes(x = Year, y = Population.total, group = Country.Name, color = Country.Name)) +
     geom_line()
