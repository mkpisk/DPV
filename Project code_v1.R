install.packages("operators")
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
                        "Population.percentgrowth",
                        "Population.female",
                        "Population.male",
                        "Population.total",
                        "Population.percentmale",
                        "Population.percentfemale",
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
                         "Population.percentgrowth",
                         "Population.female",
                         "Population.male",
                         "Population.total",
                         "Population.percentmale",
                         "Population.percentfemale",
                         "Net.migration",
                         "GDP")

#final data overview
str(datafinal)
summary(datafinal$Year)
levels(datafinal$Year)
datafinal[,4:15] <- sapply(datafinal[, 4:15], as.numeric)
datafinal$Country.Code <- as.factor(datafinal$Country.Code)
str(datafinal)
summary(datafinal[,3:15])
summary(datafinal$Country.Name)
levels(datafinal$Country.Name)


#region & country separation
library(operators)
data_countries <- subset(datafinal, Country.Name %!in% c("Middle East & North Africa",
                                                      "Sub-Saharan Africa",
                                                      "Europe & Central Asia",
                                                      "East Asia & Pacific",
                                                      "South Asia",
                                                      "Latin America & Caribbean",
                                                      "North America",
                                                      "Africa Eastern and Southern",
                                                      "Africa Western and Central",
                                                      "Arab World",
                                                      "Caribbean small states",
                                                      "Central Europe and the Baltics",
                                                      "Channel Islands",
                                                      "Early-demographic dividend",
                                                      "East Asia & Pacific (excluding high income)",
                                                      "East Asia & Pacific (IDA & IBRD countries)",
                                                      "Euro area",
                                                      "European Union",
                                                      "Europe & Central Asia (excluding high income)",
                                                      "Europe & Central Asia (IDA & IBRD countries)",
                                                      "Fragile and conflict affected situations",
                                                      "Heavily indebted poor countries (HIPC)",
                                                      "High income",
                                                      "IBRD only",
                                                      "IDA & IBRD total",
                                                      "IDA blend",
                                                      "IDA only",
                                                      "IDA total",
                                                      "Late-demographic dividend" ,
                                                      "Latin America & Caribbean (excluding high income)",
                                                      "Latin America & the Caribbean (IDA & IBRD countries)",
                                                      "Least developed countries: UN classification",
                                                      "Low & middle income",
                                                      "Low income",
                                                      "Lower middle income",
                                                      "Middle East & North Africa (excluding high income)",
                                                      "Middle East & North Africa (IDA & IBRD countries)",
                                                      "Middle income",
                                                      "North America",
                                                      "Not classified",
                                                      "OECD members",
                                                      "Other small states",
                                                      "Pacific island small states",
                                                      "Post-demographic dividend",
                                                      "Pre-demographic dividend",
                                                      "Small states",
                                                      "South Asia (IDA & IBRD)",
                                                      "Sub-Saharan Africa (excluding high income)",
                                                      "Sub-Saharan Africa (IDA & IBRD countries)",
                                                      "Upper middle income",
                                                      "World"))

data_regions <- subset(datafinal, Country.Name == c("Middle East & North Africa",
                                                    "Sub-Saharan Africa",
                                                    "Europe & Central Asia",
                                                    "East Asia & Pacific",
                                                    "South Asia",
                                                    "Latin America & Caribbean",
                                                    "North America"))

data_migration <- subset(datafinal, !is.na(datafinal$Net.migration[]))

summary(data_countries$Country.Name)
summary(data_regions$Country.Name)
colnames(data_countries)


#visualization
library(ggplot2)
library(dplyr)

#COUNTRIES
#correlation matrix
corrplot(data_countries %>% select_if(is.numeric),abs = F,method="color")





#popgrowth by birthrate
ggplot(data=data_countries, aes(x=Birth.rate, y=Population.percentgrowth))+
  geom_point()+
  geom_smooth() +
  facet_wrap(~Year, nrow = 3)

#popgrowth by lifeexpectancy
ggplot(data=data_countries, aes(x=Life.expectancy.total, y=Population.percentgrowth))+
  geom_point()+
  geom_smooth() +
  facet_wrap(~Year, nrow = 3)

#popgrowth by netmigration
ggplot(data=data_countries, aes(x=Net.migration, y=Population.percentgrowth))+
  geom_point()+
  geom_smooth()

plot(density(data_countries$Net.migration[!is.na(data_countries$Net.migration[])]), main = "Net migration density")
polygon(density(data_countries$Net.migration[!is.na(data_countries$Net.migration[])]), col="red", border="blue")



#popgrowth by gdp (not numeric yet)
ggplot(data=data_countries, aes(x=GDP, y=Population.percentgrowth))+
  geom_point()


#birthrate by lifeexpectancy
ggplot(data=data_countries, aes(x=Life.expectancy.total, y=Birth.rate))+
  geom_point(aes(color = Year))
  


#line chart
ggplot(data_regions, aes(x = Year, y = Population.total, group = Country.Name, color = Country.Name)) +
     geom_line()
