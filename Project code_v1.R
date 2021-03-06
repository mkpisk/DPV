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
datafinal <- datafinal[datafinal$Year!=2020,]
datafinal$Country.Code <- as.factor(datafinal$Country.Code)
str(datafinal)

summary(datafinal)

summary(datafinal[,3:15])
summary(datafinal$Country.Name)
levels(datafinal$Country.Name)

#standarization
pre_datafinal_scaled <- as.data.frame(scale(datafinal[4:15]))
datafinal_scaled <- cbind(datafinal[1:3], pre_datafinal_scaled)
str(datafinal_scaled)

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

data_regions <- subset(datafinal, Country.Name %in% c("Middle East & North Africa",
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

#scaled seperated data
pre_data_countries_scaled <- as.data.frame(scale(data_countries[4:15]))
data_countries_scaled <- cbind(data_countries[1:3], pre_data_countries_scaled)
str(data_countries_scaled)   

pre_data_regions_scaled <- as.data.frame(scale(data_regions[4:15]))
data_regions_scaled <- cbind(data_regions[1:3], pre_data_regions_scaled)
str(data_regions_scaled) 


#visualization
library(ggplot2)
library(dplyr)
library(arm)

#COUNTRIES
#correlation matrix
corrplot(data_countries_scaled %>% select_if(is.numeric),abs=F, n.col.legend = 8)

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


#popgrowth by gdp 
ggplot(data=data_countries, aes(x=GDP, y=Population.percentgrowth))+
  geom_point()+
  geom_smooth() +
  facet_wrap(~Year, nrow = 3)
cor(data_countries$Population.percentgrowth,data_countries$GDP)

#birthrate by lifeexpectancy
ggplot(data=data_countries, aes(x=Life.expectancy.total, y=Birth.rate))+
  geom_point()+
  geom_smooth() +
  facet_wrap(~Year, nrow = 3)  

#net mimgration vs GDP
ggplot(data=subset(data_regions_scaled, Year == 2017), aes(x=GDP, y=Net.migration, color = Country.Name))+
  geom_point()+
  scale_colour_discrete(name="Region")+
  ggtitle("Net migration in relation to GDP in 2017")+
  ylab("Net migration(scaled)") + 
  xlab("GDP (scaled)")


#REGIONS
#total population development in time
ggplot(data_regions, aes(x = Year, y = Population.total, group = Country.Name, color = Country.Name)) +
     geom_line()

#population growth developmet in time
ggplot(data_regions, aes(x = Year, y = Population.percentgrowth, group = Country.Name, color = Country.Name)) +
  geom_line()

#comparison of birthrate
ggplot(data_regions, aes(x = Year, y = Birth.rate, group = Country.Name, color = Country.Name)) +
  geom_line()

#comparison of life expectancy
ggplot(data_regions, aes(x = Year, y = Life.expectancy.total, group = Country.Name, color = Country.Name)) +
  geom_line()

#comparison of GDP
ggplot(data_regions_scaled, aes(x = Year, y = GDP, group = Country.Name, color = Country.Name)) +
  geom_line()+
  scale_colour_discrete(name="Region")+
  ggtitle("GDP from 2000-2019")+
  ylab("GDP(scaled)") + 
  xlab("Year")

#comparison of migration
ggplot(data= data_regions, aes(x=Year, y=Net.migration, color = Country.Name))+
  geom_point()+
  scale_x_discrete(breaks=c(2002, 2007, 2012, 2017))

#GDP vs Net migration
ggplot(data=subset(data_countries, Year == 2017), aes(x=GDP, y=Net.migration), color = Country.Name)+
  geom_point()+
  geom_abline()
abline

# GDP vs Life Expectancy
ggplot(data=data_countries, aes(x=GDP, y=Life.expectancy.total), color = Country.Name)+
  geom_point()+
  geom_smooth()


library(ggplot2)
library(gganimate)
library(transformr)

#GDP vs Life expectancy for regions
p <- ggplot(data=data_regions_scaled, aes(x = GDP, y= Life.expectancy.total, color = Country.Name, size = Population.total)) + 
  geom_point() + 
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 2, colour = 'grey')+
  scale_colour_discrete(name="Region")+
  scale_size_continuous(name = "Size of population (scaled)")+
  ggtitle("Life expectancy in relation to GDP from 2000-2019")+
  xlab("GDP (scaled)") + 
  ylab("Life expectancy (scaled)")
animate(p, fps = 20, width = 800, height = 350)

#GDP vs Life expectancy for countries
p2 <- ggplot(data=data_countries_scaled, aes(x = log(GDP), y= Life.expectancy.total, size = Population.total)) + 
  geom_point() + 
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 1, colour = 'grey')+
  scale_size_continuous(name = "Size of population (scaled)")+
  ggtitle("Life expectancy in relation to GDP from 2000-2019")+
  xlab("GDP (scaled)") + 
  ylab("Life expectancy (scaled)")
animate(p2, fps = 20, width = 800, height = 350)

#GDP procentage growth for regions
p3 <- ggplot(data=data_regions_scaled, aes(x = GDP, y= Population.percentgrowth, color = Country.Name, size = Population.total)) + 
  geom_point() + 
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 2, colour = 'grey')+
  scale_colour_discrete(name="Region")+
  scale_size_continuous(name = "Size of population (scaled)")+
  ggtitle("Population growth in relation to GDP from 2000-2019")+
  xlab("GDP (scaled)") + 
  ylab("Population growth (scaled)")
animate(p3, fps = 20, width = 800, height = 350)


p4 <- ggplot(data=data_countries_scaled, aes(x = GDP, y= Population.percentgrowth, size = Population.total)) + 
  geom_point() + 
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 2, colour = 'grey')+
  scale_size_continuous(name = "Size of population (scaled)")+
  ggtitle("Population growth in relation to GDP from 2000-2019")+
  xlab("GDP (scaled)") + 
  ylab("Population growth (scaled)")
animate(p4, fps = 20, width = 800, height = 350)
  
#netmig vs GDP for regions
p5 <- ggplot(data=data_regions_scaled, aes(x=GDP, y=Net.migration, color = Country.Name, size = Population.total))+
  geom_point()+
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 2, colour = 'grey')+
  scale_colour_discrete(name="Region")+
  scale_size_continuous(name = "Size of population (scaled)")+
  ggtitle("Net migration in relation to GDP from 2000-2019")+
  xlab("GDP (scaled)") + 
  ylab("Net Migration (scaled)")
animate(p5, fps = 20, width = 800, height = 350)

#birthrate vs GDP
p6 <- ggplot(data=data_regions_scaled, aes(x=GDP, y=Birth.rate, color = Country.Name, size = Population.total))+
  geom_point()+
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 2, colour = 'grey')+
  scale_colour_discrete(name="Region")+
  scale_size_continuous(name = "Size of population (scaled)")+
  ggtitle("Birth rate in relation to GDP from 2000-2019")+
  xlab("GDP (scaled)") + 
  ylab("Birth rate (scaled)")
animate(p6, fps = 20, width = 800, height = 350)

#total population vs birth rate
p7 <- ggplot(data=data_regions_scaled, aes(x=Birth.rate, y=Population.total, color = Country.Name))+
  geom_point(size = 5)+
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 2, colour = 'grey')+
  scale_colour_discrete(name="Region")+
  ggtitle("Total population in relation to birth rate from 2000-2019")+
  ylab("Total population (scaled)") + 
  xlab("Birth rate (scaled)")
animate(p7, fps = 20, width = 800, height = 350)

#total pop vs life expectancy
p8 <- ggplot(data=data_regions_scaled, aes(x=Life.expectancy.total, y=Population.total, color = Country.Name))+
  geom_point(size = 5)+
  transition_states(Year, 4, 1) + 
  shadow_mark(size = 2, colour = 'grey')+
  C
  ggtitle("Total population in relation to life expectancy from 2000-2019")+
  ylab("Total population (scaled)") + 
  xlab("Life expectancy (scaled)")
animate(p8, fps = 20, width = 800, height = 350)
