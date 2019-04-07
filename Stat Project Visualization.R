library(ggplot2)
library(plotly)
library(gganimate)
library(googleVis)
library(wbstats)
library(data.table)

# Data inserted
data1 <- read.csv("master.csv")
#data2 <- read.csv("who_suicide_statistics.csv")

# Renaming the columns (for making it clearer)
colnames(data1) <- c("Country", "Year", "Sex", "Age", "Suicides_No", "Population", 
                     "Suicides_100k_Pop", "Country_Year", "HDI_for_Year", "GDP_for_year",
                     "GDP_per_capita", "Generation")

#Null values 
sapply(data1, function(x) sum(is.na(x)))

# Rearranging the dataframe
str(data1)
levels(data1$Age) <- c("5-14 years", "15-24 years", "25-34 years", "35-54 years",
                       "55-74 years", "75+ years")
Suicidedf <- data1[order(data1$Country, data1$Year, data1$Age),]
rownames(Suicidedf) <- 1:nrow(Suicidedf)
Suicidedf$GDP_for_year <- as.numeric(gsub(",","",as.character(Suicidedf$GDP_for_year)))

# Merging of data and Summing No of Suicide and population irrespective of Gender
Suicidedf2 <- aggregate(Suicidedf[,c('Population', "Suicides_No")], 
                        by=list(Suicidedf$Country, Suicidedf$Age, Suicidedf$Year, 
                                Suicidedf$GDP_for_year, Suicidedf$GDP_per_capita), 
                        FUN = sum)
colnames(Suicidedf2) <- c("Country", "Age", "Year", "GDP_for_year", "GDP_per_capita",
                          "Population", "Suicides_No")
Suicidedf2 <- Suicidedf2[order(Suicidedf2$Country, Suicidedf2$Age, Suicidedf2$Year),]
rownames(Suicidedf2) <- 1:nrow(Suicidedf2)

# Adding Region to the data
myDT <- data.table(
  wb(indicator = c("SP.POP.TOTL",
                   "SP.DYN.LE00.IN",
                   "SP.DYN.TFRT.IN"), mrv = 60)
)  
# Download country mappings
countries <- data.table(wbcountries())

# join the data sets
setkey(myDT, iso2c)
setkey(countries, iso2c)

# Add regions to the data set, but remove aggregates
myDT <- countries[myDT][ ! region %in% "Aggregates"]

# Reshape data into a wide format
wDT <- reshape(
  myDT[, list(
    country, region, date, value, indicator)], 
  v.names = "value", 
  idvar=c("date", "country", "region"), 
  timevar="indicator", direction = "wide")
# Turn date, here year, from character into integer
wDT[, date := as.integer(date)]
setnames(wDT, names(wDT),
         c("Country", "Region",
           "Year", "Population",
           "Fertility", "LifeExpectancy"))

country_regiondf <- wDT[,c("Country", "Region")]
country_regiondf <- country_regiondf[!duplicated(country_regiondf)]

Suicidedf3 <- merge(Suicidedf2, country_regiondf, by = "Country", all.x = TRUE)

# Check for Null values Again.
sapply(Suicidedf3, function(x) sum(is.na(x)))

# Fill up null values of regions for countries whos data is not present
unique(Suicidedf3$Country[is.na(Suicidedf3$Region)])
Suicidedf3$Region[Suicidedf3$Country == 'Bahamas'] <- "Latin America & Caribbean"
Suicidedf3$Region[Suicidedf3$Country == 'Kyrgyzstan'] <- "Europe & Central Asia"
Suicidedf3$Region[Suicidedf3$Country == 'Macau'] <- "East Asia & Pacific"
Suicidedf3$Region[Suicidedf3$Country == 'Republic of Korea'] <- "East Asia & Pacific"
Suicidedf3$Region[Suicidedf3$Country == 'Saint Kitts and Nevis'] <- "Latin America & Caribbean"
Suicidedf3$Region[Suicidedf3$Country == 'Saint Lucia'] <- "Latin America & Caribbean"
Suicidedf3$Region[Suicidedf3$Country == 'Slovakia'] <- "Europe & Central Asia"
Suicidedf3$Region[Suicidedf3$Country == 'Saint Vincent and Grenadines'] <- "Latin America & Caribbean"

# check for null values again
sapply(Suicidedf3, function(x) sum(is.na(x)))

# Calculate the suicide number for all age group
Suicidedf4 <- aggregate(Suicidedf3[,c('Population', "Suicides_No")], 
                        by=list(Suicidedf3$Country, Suicidedf3$Year, 
                                Suicidedf3$GDP_for_year, Suicidedf3$GDP_per_capita,
                                Suicidedf3$Region), 
                        FUN = sum)
colnames(Suicidedf4) <- c("Country", "Year", "GDP_for_year", "GDP_per_capita", "Region",
                          "Population", "Suicides_No")
Suicidedf4 <- Suicidedf4[order(Suicidedf4$Country, Suicidedf4$Year),]
Suicidedf4 <- Suicidedf4[!(Suicidedf4$Year == 2016),]
rownames(Suicidedf4) <- 1:nrow(Suicidedf4)


# Bubble Plots
M <- gvisMotionChart(Suicidedf4, idvar = "Country",
                     timevar = "Year",
                     xvar = "Suicides_No",
                     yvar = "Population",
                     sizevar = "GDP_for_year",
                     colorvar = "Region")

# Ensure Flash player is available an enabled
plot(M)