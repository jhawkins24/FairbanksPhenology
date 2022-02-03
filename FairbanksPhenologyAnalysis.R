#Code by Jackson Hawkins
#Goals:
#      >Calculate avg. met data stats
#      >Filter phenology data down to usable species
#      >Regress Fairbanks phenology data against met data

#import library
library("tidyverse")
library("lubridate")
library("plyr")
library("dplyr")
library("ggplot2")
library("readxl")
library("writexl")
library("reshape2")
library("weathermetrics", include.only = 'fahrenheit.to.celsius')

#-------------------------------------------------
#Bring in data & clean it up, calc some averages, etc.
#You will have to change the file path (e.g. "Documents/Fall_2021/ES401/FinalData")
# to an appropriate one for your system


#Read in .csv with phenology data as a dataframe
FlowerData_Raw <- read_csv("Documents/Fall_2021/ES401/FinalData/DarwinCore_Master.csv", 
                           col_types = cols(infraspecificEpithet = 'c'))

#Read in .csv with weather data as a dataframe
MetData_Raw <- read_csv("Documents/Fall_2021/ES401/FinalData/StJMaxMin_Temps_1894_2021.csv")

#Add mean daily temperature by averaging max & min
MetData_Raw$Tmean <- apply(MetData_Raw[ ,4:5], 1, mean)

#Create year/month column
MetData_Raw$MonthYear <- paste(MetData_Raw$month, MetData_Raw$year, sep="-")

#Drop NAs
MetData_Clean <- na.omit(MetData_Raw)

#Calculate mean annual temperatures
AnnualMeanTemp <- MetData_Clean %>%
  group_by(year) %>%
  summarise_at(vars(Tmean),
               list(MeanTemp = mean))

#Convert annual mean temp to celsius
AnnualMeanTemp$MeanTemp <- fahrenheit.to.celsius(AnnualMeanTemp$MeanTemp)


#Select only the needed phenology data
FlowerData <- FlowerData_Raw[c('reproductiveCondition',
                               'eventDate',
                               'year',
                               'scientificName')]

#Get list of just flower names in the data set
names <- unique(FlowerData[c('scientificName')])


#-------------------------------------------------
#Add in date & temp data to the dataframe 

#Reformat eventDate as a 'Date' format
FlowerData <- FlowerData %>%
  mutate(eventDate = mdy(eventDate))

#Drop rows without date values
FlowerData <- na.omit(FlowerData)

#Add Day of Year column
FlowerData <- FlowerData %>%
  mutate(DayofYear = yday(eventDate))

#Add the mean annual temp (C) at the Fairbanks Museum to the FlowerData dataframe
FlowerData <- left_join(FlowerData, AnnualMeanTemp, by = "year")

#Filter by in_bloom or fruiting 
FlowerData_Bloom <- FlowerData %>%
  filter(reproductiveCondition == 'in bloom')

#-------------------------------------------------
#Visualize data quality before really digging into it

#Divide bloom data into 5 dfs to plot in a not-overwhelming way
BloomDates1 <- FlowerData_Bloom[1:4019,]

BloomDates2 <- FlowerData_Bloom[4020:8038,]

BloomDates3 <- FlowerData_Bloom[8039:12060,]

BloomDates4 <- FlowerData_Bloom[12061:16093,]

BloomDates5 <- FlowerData_Bloom[16094:20120,]


#Plot presence/absence for those 5 subset df's
ggplot(BloomDates1, aes(year,
                             scientificName))+
  geom_point()+
  labs(x = 'Year',
       y = 'Scientific Name',
       title = "Blooms 1")+
  theme_light()

ggplot(BloomDates2, aes(year,
                     scientificName))+
  geom_point()+
  labs(x = 'Year',
       y = 'Scientific Name',
       title = "Blooms 2")+
  theme_light()


ggplot(BloomDates3, aes(year,
                        scientificName))+
  geom_point()+
  labs(x = 'Year',
       y = 'Scientific Name',
       title = "Blooms 3")+
  theme_light()


ggplot(BloomDates4, aes(year,
                        scientificName))+
  geom_point()+
  labs(x = 'Year',
       y = 'Scientific Name',
       title = "Blooms 4")+
  theme_light()

ggplot(BloomDates5, aes(year,
                        scientificName))+
  geom_point()+
  labs(x = 'Year',
       y = 'Scientific Name',
       title = "Blooms 5")+
  theme_light()


#Plot bar chart of all data in descending order
ggplot(countdf, aes(x = reorder(scientificName, -freq),
                             y = freq))+
  geom_bar(stat = 'identity',
           fill = 'steelblue')+
  labs(x = 'Scientific Name',
       y = '# of Observations',
       title = "All Species")


#Plot presence/absence for data with >50 obs
ggplot(selected_data, aes(year,
                          scientificName))+
  geom_point()+
  labs(x = 'Year',
       y = 'Scientific Name',
       title = "Species w/ obs >= 50")+
  theme_light()

#-------------------------------------------------
#Filter down to desired data

numobs <- 50 #minimum number of observations


#Count how many observations of each species there are
countdf <- count(FlowerData_Bloom, 'scientificName')

#Create list of species with >50 observations
countfilt <- countdf %>%
  filter(freq >= numobs)

#Filter Bloom df down to only data with >50 observations
df_filtered <- FlowerData_Bloom %>% 
  left_join(countdf, 
            by = 'scientificName')%>%
  filter(freq >= numobs)

#Plot bar chart of species with >=50 obs
ggplot(countfilt, aes(x = reorder(scientificName, -freq),
                      y = freq))+
  geom_bar(stat = 'identity',
           fill = 'steelblue')+
  labs(x = 'Scientific Name',
       y = '# of Observations',
       title = "Species, Obs >= 50")


#-------------------------------------------------
#Select species with longest observation periods

#From data wit >= 50 observations, select those seen pre 1912
pre1912 <- df_filtered %>%
  filter(year < 1912)

#Isolate scientific name column as its own df
species_use <- pre1912$scientificName

#Turn df of scientific names into a list
species_list <- as.list(species_use)

#Filter original filtered df by list of species seen pre 1912 to get
# the usable data (yes I know there was probably an easier way to do this)
selected_data <- df_filtered[df_filtered$scientificName %in% species_list,]

#Plot the usable data to make sure there's nothing off in it
ggplot(selected_data, aes(year,
                          scientificName))+
  geom_point()+
  labs(x = 'Year',
       y = 'Scientific Name',
       title = "Species w/ obs >= 50")+
  theme_light()


#Get names of all these species to reference against other species
useful_names <- unique(selected_data[c('scientificName')])


#Create df of the goldstar species (those used in other papers and in selected_data)
goldstar <- selected_data %>%
  filter(scientificName == "Cornus alternifolia" |
           scientificName == "Cornus canadensis" |
           scientificName == "Prunus pensylvanica" |
           scientificName == "Prunus virginiana" |
           scientificName == "Sanguinaria canadensis" |
           scientificName == "Uvularia sessilifolia")


#-------------------------------------------------
#Plot regressions of bloom dates & met data

#Plot blooms over time of goldstar species
ggplot(goldstar, aes(year, 
                     DayofYear,
                     color = scientificName))+
  geom_point()+
  geom_smooth(aes(color = scientificName),
              se = F,
              method = "lm")+
  labs(x = 'Year',
       y = 'Bloom Date (Day of Year)',
       title = "Gold Star Species Blooms Over Time")+
       guides(color = guide_legend(title = "Species"))+
  scale_color_viridis_d()


#Species of interest
species <- 'Prunus virginiana' #Change the species name here to desired species


#Create a dataframe with only the plant species defined above
filt_flowers <- FlowerData_Bloom %>%
  filter(scientificName == species)

#Plot blooms over time of selected species
ggplot(filt_flowers, aes(year, 
                         DayofYear))+
  geom_point()+
  geom_smooth(se = F)+
  labs(x = 'Year',
       y = 'Bloom Date (Day of Year)',
       title = species)


#Plot selected species against mean annual temp.
ggplot(filt_flowers, aes(MeanTemp,
                         DayofYear))+
  geom_point()+
  geom_smooth(se = F,
              method = 'lm')+
  labs(x = 'Mean Annual Temp. (°C)',
       y = 'Bloom Date (DOY)',
       title = species)

#Run a linear regression to understand relationship between MAAT & Bloom date of the species
lmSpc = lm(DayofYear~MeanTemp, data = filt_flowers)
summary (lmSpc)


#-------------------------------------------------
#Export data & figures

#Save the plot!
ggsave("Pvirginiana_regress.png", width = 6, height = 4, path = "Documents/Fall_2021/ES401/JTermContinuation/Figures")

#Export df as excel
# write_xlsx(useful_names, path = "Documents/Fall_2021/ES401/JTermContinuation/FilteredNames.xlsx")

