# Run this step for every new download from EMu

# loan libraries
library(tidyverse)

# set working directory
setwd("~/lacmbirds/lacm_birds")


# load data sets
data0 <- read.csv("birds_2023.csv")
specnat <- read.csv("specnat.csv")
sta <- read.csv("states.csv")


data2 <- data0 %>% 
  select(Catalog.No, Field.No, Sex, LAF.No, Age, Spec.Nat, Measurements, Gonads, Weight, Collector, Date.Coll, Order, Family, Genus, Species, Subspecies, Continent, Country, State, County, Township, Nearest.Named.Place,
         Elevation, Latitude.Dec, Longitude.Dec) %>% 
  mutate(lacm = Catalog.No,
         field = Field.No,
         sex = Sex,
         laf = LAF.No,
         age = Age, 
         specnat = Spec.Nat,
         measure = Measurements,
         gonads = Gonads,
         wt = as.numeric(Weight),
         coll = Collector,
         datecoll = Date.Coll,
         species = paste(Genus, Species, sep = " "),
         spp = Subspecies,
         genus = Genus,
         family = Family,
         order = Order,
         locality = paste(Country, State, County, Township, Nearest.Named.Place, sep = " "),
         state = State,
         county = County,
         country = Country,
         ele = Elevation,
         lat = Latitude.Dec,
         lng = Longitude.Dec
  ) %>% 
  select(lacm, field, sex, laf, age, specnat, measure, gonads, wt, coll, datecoll, order, family, species, genus, spp, locality, country, state, county, ele, lat, lng)




# transform date into an actual date category
data2$date <- as.Date(data2$datecoll, format="%d %B %Y")

data2$year <- as.numeric(format(data2$date, "%Y"))
data2$month <- as.numeric(format(data2$date, "%m"))

# remove odd dates => skip this step for now; include all odd dates
# data3 <- data2 %>% filter(date < "2100-01-01")

# for specnat, combine with spelled out types
data4 <- data2 %>% 
  left_join(specnat, by="specnat")

# add state abbreviations
data5 <- data4 %>% 
  left_join(sta, by = c("state" = "full"))

# change county input 
## county is in all caps -> change to just first letter cap only to match urbn county map
data5$cty <- str_to_title(data5$county) 
data5$cty2 <- gsub(" Co", " County", data5$cty)

data <- data5

# add specimen nature category (simplified into 4 types)
data$nat <- ifelse(data$specnat == "AL" |
                     data$specnat == "AC" |
                     data$specnat == "AO", "fluid",
                   ifelse(data$specnat == "SS" |
                            data$specnat == "SW" |
                            data$specnat == "SA" |
                            data$specnat == "KB", "study skin",
                          ifelse(data$specnat == "SN" |
                                   data$specnat == "SNB" |
                                   data$specnat == "SNW" |
                                   data$specnat == "SO", "skeleton",
                                 "other")))

data <- data %>% mutate(decade = floor(year/10)*10)

# make sex consistent
data <- data %>% 
  mutate(Sex = toupper(sex),
         Sex = replace_na(Sex, "U"),
         Sex = sub("^$", "U", Sex))

# add unassigned for age category
data <- data %>% 
  mutate(age = sub("^$", "Unassigned", age))


write.csv(data, "data_20240328.csv")



check <- data %>% filter(year < 1800)
