library(shiny)
library(ggplot2)
library(tidyverse)
library(here)
library(readxl)

# cg <- read_excel(here("lacm_birds", "cumv", "Catharus guttatus.xlsx"))
# dc <- read_excel(here("lacm_birds", "cumv", "Dendroica coronata.xlsx"))
# ea <- read_excel(here("lacm_birds", "cumv", "Eremophila alpestris.xlsx"))
# mm <- read_excel(here("lacm_birds", "cumv", "Melospiza melodia 4.xlsx"))
# 
# cmv <- rbind(cg, dc, ea, mm)
# write.csv(cmv, "cmv.csv")


setwd("~/lacmbirds/lacm_birds")

cmv <- read.csv("cmv.csv")

#########
# step 1 ----
# explore the data set
# 1) sex
table(cmv$Sex) # change into 3 categories; F, M, and U. Only definitive F & M will be set as such

cmv$sex <- ifelse(cmv$Sex == "Female", "F", 
                  ifelse(cmv$Sex == "Male", "M", "U"))

table(cmv$sex)

# 2) date
cmv$date <- as.Date(cmv$Began.Date, format = "%m/%d/%Y")

# 3) other
table(cmv$Fat.Deposition)
table(cmv$Weight.unit)
table(cmv$Preparations..Aggregated.)

# step 2 ----
# remove duplicate rows; some rows have different collector names even though everything else is identical
# for this purpose, just select the first row 
cmv2 <- cmv %>% 
  group_by(Cat.Num) %>% 
  slice(1) %>% as.data.frame()

# Check if it worked -- yes
dupcheck <- cmv2 %>% 
    group_by(Cat.Num) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)


# step 3 ----
# remove eggs and nest in preparation type
cmv3 <- cmv2 %>% 
  filter(!grepl("nest", Preparations..Aggregated.)) %>% 
  filter(!grepl("egg", Preparations..Aggregated.))

table(cmv3$Preparations..Aggregated.)

# step 4 ----
# extract prep type into separate columns (and merge back later??)
cmv4 <- cmv3 %>% 
  select(Cat.Num, Genus, Species, date, sex, Country, County, State, Spec.Locality, Skull.Ossification, Weight, Total.Length, Wing.Span, Wing.Chord, Preparations..Aggregated.) %>%
  mutate(
    bind_rows(lapply(
      strsplit(Preparations..Aggregated., ";"),
      function(S) as.data.frame(lapply(setNames(nm = c("skin", "skeleton", "tissue", "wing", "ethanol")), 
                                       function(z) paste0(grep(pattern = z, x = S, value = TRUE), collapse = ";"))))),
    across(one_of(c("skin", "skeleton", "tissue", "wing", "ethanol")), ~ ifelse(nzchar(.), ., .[NA]))
  )


# step 5 ----
# change variable names to match that of data (simplify)
cmv5 <- cmv4 %>% 
  mutate(catalog = Cat.Num,
         ossif = Skull.Ossification,
         loc = Spec.Locality,
         wt = Weight,
         tl = Total.Length,
         ws = Wing.Span,
         wc = Wing.Chord,
         Description = Preparations..Aggregated.,
         species = paste(Genus, Species, " ")) %>% 
  select(catalog, Genus, species, sex, date, Country, County, State, loc, tl, ws, wc, ossif, wt, Description, skin, skeleton, tissue, wing, ethanol) %>% 
  rename_all(tolower) # colnames to lowercase

cmv5$species <- trimws(cmv5$species) # remove all white space before/after string

# extract year and month from date
cmv5$year <- as.numeric(format(as.Date(cmv5$date), "%Y"))
cmv5$month <- as.numeric(format(as.Date(cmv5$date), "%m"))

cmv5 <- cmv5 %>% mutate(decade = floor(year/10)*10)  

# step 6 ----
# Combine two data sets
# add collection code for both
cmv5$collection <- "CUMV"
data$collection <- "LACM"

# add family and order for CUMV
cmv5$order <- "Passeriformes"

cmv5$family <- NA
cmv5$family[cmv5$species == "Catharus guttatus"] <- "Turdidae"
cmv5$family[cmv5$species == "Eremophila alpestris"] <- "Alaudidae"
cmv5$family[cmv5$species == "Melospiza melodia"] <- "Passerellidae"
cmv5$family[cmv5$species == "Setophaga coronata"] <- "Parulidae"

cmv5$locality <- paste(cmv5$country, cmv5$state, cmv5$county, cmv5$loc, sep = " ")

cmv5$Sex <- cmv5$sex

# write new data file
write.csv(cmv5, "cmv_edit.csv")


# change in cols for data
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# extract 4 species from data
lacmd <- data %>% filter(species == "Catharus guttatus" |
                         species == "Eremophila alpestris" |
                         species == "Melospiza melodia" |
                         species == "Dendroica coronata") 

# change species name
lacmd$species[lacmd$species == "Dendroica coronata"] <- "Setophaga coronata"

# change outdated family
lacmd$family[lacmd$family == "Emberizidae"] <- "Passerellidae"
lacmd$family[lacmd$family == "Muscicapidae"] <- "Turdidae"

# change to catalog
lacmd$catalog <- lacmd$lacm

# change county spell
lacmd$county <- lacmd$cty2
#ct <- get_urbn_map(map = "counties", sf = TRUE)


# change state spell
# ma <- get_urbn_map(map = "states", sf = TRUE)
table(lacmd$state)
table(lacmd$abv)
table(cmv5$state)

lacmd$state <- str_to_title(lacmd$state)
table(lacmd$state)

lacmd$description <- lacmd$Description

# merge - use full_join to account for differences in colnames
fullda <- full_join(cmv5, lacmd) # n = 3574

table(fullda$species)
table(fullda$family)
table(fullda$order)
table(fullda$sex)
table(fullda$Sex)

fullda$Sex <- replace_na(fullda$Sex, "U") # replace NA with U
table(fullda$Sex) #use this 


fullda$name <- paste(fullda$collection, fullda$catalog, sep = " ")

write.csv(fullda, "merged_data.csv")
