library(shiny)
library(ggplot2)
library(tidyverse)
library(here)
library(readxl)
library(plotly)

cg <- read_excel(here("lacm_birds", "cumv", "Catharus guttatus_19 april.xlsx"))
dc <- read_excel(here("lacm_birds", "cumv", "Dendroica coronata_19 april.xlsx"))
ea <- read_excel(here("lacm_birds", "cumv", "Eremophila alpestris_19 april.xlsx"))
mm <- read_excel(here("lacm_birds", "cumv", "Melospiza melodia_19 april.xlsx"))

# merge into one dataset 
cmv <- rbind(cg, dc, ea, mm)


# rename columns for v2
cmv <- cmv %>% 
  mutate(catalog = `Cat Num`,
         date = `Began Date`,
         sp = Species,
         country = Country,
         county = County,
         state = State,
         loc = `Spec Locality`,
         bursa = Bursa,
         ossif = `Skull Ossification`,
         fat = `Fat Deposition`,
         repro = `Reproduct Data`,
         molt = Molt,
         ws = `Wing Span`,
         wt = Weight,
         tl = `Total Length`,
         wc = `Wing Chord`,
         preparations = `Preparations [Aggregated]`,
         lat = Latitude1,
         lng = Longitude1) %>% 
  select(catalog, Genus, sp, date, sex, country, state, county, loc, bursa, ossif, fat, repro, molt, ws, wt, tl, wc, preparations, lat, lng)




#
setwd("~/lacmbirds/lacm_birds")

write.csv(cmv, "cmv_v2.csv")
cmv <- read.csv("cmv.csv")


#########
# step 1 ----
# explore the data set
# 1) sex
table(cmv$sex) # change into 3 categories; F, M, and U. Only definitive F & M will be set as such

cmv$sex <- ifelse(cmv$Sex == "Female", "F", 
                  ifelse(cmv$Sex == "Male", "M", "U"))

table(cmv$sex)

# 2) date
cmv$date <- as.Date(cmv$date, format = "%m/%d/%Y")

# 3) other
table(cmv$fat)
table(cmv$preparations)

# step 2 ----
# remove duplicate rows; some rows have different collector names even though everything else is identical
# for this purpose, just select the first row 
cmv2 <- cmv %>% 
  group_by(catalog) %>% 
  slice(1) %>% as.data.frame()

# Check if it worked -- yes
dupcheck <- cmv2 %>% 
    group_by(catalog) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)


# step 3 ----
# remove eggs and nest in preparation type
cmv3 <- cmv2 %>% 
  filter(!grepl("nest", preparations)) %>% 
  filter(!grepl("egg", preparations))

table(cmv3$preparations)

# step 4 ----
# extract prep type into separate columns (and merge back later??)
cmv4 <- cmv3 %>% 
  mutate(
    bind_rows(lapply(
      strsplit(preparations, ";"),
      function(S) as.data.frame(lapply(setNames(nm = c("skin", "skeleton", "tissue", "wing", "ethanol")), 
                                       function(z) paste0(grep(pattern = z, x = S, value = TRUE), collapse = ";"))))),
    across(one_of(c("skin", "skeleton", "tissue", "wing", "ethanol")), ~ ifelse(nzchar(.), ., .[NA]))
  )


# step 5 ----
# change variable names to match that of data (simplify)
cmv5 <- cmv4 %>% 
  mutate(species = paste(Genus, sp, sep=" ")) %>% 
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

# add family and order for CUMV
cmv5$order <- "Passeriformes"

cmv5$family <- NA
cmv5$family[cmv5$species == "Catharus guttatus"] <- "Turdidae"
cmv5$family[cmv5$species == "Eremophila alpestris"] <- "Alaudidae"
cmv5$family[cmv5$species == "Melospiza melodia"] <- "Passerellidae"
cmv5$family[cmv5$species == "Setophaga coronata"] <- "Parulidae"

cmv5$loc <- cmv5$locality

cmv5$locality <- paste(cmv5$country, cmv5$state, cmv5$county, cmv5$loc, sep = "; ")

cmv5$Sex <- cmv5$sex

# change data type
cmv5$catalog <- as.integer(cmv5$catalog)
cmv5$wt <- as.numeric(cmv5$wt)
cmv5$ws <- as.numeric(cmv5$ws)
cmv5$wc <- as.numeric(cmv5$wc)
cmv5$tl <- as.numeric(cmv5$tl)
cmv5$lat <- as.numeric(cmv5$lat)
cmv5$lng <- as.numeric(cmv5$lng)

#cmv5$sp <- cmv5$species
#cmv5$species <- cmv5$fullname

#cmv5 <- cmv5 %>% select(catalog, species, genus, date, sex, country, state, county, loc, locality, bursa, ossif, fat, repro, molt, ws, wt, tl, wc, 
#                        preparations, lat, lng, skin, skeleton, tissue, wing, ethanol, year, month, decade, collection, order, family, Sex)

# write new data file
write.csv(cmv5, "cmv_edit.csv")


# change in cols for data
data <- read.csv("data_20240328.csv")

data$date <- as.Date(data$date, format = "%Y-%m-%d")

data$collection <- "LACM"

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

lacmd$date <- as.Date(lacmd$date, format = "%Y-%m-%d")


##
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

table(fullda$description)
table(fullda$Description)

table(fullda$skin)
table(fullda$skel)
table(fullda$ethanol)

# formatting issues
fullda[which(fullda$skin == "study skin"),]$skin <- "skin_study"
fullda[which(fullda$skin == " study skin"),]$skin <- "skin_study"
fullda[which(fullda$skin == "flat skin"),]$skin <- "skin_flat"
fullda[which(fullda$skin == "mounted skin"),]$skin <- "skin_mount"

table(fullda$skin)


fullda[which(fullda$skeleton == "partial skeleton"),]$skeleton <- "skel_part"
fullda[which(fullda$skeleton == " partial skeleton"),]$skeleton <- "skel_part"
fullda[which(fullda$skeleton == "complete skeleton"),]$skeleton <- "skel_full"
fullda[which(fullda$skeleton == " skeleton"),]$skeleton <- "skel_full"
fullda[which(fullda$skeleton == "skeleton"),]$skeleton <- "skel_full"

table(fullda$skeleton)

table(fullda$ethanol)
fullda[which(fullda$ethanol == "whole organism (ethanol)"),]$ethanol <- "fluid_full"
fullda[which(fullda$ethanol == " whole organism (ethanol)"),]$ethanol <- "fluid_full"
fullda[which(fullda$ethanol == "complete fluid"),]$ethanol <- "fluid_full"
fullda[which(fullda$ethanol == "tissue (95% ethanol)"),]$ethanol <- "tissue"
fullda[which(fullda$ethanol == " tissue (95% ethanol)"),]$ethanol <- "tissue"
fullda[which(fullda$ethanol == "tissue (95% ethanol); whole organism (ethanol)"),]$ethanol <- "fluid_full"

table(fullda$ethanol)

table(fullda$Description)
fullda[which(fullda$Description == "entire specimen preserved in alcohol"),]$ethanol <- "fluid_full"
fullda[which(fullda$Description == "study skin with body (trunk) skeleton"),]$skin <- "skin_study"
fullda[which(fullda$Description == "flat skin and complete skeleton"),]$skin <- "skin_flat"
fullda[which(fullda$Description == "flat skin and complete skeleton"),]$skeleton <- "skel_full"
fullda[which(fullda$Description == "study skin with spread wing"),]$skin <- "skin_study"
fullda[which(fullda$Description == "wings only"),]$skin <- "skin_wing"
fullda[which(fullda$Description == "taxidermied body mount"),]$skin <- "skin_mount"
fullda[which(fullda$Description == "tissue only"),]$ethanol <- "tissue"

table(fullda$skin, fullda$skeleton)


# limit to 2 decimal points
fullda$wt <- format(round(fullda$wt, 2), nsmall=2)
fullda$wt <- as.numeric(fullda$wt)

fullda <- fullda %>% 
  dplyr::mutate(Sex = replace_na(Sex, "U"))

table(fullda$Sex)


fullda2 <- fullda %>% 
  select(name, catalog, collection, species, genus, order, family, date, Sex, country, state, county, loc, locality, wt, lat, lng, 
         skin, skeleton, tissue, ethanol, wing, year, month, decade, laf)




###########
write.csv(fullda2, "merged_data.csv")

fullda <- read.csv("merged_data.csv")

# remove weird outlier
md3 <- md2 %>% 
   filter(wt < 1200)
md2 <- md2 %>% filter(wt < 90)
md2$country <- str_to_title(md2$country)
table(md2$country)

write.csv(md2, "merged_data2.csv")

#####
## testing
test <- fullda %>% filter(species == "Melospiza melodia") #1294 specimens
table(test$skin) #1181
table(test$skeleton) #83

test_filt <- test %>% filter(!is.na(skin) | !is.na(skeleton)) 

table(test$collection, test$skin)
table(test$collection, test$skeleton)
table(test$collection, test$ethanol)

reorg <- test %>% 
  group_by(collection, year) %>% count(skin, skeleton, ethanol) %>% 
  pivot_longer(c(skin, skeleton, ethanol), names_to = "type", values_to = "count") %>% drop_na() %>% 
  mutate(decade = floor(year/10)*10) %>% 
  group_by(decade, type) %>% 
  summarise(total = sum(n))

# plot long format by year
ggplot(reorg, aes(x = decade, y=total, fill = type)) + 
   geom_col(position=position_dodge2(preserve="single")) +
   scale_x_continuous(breaks=unique(reorg$decade)) +
   scale_fill_manual(values=c("seagreen", "orange", "cornflowerblue")) +
   theme_classic() +
   labs(fill = "Specimen type", color = "Specimen type", x = "Decade", y = "Count")

# split  by collex
reorg2 <- test %>% 
  group_by(collection, year) %>% count(skin, skeleton, ethanol) %>% 
  pivot_longer(c(skin, skeleton, ethanol), names_to = "type", values_to = "count") %>% drop_na() %>% 
  mutate(decade = floor(year/10)*10) %>% 
  group_by(collection, decade, type) %>% 
  summarise(total = sum(n))

# doesn't look good
ggplot(reorg2, aes(x = decade, y=total, fill = type, color=collection)) + 
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(breaks=unique(reorg$decade)) +
  scale_fill_manual(values=c("seagreen", "orange", "cornflowerblue")) +
  theme_classic() +
  labs(fill = "Specimen type", color = "Specimen type", x = "Decade", y = "Count")
  



###########
# using measurement data

ggplot(test, aes(x=Sex, y=wt)) +
  stat_boxplot(geom="errorbar", position="dodge2") +
  geom_boxplot(stat = "boxplot",
               position = "dodge2") + 
  geom_point(shape=16, alpha=0.4, position=position_jitter(0.2)) +
  theme_minimal() +
  scale_x_discrete(limits = c("M", "F", "U"), labels = c("Male", "Female", "Unknown")) +
  labs(x = "Sex", y = "Weight (g)")

# use plot_ly
plot_ly(test, x = test$Sex, y = test$wt, type = "box", 
        boxpoints = "all", jitter = 0.8,
        pointpos = 0, marker = list(size = 5),
        hoverinfo = "text",
        text = ~paste(test$name, "; ", test$wt, "g", sep=""))  %>%
  layout(boxmode = "group",
         xaxis = list(title='Grouping'),
         yaxis = list(title='Weight (g)'))

# hover yields summary
# but cannot do text (individual ID)
plot_ly(test, x = test$Sex, y = test$wt, type = "box", boxmean = T,
        boxpoints = "all", jitter = 0.8,
        pointpos = 0, marker = list(size = 5))  %>%
  layout(boxmode = "group",
         xaxis = list(title='Grouping'),
         yaxis = list(title='Weight (g)'))


# using symbols to separate by collection
plot_ly(test, x = test$Sex, y = test$wt, symbol = test$collection, type = "box",
        boxpoints = "all", jitter = 0.8,
        pointpos = 0, marker = list(size = 5),
        hoverinfo = "text",
        text = ~paste(test$name, "; ", test$wt, "g", sep=""))  %>%
  layout(boxmode = "group",
         xaxis = list(title='Grouping'),
         yaxis = list(title='Weight (g)'))



# using symbols to separate by state
plot_ly(test, x = test$state, y = test$wt, symbol = test$Sex, type = "box",
        boxpoints = "all", jitter = 0.8,
        pointpos = 0, marker = list(size = 5),
        hoverinfo = "text",
        text = ~paste(test$name, "; ", test$wt, "g", sep=""))  %>%
  layout(boxmode = "group",
         xaxis = list(title='Grouping'),
         yaxis = list(title='Weight (g)'))


# using trace to compare two species
test2 <- fullda %>% filter(species == "Eremophila alpestris")

plot_ly(test, x = test$Sex, y = test$wt, type = "box", boxmean = T, name=test$species,
        boxpoints = "all", jitter = 0.8,
        pointpos = 0, marker = list(size = 5))  %>%
  add_trace(test2, x = test2$Sex, y = test2$wt, name = test2$species) %>% 
  layout(boxmode = "group",
         xaxis = list(title='Grouping'),
         yaxis = list(title='Weight (g)'))


##########
li <- sort(unique(unlist(fullda$species, use.names = FALSE)))

ui <- fluidPage(
  selectizeInput(inputId = 'sp1', label = 'Species 1', choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
  selectizeInput(inputId = 'sp2', label = 'Species 2', choices = NULL, selected = NULL, multiple = FALSE, options = NULL),
  
  plotlyOutput("plot")

  )

server <- shinyServer(function(input, output, session) {
  updateSelectizeInput(session, "sp1", choices = li, selected=character(0), server = TRUE)
  updateSelectizeInput(session, "sp2", choices = li, selected=character(0), server = TRUE)
  
  selected <- reactive(fullda %>% filter(species == req(input$sp1)))
  selected2 <- reactive(fullda %>% filter(species == req(input$sp2)))
  
  output$plot <- renderPlotly({
    dat <- selected()
    dat2 <- selected2()
    plot_ly(dat, x = dat$Sex, y = dat$wt, type = "box", boxmean = T, name = dat$species,
            boxpoints = "all", jitter = 0.8,
            pointpos = 0, marker = list(size = 5),
            hoverinfo = "text")  %>%
      add_trace(dat2, x = dat2$Sex, y = dat2$wt, name = dat2$species) %>% 
      layout(boxmode = "group",
             xaxis = list(title='Grouping'),
             yaxis = list(title='Weight (g)'))
  })
})

shinyApp(ui = ui, server = server)


server <- shinyServer(function(input, output, session) {
  updateSelectizeInput(session, "sp1", choices = li, selected=character(0), server = TRUE)
  updateSelectizeInput(session, "sp2", choices = li, selected=character(0), server = TRUE)
  
  selected <- reactive(fullda %>% filter(species == req(input$sp1)))
  selected2 <- reactive(fullda %>% filter(species == req(input$sp2)))
  
  output$plot <- renderPlotly({
    dat <- selected()
    dat2 <- selected2()
    plot_ly(dat, x = dat$Sex, y = dat$wt, type = "box", boxmean = T, name = dat$species,
            boxpoints = "all", jitter = 0.8,
            pointpos = 0, marker = list(size = 5))  %>%
      add_trace(dat2, x = dat2$Sex, y = dat2$wt, name = dat2$species) %>% 
      layout(boxmode = "group",
             xaxis = list(title='Grouping'),
             yaxis = list(title='Weight (g)'))
  })
})
