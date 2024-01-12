data <- read.csv("Birds_Collection.csv")


data2 <- data %>% 
  select(Catalog.No, Field.No, Sex, LAF.No, Age, Spec.Nat, Measurements, Gonads, Weight, Collector, Date.Coll, Family, Genus, Species, Subspecies, Continent, Country, State, County, Township, Nearest.Named.Place) %>% 
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
         locality = paste(Country, State, County, Township, Nearest.Named.Place, sep = " "),
         state = State,
         county = County
  ) %>% 
  select(lacm, field, sex, laf, age, specnat, measure, gonads, wt, coll, datecoll, family, species, genus, spp, locality, state, county)



data3 <- data2 %>%
  mutate(alcoholic=case_when(specnat == "AL" ~ "complete",
                             specnat == "FA" ~ "complete",
                             specnat == "AC" ~ "partial",
                             specnat == "AO" ~ "partial",
                             specnat == "SA" ~ "partial"),
         skeleton=case_when(specnat == "SN" ~ "complete",
                            specnat == "FB" ~ "complete",
                            specnat == "PS" ~ "partial",
                            specnat == "SO" ~ "partial",
                            specnat == "SB" ~ "partial",
                            specnat == "KB" ~ "partial",
                            specnat == "MS" ~ "partial",
                            specnat == "SK" ~ "partial"),
         skin=case_when(specnat == "SS" ~ "study_skin",
                        specnat == "KB" ~ "study_skin",
                        specnat == "SB" ~ "study_skin",
                        specnat == "SA" ~ "study_skin",
                        specnat == "FB" ~ "flat_skin",
                        specnat == "FS" ~ "flat_skin",
                        specnat == "FA" ~ "flat_skin",
                        specnat == "WO" ~ "flat_skin"),
         body_mount=case_when(specnat == "BM" ~ "mount",
                              specnat == "MS" ~ "mount"))


sp <- "Buteo jamaicensis"
example <- data5 %>% filter(species == sp)
table(example$specnat)
table(example$alcoholic)
table(example$skeleton)
table(example$skin)


states <- get_urbn_map("states", sf = TRUE)

spat_state1 <- state %>% 
  left_join(get_urbn_map(map = "states", sf = TRUE),
            selected() %>% 
              count(state) %>% 
              mutate(state2 = substr(state, 1, 2)),
            by = c("state_abbv" = "state2"))

# need a summary table of specimen types
# how do I want it summarized? 



### 
# Next steps: 
# Within LA County -> plot points / plot by cities / plot major 5 counties surrounding LA
# Plot georeferences points ->note those that don't have georeference
# table of total number of specimen types/sex/other fields..  



############
test <- data5

sp <- "Buteo jamaicensis"
sp <- "Aphelocoma californica"
ex <- test %>% filter(species == sp) #n=178

# compare with original data set
# date filter is stringent 
ex_origin <- data1 %>% 
  dplyr::filter(Genus == "Buteo" & Species == "jamaicensis") #n=220; about 42 w/o dates 


library(sf)

# remove rows where lat field is NA
ex1 <- ex[!is.na(ex$lat),]

# my_sf <- ex1 %>% 
#   st_as_sf(coords = c('lng', 'lat')) %>%
#   st_set_crs(4326)

# usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
# 
# ggplot() + 
#   geom_sf(data = my_sf, aes(), size = 1) + 
#   geom_sf(data = usa, fill = NA)

###
# use 
library(usmap)
transformed_data <- usmap_transform(ex1, input_names = c("lng", "lat"))

plot_usmap("states") + 
  geom_point(data = transformed_data, 
             aes(x = x, y = y), 
             color = "red",
             size = 3) # not sure if points are in alaska or mexico


# try 

###
# for interactive map 
library(leaflet)
leaflet(states) %>%
  addTiles() %>%
  addPolygons()


###########
# test out radio button
test <- data5
sp <- "Buteo jamaicensis"
ex <- test %>% filter(species == sp)

ex %>% filter(spectype_sp == "Round skin")

           spectype_sp == ifelse(input$spectype == "ss", "Study skin",
                                 ifelse(input$spectype == "sk", "Skeleton",
                                        ifelse(input$spectype == "al", "Fluid",
                                               ifelse(input$spectype == "all", ., NA))))))



##########
library(ggiraph)
library(tidyverse)
library(sf)
library(rnaturalearth)

ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("California", "Nevada"))


gg <- ggplot(
  data = ex,
  mapping = aes(
    x = lat, y = lng,
    # here we add iteractive aesthetics
    tooltip = lacm, data_id = lacm
  )
) +
  geom_point_interactive(
    size = 3, hover_nearest = TRUE
  )

# turn as girafe
girafe(ggobj = gg)








###### EXAMPLE #######
# add interactive maps to a ggplot -------
library(ggplot2)
library(ggiraph)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

# create tooltips and onclick events
states_ <- sprintf("<p>%s</p>",
                   as.character(crimes$state) )
table_ <- paste0(
  "<table><tr><td>UrbanPop</td>",
  sprintf("<td>%.0f</td>", crimes$UrbanPop),
  "</tr><tr>",
  "<td>Assault</td>",
  sprintf("<td>%.0f</td>", crimes$Assault),
  "</tr></table>"
)

onclick <- sprintf(
  "window.open(\"%s%s\")",
  "http://en.wikipedia.org/wiki/",
  as.character(crimes$state)
)


crimes$labs <- paste0(states_, table_)
crimes$onclick = onclick

if (require("maps") ) {
  states_map <- map_data("state")
  gg_map <- ggplot(crimes, aes(map_id = state))
  gg_map <- gg_map + geom_map_interactive(aes(
    fill = Murder,
    tooltip = labs,
    data_id = state,
    onclick = onclick
  ),
  map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat)
  x <- girafe(ggobj = gg_map)
  if( interactive() ) print(x)
}


