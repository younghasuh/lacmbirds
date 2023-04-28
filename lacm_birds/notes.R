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