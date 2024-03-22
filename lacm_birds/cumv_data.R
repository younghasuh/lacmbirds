library(shiny)
library(ggplot2)
library(tidyverse)
library(here)
library(readxl)

cg <- read_excel(here("lacm_birds", "cumv", "Catharus guttatus.xlsx"))
dc <- read_excel(here("lacm_birds", "cumv", "Dendroica coronata.xlsx"))
ea <- read_excel(here("lacm_birds", "cumv", "Eremophila alpestris.xlsx"))
mm <- read_excel(here("lacm_birds", "cumv", "Melospiza melodia 4.xlsx"))

cumv <- rbind(cg, dc, ea, mm)

# step 1
# merge specimens with multiple prep types into 1 row