library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(readxl)
library(tidyverse)


ITFcolours=c("#003e7e","#7EC143","#9e005d","#eea320","#939598","#007dc3","#e2001a","#00909d","#1c9bd1","#00973a","grey","grey20")

# Working directory: anchored on this script's folder (portable across machines)
script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
  }
}, error = function(e) getwd())

input_dir <- script_dir  # all inputs sit alongside this script

## load data from EIB for Probability of Default and Recovery Rates

default_rate <- read_excel(file.path(input_dir, "EIB_PD_RecoveryRates.xlsx"), sheet = 1) %>%
  select(c(Country, `Average annual default rate`)) %>%
  dplyr::rename(default.rate = `Average annual default rate`) %>%
  mutate(Country = case_when(Country == "Palestinian territories" ~ "Palestine",
                             .default = Country))


recovery_rate <- read_excel(file.path(input_dir, "EIB_PD_RecoveryRates.xlsx"), sheet = 2) %>%
  select(c(Country, `Average recovery rate`)) %>%
  dplyr::rename(recovery.rate = "Average recovery rate") %>%
  mutate(Country = case_when(Country == "Palestinian territories" ~ "Palestine",
                             .default = Country))

PD_manual <- data.frame(
  Region = c("Europe", "Central Asia", "Latin America and Caribbean", "South Asia", "Sub-Saharan Africa", "East Asia and Pacific", "Middle East and North Africa"),
  PD_industrial = c(0.0381, 0.0381, 0.0272, 0.036, 0.0813, 0.0699, 0.0838),
  PD_SMEs = c(0.0477, 0.0477, 0.0338, 0.0814, 0.1009, 0.0667, 0.0688)
)


Recovery_rate_manual <- data.frame( ##AVERAGE
  Region = c("Europe", "Central Asia", "Latin America and Caribbean", "South Asia", "Sub-Saharan Africa", "East Asia and Pacific", "Middle East and North Africa"),
  RR_industrial = c(0.671, 0.671, 0.662, 0.672, 0.722, 0.801, 0.672),
  RR_SMEs = c(0.691, 0.691, 0.652, 0.717, 0.811, 0.704, 0.710))

#### grouping to regions #######

wb_regions <- read.csv(file.path(input_dir, "World_bank_regions_adjusted.csv")) %>%
  mutate(Entity = case_when(Entity == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                            Entity == "Turkey" ~ "Türkiye",
                            Entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
                            .default = Entity)) %>%
  dplyr::rename(Region = World.regions.according.to.WB)

default_rate <- default_rate %>%
  left_join(wb_regions %>% select(c(Entity, Region)), by = c("Country" = "Entity"))

recovery_rate <- recovery_rate %>%
  left_join(wb_regions %>% select(c(Entity, Region)), by = c("Country" = "Entity"))

################ calc regional avgs ################

####1. Default rates
default_rate_region <- default_rate %>%
  filter(!is.na(Region)) %>%
  mutate(Region = as.factor(Region)) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(PD = mean(default.rate, na.rm = T)) %>%
  left_join(PD_manual, by = c("Region"))

## Calculating the value for Europe + Central Asia
default_rate_Europe.CentralAsia = default_rate %>%
  filter(Region %in% c("Europe", "Central Asia")) %>% 
  dplyr::summarise(PD = mean(default.rate, na.rm = T)) %>%
  pull(PD)

## Scaling Europe and Central Asia
default_rate_region_adjusted <- default_rate_region %>%
  mutate(PD_industrial = case_when(Region == "Europe"| Region =="Central Asia" ~ PD_industrial*PD/default_rate_Europe.CentralAsia,
                                   .default = PD_industrial),
         PD_SMEs = case_when(Region == "Europe"| Region =="Central Asia" ~ PD_SMEs*PD/default_rate_Europe.CentralAsia,
                             .default = PD_SMEs))

## Including North America
default_rate_region_adjusted <- default_rate_region_adjusted %>%
  add_row(Region = "North America",
          PD = default_rate_region_adjusted$PD[default_rate_region_adjusted$Region == "Europe"],
          PD_industrial = default_rate_region_adjusted$PD_industrial[default_rate_region_adjusted$Region == "Europe"],
          PD_SMEs = default_rate_region_adjusted$PD_SMEs[default_rate_region_adjusted$Region == "Europe"])

### Saving the R data for using in Raroc modeling
# saveRDS(default_rate_region_adjusted, file.path(input_dir, "PD_WB.rds"))

default_rate_region_long <- pivot_longer(default_rate_region_adjusted, 
                                         cols = -Region,
                                         values_to = "PD",
                                         names_to = "type")

ggplot(data=default_rate_region_long)+
  geom_point(aes(x=Region, y=PD, col =type))+
  scale_color_manual(values = ITFcolours) +
  theme_bw()


#### 2. Recovery rates
recovery_rate_region <- recovery_rate %>%
  filter(!is.na(Region)) %>%
  mutate(Region = as.factor(Region)) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(RR = mean(recovery.rate, na.rm = T)) %>%
  left_join(Recovery_rate_manual, by = c("Region"))

## Calculating the value for Europe + Central Asia
recovery_rate_Europe.CentralAsia = recovery_rate %>%
  filter(Region %in% c("Europe", "Central Asia")) %>% 
  dplyr::summarise(RR = mean(recovery.rate, na.rm = T)) %>%
  pull(RR)


## Scaling Europe and Central Asia
recovery_rate_region_adjusted <- recovery_rate_region %>%
  mutate(RR_industrial = case_when(Region == "Europe"| Region =="Central Asia" ~ RR_industrial*RR/recovery_rate_Europe.CentralAsia,
                                   .default = RR_industrial),
         RR_SMEs = case_when(Region == "Europe"| Region =="Central Asia" ~ RR_SMEs*RR/recovery_rate_Europe.CentralAsia,
                             .default = RR_SMEs))

## Including North America
recovery_rate_region_adjusted <- recovery_rate_region_adjusted %>%
  add_row(Region = "North America",
          RR = recovery_rate_region_adjusted$RR[recovery_rate_region_adjusted$Region == "Europe"],
          RR_industrial = recovery_rate_region_adjusted$RR_industrial[recovery_rate_region_adjusted$Region == "Europe"],
          RR_SMEs = recovery_rate_region_adjusted$RR_SMEs[recovery_rate_region_adjusted$Region == "Europe"])

### Saving the R data for using in Raroc modeling
# saveRDS(recovery_rate_region_adjusted, file.path(input_dir, "RecoveryRate_WB-2.rds"))

recovery_rate_region_long <- pivot_longer(recovery_rate_region_adjusted, 
                                         cols = -Region,
                                         values_to = "RR",
                                         names_to = "type")


ggplot(data=recovery_rate_region_long)+
  geom_point(aes(x=Region, y=RR, col =type))+
  #  geom_line(aes(x=Year, y=Value, col=Region))+
  scale_color_manual(values = ITFcolours) +
  theme_bw()


###### ITF regions#####
ISO_convert=read.csv(file.path(input_dir, "ISO_converter.csv"))
ISO_convert=ISO_convert[,c("iso3","Country_name","Fleet_region")]#pick which region to use
names(ISO_convert)=c("iso3","Country","Region")#rename to region

## remove duplicate ISOs and join
ISO_convert=ISO_convert[!ISO_convert$iso3%in%c("MAF","IOA","SMX","RKS","TMP","ZAR","CUW","BES")&!ISO_convert$Country%in%c("Great Britain","Kazakstan")&ISO_convert$Region!="-",]
ISO_convert$Country[ISO_convert$Country=="Korea_ Republic of    "]="South Korea"

ISO_convert <- ISO_convert %>%
  dplyr::mutate(Country = str_trim(Country),
                Country = case_when(Country == "CÃƒÂ´te d'Ivoire" ~ "Côte d'Ivoire",
                                    Country == "Bolivia_ Plurinational State of" ~ "Bolivia",
                                    Country == "Congo_ the Democratic Republic of the" ~ "Democratic Republic of the Congo",
                                    Country == "Moldova_ Republic of" ~ "Moldova",
                                    Country == "Macedonia_ the former Yugoslav Republic of" ~ "North Macedonia",
                                    Country == "Palestinian Territory_ Occupied" ~ "Palestine",
                                    Country == "Russian Federation" ~ "Russia",
                                    Country == "Tanzania_ United Republic of" ~ "Tanzania",
                                    Country == "Turkey" ~ "Türkiye",
                                    Country == "Venezuela_ Bolivarian Republic of" ~ "Venezuela",
                                    Country == "Viet Nam" ~ "Vietnam",
                                    .default = Country))

default_rate_ITF <- default_rate %>%
  select(-Region) %>%
  left_join(ISO_convert %>% 
              select(c(Country, Region)),
            by = c("Country"))


default_rate_region_ITF <- default_rate_ITF %>%
  filter(!is.na(Region)) %>%
  mutate(Region = as.factor(Region)) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(PD = mean(default.rate, na.rm = T))

ggplot(data=default_rate_region_ITF)+
  geom_point(aes(x=Region, y=PD))+
  scale_color_manual(values = ITFcolours) +
  theme_bw()


