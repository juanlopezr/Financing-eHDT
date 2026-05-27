library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)

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

## load data from Worldbank for operational costs (share of the NII)
OpCost=read.csv(file.path(input_dir, "Operational Costs", "dbc88110-99df-4fbf-8e7d-461a2c5061bb_Data.csv"), stringsAsFactors = FALSE)
OpCost=OpCost[11:nrow(OpCost),c(3:4,6:16)]

colnames(OpCost)[1:2] = c("Country", "iso3")

OpCost  = melt(OpCost, id.vars = c("Country", "iso3"), variable.name="Year", 
               value.name="Value")
OpCost$Year = as.numeric(gsub("X", "",OpCost$Year))
OpCost$Value = as.numeric(OpCost$Value)

#### grouping to regions #######

###### ITF Regions
ISO_convert=read.csv(file.path(input_dir, "ISO_converter.csv"))
ISO_convert=ISO_convert[,c("iso3","Country_name","Fleet_region")]#pick which region to use
names(ISO_convert)=c("iso3","Country","Region")#rename to region

## remove duplicate ISOs and join
ISO_convert=ISO_convert[!ISO_convert$iso3%in%c("MAF","IOA","SMX","RKS","TMP","ZAR","CUW","BES")&!ISO_convert$Country%in%c("Great Britain","Kazakstan")&ISO_convert$Region!="-",]
ISO_convert$Country[ISO_convert$Country=="Korea_ Republic of    "]="South Korea"

OpCost=left_join(OpCost, ISO_convert %>% select(iso3, Region), by="iso3")

#rm small countries outside regions
OpCost = OpCost %>% filter(!is.na(Region))


####### World Bank regions

wb_regions <- read.csv(file.path(input_dir, "World_bank_regions_adjusted.csv")) %>%
  dplyr::rename(Region.wb = World.regions.according.to.WB)

OpCost_WB <- left_join(OpCost, wb_regions %>% 
                         select(Code, Region.wb), by = c("iso3" = "Code"))

#rm small countries outside regions
OpCost_WB = OpCost_WB %>% filter(!is.na(Region.wb))


################ calc regional avgs ################
OpCostShare = OpCost %>% group_by(Year, Region) %>% dplyr::summarise(Value=mean(Value, na.rm = T)/100)

OpCostShare_WB = OpCost_WB %>% group_by(Year, Region.wb) %>% dplyr::summarise(Value=mean(Value, na.rm = T)/100)

ggplot(data=OpCostShare_WB)+
  geom_point(aes(x=Year, y=Value, col=Region.wb))+
  geom_line(aes(x=Year, y=Value, col=Region.wb))+
  scale_color_manual(values = ITFcolours) +
  theme_bw()

OpCostShare = OpCostShare %>% filter(Year>2000)
OpCostShare_WB = OpCostShare_WB %>% filter(Year>2000)


OpCostShare = OpCostShare %>% group_by(Region) %>% dplyr::summarise(Value=mean(Value, na.rm = T))
OpCostShare_WB = OpCostShare_WB %>% group_by(Region.wb) %>% dplyr::summarise(Value=mean(Value, na.rm = T))

# col plot
OpCostShare = OpCostShare %>% arrange(Value) %>%
  mutate(Region = factor(Region, levels = Region))

OpCostShare_WB = OpCostShare_WB %>% arrange(Value) %>%
  mutate(Region.wb = factor(Region.wb, levels = Region.wb))

ggplot(OpCostShare_WB, aes(x = Region.wb, y = Value)) +
  geom_col(fill = ITFcolours[1]) +
  coord_flip() + 
  labs(title = "Operational Cost Share by Region",
       y = "Share of NII [-]") +
  theme_bw()

# saveRDS(OpCostShare_WB, file.path(input_dir, "OperationalCost_WB.rds"))


###################### CB interest rates #################################
CB=read.csv(file.path(input_dir, "CB_interest_rates.csv"), stringsAsFactors = FALSE)

colnames(CB)[1:3] = c("Country", "Value", "iso3")

CB$Value = as.numeric(CB$Value)

# rm Europzone 
# CB_europe = CB %>%  filter(Country == "Eurozone")
CB = CB %>%  filter(!is.na(CB$iso3))
CB = left_join(CB, ISO_convert %>% select(iso3, Region), by="iso3")

CB_WB = left_join(CB, wb_regions %>% 
                    select(Code, Region.wb), 
                  by = c("iso3" = "Code"))


## maybe weight according to GDP or the region

CB = CB %>% group_by(Region) %>% dplyr::summarise(Value=mean(Value, na.rm = T))

CB_WB = CB_WB %>% group_by(Region.wb) %>% dplyr::summarise(Value=mean(Value, na.rm = T))


CB = CB %>% arrange(Value) %>%
  mutate(Region = factor(Region, levels = Region))

CB_europe = CB %>%  filter(Region == "West Europe")

CB_WB = CB_WB %>% arrange(Value) %>%
  mutate(Region.wb = factor(Region.wb, levels = Region.wb),
         Value = case_when(Region.wb == "Europe" ~ CB_europe$Value[1],
                           .default = Value))

# saveRDS(CB_WB, file.path(input_dir, "CentralBank_WB.rds"))


ggplot(CB_WB, aes(x = Region.wb, y = Value)) +
  geom_col(fill = ITFcolours[1]) +
  coord_flip() + 
  labs(title = "Central bank interest rates by Region",
       y = "Interest rates [%]") +
  theme_bw()

