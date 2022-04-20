#============Load all the packages needed================

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(skimr)
library(janitor)
library(sf) #Simple feature
library(ggspatial) #make maps fancy

#============Load and subset data================


#loaded as a tibble
ICMP.as.imported.df <- read_csv("ICMP-export-8-mar-2022.csv",
                                guess_max = Inf,
                                show_col_types = FALSE)

#subset Aspergillus
asp.df <- ICMP.as.imported.df %>%
distinct(AccessionNumber, .keep_all= TRUE) %>% #remove dupes
  filter(str_detect(CurrentName, "^Aspergillus")) %>%
  filter(Deaccessioned == "FALSE") %>%
  filter(Country == "New Zealand") %>%
  mutate(date.isolated = ymd(IsolationDateISO, truncated = 3)) %>%
  mutate(date.collected = ymd(CollectionDateISO, truncated = 3)) %>%
  glimpse()


#============data checking================


asp.df %>%
  select("AccessionNumber",
         "StandardCollector",
         "date.collected",
         "date.isolated",
         "CurrentName",
         "VerbatimLocality",
         "DecimalLat", 
         "DecimalLong", 
         "BiostatusDescription", 
         "OccurrenceDescription") %>%
  write_csv(file='./outputs/ICMP/NZAspergillus.csv')



#============Graphics================


#Species of New Zealand Aspergillus cultures in the ICMP
ggplot(asp.df, aes(CurrentName)) +
  labs(title = "Species of New Zealand Aspergillus cultures in the ICMP") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=0, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/ICMP/ASP-species-new.png', width=7, height=7)


#plot species with a fill
ggplot(asp.df, aes(CurrentName, fill=NZAreaCode)) +
  labs(title = "Species of New Zealand Aspergillus cultures in the ICMP") +
  labs(x = "Taxon", y = "number of isolates") +
  theme(axis.text.x=element_text(angle=0, hjust=0)) +
  geom_bar() +
  coord_flip()
ggsave(file='./outputs/ICMP/ASP-species-fill-new.png', width=7, height=7)


#plot over time
ggplot(asp.df, aes(date.collected, fill = TaxonName_C2)) +
  labs(title = "Isolation dates of Aspergillus ICMP cultures") +
  labs(x = "Date of isolation", y =  "Number of cultures" , fill = "") +
  theme_bw() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  geom_histogram(binwidth=365.25)
ggsave(file='./outputs/ICMP/ASP-isolation-dates-new.png', width=5, height=5)


#============Maps================

# Loading in data 
#Reading in a NZ specific map
nz.sf <- st_read(dsn = "./data/nz-coastlines-topo-150k/nz-coastlines-topo-150k.shp", quiet = FALSE) %>%
  st_transform(2193) #Setting map projection - NZGD2000

#Transforming to an SF object
asp.sf <- asp.df %>%
  filter(!is.na(DecimalLat)) %>% #Removing missing obs as sf doesn't play with these
  st_as_sf(coords = c("DecimalLong", "DecimalLat")) %>% #Defining what the coord columns are
  st_set_crs(4326) %>% #Telling sf it is in WSG84 projection
  st_transform(2193) %>% #Changing it to NZGD2000 to match coastline polygon
  st_crop(st_bbox(nz.sf)) #Cropping out points that are outside the coastline polygons bounding box (e.g. not NZ)

#Plotting - takes a second to execute
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = asp.sf, aes(colour = CurrentName),
          size = 2, alpha = 0.8, show.legend = TRUE) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  labs(title = "Collection location of ICMP Aspergillus cultures",
       caption = "Bevan Weir - 25 Feb 2022 - CC BY 4.0")

#this is too slow so need to build in a delay
ggsave(file='./outputs/ICMP/Aspergillus-NZ-ICMP.pdf', width=8, height=10)







